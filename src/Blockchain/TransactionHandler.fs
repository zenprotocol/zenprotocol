module Blockchain.TransactionHandler

open Blockchain
open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction
open Consensus.TransactionValidation
open Consensus.Types
open State
open Logary.Message

let getUTXO = UtxoSetRepository.get

let private validateOrphanTransaction chainParams session contractPath blockNumber state txHash tx  =
    effectsWriter
        {
            match TransactionValidation.validateInContext chainParams (getUTXO session) contractPath (blockNumber + 1ul)
                    state.activeContractSet state.utxoSet txHash tx with
            | Ok (tx, acs) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                do! publish (TransactionAddedToMemPool (txHash, tx))

                eventX "Orphan transaction {hash} added to mempool"
                >> setField "hash" (Hash.toString txHash)
                |> Log.info

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with
                            activeContractSet=acs;
                            mempool=mempool;
                            utxoSet=utxoSet;
                            orphanPool=orphanPool}
            | Error Orphan
            | Error ContractNotActive ->
                // transaction is still orphan, nothing to do
                return state
            | Error BadContract ->
                eventX "Previously orphaned transaction {hash} failed to activate its contract"
                >> setField "hash" (Hash.toString txHash)
                |> Log.info

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with orphanPool = orphanPool}
            | Error error ->
                eventX "Orphan transaction {hash} failed validation: {error}"
                >> setField "hash" (Hash.toString txHash)
                >> setField "error" (error.ToString())
                |> Log.info

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with orphanPool = orphanPool}
        }

let rec validateOrphanTransactions chainParams session contractPath blockNumber state =
    effectsWriter {
        let! state' = OrphanPool.foldWriter (validateOrphanTransaction chainParams session contractPath blockNumber) state state.orphanPool

        // if orphan pool changed we run again until there is no change
        if state'.orphanPool <> state.orphanPool then
            return! validateOrphanTransactions chainParams session contractPath blockNumber state'
        else
            return state'
    }

let validateInputs chainParams session contractPath blockNumber txHash tx (state:MemoryState) shouldPublishEvents =
    effectsWriter
        {
            match TransactionValidation.validateInContext chainParams (getUTXO session) contractPath (blockNumber + 1ul)
                    state.activeContractSet state.utxoSet txHash tx with
            | Error Orphan ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool

                eventX "Transaction {hash} is an orphan. Adding to orphan pool."
                >> setField "hash" (Hash.toString txHash)
                |> Log.info
                
                return {state with orphanPool = orphanPool}
            | Error ContractNotActive ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool

                eventX "Transaction {hash} tried to run an inactive contract. Adding to orphan pool."
                >> setField "hash" (Hash.toString txHash)
                |> Log.info

                return {state with orphanPool = orphanPool}
            | Error BadContract ->
                eventX "Transaction {hash} failed to activate its contract"
                >> setField "hash" (Hash.toString txHash)
                |> Log.info

                return state
            | Error error ->
                eventX "Transaction {hash} failed inputs validation:"
                >> setField "hash" (Hash.toString txHash)
                >> setField "error" (error.ToString())
                |> Log.info

                return state
            | Ok (tx, acs) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                if shouldPublishEvents then
                    do! publish (TransactionAddedToMemPool (txHash,tx))

                eventX "Transaction {hash} added to mempool"
                >> setField "hash" (Hash.toString txHash)
                |> Log.info

                let state = {state with
                                activeContractSet=acs;
                                mempool=mempool;
                                utxoSet=utxoSet;
                             }

                return! validateOrphanTransactions chainParams session contractPath blockNumber state
        }


let validateTransaction chainParams session contractPath blockNumber tx (state:MemoryState) =
    effectsWriter {
        let txHash = Transaction.hash tx

        if MemPool.containsTransaction txHash state.mempool ||
           OrphanPool.containsTransaction txHash state.orphanPool ||
           TransactionRepository.isPartOfMainChain session txHash then
            return state
        else
            match TransactionValidation.validateBasic tx with
            | Error error ->
                eventX "Transaction {hash} failed basic validation: {error}"
                >> setField "hash" (Hash.toString txHash)
                >> setField "error" (error.ToString())
                |> Log.info

                return state
            | Ok tx ->
                return! validateInputs chainParams session contractPath blockNumber txHash tx state true
    }

let executeContract session txSkeleton cHash command sender data state =
    let isInTxSkeleton (txSkeleton:TxSkeleton.T) (outpoint,_)  =
        List.exists (fun input ->
            match input with
            | TxSkeleton.Mint _ -> false
            | TxSkeleton.PointedOutput (outpoint',_) -> outpoint' = outpoint) txSkeleton.pInputs

    let rec run cHash command sender data (txSkeleton:TxSkeleton.T) witnesses totalCost =
        match ActiveContractSet.tryFind cHash state.activeContractSet with
        | None -> Error "Contract not active"
        | Some contract ->
            let contractWallet =
                ContractUtxoRepository.getContractUtxo session cHash state.utxoSet
                |> List.reject (isInTxSkeleton txSkeleton)

            Contract.run contract command sender data contractWallet txSkeleton
            |> Result.bind (fun (tx, message) ->

                TxSkeleton.checkPrefix txSkeleton tx
                |> Result.bind (fun finalTxSkeleton ->
                    let witness = TxSkeleton.getContractWitness contract.hash command data txSkeleton finalTxSkeleton 0L

                    // To commit to the cost we need the real contract wallet
                    let contractWallet = TransactionValidation.getContractWallet tx witness
                    let cost = Contract.getCost contract command sender data contractWallet txSkeleton
                    let totalCost = cost + totalCost

                    // We can now commit to the cost, so lets alter it with the real cost
                    let witness = {witness with cost = uint32 cost}

                    let witnesses = List.add (ContractWitness witness) witnesses

                    match message with
                    | Some {cHash=cHash; command=command; data=data} ->
                        run cHash command (ContractSender contract.hash) data finalTxSkeleton witnesses totalCost
                    | None ->
                        Ok (finalTxSkeleton, witnesses, totalCost)
                )
            )

    let sender =
        match sender with
        | Some publicKey -> PKSender publicKey
        | None -> Anonymous

    run cHash command sender data txSkeleton [] 0L
    |> Result.map (fun (finalTxSkeleton, witnesses, totalCost) ->
        eventX "Running contract chain with cost: {totalCost}"
        >> setField "totalCost" totalCost
        |> Log.info
        
        let tx =
            Transaction.fromTxSkeleton finalTxSkeleton
            |> Transaction.addWitnesses witnesses

        tx
    )


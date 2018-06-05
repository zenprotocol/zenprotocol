module Blockchain.TransactionHandler

open Blockchain
open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction
open Consensus.ValidationError
open Consensus.TransactionValidation
open Consensus.Types
open State
open Logary.Message
open Zen.Types.Main

let getUTXO = UtxoSetRepository.get
let getContractState = ContractStateRepository.get

let private validateOrphanTransaction chainParams session contractPath blockNumber timestamp state txHash tx  =
    effectsWriter
        {
            match TransactionValidation.validateInContext chainParams (getUTXO session) contractPath (blockNumber + 1ul) timestamp
                    state.activeContractSet state.contractCache state.utxoSet state.contractStates txHash tx with
            | Ok (tx, acs, contractCache, contractStates) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                do! publish (TransactionAddedToMemPool (txHash, tx))

                eventX "Orphan transaction {hash} added to mempool"
                >> setField "hash" (Hash.toString txHash)
                |> Log.info

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {
                    state with
                        activeContractSet = acs
                        mempool = mempool
                        utxoSet = utxoSet
                        orphanPool = orphanPool
                        contractCache = contractCache
                        contractStates = contractStates
                    }
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

let rec validateOrphanTransactions chainParams session contractPath blockNumber timestamp state =
    effectsWriter {
        let! state' = OrphanPool.foldWriter (validateOrphanTransaction chainParams session contractPath blockNumber timestamp) state state.orphanPool

        // if orphan pool changed we run again until there is no change
        if state'.orphanPool <> state.orphanPool then
            return! validateOrphanTransactions chainParams session contractPath blockNumber timestamp state'
        else
            return state'
    }

let validateInputs chainParams session contractPath blockNumber timestamp txHash tx state shouldPublishEvents =
    effectsWriter
        {
            match TransactionValidation.validateInContext chainParams (getUTXO session) contractPath (blockNumber + 1ul) timestamp
                    state.activeContractSet state.contractCache state.utxoSet state.contractStates txHash tx with
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
                eventX "Transaction {hash} failed inputs validation: {error}"
                >> setField "hash" (Hash.toString txHash)
                >> setField "error" (error.ToString())
                |> Log.info

                return state
            | Ok (tx, acs, contractCache, contractStates) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let mempool = MemPool.add txHash tx state.mempool

                if shouldPublishEvents then
                    do! publish (TransactionAddedToMemPool (txHash,tx))

                eventX "Transaction {hash} added to mempool"
                >> setField "hash" (Hash.toString txHash)
                |> Log.info

                let state = {
                    state with
                        activeContractSet = acs
                        mempool = mempool
                        utxoSet = utxoSet
                        contractCache = contractCache
                        contractStates = contractStates
                    }

                return! validateOrphanTransactions chainParams session contractPath blockNumber timestamp state
        }


let validateTransaction chainParams session contractPath blockNumber timestamp tx (state:MemoryState) =
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
                return! validateInputs chainParams session contractPath blockNumber timestamp txHash tx state true
    }

let executeContract session txSkeleton timestamp contractId command sender messageBody state commitToState =
    let isInTxSkeleton (txSkeleton:TxSkeleton.T) (outpoint,_)  =
        List.exists (fun input ->
            match input with
            | TxSkeleton.Mint _ -> false
            | TxSkeleton.PointedOutput (outpoint',_) -> outpoint' = outpoint) txSkeleton.pInputs

    let rec run (txSkeleton:TxSkeleton.T) (contractId:ContractId) command sender messageBody witnesses totalCost (contractStates:ContractStates.T) =
        match ActiveContractSet.tryFind contractId state.memoryState.activeContractSet with
        | Some contract ->
            let contractWallet =
                ContractUtxoRepository.getContractUtxo session contractId state.memoryState.utxoSet
                |> List.reject (isInTxSkeleton txSkeleton)

            let contractContext : ContractContext = 
                {
                    blockNumber = state.tipState.tip.header.blockNumber
                    timestamp = timestamp
                }

            let contractState =
                ContractStates.tryGetState (getContractState session) contractStates contract.contractId

            let stateCommitment = 
                if commitToState then
                    match contractState with
                    | Some data -> 
                        Serialization.Data.serialize data
                        |> Hash.compute
                        |> State
                    | None -> NoState
                else
                    NotCommitted
            
            contractState
            |> Contract.run contract txSkeleton contractContext command sender messageBody contractWallet
            |> Result.bind (fun (tx, message, updatedState) ->

                TxSkeleton.checkPrefix txSkeleton tx
                |> Result.bind (fun finalTxSkeleton ->
                    let contractStates, contractState = 
                        match updatedState with
                        | stateUpdate.Delete -> 
                            ContractStates.delete contract.contractId contractStates, None
                        | stateUpdate.NoChange ->
                            contractStates, ContractStates.tryFind contract.contractId contractStates
                        | stateUpdate.Update data ->
                            ContractStates.update contract.contractId data contractStates, Some data

                    let witness = TxSkeleton.getContractWitness contract.contractId command messageBody stateCommitment txSkeleton finalTxSkeleton 0L
                                        
                    // To commit to the cost we need the real contract wallet
                    let contractWallet = Contract.getContractWallet tx witness

                    let cost = Contract.getCost contract txSkeleton contractContext command sender messageBody contractWallet contractState
                    
                    let totalCost = cost + totalCost

                    // We can now commit to the cost, so lets alter it with the real cost
                    let witness = {witness with cost = uint32 cost}

                    let witnesses = List.add (ContractWitness witness) witnesses

                    match message with
                    | Some {recipient=recipient; command=command; body=messageBody} ->
                        run finalTxSkeleton recipient command (ContractSender contract.contractId) messageBody witnesses totalCost contractStates
                    | None ->
                        Ok (finalTxSkeleton, witnesses, totalCost)
                )
            )
        | _ -> Error "Contract not active"

    let sender =
        match sender with
        | Some publicKey -> PKSender publicKey
        | None -> Anonymous

    run txSkeleton contractId command sender messageBody [] 0L state.memoryState.contractStates
    |> Result.map (fun (finalTxSkeleton, witnesses, totalCost) ->
        eventX "Running contract chain with cost: {totalCost}"
        >> setField "totalCost" totalCost
        |> Log.info

        let tx =
            Transaction.fromTxSkeleton finalTxSkeleton
            |> Transaction.addWitnesses witnesses

        tx
    )

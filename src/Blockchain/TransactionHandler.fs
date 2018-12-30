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
open Result

let getUTXO = UtxoSetRepository.get
let getContractState = ContractStateRepository.get

let private validateOrphanTransaction chainParams session contractPath blockNumber timestamp state ex  =
    effectsWriter
        {
            match TransactionValidation.validateInContext chainParams (getUTXO session) contractPath (blockNumber + 1ul) timestamp
                    state.activeContractSet state.contractCache state.utxoSet (getContractState session) state.contractStates ex with
            | Ok (tx, acs, contractCache, contractStates) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) ex.txHash ex.tx state.utxoSet
                let mempool = MemPool.add ex state.mempool

                do! publish (TransactionAddedToMemPool (ex.txHash, ex.tx))

                eventX "Orphan transaction {hash} added to mempool"
                >> setField "hash" (Hash.toString ex.txHash)
                |> Log.info

                let orphanPool = OrphanPool.remove ex.txHash state.orphanPool
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
                >> setField "hash" (Hash.toString ex.txHash)
                |> Log.info

                let orphanPool = OrphanPool.remove ex.txHash state.orphanPool
                return {state with orphanPool = orphanPool}
            | Error error ->
                eventX "Orphan transaction {hash} failed validation: {error}"
                >> setField "hash" (Hash.toString ex.txHash)
                >> setField "error" (error.ToString())
                |> Log.info

                let orphanPool = OrphanPool.remove ex.txHash state.orphanPool
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

let validateInputs chainParams session contractPath blockNumber timestamp ex state shouldPublishEvents =
    effectsWriter
        {
            match TransactionValidation.validateInContext chainParams (getUTXO session) contractPath (blockNumber + 1ul) timestamp
                    state.activeContractSet state.contractCache state.utxoSet (getContractState session) state.contractStates ex with
            | Error Orphan ->
                let orphanPool = OrphanPool.add ex state.orphanPool

                eventX "Transaction {hash} is an orphan. Adding to orphan pool."
                >> setField "hash" (Hash.toString ex.txHash)
                |> Log.info

                return {state with orphanPool = orphanPool}
            | Error ContractNotActive ->
                let orphanPool = OrphanPool.add ex state.orphanPool

                eventX "Transaction {hash} tried to run an inactive contract. Adding to orphan pool."
                >> setField "hash" (Hash.toString ex.txHash)
                |> Log.info

                return {state with orphanPool = orphanPool}
            | Error BadContract ->
                eventX "Transaction {hash} failed to activate its contract"
                >> setField "hash" (Hash.toString ex.txHash)
                |> Log.info
                
                let state = {
                    state with
                        invalidTxHashes = Set.add ex.txHash state.invalidTxHashes
                    }

                return state
            | Error error ->
                eventX "Transaction {hash} failed inputs validation: {error}"
                >> setField "hash" (Hash.toString ex.txHash)
                >> setField "error" (error.ToString())
                |> Log.info

                return state
            | Ok (_, acs, contractCache, contractStates) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) ex.txHash ex.tx state.utxoSet
                let mempool = MemPool.add ex state.mempool

                if shouldPublishEvents then
                    do! publish (TransactionAddedToMemPool (ex.txHash,ex.tx))

                eventX "Transaction {hash} added to mempool"
                >> setField "hash" (Hash.toString ex.txHash)
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

let validateTransaction chainParams session contractPath blockNumber timestamp ex (state:MemoryState) =
    effectsWriter {        
        if MemPool.containsTransaction ex.txHash state.mempool ||
           OrphanPool.containsTransaction ex.txHash state.orphanPool ||
           TransactionRepository.isPartOfMainChain session ex.txHash then
            return state
        else
            match TransactionValidation.validateBasic ex.tx with
            | Error error ->
                eventX "Transaction {hash} failed basic validation: {error}"
                >> setField "hash" (Hash.toString ex.txHash)
                >> setField "error" (error.ToString())
                |> Log.info

                return state
            | Ok _ ->
                return! validateInputs chainParams session contractPath blockNumber timestamp ex state true
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
                ContractStates.tryGetState (getContractState session) contract.contractId contractStates

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
            >>= (fun (tx, message, updatedState) ->

                TxSkeleton.checkPrefix txSkeleton tx
                >>= (fun finalTxSkeleton ->
                    let contractStates, contractState =
                        match updatedState with
                        | stateUpdate.Delete ->
                            ContractStates.delete contract.contractId contractStates, None
                        | stateUpdate.NoChange ->
                            contractStates, contractState
                        | stateUpdate.Update data ->
                            ContractStates.update contract.contractId data contractStates, Some data

                    let witness = TxSkeleton.getContractWitness contract.contractId command messageBody stateCommitment txSkeleton finalTxSkeleton 0L

                    // To commit to the cost we need the real contract wallet
                    let contractWallet = Contract.getContractWallet tx witness

                    Contract.getCost contract txSkeleton contractContext command sender messageBody contractWallet contractState
                    >>= (fun cost ->
                        let totalCost = cost + totalCost
    
                        // We can now commit to the cost, so lets alter it with the real cost
                        let witness = {witness with cost = uint64 cost}
    
                        let witnesses = List.add (ContractWitness witness) witnesses
    
                        match message with
                        | Some {recipient=recipient; command=command; body=messageBody} ->
                            run finalTxSkeleton recipient command (ContractSender contract.contractId) messageBody witnesses totalCost contractStates
                        | None ->
                            Ok (finalTxSkeleton, witnesses, totalCost))
                )
            )
        | _ -> Error "Contract not active"

    let sender =
        match sender with
        | Some publicKey -> PKSender publicKey
        | None -> Anonymous

    run txSkeleton contractId command sender messageBody [] 0L state.memoryState.contractStates
    <@> (fun (finalTxSkeleton, witnesses, totalCost) ->
        eventX "Running contract chain with cost: {totalCost}"
        >> setField "totalCost" totalCost
        |> Log.info

        Transaction.fromTxSkeleton finalTxSkeleton
        |> Transaction.pushWitnesses witnesses
    )

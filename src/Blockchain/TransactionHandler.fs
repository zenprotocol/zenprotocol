module Blockchain.TransactionHandler

open Blockchain
open Blockchain
open Blockchain
open Blockchain.EffectsWriter
open Messaging.Events
open Infrastructure
open Consensus
open Consensus.Transaction
open Consensus.TransactionValidation
open State
open Infrastructure.ServiceBus.Agent

let getUTXO = UtxoSetRepository.get

let private activateContract contractPath acs (tx : Types.Transaction) =
    match tx.contract with
    | Some code ->
        match Contract.compile contractPath code with
        | Ok contract ->                    
            ActiveContractSet.add contract.hash contract acs
            |> Ok
        | Error err -> Error err
    | None ->
        Ok acs

let private validateOrphanTransaction session contractPath state txHash tx  =
    effectsWriter 
        {            
            match TransactionValidation.validateInputs (getUTXO session)
                    state.activeContractSet state.utxoSet txHash tx with
            | Ok tx ->                
                match activateContract contractPath state.activeContractSet tx with
                | Ok acs ->
                    let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                    let mempool = MemPool.add txHash tx state.mempool
    
                    do! publish (TransactionAddedToMemPool (txHash, tx))
                    Log.info "Orphan transaction %s added to mempool" (Hash.toString txHash)

                
                    let orphanPool = OrphanPool.remove txHash state.orphanPool
                    return {state with 
                                activeContractSet=acs;
                                mempool=mempool;
                                utxoSet=utxoSet; 
                                orphanPool=orphanPool}
                | Error error ->
                    Log.info "Orphan transacation %s contract failed activation %A" (Hash.toString txHash) error 
                    
                    let orphanPool = OrphanPool.remove txHash state.orphanPool
                    return {state with orphanPool = orphanPool}
            | Error Orphan 
            | Error ContractNotActive ->
                // transacation is still orphan, nothing to do
                return state
            | Error error ->
                Log.info "Orphan transacation %s failed validation: %A" (Hash.toString txHash) error

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with orphanPool = orphanPool}
        }
        
let rec validateOrphanTransactions session contractPath state =      
    effectsWriter {
        let! state' = OrphanPool.foldWriter (validateOrphanTransaction session contractPath) state state.orphanPool
        
        // if orphan pool changed we run again until there is no change
        if state'.orphanPool <> state.orphanPool then
            return! validateOrphanTransactions session contractPath state'
        else
            return state'
    }
          
let validateInputs session contractPath txHash tx (state:MemoryState) shouldPublishEvents =
    effectsWriter
        {           
            match TransactionValidation.validateInputs (getUTXO session)
                    state.activeContractSet state.utxoSet txHash tx with
            | Error Orphan ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool

                Log.info "Transaction %s is orphan, adding to orphan pool" (Hash.toString txHash)

                return {state with orphanPool = orphanPool}
            | Error ContractNotActive ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool
                
                Log.info "Transaction %s try to run inactive contract, adding to orphan pool" (Hash.toString txHash)

                return {state with orphanPool = orphanPool} 
            | Error error ->
                 Log.info "Transacation %s failed inputs validation: %A" (Hash.toString txHash) error

                 return state
            | Ok tx ->
                match activateContract contractPath state.activeContractSet tx with
                | Ok acs ->
                    let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                    let mempool = MemPool.add txHash tx state.mempool
    
                    if shouldPublishEvents then
                        do! publish (TransactionAddedToMemPool (txHash,tx))
    
                    Log.info "Transaction %s added to mempool" (Hash.toString txHash)
    
                    let state = {state with 
                                    activeContractSet=acs;
                                    mempool=mempool;
                                    utxoSet=utxoSet;
                                 }   
                    
                    return! validateOrphanTransactions session contractPath state
                | Error error ->
                     Log.info "Transacation %s contract failed activation: %A" (Hash.toString txHash) error
                    
                     return state
        }


let validateTransaction chain session contractPath tx (state:MemoryState) =
    effectsWriter {
        let txHash = Transaction.hash tx

        if MemPool.containsTransaction txHash state.mempool || 
           OrphanPool.containsTransaction txHash state.orphanPool || 
           TransactionRepository.isPartOfMainChain session txHash then
            return state
        else
            match TransactionValidation.validateBasic tx with
            | Error error ->
                Log.info "Transacation %s failed basic validation: %A" (Hash.toString txHash) error

                return state
            | Ok tx ->
                return! validateInputs session contractPath txHash tx state true
    }

let executeContract session txSkeleton cHash command returnAddress state =
    match ActiveContractSet.tryFind cHash state.activeContractSet with
    | Some contract ->      
        let contractWallet = ContractUtxoRepository.getContractUtxo session cHash state.utxoSet 

        Contract.getCost contract command (Some returnAddress) contractWallet txSkeleton
        |> Result.map (Log.info "Running contract with cost: %A")
        |> Result.mapError (Log.info "Error getting contract with cost: %A")
        |> ignore
                
        Contract.run contract command (Some returnAddress) contractWallet txSkeleton                
        |> Result.bind (TxSkeleton.checkPrefix txSkeleton)
        |> Result.map (fun finalTxSkeleton ->            
            let tx = Transaction.fromTxSkeleton finalTxSkeleton
            
            Transaction.addContractWitness contract.hash command returnAddress txSkeleton finalTxSkeleton tx)
            
            
    | None -> Error "Contract not active"
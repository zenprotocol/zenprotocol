module Blockchain.TransactionHandler

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
let getWallet = ContractWalletRepository.get

let private activateContract chain contractPath acs (tx : Types.Transaction) shouldPublishEvents =
    effectsWriter
        {
            match tx.contract with
            | Some code ->
                match Contract.compile contractPath code with
                | Ok contract ->
                    Log.warning "activating contract: %A" (Address.encode chain (Address.Contract contract.hash))

                    return ActiveContractSet.add contract.hash contract acs
                | Error err ->
                    Log.info "handle contract error: %A" err
                    return acs
            | None ->
                return acs
        }

let private validateOrphanTransaction chain session contractPath state txHash tx  =
    effectsWriter 
        {            
            match TransactionValidation.validateInputs (getUTXO session) (getWallet session)
                    state.activeContractSet state.utxoSet state.contractWallets txHash tx with
            | Ok (tx,pInputs) ->
                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let contractWallets = ContractWallets.handleTransaction (getWallet session) state.contractWallets txHash tx pInputs 
                let mempool = MemPool.add txHash tx state.mempool

                do! publish (TransactionAddedToMemPool (txHash, tx))
                Log.info "Orphan transaction %s added to mempool" (Hash.toString txHash)

                let! acs = activateContract chain contractPath state.activeContractSet tx true

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with 
                            activeContractSet=acs;
                            mempool=mempool;
                            utxoSet=utxoSet; 
                            contractWallets=contractWallets; 
                            orphanPool=orphanPool}
            | Error (Orphan _) ->
                // transacation is still orphan, nothing to do
                return state
            | Error error ->
                Log.info "Orphan transacation %s failed validation: %A" (Hash.toString txHash) error

                let orphanPool = OrphanPool.remove txHash state.orphanPool
                return {state with orphanPool = orphanPool}
        }
        
let rec validateOrphanTransactions chain session contractPath state =      
    effectsWriter {
        let! state' = OrphanPool.foldWriter (validateOrphanTransaction chain session contractPath) state state.orphanPool
        
        // if orphan pool changed we run again until there is no change
        if state'.orphanPool <> state.orphanPool then
            return! validateOrphanTransactions chain session contractPath state'
        else
            return state'
    }
          
let validateInputs chain session contractPath txHash tx (state:MemoryState) shouldPublishEvents =
    effectsWriter
        {           
            match TransactionValidation.validateInputs (getUTXO session) (getWallet session)
                    state.activeContractSet state.utxoSet state.contractWallets txHash tx with
            | Error Orphan ->
                let orphanPool = OrphanPool.add txHash tx state.orphanPool

                Log.info "Transaction %s is orphan, adding to orphan pool" (Hash.toString txHash)

                return {state with orphanPool = orphanPool}
            | Error error ->
                 Log.info "Transacation %s failed inputs validation: %A" (Hash.toString txHash) error

                 return state
            | Ok (tx,pInputs) ->
                let! acs = activateContract chain contractPath state.activeContractSet tx shouldPublishEvents

                let utxoSet = UtxoSet.handleTransaction (getUTXO session) txHash tx state.utxoSet
                let contractWallets = ContractWallets.handleTransaction (getWallet session) state.contractWallets txHash tx pInputs
                let mempool = MemPool.add txHash tx state.mempool

                if shouldPublishEvents then
                    do! publish (TransactionAddedToMemPool (txHash,tx))

                Log.info "Transaction %s added to mempool" (Hash.toString txHash)

                let state = {state with 
                                activeContractSet=acs;
                                mempool=mempool;
                                utxoSet=utxoSet;
                                contractWallets=contractWallets}

                
                return! validateOrphanTransactions chain session contractPath state
        }


let validateTransaction chain session contractPath tx (state:MemoryState) =
    effectsWriter {
        let txHash = Transaction.hash tx

        if MemPool.containsTransaction txHash state.mempool || OrphanPool.containsTransaction txHash state.orphanPool then
            return state
        else
            match TransactionValidation.validateBasic tx with
            | Error error ->
                Log.info "Transacation %s failed basic validation: %A" (Hash.toString txHash) error

                return state
            | Ok tx ->
                return! validateInputs chain session contractPath txHash tx state true
    }

let executeContract session txSkeleton cHash command state =
    match ActiveContractSet.tryFind cHash state.activeContractSet with
    | Some contract ->
        
        let contractWallet = 
            ContractWallets.get (ContractWalletRepository.get session) cHash state.contractWallets 

        Contract.getCost contract command contractWallet txSkeleton
        |> Result.map (Log.info "Running contract with cost: %A")
        |> Result.mapError (Log.info "Error getting contract with cost: %A")
        |> ignore
                
        Contract.run contract command contractWallet txSkeleton                
        |> Result.bind (TxSkeleton.checkPrefix txSkeleton)
        |> Result.map (fun finalTxSkeleton ->            
            let tx = Transaction.fromTxSkeleton finalTxSkeleton
            
            Transaction.addContractWitness contract.hash txSkeleton finalTxSkeleton tx)
            
            
    | None -> Error "Contract not active"
module Consensus.ContractWallets

open Consensus
open Consensus.Types
open Infrastructure

type ContractWallet = Map<Hash.Hash, PointedOutput list>

type T = Map<Hash.Hash, ContractWallet>

let asDatabase = Map.empty

let get (getWallet:Hash.Hash -> ContractWallet) (cHash:Hash.Hash) (wallets:T) = 
    match Map.tryFind cHash wallets with
    | Some wallet -> wallet
    | None -> getWallet cHash

let private chooseContractLocks output =
    match output.lock with
    | Contract cHash -> Some cHash
    | _ -> None
    
let private isLockToContract cHash (_,output) = output.lock = Contract cHash    

let private addToAssetOutputs newItem list = 
    let (>=) (_,x) (_,y) = x.spend.amount >= y.spend.amount
    let (<) (_,x) (_,y) = x.spend.amount < y.spend.amount

    match list with
    | [] -> [newItem]
    | head :: _ when newItem >= head -> newItem :: list
    | _ ->   
        List.foldBack (fun left tail ->
            match tail with
            | [] -> if left >= newItem then [left;newItem] else [left]
            | right :: _ ->                
                if left >= newItem && right < newItem then
                    left :: newItem :: tail
                else
                    left :: tail) list []  

let handleTransaction getWallet wallets txHash tx (inputs:Output list) =           
    let handleTransaction' cHash wallet =      
        let isLockToContract = isLockToContract cHash
    
        let inputs = 
            List.zip tx.inputs inputs
            |> List.filter isLockToContract
            
        // removing the spent inputs of the transaction from the wallet            
        let wallet =    
            List.fold (fun wallet (input,output) ->
                // If we have an input for that specific asset we assume the asset must exist
                // otherwise the contract validation should have failed. 
                // This is why we are not using tryFind
                let assetInputs = 
                    Map.find output.spend.asset wallet
                    |> List.remove (input,output) // removing the spent input from the contract wallet
                                                
                // add the modified asset inputs back to the wallet
                if List.isEmpty assetInputs then
                    Map.remove output.spend.asset wallet
                else                                                            
                    Map.add output.spend.asset assetInputs wallet                                
            ) wallet inputs
                        
        let outputs = 
            tx.outputs
            |> List.mapi (fun index output -> {txHash=txHash;index=uint32 index},output)
            |> List.filter isLockToContract
            
        // adding the unspent outputs of the transaction to the wallet     
        let wallet = 
            List.fold (fun wallet (input,output) ->        
                // Either find the asset inputs or create a new empty one
                let assetInputs =
                    match Map.tryFind output.spend.asset wallet with
                    | None -> []
                    | Some xs -> xs
                    
                let assetInputs = addToAssetOutputs (input,output) assetInputs
                   
                // add the modified asset inputs back to the wallet                                            
                Map.add output.spend.asset assetInputs wallet) wallet outputs  
                        
        wallet       
                        
    // Find out all contracts                         
    let affectedContracts = 
        let byInputs = List.choose chooseContractLocks inputs
        let byOutputs = List.choose chooseContractLocks tx.outputs
        
        List.append byInputs byOutputs
        |> List.distinct
        
    List.fold (fun wallets cHash ->
        let wallet = 
            get getWallet cHash wallets
            |> handleTransaction' cHash
        
        Map.add cHash wallet wallets) wallets affectedContracts
    
let private undoTransaction txHash tx inputs cHash wallet =
    let isLockToContract = isLockToContract cHash
                  
    let inputs = 
        List.zip tx.inputs inputs
        |> List.filter isLockToContract
        
    // readding the spent inputs of the transaction to the wallet            
    let wallet =    
        List.fold (fun wallet (input,output) ->
            // Either find the asset inputs or create a new empty one
            let assetInputs =
                match Map.tryFind output.spend.asset wallet with
                | None -> []
                | Some xs -> xs
                
            let assetInputs = addToAssetOutputs (input,output) assetInputs
                                            
            // add the modified asset inputs back to the wallet                                            
            Map.add output.spend.asset assetInputs wallet                                
        ) wallet inputs
                    
    let outputs = 
        tx.outputs
        |> List.mapi (fun index output -> {txHash=txHash;index=uint32 index},output)
        |> List.filter isLockToContract

    // removing the outputs of the transaction from the wallet
    List.fold (fun wallet (input,output) ->        
        let assetInputs = 
            Map.find output.spend.asset wallet
            |> List.remove (input,output)
                                        
        // add the modified asset inputs back to the wallet
        if List.isEmpty assetInputs then
            Map.remove output.spend.asset wallet
        else                                                            
            Map.add output.spend.asset assetInputs wallet                                                
        ) wallet outputs                        
        
let undoBlock getOutput getWallet block wallets = 
    // This heavy operation, for every transaction we must get all the inputs output to check if any contract is affected
    List.fold (fun wallets tx ->
        let inputs = List.map getOutput tx.inputs 
                
        let affectedContracts = 
            let byInputs = List.choose chooseContractLocks inputs
            let byOutputs = List.choose chooseContractLocks tx.outputs
            
            List.append byInputs byOutputs
            |> List.distinct
        
        if not <| List.isEmpty affectedContracts then
            let txHash = Transaction.hash tx
            
            List.fold (fun wallets cHash ->
                let wallet = 
                    get getWallet cHash wallets
                    |> undoTransaction txHash tx inputs cHash 
                
                Map.add cHash wallet wallets) wallets affectedContracts
        else
            wallets                                                                
        ) wallets block.transactions  
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    
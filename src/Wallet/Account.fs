module Wallet.Account

open Consensus
open Consensus.Hash
open Consensus.Crypto
open Consensus.Types

type T = {
    outpoints: Map<Outpoint, Spend>;
    secretKey: SecretKey
    publicKeyHash: Hash
}

let create () = 
    let secretKey, publicKey = KeyPair.create ()

    {
        outpoints = Map.empty;
        secretKey = secretKey;
        publicKeyHash = PublicKey.hash publicKey;
    }
        
let handleTransaction account txHash (tx:Transaction) =
    
    let handleOutput (index,output) outpoints = 
        match output.lock with
        | PK pkHash -> 
            match pkHash = account.publicKeyHash with
            | false -> outpoints
            | true -> 
                let outpoint = {txHash=txHash;index=index;}
                Map.add outpoint output.spend outpoints            
        | _ -> outpoints
    
    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs
    
    let outpoints = 
        account.outpoints
        |> List.foldBack Map.remove tx.inputs
        |> List.foldBack handleOutput outputsWithIndex
        
    {account with outpoints = outpoints}
    
let getBalance account =
    let balance = Map.empty
    
    Map.fold (fun balance _ spend -> 
     match Map.tryFind spend.asset balance with
     | Some amount -> Map.add spend.asset (amount+spend.amount) balance
     | None -> Map.add spend.asset spend.amount balance) Map.empty account.outpoints
     
let getAddress account = 
    Address.getPublicKeyAddress account.publicKeyHash
     
let createTransaction account address asset amount =
    let collectInputs (inputs, collectedAmount) outpoint spend =
        match spend.asset with
        | asset when amount > collectedAmount -> (outpoint :: inputs, spend.amount + collectedAmount)
        | _ -> (inputs, collectedAmount)  
 
    // TODO: should the address be validated at a higher level?
    match Address.getPublicKeyHash address with
    | None -> Error "Invalid address"
    | Some pkHash -> 
        let inputs, collectedAmount = Map.fold collectInputs ([],0UL) account.outpoints
        
        match collectedAmount >= amount with
        | false -> Error "Not enough tokens"
        | true ->
            let output = {spend={asset=asset;amount=amount};lock=PK pkHash}
                                              
            //  checking if change is needed
            let tx = 
                match collectedAmount = amount with
                | false ->
                    let change = {spend={asset=asset;amount=(collectedAmount - amount)};lock=PK account.publicKeyHash}
                    {inputs=inputs; outputs=[output;change]; witnesses=[]}
                | true -> {inputs=inputs; outputs=[output]; witnesses=[]}
                
            Ok (Transaction.sign tx account.secretKey)
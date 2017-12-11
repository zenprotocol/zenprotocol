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

let rootSecretKey = SecretKey [|189uy; 140uy; 82uy; 12uy; 79uy; 140uy; 35uy; 59uy; 11uy; 41uy; 199uy;
                           58uy; 23uy; 63uy; 112uy; 239uy; 45uy; 147uy; 51uy; 246uy; 34uy; 16uy;
                           156uy; 2uy; 111uy; 184uy; 140uy; 218uy; 136uy; 240uy; 57uy; 24uy |]

let create () = 
    let secretKey, publicKey = KeyPair.create ()

    {
        outpoints = Map.empty;
        secretKey = secretKey;
        publicKeyHash = PublicKey.hash publicKey;
    }                   
        
let handleTransaction txHash (tx:Transaction) account =
    
    let handleOutput outpoints (index,output) = 
        match output.lock with
        | PK pkHash -> 
            match pkHash = account.publicKeyHash with
            | false -> outpoints
            | true -> 
                let outpoint = {txHash=txHash;index=index;}
                Map.add outpoint output.spend outpoints            
        | _ -> outpoints
           
    let outpoints = List.fold (fun state o -> Map.remove o state) account.outpoints tx.inputs         
    
    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs
    let outpoints' = List.fold handleOutput outpoints outputsWithIndex
        
    {account with outpoints = outpoints'}
    
let getBalance account =
    let balance = Map.empty
    
    Map.fold (fun balance _ spend -> 
     match Map.tryFind spend.asset balance with
     | Some amount -> Map.add spend.asset (amount+spend.amount) balance
     | None -> Map.add spend.asset spend.amount balance) Map.empty account.outpoints
     
let getAddress account chain = 
    Address.getPublicKeyAddress account.publicKeyHash chain
     
let createTransaction account address asset amount =
    let collectInputs (inputs, collectedAmount) outpoint spend =
        if amount > collectedAmount && asset = spend.asset then
            (outpoint :: inputs, spend.amount + collectedAmount)
        else 
            (inputs, collectedAmount)  
                              
    // TODO: should the address be validated at a higher level + checking the HRP
    // match the chain
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
            

let createRoot () =             
    let account = 
        {
            outpoints = Map.empty;
            secretKey = rootSecretKey;
            publicKeyHash = ChainParameters.rootPKHash;
        }
        
    handleTransaction ChainParameters.rootTxHash ChainParameters.rootTx account             
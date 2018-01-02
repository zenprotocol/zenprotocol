module Wallet.Account

open Consensus
open Consensus.Hash
open Consensus.Crypto
open Consensus.Types

type T = {
    outpoints: Map<Outpoint, Output>
    keyPair: KeyPair
    publicKeyHash: Hash
}

let rootSecretKey = SecretKey [|189uy; 140uy; 82uy; 12uy; 79uy; 140uy; 35uy; 59uy; 11uy; 41uy; 199uy;
                           58uy; 23uy; 63uy; 112uy; 239uy; 45uy; 147uy; 51uy; 246uy; 34uy; 16uy;
                           156uy; 2uy; 111uy; 184uy; 140uy; 218uy; 136uy; 240uy; 57uy; 24uy |]

let create () = 
    let secretKey, publicKey = KeyPair.create ()

    {
        outpoints = Map.empty
        keyPair = (secretKey, publicKey)
        publicKeyHash = PublicKey.hash publicKey
    }                   
        
let handleTransaction txHash (tx:Transaction) account =
    
    let handleOutput outpoints (index,output) = 
        match output.lock with
        | PK pkHash -> 
            match pkHash = account.publicKeyHash with
            | false -> outpoints
            | true -> 
                let outpoint = {txHash=txHash;index=index;}
                Map.add outpoint output outpoints            
        | _ -> outpoints
           
    let outpoints = List.fold (fun state o -> Map.remove o state) account.outpoints tx.inputs         
    
    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs
    let outpoints' = List.fold handleOutput outpoints outputsWithIndex
        
    {account with outpoints = outpoints'}
    
let getBalance account =
    Map.fold (fun balance _ output -> 
        match Map.tryFind output.spend.asset balance with
        | Some amount -> Map.add output.spend.asset (amount+output.spend.amount) balance
        | None -> Map.add output.spend.asset output.spend.amount balance) Map.empty account.outpoints
     
let getAddress account chain = 
    Address.getPublicKeyAddress account.publicKeyHash chain
     
let createTransaction account address asset amount =
    let collectInputs ((inputs, keys), collectedAmount) outpoint output =
        if amount > collectedAmount && asset = output.spend.asset then
            ((outpoint :: inputs, account.keyPair :: keys), output.spend.amount + collectedAmount)
        else 
            ((inputs, keys), collectedAmount)  
                              
    // TODO: should the address be validated at a higher level + checking the HRP
    // match the chain
    match Address.getPublicKeyHash address with
    | None -> Error "Invalid address"
    | Some pkHash -> 
        let (inputs, keys), collectedAmount = Map.fold collectInputs (([],[]),0UL) account.outpoints
        
        match collectedAmount >= amount with
        | false -> Error "Not enough tokens"
        | true ->
            let outputs = 
                {spend={asset=asset;amount=amount};lock=PK pkHash} :: 
                // checking if change is needed
                match collectedAmount = amount with
                    | true ->
                        []
                    | false -> 
                        [{spend={asset=asset;amount=(collectedAmount - amount)};lock=PK account.publicKeyHash}]
            Ok (Transaction.sign {inputs=inputs; outputs=outputs; witnesses=[]; contract = None} keys)
            
let createContractActivationTransaction account code =
    let input, output = Map.toSeq account.outpoints |> Seq.head
    let output' = {output with lock=PK account.publicKeyHash}
    printfn "%A" code
    Ok (Transaction.sign {inputs=[ input ]; outputs=[ output' ]; witnesses=[]; contract = Some code} [ account.keyPair ])

let createRoot () =                
    let account = 
        {
            outpoints = Map.empty
            keyPair = KeyPair.fromSecretKey rootSecretKey
            publicKeyHash = Transaction.rootPKHash
        }
        
    handleTransaction Transaction.rootTxHash Transaction.rootTx account             
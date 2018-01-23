module Wallet.Account

open Consensus
open Consensus.Hash
open Consensus.Crypto
open Consensus.Types

type TransactionResult = Messaging.Services.TransactionResult

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
    
let handleBlock block account = 
    List.fold (fun account tx ->
        let txHash = Transaction.hash tx
        handleTransaction txHash tx account) account block.transactions
           
let getBalance account =
    Map.fold (fun balance _ output -> 
        match Map.tryFind output.spend.asset balance with
        | Some amount -> Map.add output.spend.asset (amount+output.spend.amount) balance
        | None -> Map.add output.spend.asset output.spend.amount balance) Map.empty account.outpoints
     
let getAddress chain account = 
    Address.encode chain (Address.PK account.publicKeyHash)
     
let createTransaction chain account pkHash spend =
    let collectInputs ((inputs, keys), collectedAmount) outpoint output =
        if spend.amount > collectedAmount && spend.asset = output.spend.asset then
            ((outpoint :: inputs, account.keyPair :: keys), output.spend.amount + collectedAmount)
        else 
            ((inputs, keys), collectedAmount)

    let (inputs, keys), collectedAmount = Map.fold collectInputs (([],[]),0UL) account.outpoints
    
    match collectedAmount >= spend.amount with
    | false -> Error "Not enough tokens"
    | true ->
        let outputs = 
            {spend=spend;lock=PK pkHash} :: 
            // checking if change is needed
            match collectedAmount = spend.amount with
                | true ->
                    []
                | false -> 
                    [{spend={spend with amount=(collectedAmount - spend.amount)};lock=PK account.publicKeyHash}]
        Ok (Transaction.sign keys {inputs=inputs; outputs=outputs; witnesses=[]; contract = None})
            
let createActivateContractTransaction account code =
    Contract.recordHints code
    |> Result.map (fun hints ->
        let input, output = Map.toSeq account.outpoints |> Seq.head
        let output' = {output with lock=PK account.publicKeyHash}
        { inputs=[ input ]; outputs=[ output' ]; witnesses=[]; contract = Some (code, hints) })
    |> Result.map (Transaction.sign [ account.keyPair ])

let createRoot () =                
    let account = 
        {
            outpoints = Map.empty
            keyPair = KeyPair.fromSecretKey rootSecretKey
            publicKeyHash = Transaction.rootPKHash
        }
        
    account
        
    handleTransaction Transaction.rootTxHash Transaction.rootTx account             

let createExecuteContractTransaction client chain cHash data spends =
    let input = TxSkeleton.empty //TODO: create origin txskeleton
    input
    |> Messaging.Services.Blockchain.executeContract client cHash 
    |> function
    | TransactionResult.Ok tx -> Ok tx
    | TransactionResult.Error e -> Error e
    //TODO: use contract lock instead
    //TODO: sign the transaction
    //TODO: send publish command
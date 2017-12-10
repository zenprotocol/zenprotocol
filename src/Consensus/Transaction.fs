module Consensus.Transaction

open Consensus.Types
open Consensus.UtxoSet
open Consensus.Crypto

open MBrace.FsPickler.Combinators

let pickler = Pickler.auto<Transaction>

type SerializationMode = 
    | Full
    | WithoutWitness

let serialize mode tx =
    let tx = 
        match mode with
        | Full -> tx
        | WithoutWitness -> {tx with witnesses=[]}

    Binary.pickle pickler tx

let deserialize =
    // TODO: should return an option
    Binary.unpickle pickler 

let hash =
    serialize WithoutWitness >> Hash.compute

let private validateOrphancy set tx =
    match getUtxos set tx.inputs with
    | Some utxos -> 
        Ok (tx, utxos)
    | None -> Error "orphan"

let private validateAmounts (tx, outputs) =
    let addToMap newKey newValue map =
        if Map.containsKey newKey map
        then
            Map.map (
                fun key value -> 
                    if (key = newKey) 
                    then newValue + value 
                    else newValue)
                map
        else
            Map.add newKey newValue map            
    let sumAmounts = List.fold (fun map output -> addToMap output.spend.asset output.spend.amount map) Map.empty

    let inputSums = sumAmounts tx.outputs
    let outputSums = sumAmounts outputs
    
    if Map.count inputSums <> Map.count outputSums then 
        Error "invalid amounts"
    else
        let validateAmount state inputAsset inputAmount = 
            match state with 
            | false -> false
            | true -> 
                match Map.tryFind inputAsset outputSums with
                | None -> false
                | Some outputAmount -> inputAmount = outputAmount
    
        if Map.fold validateAmount true inputSums
            then Ok tx
            else Error "invalid amounts"

let validate set =
    let (>=>) f1 f2 x = Result.bind f2 (f1 x)

    validateOrphancy set
    >=> 
    validateAmounts 
    
let sign tx secretKey =
    let txHash = hash tx

    let witnessInput _ = 
        PKWitness (Crypto.sign secretKey txHash)
         
    // TODO: Should we also use sighash and not sign entire transaction?
    let witnesses = List.foldBack (fun input witnesses -> 
        (witnessInput input) :: witnesses) tx.inputs []         
        
    {tx with witnesses = witnesses}
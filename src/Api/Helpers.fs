module Api.Helpers

open FSharp.Data
open Api.Types
open Consensus.Types
open Consensus
open Wallet

let rec omitNullFields = function
    | JsonValue.Array items ->
        items
        |> Array.map omitNullFields
        |> JsonValue.Array
    | JsonValue.Record properties ->
        properties
        |> Array.filter (fun (_,jsonValue) ->
            match jsonValue with
            | JsonValue.Null -> false
            | _ -> true)
        |> Array.map (fun (name,value) -> name, omitNullFields value)
        |> JsonValue.Record
    | value -> value

let spendEncoder spend =
    SpendJson.Root (spend.asset.AsString, (int64)spend.amount)
    |> fun j -> j.JsonValue

let inputEncoder (input:Input) =
    match input with
    | Outpoint pnt ->
        OutpointJson.Root (pnt.txHash.AsString,(int)pnt.index)
        |> fun j -> JsonValue.Record [| ("outpoint", j.JsonValue) |]
    | Mint spend ->
        SpendJson.Root (spend.asset.AsString, (int64)spend.amount)
        |> fun j -> JsonValue.Record [| ("mint", j.JsonValue) |]

let lockEncoder chain (lock:Lock) =
    match lock with
    | PK hash ->
        let pkJson = LockJson.Pk(hash.AsString, Address.encode chain (Address.PK hash))
        LockJson.Record (Some pkJson,None,None,None,None)
        |> LockJson.Root
    | Contract cId ->
        let cJson = LockJson.Contract (cId.AsString, Address.encode chain (Address.Contract cId))
        LockJson.Record (None, Some cJson, None, None, None)
        |> LockJson.Root
    | Coinbase (blockNumber, pkHash) ->
        let cJson = LockJson.Coinbase ((int)blockNumber, pkHash.AsString)
        LockJson.Record (None, None, Some cJson, None, None)
        |> LockJson.Root
    | Fee -> LockJson.Root "Fee"
    | ActivationSacrifice -> LockJson.Root "ActivationSacrifice"
    | ExtensionSacrifice cId ->
        let esJson = LockJson.ExtensionSacrifice (cId.AsString)
        LockJson.Record (None, None, None, Some esJson, None)
        |> LockJson.Root
    | Destroy -> LockJson.Root "Destroy"
    | HighVLock (identifier, data) ->
        let hvJson = LockJson.HighVLock ((int)identifier, FsBech32.Base16.encode data)
        LockJson.Record (None, None, None, None, Some hvJson)
        |> LockJson.Root
    |> fun json -> json.JsonValue

let outputEncoder chain (output:Output) =
    JsonValue.Record [|
        ("lock", lockEncoder chain output.lock);
        ("spend", spendEncoder output.spend)
    |]

let pointedOutputEncoder chain (pointedOutput:PointedOutput) =
    let outpoint, output = fst pointedOutput, snd pointedOutput
    let outpointJson = OutpointJson.Root (outpoint.txHash.AsString, (int)outpoint.index)
    JsonValue.Record [|
        ("outpoint", outpointJson.JsonValue)
        ("lock", lockEncoder chain output.lock)
        ("spend", spendEncoder output.spend)
    |]

let contractEncoder chain (tx:Transaction) =
        match tx.contract with 
        | Some (V0 c) -> 
            let contractId = Contract.makeContractId Version0 c.code
                        
            let address = Address.encode chain (Address.Contract contractId)
                    
            JsonValue.Record [|("contract", JsonValue.Record [| ("contractId", JsonValue.String (contractId.ToString())); ("address",JsonValue.String address); ("code",JsonValue.String c.code) |])|]
        | _ -> JsonValue.Null
    
let transactionEncoder chain (tx:Transaction) =
    JsonValue.Record
        [|
            ("version",JsonValue.Number ((decimal) tx.version));
            ("inputs", JsonValue.Array [| for i in tx.inputs -> inputEncoder i |]);
            ("outputs", JsonValue.Array [| for op in tx.outputs -> outputEncoder chain op |])
            ("contract", contractEncoder chain tx)
            // witnesses not yet set
        |]
    |> omitNullFields

let blockHeaderEncoder (bh:BlockHeader) =
    BlockHeaderJson.Root (
        version=(int)bh.version,
        parent=bh.parent.AsString,
        blockNumber=(int)bh.blockNumber,
        timestamp=(int64)bh.timestamp,
        difficulty=(int)bh.difficulty,
        nonce=[|(int64)(fst bh.nonce);(int64)(snd bh.nonce)|],
        commitments=bh.commitments.AsString
    ) |> fun j -> j.JsonValue

let blockEncoder chain blockHash (bk:Block) =    
    let txsJson =
        JsonValue.Record
            [| for ex in bk.transactions do
                yield (ex.txHash |> Consensus.Hash.toString, transactionEncoder chain ex.tx)
            |]
    JsonValue.Record
        [|
            ("hash", blockHash |> Consensus.Hash.toString  |> JsonValue.String)
            ("header", blockHeaderEncoder bk.header);
            ("transactions", txsJson)
        |]
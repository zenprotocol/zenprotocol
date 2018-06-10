module Api.Helpers

open FSharp.Data
open Api.Types
open Consensus.Types

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

let lockEncoder (lock:Lock) =
    match lock with
    | PK hash ->
        let pkJson = LockJson.Pk hash.AsString
        LockJson.Record (Some pkJson,None,None,None,None)
        |> LockJson.Root
    | Contract cId ->
        let cJson = LockJson.Contract (cId.AsString)
        LockJson.Record (None, Some cJson, None, None, None)
        |> LockJson.Root
    | Coinbase (blockNumber, pkHash) ->
        let cJson = LockJson.Coinbase ((int)blockNumber, pkHash.AsString)
        LockJson.Record (None, None, Some cJson, None, None)
        |> LockJson.Root
    | Fee -> LockJson.Root "Fee"
    | ActivationSacrifice -> LockJson.Root "ActivationSacrifice"
    | ExtensionSacrifice cId ->
        let esJson = LockJson.Contract (cId.AsString)
        LockJson.Record (None, None, None, Some esJson, None)
        |> LockJson.Root
    | Destroy -> LockJson.Root "Destroy"
    | HighVLock (identifier, data) ->
        let hvJson = LockJson.HighVLock ((int)identifier, FsBech32.Base16.encode data)
        LockJson.Record (None, None, None, None, Some hvJson)
        |> LockJson.Root
    |> fun j -> j.JsonValue


let outputEncoder (output:Output) =
    JsonValue.Record [|
        ("lock", lockEncoder output.lock);
        ("spend", spendEncoder output.spend)
    |]

let transactionEncoder (tx:Transaction) =
    JsonValue.Record
        [|
            ("version",JsonValue.Number ((decimal) tx.version));
            ("inputs", JsonValue.Array [| for i in tx.inputs -> inputEncoder i |]);
            ("outputs", JsonValue.Array [| for op in tx.outputs -> outputEncoder op |])
            // witnesses and contracts not yet sent
        |]

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

let blockEncoder (bk:Block) =
    let txs = bk.transactions
    let txHashes = List.map (Consensus.Transaction.hash >> Consensus.Hash.toString) txs
    let txsJson =
        JsonValue.Record
            [| for (h,tx) in List.zip txHashes txs do
                yield (h, transactionEncoder tx)
            |]
    JsonValue.Record
        [|
            ("header", blockHeaderEncoder bk.header);
            ("transactions", txsJson)
        |]
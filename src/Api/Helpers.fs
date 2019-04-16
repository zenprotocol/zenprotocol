module Api.Helpers

open FSharp.Data
open Api.Types
open Consensus.Types
open Consensus
open Consensus.Crypto
open Wallet
open Zen.Types.Data
open FsBech32

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
    
let emptyRecord = JsonValue.Record [| |]

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

let recipientEncoder chain = function
        | PKRecipient pkHash -> Address.encode chain (Address.PK pkHash)
        | ContractRecipient contractId -> contractId.ToString()

let voteDataEncoder chain (voteData:VoteData) =
    let allocation =
        match voteData.allocation with
        | Some allocation -> JsonValue.Number (decimal allocation)
        | None _ -> emptyRecord
    let payout =
        match voteData.payout with
        | Some (recipient, amount) -> PayoutResultJson.Root(recipientEncoder chain recipient, int64 amount).JsonValue
        | None _ -> emptyRecord
    JsonValue.Record [|
        ( "allocation",  allocation)
        ( "payout", payout)
    |]

let lockEncoder chain (lock:Lock) =
    match lock with
    | PK hash ->
        PKLockJson.Root (hash.AsString, Address.encode chain (Address.PK hash))
        |> fun j -> JsonValue.Record [| ("PK", j.JsonValue) |]
    | Contract cId ->
        ContractLockJson.Root (cId.AsString, Address.encode chain (Address.Contract cId))
        |> fun j -> JsonValue.Record [| ("Contract", j.JsonValue) |]
    | Coinbase (blockNumber, pkHash) ->
        CoinbaseLockJson.Root ((int)blockNumber, pkHash.AsString, Address.encode chain (Address.PK pkHash))
        |> fun j -> JsonValue.Record [| ("Coinbase", j.JsonValue) |]
    | Fee -> JsonValue.String "Fee"
    | ActivationSacrifice -> JsonValue.String "ActivationSacrifice"
    | ExtensionSacrifice cId ->
        ExtensionSacrificeLockJson.Root (cId.AsString, Address.encode chain (Address.Contract cId))
        |> fun j -> JsonValue.Record [| ("ExtensionSacrifice", j.JsonValue) |]
    | Destroy -> JsonValue.String "Destroy"
    | HighVLock (identifier, data) ->
        HighVLockLockJson.Root ((int)identifier, FsBech32.Base16.encode data)
        |> fun j -> JsonValue.Record [| ("HighVLock", j.JsonValue) |]
    | Vote (voteData, interval, pkHash) ->
        let voteProprierty =
           JsonValue.Record [|
               ( "voteData", voteDataEncoder chain voteData  )
               ( "interval", JsonValue.Number ((decimal) interval))
               ( "pkHash", JsonValue.String pkHash.AsString )
            |]
        JsonValue.Record [| ("Vote", voteProprierty) |] 

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

            JsonValue.Record [| ("contractId", JsonValue.String (contractId.ToString())); ("address",JsonValue.String address); ("code",JsonValue.String c.code) |]
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
    
let transactionHistoryEncoder chain txHash asset (amount:int64) (confirmations:uint32) lock=
    JsonValue.Record
        [|
            ("txHash",JsonValue.String (Hash.toString txHash));
            ("asset", JsonValue.String (Asset.toString asset));
            ("amount", JsonValue.Number (decimal amount))
            ("confirmations", JsonValue.Number (decimal confirmations))
            ("lock", lockEncoder chain lock)
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

let dataEncoder chain data =

    let dataName = function
        | I64 _ -> "i64"
        | Byte _ -> "byte"
        | ByteArray _ -> "byteArray"
        | U32 _ -> "u32"
        | U64 _ -> "u64"
        | String _ -> "string"
        | Hash _ -> "hash"
        | Lock _ -> "lock"
        | Signature _ -> "signature"
        | PublicKey _ -> "pk"
        | Collection (Array _) -> "array"
        | Collection (Dict _) -> "dict"
        | Collection (List _) -> "list"

    let rec dataValue  = function
        | I64 v ->
            v
            |> decimal
            |> JsonValue.Number
        | Byte v ->
            v
            |> decimal
            |> JsonValue.Number
        | ByteArray v ->
            v
            |> Array.map (decimal >> JsonValue.Number)
            |> JsonValue.Array
        | U32 v ->
            v
            |> decimal
            |> JsonValue.Number
        | U64 v ->
            v
            |> decimal
            |> JsonValue.Number
        | String v ->
            v
            |> ZFStar.fstToFsString
            |> JsonValue.String
        | Hash v ->
            v
            |> Base16.encode
            |> JsonValue.String
        | Lock v ->
            v
            |> ZFStar.fstToFsLock
            |> lockEncoder chain
        | Signature v ->
            v
            |> ZFStar.fstToFsSignature
            |> Signature.toString
            |> JsonValue.String
        | PublicKey v ->
            v
            |> ZFStar.fstToFsPublicKey
            |> PublicKey.toString
            |> JsonValue.String
        | Collection (Array v) ->
            v
            |> Array.map (fun data -> JsonValue.Record [| dataName data, dataValue data |])
            |> JsonValue.Array
        | Collection (Dict (v, _)) ->
            v
            |> Map.toArray
            |> Array.map (fun (key, data) ->
                JsonValue.Array [|
                    key
                    |> ZFStar.fstToFsString
                    |> JsonValue.String
                    JsonValue.Record [| dataName data, dataValue data |]
                |]
            )
            |> JsonValue.Array
        | Collection (List v) ->
            v
            |> ZFStar.fstToFsList
            |> List.map (fun data -> JsonValue.Record [| dataName data, dataValue data |])
            |> List.toArray
            |> JsonValue.Array

    JsonValue.Record [| dataName data, dataValue data |]
    
let allocationEncoder allocation =
    let allocation =
            allocation
            |> Map.toSeq
            |> Seq.map (fun (amount:byte, count:uint64) -> new AllocationVoteResult.Root(int amount, int64 count))
            |> Seq.map (fun json -> json.JsonValue)
            |> Seq.toArray
    JsonValue.Record [| ("votes", JsonValue.Array (allocation)) |]

let payoutEncoder chain payout =
    let payout =
        payout
        |> Map.toSeq
        |> Seq.map (fun ((recipient:Types.Recipient, amount: uint64), count: uint64) ->  PayoutVoteResult.Root(recipientEncoder chain recipient, int64 amount, int64 count))
        |> Seq.map (fun json -> json.JsonValue)
        |> Seq.toArray
    JsonValue.Record [| ("votes", JsonValue.Array (payout)) |]

let cgpEncoder chain (cgp:CGP.T) =
    let result =
        match cgp.payout with
        | Some res ->
            res
            |> (fun (recipient:Recipient, amount: uint64) -> PayoutResultJson.Root(recipientEncoder chain recipient, int64 amount))
            |> fun j -> j.JsonValue
        | _ -> emptyRecord
    let tallies = 
        cgp.tallies
        |> Map.toSeq
        |>Seq.map (fun (interval:uint32, tally:Tally.T) -> 
        JsonValue.Record 
            [|
                ("interval", JsonValue.Number (decimal interval))
                ("allocation", allocationEncoder tally.allocation)
                ("payout", payoutEncoder chain tally.payout)
            |])
       |> Seq.toArray
    JsonValue.Record
        [|
            ("tallies", JsonValue.Array(tallies))
            ("resultAllocation", JsonValue.Number (decimal cgp.allocation))
            ("resultPayout", result) 
            ("fund", JsonValue.Number ((decimal) cgp.amount))
        |]
    |> omitNullFields

let cgpHistoryEncoder chain (cgp:CGP.T list) =
    cgp
    |> List.map (cgpEncoder chain)
    |> List.toArray
    |> JsonValue.Array
    |> omitNullFields

let voteUtilizationEncoder chain (outstanding:uint64) (utilized:uint64) voteData =
    let voteJson =
        match voteData with
        | Some vote -> voteDataEncoder chain vote
        | None -> JsonValue.Null
    JsonValue.Record
        [|
            ("outstanding",  JsonValue.Number (decimal outstanding))
            ("utilized", JsonValue.Number (decimal utilized));
            ("vote", voteJson)
        |]
        

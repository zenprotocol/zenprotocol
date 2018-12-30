module Consensus.ZFStar
#nowarn "664"   // Ignore type erasure warnings.
#nowarn "62"    // Ignore ML compatibility warnings.

open Operators.Checked

open Consensus
open Consensus.Crypto
open Types
open Hash
open TxSkeleton
open FSharp.Compatibility.OCaml
open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.TxSkeleton
open Zen.Types.Main
open FStar.Pervasives
open MBrace.FsPickler.Combinators

module Cost = Zen.Cost.Realized

let unCost (Cost.C inj:Zen.Cost.Realized.cost<'Aa, 'An>) : 'Aa = inj.Force()

let pickler = Pickler.auto<Zen.Types.Data.data>

let fsToFstString (s : string) =
    if String.forall(fun c -> Char.code c <= 255) s
    then System.Text.Encoding.ASCII.GetBytes s : Prims.string
    else failwithf "Cannot encode string as ASCII: \"%s\"" s

let fstToFsString : Prims.string -> string =
    System.Text.Encoding.ASCII.GetString

let toResult = function
    | ERR err -> Error (fstToFsString err)
    | EX err -> Error err.Message //TODO: remove EX
    | OK value -> Ok value

let private throwNotImplemented s1 s2 =
    sprintf "%s %s" s1 s2
    |> System.NotImplementedException
    |> raise

let fsToFstOption value =
    match value with
    | FSharp.Core.Some value -> FStar.Pervasives.Native.Some value
    | FSharp.Core.None -> FStar.Pervasives.Native.None

let fstToFsOption value =
    match value with
    | Native.Some value -> FSharp.Core.Some value
    | Native.None -> FSharp.Core.None

let fsToFstContractId (ContractId (version, Hash.Hash hash)) : contractId = version,hash

let fstToFsContractId ((version,hash):contractId) : ContractId = ContractId (version, Hash.Hash hash)

let fsToFstLock (outputLock:Types.Lock) : lock =
    match outputLock with
    | Types.PK (Hash.Hash pkHash) ->
        PKLock pkHash
    | Types.Lock.Vote (_, _, Hash.Hash pkHash) ->
        PKLock pkHash
    | Types.Contract contractId ->
        ContractLock (fsToFstContractId contractId)
    | Destroy ->
        DestroyLock
    | Fee ->
        FeeLock
    | ActivationSacrifice ->
        ActivationSacrificeLock
    | ExtensionSacrifice (ContractId (version, Hash.Hash cHash)) ->
        ExtensionSacrificeLock (version,cHash)
    | Coinbase (blockNumber, (Hash.Hash pkHash)) ->
        CoinbaseLock (blockNumber,pkHash)
    | Types.HighVLock (identifier, bytes) ->
        HighVLock (identifier, Prims.Mkdtuple2 (int64 (Array.length bytes), bytes))

let fsToFstOutpoint (o:Outpoint) : outpoint = {txHash = Hash.bytes o.txHash;index = o.index}

let fsToFstPublicKey (Crypto.PublicKey pk) : publicKey = pk

let fstToFsPublicKey (pk: publicKey) : Crypto.PublicKey  = Crypto.PublicKey pk

let fstToFsSignature (signature: signature) : Crypto.Signature = Crypto.Signature signature

let fsToFstSignature (Crypto.Signature signature) : signature = signature

let fstToFsLock (outputLock:lock) : Types.Lock =
    match outputLock with
    | PKLock pkHash ->
        Types.PK (Hash.Hash pkHash)
    | ContractLock contractId ->
        Types.Contract (fstToFsContractId contractId)
    | DestroyLock -> Destroy
    | FeeLock -> Fee
    | ActivationSacrificeLock -> ActivationSacrifice
    | ExtensionSacrificeLock (version,cHash) -> ExtensionSacrifice (ContractId (version, Hash.Hash cHash))
    | CoinbaseLock (blockNumber,pkHash) ->
        Coinbase (blockNumber, Hash.Hash pkHash)
    | HighVLock (identifier, (Prims.Mkdtuple2 (_, bytes))) ->
        Types.HighVLock (identifier, bytes)

let private fsToFstSpend (spend:Types.Spend) : spend =
    let (Asset (ContractId (version, assetType), subType)) = spend.asset
    { asset = version, Hash.bytes assetType, Hash.bytes subType; amount = spend.amount }

let private fstToFsSpend (spend:spend) : Types.Spend =
    let version, assetType, subType = spend.asset
    { asset = Asset(ContractId (version,Hash.Hash assetType), Hash.Hash subType); amount = spend.amount }

let private fsToFstOutput (output:Types.Output) : output =
    { lock = fsToFstLock output.lock; spend = fsToFstSpend output.spend }

let private fstToFsOutput (output:output) : Types.Output =
    { lock = fstToFsLock output.lock; spend = fstToFsSpend output.spend }

let private fstToFsInput (input: input) : Input =
    match input with
    | PointedOutput (outpoint, output) ->
        let outpoint = { Outpoint.txHash = Hash.Hash outpoint.txHash; index = outpoint.index }
        let output = fstToFsOutput output
        Input.PointedOutput <| (outpoint, output)
    | Mint spend ->
        Input.Mint <| fstToFsSpend spend

let private fsToFstInput (input: Input) : input =
    match input with
    | Input.PointedOutput (outpoint, output) ->
        let outpoint = { txHash = Hash.bytes outpoint.txHash; index = outpoint.index }
        let output = fsToFstOutput output
        PointedOutput <| (outpoint, output)
    | Input.Mint spend ->
        Mint <| fsToFstSpend spend

let rec fsToFstList : list<'a> -> Prims.list<'a> = function
    | hd :: tl -> Prims.Cons (hd, (fsToFstList tl))
    | [] -> Prims.Nil

let rec fstToFsList : Prims.list<'a> -> list<'a> = function
    | Prims.Cons(hd, tl) -> hd::fstToFsList tl
    | Prims.Nil -> []

let private fsToFstPointedOutput ((outpoint, output):PointedOutput) : pointedOutput =
    { txHash = Hash.bytes outpoint.txHash; index = outpoint.index }, fsToFstOutput output

let private fsToFstOutpointOutputTuple (outpoint:Types.Outpoint, output) : (outpoint * output) =
    { txHash = Hash.bytes outpoint.txHash; index = outpoint.index }, fsToFstOutput output

let private fstToFsTx: txSkeleton -> TxSkeleton.T = function
    | {inputs=_,inputMap; outputs=_,outputMap} ->
        let pInputs = inputMap  |> Map.toList
                                |> List.collect(snd >> snd)
                                |> List.sortBy fst
                                |> List.map (snd >> fstToFsInput)
        let outputs = outputMap |> Map.toList
                                |> List.collect(snd >> snd)
                                |> List.sortBy fst
                                |> List.map (snd >> fstToFsOutput)
         
        { pInputs=pInputs; outputs=outputs }

let fstTofsMessage (msg: Zen.Types.Main.message) : Consensus.Types.Message =
    { recipient = fstToFsContractId msg.recipient
      command = fstToFsString msg.command
      body = fstToFsOption msg.body }

let convertResult (result : contractReturn) =
    fstToFsTx result.tx,
    result.message
        |> fstToFsOption
        |> Option.map fstTofsMessage,
    result.state

let fsToFstWallet (wallet:PointedOutput list) : Prims.list<pointedOutput> =
    List.map fsToFstPointedOutput wallet
    |> fsToFstList

let convertContext (context:ContractContext) : context =
    match context with
    | {blockNumber=blockNumber;timestamp=timestamp}
        -> {blockNumber=blockNumber;timestamp=timestamp}

let fsToFstTxSkeleton (txSkeleton:TxSkeleton.T) : txSkeleton =
    let insertInput txSkeleton pointedOutput =
        insertInput pointedOutput txSkeleton
    let insertOutput txSkeleton output =
        insertOutput output txSkeleton

    let inputs = txSkeleton.pInputs |> List.map fsToFstInput
    let outputs = txSkeleton.outputs |> List.map fsToFstOutput

    let txSkeletonWithInputsOnly =
        List.fold insertInput emptyTxSkeleton inputs

    List.fold insertOutput txSkeletonWithInputsOnly outputs

let listLength : Prims.list<'a> -> Prims.nat = Prims.length

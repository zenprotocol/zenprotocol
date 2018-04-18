module Consensus.ZFStar
#nowarn "664"   // Ignore type erasure warnings.
#nowarn "62"    // Ignore ML compatibility warnings.

open Operators.Checked

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.TxSkeleton
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

let fsToFstLock (outputLock:Types.Lock) : lock =
    match outputLock with
    | Consensus.Types.PK (Hash.Hash pkHash) ->
        PKLock pkHash
    | Consensus.Types.Contract (Hash.Hash pkHash) ->
        ContractLock pkHash
    | Destroy ->
        DestroyLock
    | Fee ->
        FeeLock
    | ActivationSacrifice ->
        ActivationSacrificeLock
    | ExtensionSacrifice (Hash.Hash cHash) ->
        ExtensionSacrificeLock cHash
    | Coinbase (blockNumber, (Hash.Hash pkHash)) ->
        CoinbaseLock (blockNumber,pkHash)

let fsToFstOutpoint (o:Outpoint) : outpoint = {txHash = Hash.bytes o.txHash;index = o.index}

let fsToFstPublicKey (Crypto.PublicKey pk) : publicKey = pk

let fsToFstSignature (Crypto.Signature signature) : signature = signature

let fstToFsLock (outputLock:lock) : Types.Lock =
    match outputLock with
    | PKLock pkHash ->
        Consensus.Types.PK (Hash.Hash pkHash)
    | ContractLock pkHash ->
        Consensus.Types.Contract (Hash.Hash pkHash)
    | DestroyLock -> Destroy
    | FeeLock -> Fee
    | ActivationSacrificeLock -> ActivationSacrifice
    | ExtensionSacrificeLock cHash -> ExtensionSacrifice (Hash.Hash cHash)
    | CoinbaseLock (blockNumber,pkHash) ->
            Coinbase (blockNumber, Hash.Hash pkHash)

let private fsToFstSpend (spend:Types.Spend) : spend =
    let tokenContract, tokenHash = spend.asset
    { asset = Hash.bytes tokenContract, Hash.bytes tokenHash; amount = spend.amount }

let private fstToFsSpend (spend:spend) : Types.Spend =
    let tokenContract, tokenHash = spend.asset
    { asset = Hash.Hash tokenContract, Hash.Hash tokenHash; amount = spend.amount }

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

let rec private fsToFstList : list<'a> -> Prims.list<'a> = function
    | hd :: tl -> Prims.Cons (hd, (fsToFstList tl))
    | [] -> Prims.Nil

let rec private fstToFsList : Prims.list<'a> -> list<'a> = function
    | Prims.Cons(hd, tl) -> hd::fstToFsList tl
    | Prims.Nil -> []

let private fsToFstPointedOutput ((outpoint, output):PointedOutput) : pointedOutput =
    { txHash = Hash.bytes outpoint.txHash; index = outpoint.index }, fsToFstOutput output

let private fsToFstOutpointOutputTuple (outpoint:Types.Outpoint, output) : (outpoint * output) =
    { txHash = Hash.bytes outpoint.txHash; index = outpoint.index }, fsToFstOutput output

let private fstToFsTx (txSkeleton: txSkeleton) : TxSkeleton.T =
    { pInputs = txSkeleton.inputs |> snd
                                  |> Map.toList
                                  |> List.collect(snd >> snd)
                                  |> List.sortBy fst
                                  |> List.map (snd >> fstToFsInput);
     outputs = txSkeleton.outputs |> snd
                                  |> Map.toList
                                  |> List.collect(snd >> snd)
                                  |> List.sortBy fst
                                  |> List.map (snd >> fstToFsOutput) }

let fstTofsMessage (msg: Zen.Types.Main.message) : Consensus.Types.Message =
    { cHash = Hash.Hash msg.cHash
      command = fstToFsString msg.command
      data = fstToFsOption msg.data }

let convertResult (tx, message : Native.option<message>)
    : TxSkeleton.T * Option<Message> =
    fstToFsTx tx, message
                  |> fstToFsOption
                  |> Option.map fstTofsMessage

let convertWallet (wallet:PointedOutput list) : Prims.list<pointedOutput>=
    List.map fsToFstPointedOutput wallet
    |> fsToFstList

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
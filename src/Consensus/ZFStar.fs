module Consensus.ZFStar
#nowarn "664"   // Ignore type erasure warnings.

open Operators.Checked

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.SparseMerkleTree
open Consensus.TxSkeleton
open FSharp.Compatibility.OCaml
open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.TxSkeleton
open Zen.Types.Main
open FStar.Pervasives
open Zen.Vector
open Newtonsoft.Json
open MBrace.FsPickler.Combinators

module Cost = Zen.Cost.Realized

let unCost (Cost.C inj:Zen.Cost.Realized.cost<'Aa, 'An>) : 'Aa = inj.Force()

let pickler = Pickler.auto<Zen.Types.Data.data>

let fsToFstString (s:string) =
    if String.forall(fun c -> Char.code c <= 255) s
    then System.Text.Encoding.ASCII.GetBytes s
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

let fsToFstOption mapper value =
    match value with
    | FSharp.Core.Some value -> mapper value |> FStar.Pervasives.Native.Some
    | FSharp.Core.None -> FStar.Pervasives.Native.None

let fsToFstLock (outputLock:Types.Lock) : lock =
    match outputLock with
    | PK (Hash.Hash pkHash) ->
        PKLock pkHash
    | Contract (Hash.Hash pkHash) ->
        ContractLock pkHash
    | Destroy ->
        DestroyLock
    | Fee ->
        FeeLock
    | ActivationSacrifice ->
        ActivationSacrificeLock
    | Coinbase (blockNumber, (Hash.Hash pkHash)) ->
        CoinbaseLock (blockNumber,pkHash)


let private fstToFsLock (outputLock:lock) : Types.Lock =
    match outputLock with
    | PKLock pkHash ->
        PK (Hash.Hash pkHash)
    | ContractLock pkHash ->
        Contract (Hash.Hash pkHash)
    | DestroyLock -> Destroy
    | FeeLock -> Fee
    | ActivationSacrificeLock -> ActivationSacrifice
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
    let tokenContract, tokenHash = output.spend.asset
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

let private vectorToList (z:Zen.Vector.t<'Aa, _>) : List<'Aa> =
     // 0I's are eraseable
     Zen.Vector.foldl 0L 0L (fun acc e -> Cost.ret (e::acc)) [] z
     |> unCost

let private listToVector (ls:List<'Aa>) : Zen.Vector.t<'Aa, _> =
    let len = List.length ls
    let lsIndexed = List.mapi (fun i elem -> int64 (len - i - 1), elem) ls // vectors are reverse-zero-indexed
    List.fold (fun acc (i,x) -> Zen.Vector.VCons (i, x, acc)) Zen.Vector.VNil lsIndexed

let private fstToFsTx : txSkeleton -> _ = function
    | {inputs=_,inputMap; outputs=_,outputMap} ->
        let inputs =
            Map.toList inputMap
            |> List.collect(fun (_, (_, inputs)) -> inputs)
            |> List.sortBy fst
            |> List.map (snd >> fstToFsInput)
        let outputs =
            Map.toList outputMap
            |> List.collect(fun (_, (_, outputs)) -> outputs)
            |> List.sortBy fst
            |> List.map (snd >> fstToFsOutput)
        Ok {pInputs=inputs; outputs=outputs}

let fstToFsData =
//    Binary.pickle pickler
    JsonConvert.SerializeObject
    >> System.Text.Encoding.ASCII.GetBytes
    >> Types.Data

let fsToFstData (Data data) =
//    Binary.unpickle pickler
    System.Text.Encoding.ASCII.GetString data
    |> JsonConvert.DeserializeObject<Zen.Types.Data.data>

let fstTofsMessage = function
    | Native.option.Some ({ cHash = cHash; command = command; data = data } : Zen.Types.Main.message) ->
        ({
            cHash = Hash.Hash cHash
            command = fstToFsString command
            data = fstToFsData data
        } : Consensus.Types.Message)
        |> Some
        |> Ok
    | Native.option.None ->
        Ok None

let convertResult (tx, message : message Native.option) =
    fstTofsMessage message
    |> Result.bind (fun message ->
        fstToFsTx tx
        |> Result.map (fun tx' -> (tx', message))
    )

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

let vectorLength v =
    match v with
    | VCons (l,_,_) -> l + 1L
    | VNil -> 0L

let listLength : Prims.list<'a> -> Prims.nat = Prims.length
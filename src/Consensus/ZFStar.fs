module Consensus.ZFStar

open Operators.Checked

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

module Cost = Zen.Cost.Realized

let unCost (Cost.C inj:Zen.Cost.Realized.cost<'Aa, 'An>) : 'Aa = inj.Force()

let toResult = function
    | ERR err -> Error err
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
    //| Types.Lock.Contract (Hash.Hash pkHash, null) ->
    //    ContractLock (pkHash, 0I, Empty)
    //| Types.Lock.Contract (Hash.Hash pkHash, [||]) ->
        //ContractLock (pkHash, 0I, Empty)
    | Contract (Hash.Hash pkHash) ->
        ContractLock pkHash
    | Destroy ->
        DestroyLock


    //TODO:
    //| Types.Lock.Contract (Hash.Hash pkHash, bytes) ->
        //let serializer = context.GetSerializer<data<unit>>()
        //let data = serializer.UnpackSingleObject bytes
        //ContractLock (pkHash, getDataPoints data, data)


    | _ ->
        //TODO
        throwNotImplemented "fsToFstLock" (outputLock.ToString())

let private fstToFsLock (outputLock:lock) : Types.Lock =
    match outputLock with
    | PKLock pkHash ->
        PK (Hash.Hash pkHash)


    //TODO:
    | ContractLock pkHash ->
        Contract (Hash.Hash pkHash)
    | DestroyLock -> Destroy
    //| ContractLock (pkHash, _, _) ->
        //Types.Lock.Contract (Hash.Hash pkHash, [||])
    //| ContractLock (pkHash, _, data) ->
        //let serializer = context.GetSerializer<data<unit>>()
        //Types.Lock.Contract (Hash.Hash pkHash, serializer.PackSingleObject data)


    | _ ->
        //TODO
        throwNotImplemented "fstToFsLock" (outputLock.ToString())

let private fsToFstOutput (output:Types.Output) : output =
    let tokenContract, tokenHash = output.spend.asset
    { lock = fsToFstLock output.lock; spend = { asset = Hash.bytes tokenContract, Hash.bytes tokenHash; amount = output.spend.amount }}

let private fstToFsOutput (output:output) : Types.Output =
    let tokenContract, tokenHash = output.spend.asset
    { lock = fstToFsLock output.lock; spend = { asset = Hash.Hash tokenContract, Hash.Hash tokenHash; amount = output.spend.amount }}

let private fstToFsPointedOutput (outpoint, output) : PointedOutput =
    { txHash = Hash.Hash outpoint.txHash; index = outpoint.index }, fstToFsOutput output

let private fsToFstPointedOutput (outpoint:Types.Outpoint, output) : pointedOutput =
    { txHash = Hash.bytes outpoint.txHash; index = outpoint.index }, fsToFstOutput output

let private vectorToList (z:Zen.Vector.t<'Aa, _>) : List<'Aa> =
     // 0I's are eraseable
     Zen.Vector.foldl 0I 0I (fun acc e -> Cost.ret (e::acc)) [] z
     |> unCost

let private listToVector (ls:List<'Aa>) : Zen.Vector.t<'Aa, _> =
    let len = List.length ls
    let lsIndexed = List.mapi (fun i elem -> bigint (len - i - 1), elem) ls // vectors are reverse-zero-indexed
    List.fold (fun acc (i,x) -> Zen.Vector.VCons (i, x, acc)) Zen.Vector.VNil lsIndexed

let private fstToFsTx : txSkeleton -> _ = function
    | {inputs=_,inputMap; outputs=_,outputMap} ->
        let inputs =
            Map.toList inputMap
            |> List.collect(fun (_, (_, inputs)) -> inputs)
            |> List.sortBy fst
            |> List.map (snd >> fstToFsPointedOutput)
        let outputs =
            Map.toList outputMap
            |> List.collect(fun (_, (_, outputs)) -> outputs)
            |> List.sortBy fst
            |> List.map (snd >> fstToFsOutput)
        Ok {pInputs=inputs; outputs=outputs}
    | _ ->
        Error "malformed txSkeleton"

let fstTofsMessage = function
    | Native.option.Some { Zen.Types.Main.message.cHash = cHash; command = command } ->
        Ok <| Some { Consensus.Types.Message.cHash = Hash.Hash cHash; command = command }
    | Native.option.None ->
        Ok None
    | _ ->
        Error "malformed message"

let convertResult (tx, message : message Native.option) =
    fstTofsMessage message
    |> Result.bind (fun message ->
        fstToFsTx tx
        |> Result.map (fun tx' -> (tx', message))
    )

let convertWallet (wallet:PointedOutput list) =
    List.map fsToFstPointedOutput wallet
    |> listToVector

let convertInput (txSkeleton:TxSkeleton.T) : txSkeleton =
    let insertPointedOutput txSkeleton pointedOutput =
        insertPointedOutput pointedOutput txSkeleton
    let insertOutput txSkeleton output =
        insertOutput output txSkeleton
    let inputs = txSkeleton.pInputs |> List.map fsToFstPointedOutput
    let outputs= txSkeleton.outputs |> List.map fsToFstOutput

    let txSkeletonWithInputsOnly =
        List.fold insertPointedOutput emptyTxSkeleton inputs

    List.fold insertOutput txSkeletonWithInputsOnly outputs

let vectorLength v =
    match v with
    | VCons (l,_,_) -> l + 1I
    | VNil -> 0I

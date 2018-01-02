module Consensus.ZFStar

open Consensus.Types
open Consensus.TxSkeleton
open Zen.Types.Extracted
open FStar.Pervasives
open Zen.Vector

let private unCost (Zen.Cost.Realized.C inj:Zen.Cost.Realized.cost<'Aa, 'An>) : 'Aa = inj.Force()

let private throwNotImplemented s1 s2 =
    sprintf "%s %s" s1 s2
    |> System.NotImplementedException
    |> raise
     
let private fsToFstLock (outputLock:Types.Lock) : outputLock =
    match outputLock with 
    | Types.Lock.PK (Hash.Hash pkHash) ->
        PKLock pkHash
    | Types.Lock.Contract (Hash.Hash pkHash, null) ->
        ContractLock (pkHash, 0I, Empty)
    | Types.Lock.Contract (Hash.Hash pkHash, [||]) ->
        ContractLock (pkHash, 0I, Empty)


    //TODO:
    //| Types.Lock.Contract (Hash.Hash pkHash, bytes) ->
        //let serializer = context.GetSerializer<data<unit>>()
        //let data = serializer.UnpackSingleObject bytes
        //ContractLock (pkHash, getDataPoints data, data)


    | _ ->
        //TODO
        throwNotImplemented "fsToFstLock" (outputLock.ToString())

let private fstToFsLock (outputLock:outputLock) : Types.Lock =
    match outputLock with 
    | PKLock (pkHash) ->
        Types.Lock.PK (Hash.Hash pkHash)


    //TODO:
    | ContractLock (pkHash, _, _) ->
        Types.Lock.Contract (Hash.Hash pkHash, [||])
    //| ContractLock (pkHash, _, data) ->
        //let serializer = context.GetSerializer<data<unit>>()
        //Types.Lock.Contract (Hash.Hash pkHash, serializer.PackSingleObject data)


    | _ ->
        //TODO
        throwNotImplemented "fstToFsLock" (outputLock.ToString())

let private fsToFstOutput (output:Types.Output) : output =
    { lock = fsToFstLock output.lock; spend = { asset = Hash.bytes output.spend.asset; amount = output.spend.amount }}

let private fstToFsOutput (output:output) : Consensus.Types.Output =
    { lock = fstToFsLock output.lock; spend = { asset = Hash.Hash output.spend.asset; amount = output.spend.amount }}

let private fstToFsOutpoint (a:outpoint) : Types.Outpoint =
    { txHash = Hash.Hash a.txHash; index = a.index }

let private fsToFstOutpoint (a:Types.Outpoint) : outpoint =
    { txHash = Hash.bytes a.txHash; index = a.index }

let private vectorToList (z:Zen.Vector.t<'Aa, _>) : List<'Aa> =
     // 0I's are eraseable
     Zen.Vector.foldl 0I 0I (fun acc e -> Zen.Cost.Realized.ret (e::acc)) [] z 
     |> unCost
     |> List.rev

let private listToVector (ls:List<'Aa>) : Zen.Vector.t<'Aa, _> =
    let len = List.length ls 
    let lsIndexed = List.mapi (fun i elem -> bigint (len - i - 1), elem) ls // vectors are reverse-zero-indexed
    List.foldBack (fun (i,x) acc -> Zen.Vector.VCons (i, x, acc)) lsIndexed Zen.Vector.VNil

let convertResult (Tx (_, outpoints, _, outputs, _)) =
    let vectorToList f vector = List.map f (vectorToList vector)
    {
        inputs = vectorToList fstToFsOutpoint outpoints
        outputs = vectorToList fstToFsOutput outputs
    }

let convertInput txSkeleton =
    let listToVector f list = listToVector (List.map f list)
    Tx (
        bigint (List.length txSkeleton.inputs), 
        listToVector fsToFstOutpoint txSkeleton.inputs, 
        bigint (List.length txSkeleton.outputs), 
        listToVector fsToFstOutput txSkeleton.outputs, 
        Native.option.None
    )
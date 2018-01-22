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
    | PK (Hash.Hash pkHash) ->
        PKLock pkHash
    //| Types.Lock.Contract (Hash.Hash pkHash, null) ->
    //    ContractLock (pkHash, 0I, Empty)
    //| Types.Lock.Contract (Hash.Hash pkHash, [||]) ->
        //ContractLock (pkHash, 0I, Empty)
    | Contract (Hash.Hash pkHash) ->
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
        PK (Hash.Hash pkHash)


    //TODO:
    | ContractLock (pkHash, _, _) ->
        Contract (Hash.Hash pkHash)
    //| ContractLock (pkHash, _, _) ->
        //Types.Lock.Contract (Hash.Hash pkHash, [||])
    //| ContractLock (pkHash, _, data) ->
        //let serializer = context.GetSerializer<data<unit>>()
        //Types.Lock.Contract (Hash.Hash pkHash, serializer.PackSingleObject data)


    | _ ->
        //TODO
        throwNotImplemented "fstToFsLock" (outputLock.ToString())

let private fsToFstOutput (output:Types.Output) : output =
    { lock = fsToFstLock output.lock; spend = { asset = Hash.bytes output.spend.asset; amount = output.spend.amount }}

let private fstToFsOutput (output:output) : Types.Output =
    { lock = fstToFsLock output.lock; spend = { asset = Hash.Hash output.spend.asset; amount = output.spend.amount }}

let private fstToFsPointedOutput (outpoint, output) : PointedOutput =
    { txHash = Hash.Hash outpoint.txHash; index = outpoint.index }, fstToFsOutput output

let private fsToFstPointedOutput (outpoint:Types.Outpoint, output) : pointedOutput =
    { txHash = Hash.bytes outpoint.txHash; index = outpoint.index }, fsToFstOutput output

let private vectorToList (z:Zen.Vector.t<'Aa, _>) : List<'Aa> =
     // 0I's are eraseable
     Zen.Vector.foldl 0I 0I (fun acc e -> Zen.Cost.Realized.ret (e::acc)) [] z 
     |> unCost

let private listToVector (ls:List<'Aa>) : Zen.Vector.t<'Aa, _> =
    let len = List.length ls 
    let lsIndexed = List.mapi (fun i elem -> bigint (len - i - 1), elem) ls // vectors are reverse-zero-indexed
    List.fold (fun acc (i,x) -> Zen.Vector.VCons (i, x, acc)) Zen.Vector.VNil lsIndexed

let convertResult : Zen.Cost.Realized.cost<result<transactionSkeleton>,unit> -> Result<TxSkeleton, string> =
    unCost >> function
    | OK (Tx (_, pOutputs, _, outputs, _)) ->
        let vectorToList f vector = List.map f (vectorToList vector)
        let pOutputs' = pOutputs |> vectorToList fstToFsPointedOutput
        {
            pInputs = pOutputs'
            outputs = vectorToList fstToFsOutput outputs
        }
        |> Ok
    | ERR err -> Error err
    | EX err -> Error err.Message //TODO: remove EX

let convertInput txSkeleton =
    let listToVector f list = listToVector (List.map f list)
    Tx (
        bigint (List.length txSkeleton.pInputs), 
        txSkeleton.pInputs |> listToVector fsToFstPointedOutput, 
        bigint (List.length txSkeleton.outputs), 
        txSkeleton.outputs |> listToVector fsToFstOutput, 
        Native.option.None
    )
module Consensus.ZFStar

open Operators.Checked

open Consensus.Types
open Consensus.TxSkeleton
open Zen.Types.Extracted
open Zen.Types.TxSkeleton
open FStar.Pervasives
open Zen.Vector

module Cost = Zen.Cost.Realized

let private unCost (Cost.C inj:Zen.Cost.Realized.cost<'Aa, 'An>) : 'Aa = inj.Force()

let private throwNotImplemented s1 s2 =
    sprintf "%s %s" s1 s2
    |> System.NotImplementedException
    |> raise

let private fsToFstLock (outputLock:Types.Lock) : lock =
    match outputLock with
    | PK (Hash.Hash pkHash) ->
        PKLock pkHash
    //| Types.Lock.Contract (Hash.Hash pkHash, null) ->
    //    ContractLock (pkHash, 0I, Empty)
    //| Types.Lock.Contract (Hash.Hash pkHash, [||]) ->
        //ContractLock (pkHash, 0I, Empty)
    | Contract (Hash.Hash pkHash) ->
        ContractLock pkHash


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
     Zen.Vector.foldl 0I 0I (fun acc e -> Cost.ret (e::acc)) [] z
     |> unCost

let private listToVector (ls:List<'Aa>) : Zen.Vector.t<'Aa, _> =
    let len = List.length ls
    let lsIndexed = List.mapi (fun i elem -> bigint (len - i - 1), elem) ls // vectors are reverse-zero-indexed
    List.fold (fun acc (i,x) -> Zen.Vector.VCons (i, x, acc)) Zen.Vector.VNil lsIndexed

let convertResult (txSkeleton : Cost.t<result<txSkeleton>,unit>)
    : Result<TxSkeleton, string> =
    match unCost txSkeleton with
    | ERR err -> Error err
    | EX err -> Error err.Message //TODO: remove EX
    | OK {inputs=inputs; outputs=outputs} ->
        let inputs =
            Map.toList inputs
            |> List.collect(fun (_, (_, inputs)) -> inputs)
            |> List.map fstToFsPointedOutput
        let outputs =
            Map.toList outputs
            |> List.collect(fun (_, (_, outputs)) -> outputs)
            |> List.map fstToFsOutput
        Ok {pInputs=inputs; outputs=outputs}

let private addInputToMap map (pointedOutput:pointedOutput) =
    let {spend.asset = asset; amount = amount} = (snd pointedOutput).spend

    if Map.containsKey asset map then
        let oldAmount, pointedOutputs = Map.find asset map
        map |> Map.add asset (amount + oldAmount, pointedOutput :: pointedOutputs)
    else
        map |> Map.add asset (amount, [pointedOutput])

let private addOutputToMap map (output:output) =
    let { spend.asset = asset; amount = amount } = output.spend

    if Map.containsKey asset map then
        let oldAmount, outputs = Map.find asset map
        map |> Map.add asset (amount + oldAmount, output :: outputs)
    else
        map |> Map.add asset (amount, [output])

let convertInput (txSkeleton:TxSkeleton) : txSkeleton =
    let inputs =
        txSkeleton.pInputs
        |> List.map fsToFstPointedOutput
        |> List.fold addInputToMap Map.empty
    let outputs =
        txSkeleton.outputs
        |> List.map fsToFstOutput
        |> List.fold addOutputToMap Map.empty
    {inputs=inputs; outputs=outputs}

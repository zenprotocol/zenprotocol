module Zen.Types.TxSkeleton

open Microsoft.FSharp.Core.Operators.Checked
open Zen.Types.Extracted
open FSharpx.Collections.Map

module Cost = Zen.Cost.Realized
module V = Zen.Vector

type txSkeleton = {
    inputs : Collections.Map<asset, list<pointedOutput>>
    outputs: Collections.Map<asset, list<output>>
    }

let getAvailableTokens
        (asset : asset)
        (txSkeleton : txSkeleton)
        : Cost.t<uint64,unit> =

    lazy(

        let unlockedAmount =
          match Map.tryFind asset txSkeleton.inputs with
          | None -> 0UL
          | Some pointedOutputs ->
          pointedOutputs |> List.sumBy(fun (_,{spend={amount=amount}}) -> amount)

        let lockedAmount =
            match Map.tryFind asset txSkeleton.outputs with
            | None -> 0UL
            | Some outputs ->
                outputs |> List.sumBy(fun {spend={amount=amount}} -> amount)

        unlockedAmount - lockedAmount
    )
    |> Cost.C

let addInput
        (pointedOutput:pointedOutput)
        (txSkeleton:txSkeleton)
        : Cost.t<txSkeleton, unit> =

    lazy(
        let asset = (snd pointedOutput).spend.asset
        let inputs' =
            insertWith (@) asset [pointedOutput] txSkeleton.inputs
        {txSkeleton with inputs=inputs'}
    )
    |> Cost.C

let rec addInputs
        (_:Prims.nat)
        (pointedOutputs: V.t<pointedOutput, unit>)
        (txSkeleton : txSkeleton)
        : Cost.t<txSkeleton, unit> =

    lazy(
        match pointedOutputs with
        | V.VNil -> txSkeleton
        | V.VCons(n, pointedOutput, pointedOutputs) ->
              addInput pointedOutput txSkeleton
              |> Cost.__force
              |> addInputs n pointedOutputs
              |> Cost.__force
    )
    |> Cost.C

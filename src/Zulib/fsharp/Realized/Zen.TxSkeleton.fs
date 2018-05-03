module Zen.TxSkeleton

open FSharpx.Collections.Map
open Microsoft.FSharp.Core.Operators.Checked
open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.Wallet

module Cost = Zen.Cost.Realized
module U64 = FStar.UInt64
module Native = FStar.Pervasives.Native

let emptyTxSkeleton : txSkeleton =
    { inputs = 0UL, Map.empty
      outputs= 0UL, Map.empty }

let getAvailableTokens (asset : asset) (txSkeleton : txSkeleton) : Cost.t<uint64, unit> =
    lazy (let unlockedAmount =
              match snd txSkeleton.inputs |> Map.tryFind asset with
              | None -> 0UL
              | Some(amount, _) -> amount

          let lockedAmount =
              match snd txSkeleton.outputs |> Map.tryFind asset with
              | None -> 0UL
              | Some(amount, _) -> amount

          unlockedAmount - lockedAmount)
    |> Cost.C

let insertInput (input : input)
    (txSkeleton : txSkeleton) : txSkeleton =
    let { asset = asset; amount = amount } =
        match input with
        | PointedOutput (_, output) -> output.spend
        | Mint spend -> spend

    let num_inputs, inputMap = txSkeleton.inputs
    let inputMap =
        if inputMap |> Map.containsKey asset then
            let oldAmount, inputs = Map.find asset inputMap
            inputMap |> Map.add asset
                            (amount + oldAmount,
                            (num_inputs+1UL, input)::inputs)
        else inputMap |> Map.add asset (amount, [num_inputs+1UL, input])
    { txSkeleton with inputs = num_inputs + 1UL, inputMap }

let insertOutput (output : output) (txSkeleton : txSkeleton) : txSkeleton =
    let { asset = asset; amount = amount } = output.spend

    let num_outputs, outputMap = txSkeleton.outputs
    let outputMap =
        if Map.containsKey asset outputMap then
            let oldAmount, outputs = Map.find asset outputMap
            outputMap |> Map.add asset
                            (amount + oldAmount,
                            (num_outputs+1UL, output)::outputs)
        else outputMap |> Map.add asset (amount, [num_outputs+1UL, output])
    { txSkeleton with outputs = num_outputs+1UL, outputMap }

let addInput (input : input) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (insertInput input txSkeleton) |> Cost.C

let rec private addPointedOutputs (pointedOutputs : Prims.list<pointedOutput>)
        (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (match pointedOutputs with
          | Prims.Nil -> txSkeleton
          | Prims.Cons(pointedOutput, pointedOutputs) ->
              insertInput (PointedOutput pointedOutput) txSkeleton
              |> addPointedOutputs pointedOutputs
              |> Cost.__force)
    |> Cost.C

let rec addInputs (inputs : Prims.list<input>)
        (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (match inputs with
          | Prims.Nil -> txSkeleton
          | Prims.Cons(input, inputs) ->
              insertInput input txSkeleton
              |> addInputs inputs
              |> Cost.__force)
    |> Cost.C

let addOutput (output : output) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (insertOutput output txSkeleton) |> Cost.C

let lockToContract (asset : asset) (amount:uint64) (contractId : contractId)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let lock = ContractLock contractId

          let output =
              { lock = lock;
                spend = {asset=asset;amount=amount} }
          insertOutput output txSkeleton)
    |> Cost.C

let lockToPubKey (asset : asset) (amount:uint64) (pkHash : hash) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let lock = PKLock pkHash

          let output =
              { lock = lock;
                spend = {asset=asset;amount=amount} }
          insertOutput output txSkeleton)
    |> Cost.C

let lockToAddress (asset : asset) (amount:uint64) (address : lock) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (
          let output =
              { lock = address;
                spend = {asset=asset;amount=amount} }
          insertOutput output txSkeleton)
    |> Cost.C

let addChangeOutput (asset : asset) (contractId : contractId)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let tokensAvailable =
              getAvailableTokens asset txSkeleton |> Cost.__force

          lockToContract asset tokensAvailable contractId txSkeleton |> Cost.__force)
    |> Cost.C

let mint (amount : U64.t) (asset : asset)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let mintSpend =
              { asset = asset;
                amount = amount }

          let input = Mint mintSpend

          addInput input txSkeleton |> Cost.__force)
    |> Cost.C

let destroy (amount : U64.t) (asset : asset)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let destroySpend =
              { asset = asset;
                amount = amount }

          let destroyOutput =
              { lock = DestroyLock;
                spend = destroySpend }

          addOutput destroyOutput txSkeleton |> Cost.__force)
    |> Cost.C


let fromWallet (asset:asset) (amount:U64.t)
                (contractId:contractId) (wallet:wallet) (txSkeleton:txSkeleton) =

    lazy (
        let inputs,collectedAmount = collect asset amount wallet Prims.Nil 0UL

        match inputs with
        | Prims.Nil -> Native.None
        | _ ->
            let txSkeleton = addPointedOutputs inputs txSkeleton |> Cost.__force

            // Do we need a change?
            if collectedAmount > amount then
                lockToContract asset (collectedAmount - amount) contractId txSkeleton
                |> Cost.__force
                |> Native.Some
            else
                txSkeleton
                |> Native.Some

    ) |> Cost.C


let isValid (txSkeleton : txSkeleton) : Cost.t<bool, unit> =
    let _, inputMap = txSkeleton.inputs
    let _, outputMap = txSkeleton.outputs
    lazy (if keySet inputMap <> keySet outputMap then false
          else
              inputMap
              |> Map.forall(fun asset (inputAmount, _) ->
                                let outputAmount, _ =
                                           Map.find asset outputMap
                                inputAmount = outputAmount)
    ) |> Cost.C

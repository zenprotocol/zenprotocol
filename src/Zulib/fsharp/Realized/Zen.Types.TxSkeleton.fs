module Zen.Types.TxSkeleton

open FSharpx.Collections.Map
open Microsoft.FSharp.Core.Operators.Checked
open Zen.Types.Extracted

module Cost = Zen.Cost.Realized
module V = Zen.Vector
module U64 = FStar.UInt64

type txSkeleton =
    { inputs : Collections.Map<asset, U64.t * list<pointedOutput>>;
      outputs : Collections.Map<asset, U64.t * list<output>> }

let getAvailableTokens (asset : asset) (txSkeleton : txSkeleton) : Cost.t<uint64, unit> =
    lazy (let unlockedAmount =
              match Map.tryFind asset txSkeleton.inputs with
              | None -> 0UL
              | Some(amount, _) -> amount

          let lockedAmount =
              match Map.tryFind asset txSkeleton.outputs with
              | None -> 0UL
              | Some(amount, _) -> amount

          unlockedAmount - lockedAmount)
    |> Cost.C

let insertPointedOutput (pointedOutput : pointedOutput)
    (txSkeleton : txSkeleton) : txSkeleton =
    let { asset = asset; amount = amount } = (snd pointedOutput).spend

    let inputs =
        if Map.containsKey asset txSkeleton.inputs then
            let oldAmount, pointedOutputs = Map.find asset txSkeleton.inputs
            txSkeleton.inputs
            |> Map.add asset
                   (amount + oldAmount, pointedOutput :: pointedOutputs)
        else txSkeleton.inputs |> Map.add asset (amount, [ pointedOutput ])
    { txSkeleton with inputs = inputs }

let insertOutput (output : output) (txSkeleton : txSkeleton) : txSkeleton =
    let { asset = asset; amount = amount } = output.spend

    let outputs =
        if Map.containsKey asset txSkeleton.inputs then
            let oldAmount, outputs = Map.find asset txSkeleton.outputs
            txSkeleton.outputs
            |> Map.add asset (amount + oldAmount, output :: outputs)
        else txSkeleton.outputs |> Map.add asset (amount, [ output ])
    { txSkeleton with outputs = outputs }

let addInput (pointedOutput : pointedOutput) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (insertPointedOutput pointedOutput txSkeleton) |> Cost.C

let rec addInputs (_ : Prims.nat) (pointedOutputs : V.t<pointedOutput, unit>)
        (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (match pointedOutputs with
          | V.VNil -> txSkeleton
          | V.VCons(n, pointedOutput, pointedOutputs) ->
              insertPointedOutput pointedOutput txSkeleton
              |> addInputs n pointedOutputs
              |> Cost.__force)
    |> Cost.C

let addOutput (output : output) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (insertOutput output txSkeleton) |> Cost.C

let lockToContract (spend : spend) (contractHash : hash)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let lock = ContractLock(contractHash, 0I, Empty)

          let output =
              { lock = lock;
                spend = spend }
          insertOutput output txSkeleton)
    |> Cost.C

let lockToPubKey (spend : spend) (pkHash : hash) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let lock = PKLock pkHash

          let output =
              { lock = lock;
                spend = spend }
          insertOutput output txSkeleton)
    |> Cost.C

let lockToAddress (spend : spend) (address : hash) (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lockToPubKey spend address txSkeleton

let addChangeOutput (asset : asset) (contractHash : contractHash)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let tokensAvailable =
              getAvailableTokens asset txSkeleton |> Cost.__force

          let spend =
              { asset = asset;
                amount = tokensAvailable }
          lockToContract spend contractHash txSkeleton |> Cost.__force)
    |> Cost.C

let mint (amount : U64.t) (contractHash : contractHash)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let mintSpend =
              { asset = contractHash;
                amount = amount }

          let mintLock = ContractLock(contractHash, 0I, Empty)

          let mintOutput =
              { lock = mintLock;
                spend = mintSpend }

          let mintTxHash = Array.create 32 0uy

          let mintOutpoint =
              { txHash = mintTxHash;
                index = 0u }

          let pointedOutput = mintOutpoint, mintOutput
          addInput pointedOutput txSkeleton |> Cost.__force)
    |> Cost.C

let isValid(txSkeleton : txSkeleton) : Cost.t<bool, unit> =
    lazy (if keySet txSkeleton.inputs <> keySet txSkeleton.outputs then false
          else
              txSkeleton.inputs |> Map.forall(fun asset (inputAmount, _) ->
                                       let outputAmount, _ =
                                           Map.find asset txSkeleton.outputs
                                       inputAmount = outputAmount))
    |> Cost.C

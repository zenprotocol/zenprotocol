module Zen.TxSkeleton

open FSharpx.Collections.Map
open Microsoft.FSharp.Core.Operators.Checked
open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.Wallet

module Cost = Zen.Cost.Realized
module V = Zen.Vector
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

let insertPointedOutput (pointedOutput : pointedOutput)
    (txSkeleton : txSkeleton) : txSkeleton =
    let { asset = asset; amount = amount } = (snd pointedOutput).spend

    let num_inputs, inputMap = txSkeleton.inputs
    let inputMap =
        if inputMap |> Map.containsKey asset then
            let oldAmount, pointedOutputs = Map.find asset inputMap
            inputMap |> Map.add asset
                            (amount + oldAmount,
                            (num_inputs+1UL, pointedOutput)::pointedOutputs)
        else inputMap |> Map.add asset (amount, [num_inputs+1UL, pointedOutput])
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
                             
let lockToContract (asset : asset) (amount:uint64) (contractHash : hash)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let lock = ContractLock contractHash

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

let addChangeOutput (asset : asset) (contractHash : contractHash)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let tokensAvailable =
              getAvailableTokens asset txSkeleton |> Cost.__force

          lockToContract asset tokensAvailable contractHash txSkeleton |> Cost.__force)
    |> Cost.C

let mint (amount : U64.t) (contractHash : contractHash)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let mintSpend =
              { asset = contractHash;
                amount = amount }

          let mintLock = ContractLock contractHash

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

let destroy (amount : U64.t) (contractHash : contractHash)
    (txSkeleton : txSkeleton) : Cost.t<txSkeleton, unit> =
    lazy (let destroySpend =
              { asset = contractHash;
                amount = amount }

          let destroyOutput =
              { lock = DestroyLock;
                spend = destroySpend }

          addOutput destroyOutput txSkeleton |> Cost.__force)
    |> Cost.C
    
   
let fromWallet (_: Prims.nat) (asset:asset) (amount:U64.t) 
                (contractHash:hash) (wallet:wallet<Prims.unit>) (txSkeleton:txSkeleton) =
                
    lazy (                
        let n,inputs,collectedAmount = collect asset amount wallet 0I V.VNil 0UL
        
        match inputs with
        | V.VNil -> Native.None
        | _ ->
            let txSkeleton = addInputs n inputs txSkeleton |> Cost.__force
            
            // Do we need a change?
            if collectedAmount > amount then 
                lockToContract asset (collectedAmount - amount) contractHash txSkeleton
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

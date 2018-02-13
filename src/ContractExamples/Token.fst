open Zen.Types
open Zen.Vector
open Zen.Base
open Zen.Cost
open Zen.Asset
open FStar.Mul
        
module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton
        
val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 15
let cf _ _ _ #l _ = ret (64 + (64 + 64 + (l * 128 + 192)) + 19 + 22)

let buy txSkeleton contractHash returnAddress = 
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in
        
  let txSkeleton = 
    Tx.lockToContract zenAsset tokens contractHash txSkeleton
    >>= Tx.mint tokens contractHash
    >>= Tx.lockToAddress contractHash tokens returnAddress in
         
  ET.retT txSkeleton	
    
let redeem #l txSkeleton contractHash returnAddress (wallet:wallet l) = 
  let! tokens = Tx.getAvailableTokens contractHash txSkeleton in

  let txSkeleton = 
    Tx.destroy tokens contractHash txSkeleton
    >>= Tx.lockToAddress zenAsset tokens returnAddress
    >>= Tx.fromWallet zenAsset tokens contractHash wallet in

  ET.of_optionT "contract doesn't have enough zens to pay you" txSkeleton

val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result txSkeleton) (64 + (64 + 64 + (l * 128 + 192)) + 19 + 22)
let main txSkeleton contractHash command returnAddress #l wallet =
  match returnAddress with
  | Some returnAddress -> 
      if command = "redeem" then
        redeem txSkeleton contractHash returnAddress wallet
      else if command = "" || command = "buy" then
        buy txSkeleton contractHash returnAddress
        |> autoInc    
      else 
        ET.autoFailw "unsupported command"
  | None ->
      ET.autoFailw "returnAddress is required"
 

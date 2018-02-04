open Zen.Types
open Zen.Vector
open Zen.Util
open Zen.Base
open Zen.Cost
open Zen.Assets
open FStar.Mul
        
module ET = Zen.ErrorT
module Tx = Zen.TxSkeleton

val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 11
let cf _ _ _ #l _ = 
    ret (64 + l * 128 + 192 + 15 + 9)
          
let get #l txSkeleton contractHash returnAddress (wallet:wallet l) =  
  match returnAddress with
  | Some returnAddress ->         
      let txSkeleton = 
        Tx.lockToAddress zenAsset 10000UL returnAddress txSkeleton
        >>= Tx.fromWallet zenAsset 10000UL contractHash wallet in
        
      ET.of_optionT "contract doesn't have enough zens to pay you" txSkeleton
  | None ->
      ET.autoFailw "returnAddress is required"
    
let init txSkeleton contractHash  = 
  let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in

  let txSkeleton = Tx.lockToContract zenAsset tokens contractHash txSkeleton in
  ET.retT txSkeleton  
  
val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result txSkeleton) (64 + l * 128 + 192 + 15 + 9)
let main txSkeleton contractHash command returnAddress #l wallet =  
  if command <> "init" then
    get txSkeleton contractHash returnAddress wallet
  else
    init txSkeleton contractHash     
    |> autoInc
  
  
 

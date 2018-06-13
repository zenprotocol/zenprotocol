open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Asset
open Zen.Data

module D = Zen.Dictionary
module Tx = Zen.TxSkeleton
module C = Zen.Cost
module RT = Zen.ResultT

let cf _ _ _ _ _ wallet _ =
    4 + 64 + 2 + (64 + (Zen.Wallet.size wallet * 128 + 192) + 0 + 13) + (0 + 5) + 22
    |> C.ret #nat
    
let get txSkeleton contractId (wallet:wallet) returnAddress =
    Tx.lockToAddress zenAsset 10000UL returnAddress txSkeleton
    >>= Tx.fromWallet zenAsset 10000UL contractId wallet
    >>= RT.of_option "contract doesn't have enough zens to pay you"

let init txSkeleton contractId =
    let! tokens = Tx.getAvailableTokens zenAsset txSkeleton in
    Tx.lockToContract zenAsset tokens contractId txSkeleton
    
let main txSkeleton _ contractId command _ messageBody wallet _ =
    let open RT in
    
    begin
        if command <> "init" then
            messageBody 
            >!= tryDict
            >?= D.tryFind "returnAddress"
            >?= tryLock
            |> RT.of_optionT "returnAddress is required"
            >>= get txSkeleton contractId wallet
        else
            init txSkeleton contractId
            |> liftCost 
            |> autoInc
    end
    >>= (fun tx -> RT.ok @ { tx = tx; message = None; state = NoChange })
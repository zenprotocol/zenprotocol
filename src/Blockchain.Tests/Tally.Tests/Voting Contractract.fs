module Blockchain.Tests.ContractCode

let votingContractCode = "    open Zen.Types\nopen Zen.Base\nopen Zen.Cost\nmodule RT = Zen.ResultT\nmodule Tx = Zen.TxSkeleton\nmodule C = Zen.Cost\nlet name = \"Testnet Voting Contract\"\nlet main txSkeleton _ contractId command sender messageBody wallet state =\n  RT.ok @ {\n    tx = txSkeleton;\n    message = None;\n    state = NoChange;\n  }\nlet cf _ _ _ _ _ _ _ =\n    5\n    |> cast nat\n    |> C.ret"

let cgpContractCode = """
open Zen.Base
open Zen.Cost
open Zen.Types
open Zen.Data

module U32    = FStar.UInt32
module RT     = Zen.ResultT
module Dict   = Zen.Dictionary
module TX     = Zen.TxSkeleton
module CR     = Zen.ContractResult
module Asset  = Zen.Asset
module OptT   = Zen.OptionT
module Wallet = Zen.Wallet



let maxOutputs : nat = 100

let payoutBlock : nat = 10

let intervalLength : nat = 100



(*
-------------------------------------------------------------------------------
========== UTILITY FUNCTIONS ==================================================
-------------------------------------------------------------------------------
*)

// tries to map a function over a list.
// if all of the mappings return Some, then returns Some list.
// otherwise returns None.
val tryMap(#a #b: Type)(#n: nat):
    (a -> option b `cost` n)
    -> ls:list a
    -> option (ls':list b{length ls' == length ls}) `cost` (length ls * (n + 20) + 20)
let rec tryMap #a #b #n f ls = //20
    match ls with
    | hd::tl ->
        let! hd' = f hd in
        let! tl' = tryMap f tl in
        begin match hd', tl' with
        | Some hd', Some tl' ->
            let (result: list b{length result == length ls}) = hd'::tl' in
            OptT.some result
        | _ -> OptT.none end
    | [] -> [] |> OptT.incSome (length ls * (n + 20))

val tryFold (#a #b : Type) (#n : nat) :
    (b -> a -> option b `cost` n)
    -> b
    -> ls:list a
    -> option b `cost` (length ls * (n + 12) + 10)
let rec tryFold #a #b #n f x ls = // 10
    let open OptT in
    match ls with
    | [] ->
        incSome (length ls * (n + 12)) x
    | hd :: tl ->
        tryFold f x tl >>= (fun r -> f r hd) // (length ls * (n + 12))



(*
-------------------------------------------------------------------------------
========== PARSING ============================================================
-------------------------------------------------------------------------------
*)

val parseDict:
    option data
    -> result (option (Dict.t data)) `cost` 15
let parseDict data = // 11
    match data with
    | Some data ->
        data
        |> tryDict // 4
        |> RT.ofOptionT "Data parsing failed - the message body isn't a dictionary"
        |> RT.map Some
    | None ->
        RT.incFailw 4 "Data parsing failed - the message body is empty"

val parseField (#a:Type) (#m:nat)
    : (data -> option a `cost` m)
    -> fieldName:string
    -> errMsg:string
    -> option (Dict.t data)
    -> result a `cost` (m + 75)
let parseField #_ #_ parser fieldName errMsg dict = // 11
    let! value = dict >!= Dict.tryFind fieldName >?= parser in // (m + 64)
    match value with
    | Some value ->
        RT.ok value
    | None ->
        RT.failw errMsg

val extractSpend :
    list data
    -> option spend `cost` 84
let extractSpend ls = // 17
    match ls with
    | asset' :: amount' :: [] ->
        let open OptT in
        let sAsset  = tryString asset'       in // 2
        let oAsset  = sAsset >>= Asset.parse in // 64
        let oAmount = tryU64 amount'         in // 2
        oAsset  >>= (fun asset  -> // 2
        oAmount >>= (fun amount -> // 3
            OptT.ret ({ asset=asset; amount=amount })
            ))
    | _ ->
        OptT.incNone 73

val trySpend :
    data
    -> option spend `cost` 91
let trySpend d = // 3
    let open OptT in
    tryList d >>= extractSpend

val extractOutput :
    list data
    -> option output `cost` 107
let extractOutput ls = // 9
    match ls with
    | lock' :: spend' :: [] ->
        let open OptT in
        let oLock  = tryLock lock'   in // 2
        let oSpend = trySpend spend' in // 91
        oLock  >>= (fun lock  -> // 2
        oSpend >>= (fun spend -> // 3
            OptT.ret ({ lock=lock; spend=spend })
            ))
    | _ ->
        OptT.incNone 98

val tryOutput :
    data
    -> option output `cost` 114
let tryOutput d = // 3
let open OptT in
    tryList d >>= extractOutput

val extractOutputList :
    ls:list data
    -> option (ls':list output { length ls' == length ls }) `cost` (length ls * 134 + 22)
let extractOutputList ls = // 2
    tryMap tryOutput ls

val toBounded :
    size:nat
    -> list data
    -> option (ls:list data { let len = length ls in 1 <= len && len <= size }) `cost` 11
let toBounded size ls = // 11
    if (let len = length ls in 1 <= len && len <= size)
        then OptT.ret (ls <: ls : list data { length ls <= size })
        else OptT.none

val extractOutputListBounded :
    size:nat
    -> ls:list data { let len = length ls in 1 <= len && len <= size }
    -> option (ls':list output { let len' = length ls' in 1 <= len' && len' <= size }) `cost` (size * 134 + 22 + 11)
let extractOutputListBounded size ls = // 11
    let open OptT in
    (extractOutputList ls $> (fun ls' -> ls' <: ls':list output { let len' = length ls' in 1 <= len' && len' <= size }))
    |> inc ((size - length ls) * 134)

val parseRawOutputs :
    option (Dict.t data)
    -> result (ls:list data) `cost` 83
let parseRawOutputs dict = // 4
    parseField tryList "Outputs" "Couldn't parse Outputs" dict

val parseOutputs :
    size:nat
    -> option (Dict.t data)
    -> result (ls:list output { let len = length ls in 1 <= len && len <= size }) `cost` (83 + 11 + (size * 134 + 22 + 11) + 15)
let parseOutputs size dict = // 15
    let open RT in
    ret dict
    >>= parseRawOutputs                                                                    // 83
    >>= (toBounded size                >> ofOptionT "Outputs list size is out of bounds" ) // 11
    >>= (extractOutputListBounded size >> ofOptionT "Invalid outputs structure")           // (size * 134 + 22 + 11)



(*
-------------------------------------------------------------------------------
========== PAYOUT =============================================================
-------------------------------------------------------------------------------
*)

val lockOutput :
    contractId
    -> (w:wallet)
    -> txSkeleton
    -> output
    -> option txSkeleton `cost` (Wallet.size w * 128 + 192 + 64 + 19)
let lockOutput contractId w txSkel outp = // 19
    let asset  = outp.spend.asset  in
    let amount = outp.spend.amount in
    let open OptT in
    ret txSkel
    >>= TX.fromWallet asset amount contractId w               // (Wallet.size w * 128 + 192)
    >>= (TX.lockToAddress asset amount outp.lock >> liftCost) // 64

val lockOutputs:
    contractId
    -> (w:wallet)
    -> txSkeleton
    -> (ls:list output)
    -> option txSkeleton `cost` (length ls * ((Wallet.size w * 128 + 192 + 64 + 19) + 12) + 10 + 5)
let lockOutputs contractId w txSkel ls = // 5
    tryFold (lockOutput contractId w) txSkel ls

val lockOutputsBounded :
    size:nat
    -> contractId
    -> (w:wallet)
    -> txSkeleton
    -> (ls:list output { let len = length ls in 1 <= len && len <= size })
    -> option txSkeleton `cost` (size * ((Wallet.size w * 128 + 192 + 64 + 19) + 12) + 10 + 5 + 23)
let lockOutputsBounded size contractId w txSkel ls = // 23
    let open OptT in
    lockOutputs contractId w txSkel ls
    |> inc ((size - length ls) * ((Wallet.size w * 128 + 192 + 64 + 19) + 12))



(*
-------------------------------------------------------------------------------
========== VALIDATION =========================================================
-------------------------------------------------------------------------------
*)

val isPayoutBlock :
    context
    -> bool `cost` 9
let isPayoutBlock context = // 14
    let r = context.blockNumber `U32.rem` (U32.uint_to_t intervalLength) in
    r `U32.eq` (U32.uint_to_t payoutBlock)
    |> ret

val validateBlockNumber (#a:Type) :
    context
    -> a
    -> result a `cost` 16
let validateBlockNumber #_ context txSkel = // 7
    let! b = isPayoutBlock context in // 9
    if b
        then RT.ret txSkel
        else RT.failw "Not a payout block"



(*
-------------------------------------------------------------------------------
========== MAIN ===============================================================
-------------------------------------------------------------------------------
*)

let main txSkel context contractId command sender messageBody w state = // 20
    let open RT in
    ret messageBody
    >>= validateBlockNumber context
        // 16
    >>= parseDict
        // 15
    >>= parseOutputs maxOutputs
        // (83 + 11 + (maxOutputs * 134 + 22 + 11) + 15)
    >>= (lockOutputsBounded maxOutputs contractId w txSkel >> ofOptionT "Insufficient funds")
        // (maxOutputs * ((Wallet.size w * 128 + 192 + 64 + 19) + 12) + 10 + 5 + 23)
    >>= CR.ofTxSkel
        // 3

val cf:
       txSkel     : txSkeleton
    -> context    : context
    -> command    : string
    -> sender     : sender
    -> messageBody: option data
    -> w          : wallet
    -> state      : option data
    -> nat `cost` 43
let cf _ _ _ _ _ w _ =
    ((15
    + (83 + 11 + (maxOutputs * 134 + 22 + 11) + 15)
    + (maxOutputs * ((Wallet.size w * 128 + 192 + 64 + 19) + 12) + 10 + 5 + 23)
    + 16
    + 3
    + 20)
    <: nat) |> ret
"""
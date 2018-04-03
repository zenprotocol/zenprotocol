module Zen.ContractResult.NoMessage

open Zen.Base
open Zen.Types
open Zen.Cost

module CR = Zen.ContractResult
module ET = Zen.ErrorT

type t = CR.t

val ofResult: result txSkeleton -> CR.t `cost` 0
let ofResult =
    function
    | OK txSkeleton -> ET.ret (txSkeleton,None)
    | EX e -> ET.autoFail e
    | ERR msg -> ET.autoFailw msg

val ofOption: string -> option txSkeleton -> CR.t `cost` 0
let ofOption msg =
    function
    | Some txSkeleton -> ET.ret (txSkeleton,None)
    | None -> ET.failw msg

val ret: txSkeleton -> CR.t `cost` 0
let ret txSkeleton = ET.ret (txSkeleton,None)

module Zen.ContractResult.NoMessage

open Zen.Base
open Zen.Types
open Zen.Cost

module CR = Zen.ContractResult
module RT = Zen.ResultT

type t = CR.t

val ofResult: result txSkeleton -> CR.t `cost` 0
let ofResult =
    function
    | OK txSkeleton -> RT.ok (txSkeleton,None)
    | EX e -> RT.autoFail e
    | ERR msg -> RT.autoFailw msg

val ofOption: string -> option txSkeleton -> CR.t `cost` 0
let ofOption msg =
    function
    | Some txSkeleton -> RT.ok (txSkeleton,None)
    | None -> RT.failw msg

val ret: txSkeleton -> CR.t `cost` 0
let ret txSkeleton = RT.ok (txSkeleton,None)

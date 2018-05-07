module Consensus.ValidationError

open Infrastructure
open Result

type ValidationError =
    | Orphan
    | DoubleSpend
    | ContractNotActive
    | BadContract
    | General of string

let GeneralError msg =
    msg |> General |> Error

let result = new Result.ResultBuilder<ValidationError>()
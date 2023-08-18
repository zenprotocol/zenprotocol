module Consensus.Fund

open Types

type T = Map<Asset, uint64>

let inline find asset fund =
    Map.tryFind asset fund |> Option.defaultValue 0UL

let inline addOrUpdate asset delta fund =
    let currentAmount = find asset fund
    Map.add asset (currentAmount + delta) fund

let addSpend fund (spend: Spend) =
    addOrUpdate spend.asset spend.amount fund

let replaceSpend fund (spend: Spend) =
    Map.add spend.asset spend.amount fund

let empty = Map.empty

let accumulateSpendsInto chooser fund xs =
    xs
    |> List.choose chooser
    |> List.fold addSpend fund

let accumulateSpends chooser xs =
    accumulateSpendsInto chooser empty xs
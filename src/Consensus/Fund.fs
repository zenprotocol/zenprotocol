module Consensus.Fund

open Types

type T = Map<Asset, uint64>

let find (asset : Asset) (fund : T) : uint64 =
     Map.tryFind asset fund
     |> Option.defaultValue 0UL

let add (asset : Asset) (amount : uint64) (fund : T) : T =
     let oldAmount =
          Map.tryFind asset fund
          |> Option.defaultValue 0UL
     Map.add asset (oldAmount + amount) fund

let addSpend (fund : T) (spend : Spend) : T =
    add spend.asset spend.amount fund

let replace (asset : Asset) (amount : uint64) (fund : T) : T =
     Map.add asset amount fund

let replaceSpend (fund : T) (spend : Spend) : T =
     replace spend.asset spend.amount fund

let empty = Map.empty

let accumulateSpendsInto (chooser : 'a -> Spend option) (fund : T) (xs : 'a list) : T =
  xs
  |> List.choose chooser
  |> List.fold addSpend fund

let accumulateSpends (chooser : 'a -> Spend option) (xs : 'a list) : T =
  accumulateSpendsInto chooser empty xs
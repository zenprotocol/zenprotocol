module Blockchain.Tally.Tally

module Result = Infrastructure.Result
module Option = Infrastructure.Option

open Consensus
open Types
open Checked
open Consensus.Crypto
open Consensus.Serialization.Serialization

type allocation = byte

type payout = Recipient * Spend list

type PK = Crypto.PublicKey

type PKHash = Hash.Hash

type Check = Option<unit>

type CoinbaseRatio =
    | CoinbaseRatio of byte

type T =
    {
        coinbaseRatio : Map<CoinbaseRatio, uint64>
        payout        : Map<payout, uint64>
    }

let empty : T =
    {
        coinbaseRatio = Map.empty
        payout        = Map.empty
    }

let isEmpty (tally : T) : bool =
    Map.isEmpty tally.coinbaseRatio && Map.isEmpty tally.payout

let option = FSharpx.Option.maybe

let private (|@>) x f = Option.map f x

let check (b : bool) : Check =
    option { if b then return () }

let (|<-) (x : Option<'a>) (y : 'b) : Option<'b> =
    x |@> fun _ -> y

let (|<--) (x : Option<'a>) (y : Lazy<'b>) : Option<'b> =
    x |> Option.map (fun _ -> y.Force())

let ( *>) : Option<'a> -> Option<'b> -> Option<'b> =
    FSharpx.Option.( *>)

let ignoreResult (x : Option<'a>) : Check =
    x |<- ()

let allocationToCoinbaseRatio (allocation : allocation) : CoinbaseRatio =
    CoinbaseRatio (100uy - allocation)

let coinbaseRatioToAllocation (CoinbaseRatio coinbaseRatio : CoinbaseRatio) : allocation =
    (100uy - coinbaseRatio)

let getRatio (CoinbaseRatio x) = x

let mapMapKeys f m = m |> Map.toSeq |> Seq.map (fun (k,x) -> (f k, x)) |> Map.ofSeq

let accumulate (key : 'a) (value : uint64) (map : Map<'a,uint64>) : Map<'a,uint64> =
    let oldValue = Map.tryFind key map |> Option.defaultValue 0UL
    map |> if value > 0UL then Map.add key (oldValue + value) else id

let integrateMaps (amounts : Map<'key1,uint64>) (keys : Map<'key1,'key2>) : Map<'key2,uint64> =
    
    let addToMap (optKey : Option<'a>) (amount : uint64) (m : Map<'a, uint64>) : Map<'a, uint64> =
        optKey
        |@> fun key -> accumulate key amount m
        |> Option.defaultValue m
    
    let addAmount (acc : Map<'key2,uint64>) (oldKey : 'key1) (amount : uint64) : Map<'key2,uint64> =
        keys
        |> Map.tryFind oldKey
        |> fun key -> addToMap key amount acc
    
    Map.fold addAmount Map.empty amounts

let integrateBallots (balances : Map<PKHash, uint64>) (votes : Map<PK, 'a>) : Map<'a, uint64> =
    votes
    |> mapMapKeys PublicKey.hash
    |> integrateMaps balances

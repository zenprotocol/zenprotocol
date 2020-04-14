module Tally.Tests.TallyTests

open Blockchain.Tally.Tally
open Consensus.Types
open Consensus
open NUnit.Framework

let someone = PKRecipient Hash.zero

let asset0 = Asset.defaultOf (ContractId (1u, Hash.zero))

let ``should be`` (expected : 'a) (actual : 'a) : unit =
     if expected = actual
          then ()
          else failwithf "expected: %A, actual: %A" expected actual



[<Test>]
let ``validateCoibaseRatio coinbase correction cap`` () =
     
     let env = {
          coinbaseCorrectionCap = CoinbaseRatio 90uy
          lowerCoinbaseBound    = CoinbaseRatio 10uy
          lastCoinbaseRatio     = CoinbaseRatio 20uy
          lastFund              = Map.empty
     }
     
     let vote = 20uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 21uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 22uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 23uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` None
     
     let vote = 24uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` None
     
     let vote = 19uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 18uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 17uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` None
     
     let vote = 16uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` None

[<Test>]
let ``validateCoibaseRatio lower coinbase bound`` () =
     
     let env = {
          coinbaseCorrectionCap = CoinbaseRatio 80uy
          lowerCoinbaseBound    = CoinbaseRatio 20uy
          lastCoinbaseRatio     = CoinbaseRatio 22uy
          lastFund              = Map.empty
     }
     
     let vote = 22uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 21uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 20uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 19uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` None
     
     let env = {
          coinbaseCorrectionCap = CoinbaseRatio 20uy
          lowerCoinbaseBound    = CoinbaseRatio 5uy
          lastCoinbaseRatio     = CoinbaseRatio 8uy
          lastFund              = Map.empty
     }
     
     let vote = 8uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 7uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 6uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 5uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 4uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` None

[<Test>]
let ``validateCoibaseRatio upper coinbase bound`` () =
     
     let env = {
          coinbaseCorrectionCap = CoinbaseRatio 80uy
          lowerCoinbaseBound    = CoinbaseRatio 20uy
          lastCoinbaseRatio     = CoinbaseRatio 98uy
          lastFund              = Map.empty
     }
     
     let vote = 99uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 100uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` (Some <| CoinbaseRatio vote)
     
     let vote = 101uy
     validateCoinbaseRatio env (CoinbaseRatio vote)
     |> ``should be`` None
     

[<Test>]
let ``validatePayout tests`` () =
     
     let env = {
          coinbaseCorrectionCap = CoinbaseRatio 90uy
          lowerCoinbaseBound    = CoinbaseRatio 10uy
          lastCoinbaseRatio     = CoinbaseRatio 20uy
          lastFund              = Map.empty
     }
     
     let vote = (someone, [])
     validatePayout env vote
     |> ``should be`` None
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}])
     validatePayout env vote
     |> ``should be`` None
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=6UL}])
     validatePayout env vote
     |> ``should be`` None
     
     let env = { env with lastFund = Map.empty |> Map.add Asset.Zen 1UL }
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let env = { env with lastFund = Map.empty |> Map.add Asset.Zen 2UL }
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let env = { env with lastFund =
          Map.empty
          |> Map.add Asset.Zen 2UL
          |> Map.add asset0 5UL
          }
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=1UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=5UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=6UL}])
     validatePayout env vote
     |> ``should be`` None

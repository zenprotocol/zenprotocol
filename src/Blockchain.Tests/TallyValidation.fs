module Tally.Tests.TallyTests

open Blockchain.Tally.Tally
open Blockchain.Tally.Voting
open Consensus.Types
open Consensus
open Consensus.Tests
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Infrastructure

let someone = PKRecipient Hash.zero
let cgpContract = ContractRecipient BlockCGPTests.cgpContractId
let someoneElse = ContractRecipient (ContractId (0ul, Hash.zero))

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
          candidates            = []
          balances              = Map.empty
          allocationBallots     = Map.empty
          payoutBallots         = Map.empty
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
          candidates            = []
          balances              = Map.empty
          allocationBallots     = Map.empty
          payoutBallots         = Map.empty
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
          candidates            = []
          balances              = Map.empty
          allocationBallots     = Map.empty
          payoutBallots         = Map.empty
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
          candidates            = []
          balances              = Map.empty
          allocationBallots     = Map.empty
          payoutBallots         = Map.empty
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
     

(*
// TODO: fix it
[<Test; IgnoreAttribute("add Nominees")>]
let ``validatePayout tests`` () =
     
     let env = {
          coinbaseCorrectionCap = CoinbaseRatio 90uy
          lowerCoinbaseBound    = CoinbaseRatio 10uy
          lastCoinbaseRatio     = CoinbaseRatio 20uy
          candidates            = []
          balances              = Map.empty
          allocationBallots     = Map.empty
          payoutBallots         = Map.empty
     }
     
     let vote = (someone, [])
     validatePayout env vote
     |> ``should be`` None
     
     let vote = (someone, [{asset=Asset.Zen; amount=0UL}])
     validatePayout env vote
     |> ``should be`` None
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=6UL}])
     validatePayout env vote
     |> ``should be`` None
     
     let genPk = PublicKey.fromString "02bc15ee2d0073ec3f9d0f9a61f63027e9d5b6202faed8792836c42cbdb3cd4423" |> Option.get
     let genPkHash = genPk |> PublicKey.hash
     let env = { env with
                    nomineesBallots = Map.add genPk (someone, [{asset=Asset.Zen; amount=1UL }]) Map.empty
                    balances        = Map.add genPkHash 10000UL Map.empty
                  }
     
     let vote = (cgpContract, [{asset=Asset.Zen; amount=1UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let env = { env with lastFund = Map.empty |> Map.add Asset.Zen 2UL }
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let vote = (someone, [{asset=Asset.Zen; amount=0UL}])
     validatePayout env vote
     |> ``should be`` None
     
     let env = { env with
                         lastFund =
                              Map.empty
                              |> Map.add Asset.Zen 2UL
                              |> Map.add asset0 5UL
                         nomineesBallots =
                              Map.empty
                              |> Map.add genPk (someone, [{asset=Asset.Zen; amount=2UL}])
                         balances        = Map.add genPkHash 10000UL Map.empty
                              }
     
     let vote = (cgpContract, [{asset=Asset.Zen; amount=1UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let vote = (someone, [{asset=Asset.Zen; amount=2UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}; {asset=Asset.Zen; amount=2UL}])
     validatePayout env vote
     |> ``should be`` None
     
     let env = { env with
                    lastFund =
                         Map.empty
                         |> Map.add Asset.Zen 2UL
                         |> Map.add asset0 5UL
                    nomineesBallots =
                         Map.empty
                         |> Map.add genPk (someoneElse, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=5UL}])
                    balances        = Map.add genPkHash 10000UL Map.empty
                         }
     
     let vote = (someoneElse, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=5UL}])
     validatePayout env vote
     |> ``should be`` (Some vote)
     
     let vote = (someone, [{asset=Asset.Zen; amount=1UL}; {asset=asset0; amount=6UL}])
     validatePayout env vote
     |> ``should be`` None
*)

[<Test>]
let ``Payout votes are correctly tallied`` () =
     
     // same Amount should yield None payout
     let vote = (someone , [{asset=Asset.Zen; amount=1UL}])
     let mapPayoutAmount = Map.add vote 100UL Map.empty
     let vote = (someoneElse , [{asset=Asset.Zen; amount=1UL}])
     let finalMapPayoutAmount = Map.add vote 100UL mapPayoutAmount
     let tally : T = {
        coinbaseRatio = Map.empty
        payout        = finalMapPayoutAmount
     }
     
     let winner = getWinner tally
     
     match winner with
     | Some winner ->
          winner.payout
          |> ``should be`` None
     | None -> failwith "Error in the test"
     
     // different Amount should yield Some payout
     let vote = (someone , [{asset=Asset.Zen; amount=1UL}])
     let mapPayoutAmount = Map.add vote 100UL Map.empty
     let vote = (someoneElse , [{asset=Asset.Zen; amount=1UL}])
     let finalMapPayoutAmount = Map.add vote 101UL mapPayoutAmount
     let tally : T = {
        coinbaseRatio = Map.empty
        payout        = finalMapPayoutAmount
     }
     
     let winner = getWinner tally
     
     match winner with
     | Some winner ->
          winner.payout
          |> ``should be`` (Some vote)
     | None -> failwith "Error in the test"
     
     // No amount should yield None
     let voteData = 99uy
     let vote = CoinbaseRatio voteData
     
     let tally : T = {
        coinbaseRatio = Map.add vote 100UL Map.empty
        payout        = Map.empty
     }
     
     let winner = getWinner tally
     
     match winner with
     | Some winner ->
          winner.payout
          |> ``should be`` None
     | None -> failwith "Error in the test"

[<Test>]
let ``Payout Winner only when unique `` () =
     
     let seq = [(1,1UL);(2,2UL);(3,3UL) ] |> List.toSeq
     seqUniqueMaxBy snd seq
     |> ``should be`` (Some (3,3UL))
     
     let seq = [(1,1UL);(3,3UL);(3,3UL) ] |> List.toSeq
     seqUniqueMaxBy snd seq
     |> ``should be`` None
(*
[<Test>]
let ``validate Tally`` () =
     let genPk = PublicKey.fromString "02bc15ee2d0073ec3f9d0f9a61f63027e9d5b6202faed8792836c42cbdb3cd4423" |> Option.get
     let genPkHash = genPk |> PublicKey.hash
     let genAsset (n : uint32) = Asset.defaultOf (ContractId (1u, n |> Infrastructure.BigEndianBitConverter.uint32ToBytes |> Hash.compute))
     let assets = genAsset 100ul
     
     let env = {
          cgpContractId         = BlockCGPTests.cgpContractId
          nominationThreshold   = 0UL
          coinbaseCorrectionCap = CoinbaseRatio 90uy
          lowerCoinbaseBound    = CoinbaseRatio 10uy
          lastCoinbaseRatio     = CoinbaseRatio 100uy
          lastFund              =
               List.fold (fun map n -> Map.add assets 500UL map) Map.empty [1..100]
          nomineesBallots       =
               Map.add (genPk) (PKRecipient (genPk |> PublicKey.hash), [1] |> List.map (fun n -> {asset=assets; amount=1UL *(uint64 n)})) Map.empty
          balances              = Map.add (genPkHash) 50UL Map.empty
          allocationBallots     = Map.add (genPk ) 1uy Map.empty
          payoutBallots         =
               Map.add (genPk) (PKRecipient (genPk |> PublicKey.hash), [1] |> List.map (fun n -> {asset=assets; amount=1UL *(uint64 n)})) Map.empty
     }
     
     let tally = createTally env
     
     let winner:Option<Winner> = Some {payout = Some (PKRecipient (genPk |> PublicKey.hash),[{asset=assets; amount=1UL}]); allocation = Some 1uy}
     
     getWinner tally
     |> ``should be`` winner
*)

let private testWeightedMedian (votes : seq<byte * uint64>) (wm : byte) : bool =
    let sortedVotes = Seq.sortBy fst votes
    let lower       = Seq.filter (fun (x,_) -> x < wm) sortedVotes
    let higher      = Seq.filter (fun (x,_) -> x > wm) sortedVotes
    let sumLower    = Seq.sumBy snd lower
    let sumHigher   = Seq.sumBy snd higher
    let sumTotal    = Seq.sumBy snd sortedVotes
    2UL * sumLower <= sumTotal && 2UL * sumHigher <= sumTotal

let private weightedMediansLowHighTest (votes : seq<byte * uint64>) : bool =
    let sortedVotes     = Seq.sortBy fst votes
    let weights         = Seq.map snd sortedVotes
    let totalWeight     = weights |> Seq.sum
    let weightsBefore   = Seq.scan (+) 0UL         weights
    let weightsAfter    = Seq.scan (-) totalWeight weights
    let coupledVotes    = Seq.zip3 weightsBefore sortedVotes (Seq.tail weightsAfter)
    let cond (b,_,a)    = 2UL * b <= totalWeight  &&  2UL * a <= totalWeight
    let bl, (l, _), al  = Seq.find     cond coupledVotes
    let bh, (h, _), ah  = Seq.findBack cond coupledVotes
    l = h || l < h 
        && 2UL * bl < totalWeight && 2UL * al = totalWeight
        && 2UL * bh = totalWeight && 2UL * ah < totalWeight

#if DEBUG
type VoteGenerators =
    static member ValidVote() =
         Arb.fromGen (
            gen {
            let! b = Arb.generate<byte>
            let! u = Arb.generate<uint64>
            if b > 0uy && b < 90uy && u > 0UL then
                 return! Gen.nonEmptyListOf (gen{ return b,u})
            else
                 return [(1uy,System.UInt64.MaxValue)]
        })

[<OneTimeSetUp>]
let setup = fun () ->
    Arb.register<VoteGenerators>() |> ignore


[<Property(MaxTest=100)>]
let ``The weighted median is a weighted median`` (votes : List<byte * uint64>) =
    testWeightedMedian votes (weightedMedianTest votes)


[<Property>]
let ``Higher and lower weighted medians`` (votes : List<byte * uint64>) =
     weightedMediansLowHighTest votes
    

[<Property>]
let ``weighted median never fails`` (votes : List<byte * uint64>) =
     weightedMedianTest votes |> ignore

[<Property>]
let ``isSortedBy validates sorted lists`` (xs : int list) =
     List.Sorted.isSortedBy id (List.sort xs)
     |> Option.toBool

[<Property>]
let ``isSortedBy invalidates unsorted lists`` (xs : int list) =
     xs = List.sort xs
     || not (List.Sorted.isSortedBy id xs |> Option.toBool)

[<Property>]
let ``isUnique validates unique lists`` (xs : int list) =
     List.Sorted.Distinct.isDistinct (xs |> List.Sorted.sort |> List.Sorted.Distinct.distinct).sorted
     |> Option.toBool

[<Property>]
let ``isUnique invalidates non-unique lists`` (x : int) (xs : int list) (ys : int list) (zs : int list) =
     not ((xs @ [x] @ ys @ [x] @ zs) |> List.Sorted.sort |> List.Sorted.Distinct.isDistinct |> Option.toBool)

#endif
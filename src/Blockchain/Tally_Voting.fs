module Blockchain.Tally.Voting

open Blockchain.Tally
open Consensus
open Types
open Checked
open Infrastructure.Functional
open Blockchain.Tally.Tally

let private (>>=) = FSharpx.Option.(>>=)

let private (|@>) x f = Option.map f x

let validateCoinbaseRatio (env : Env) (CoinbaseRatio coinbaseRatio : CoinbaseRatio) : Option<CoinbaseRatio> =
    
    let lastCoinbaseRatio = env.lastCoinbaseRatio     |> getRatio |> uint16
    let correctionCap     = env.coinbaseCorrectionCap |> getRatio |> uint16
    let globalRatioMin    = env.lowerCoinbaseBound    |> getRatio
    
    let localRatioMin =
        byte <| (lastCoinbaseRatio * correctionCap) / 100us
    
    let localRatioMax =
        byte <| (lastCoinbaseRatio * 100us) / correctionCap
    
    let ratioMin =
        max globalRatioMin localRatioMin
    
    let ratioMax =
        min 100uy localRatioMax
        
    check (ratioMin <= coinbaseRatio && coinbaseRatio <= ratioMax)
    |<- CoinbaseRatio coinbaseRatio

let validatePercentage (allocation : Blockchain.Tally.Types.Allocation) : Option<Blockchain.Tally.Types.Allocation> =
    check (allocation <= 100uy)
    |<- allocation

let validateAllocation env allocation =
    Some allocation
    >>= validatePercentage
    |@> allocationToCoinbaseRatio
    >>= validateCoinbaseRatio env
    |@> coinbaseRatioToAllocation

let validatePayout (env : Env) (vote : payout) : Option<payout> =
    
    let nominees = Nomination.computeNominees env
    
    if nominees |> List.contains vote then
        Some vote
    else
        None
    

let validateVote (env : Env) (vote : Ballot) : Option<Ballot> =
    match vote with
    | Allocation allocation ->
        allocation
        |> validateAllocation env
        |@> Allocation
    | Payout (pay,out) ->
        (pay,out)
        |> validatePayout env
        |@> Payout

let addVote (env : Env) (tally:T) (vote:Ballot) amount : T =
    validateVote env vote |@> function
    | Allocation allocation ->
        let voteCoinbaseRatio = allocationToCoinbaseRatio allocation
        { tally with coinbaseRatio = accumulate voteCoinbaseRatio amount tally.coinbaseRatio }
    | Payout (pay,out) ->
        let votePayout = (pay,out)
        { tally with payout = accumulate votePayout amount tally.payout}
    |> Option.defaultValue tally

let mergeBallots (env : Env) (allocationVotes : Map<allocation, uint64>) (payoutVotes : Map<payout, uint64>) : T =
    let allocationBallots     = mapMapKeys Allocation allocationVotes 
    let payoutBallots         = mapMapKeys Payout     payoutVotes
    let collect ballots tally = Map.fold (addVote env) tally ballots
    
    empty
    |> collect allocationBallots
    |> collect payoutBallots

let createTally (env : Env) : T =
    
    let allocationVotes = integrateBallots env.balances env.allocationBallots
    let payoutVotes     = integrateBallots env.balances env.payoutBallots
    
    mergeBallots env allocationVotes payoutVotes

// Naive weighted median by sorting and searching (assumes the list of votes is nonempty) 
let private weightedMedian (votes : seq<byte * uint64>) : byte =
    //check for arithmetic overflow
    try
        // Where do we check for uint64 different then 0?
        let sortedVotes   = Seq.sortBy fst votes
        let weights       = Seq.map snd sortedVotes
        let totalWeight   = weights |> Seq.sum
        let weightsBefore = Seq.scan (+) 0UL         weights
        let weightsAfter  = Seq.scan (-) totalWeight weights
        let coupledVotes  = Seq.zip3 weightsBefore sortedVotes (Seq.tail weightsAfter)
        let cond (b,_,a)  = 2UL * b <= totalWeight  &&  2UL * a <= totalWeight
        let l, wl         = Seq.find     cond coupledVotes |> fun (_,x,_) -> x
        let h, wh         = Seq.findBack cond coupledVotes |> fun (_,x,_) -> x
        if l = h then
            l
        else
            byte ((uint64 l * wl + uint64 h * wh) / (wl + wh))
    with | :? System.OverflowException ->
        let votes         = votes |> Seq.map (fun (x , w) -> (x , bigint w))
        let sortedVotes   = Seq.sortBy fst votes
        let weights       = Seq.map snd sortedVotes
        let totalWeight   = weights |> Seq.sum
        let weightsBefore = Seq.scan (+) (bigint 0UL) weights
        let weightsAfter  = Seq.scan (-) totalWeight  weights
        let coupledVotes  = Seq.zip3 weightsBefore sortedVotes (Seq.tail weightsAfter)
        let cond (b,_,a)  = bigint 2UL * b <= totalWeight  &&  bigint 2UL * a <= totalWeight
        let l, wl         = Seq.find     cond coupledVotes |> fun (_,x,_) -> x
        let h, wh         = Seq.findBack cond coupledVotes |> fun (_,x,_) -> x
        if l = h then
            l
        else
            byte ((bigint (uint64 l) * wl + bigint (uint64 h) * wh) / (wl + wh))

#if DEBUG
let weightedMedianTest = weightedMedian
#endif

let private getResultWith aggregator map =
    if Map.isEmpty map then
        None
    else
        map
        |> Map.toSeq
        |> aggregator
        |> Some

let uniqueMaxBy (weight : 'a -> uint64) ((_, y) as last : bool * 'a) (x : 'a) : bool * 'a =
    if weight x > weight y then
        (false, x)
    elif weight x < weight y then
        last
    else    // (weight x = weight y)
        (true, x)
 
let seqUniqueMaxBy (weight : 'a -> uint64) (source : seq<'a>) : Option<'a> =
    
    let initState = (false, Seq.head source)
    
    Seq.fold (uniqueMaxBy weight) initState (Seq.tail source)
    |> fun (isDup, x) -> if isDup then None else Some x 

let getAllocationResult =
    getResultWith weightedMedian

let getPayoutResult =
    getResultWith (seqUniqueMaxBy snd >> Option.map fst)
    >> Option.bind id

let getWinner tally =
    if isEmpty tally then
        None
    else 
        let allocation =
            tally.coinbaseRatio
            |> mapMapKeys coinbaseRatioToAllocation
            |> getAllocationResult
    
        let payout =
            tally.payout
            |> getPayoutResult
        Some {allocation=allocation; payout=payout}
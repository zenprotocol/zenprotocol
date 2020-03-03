module Blockchain.Tally.Nomination

open System
open Consensus
open Types
open Checked
open Consensus.Crypto
open Consensus.Serialization.Serialization
open Infrastructure.Functional
open Blockchain.Tally.Tally

/// Nomination Environment
type Env =
    {
        cgpContractId   : ContractId
        threshold       : uint64
        lastFund        : Fund.T
        nomineesBallots : Map<PK, payout>
        balances        : Map<PKHash, uint64>
    }


let private (>>=) = FSharpx.Option.(>>=)

let private (|@>) x f = Option.map f x

let isUnique (ls : List<'a>) : bool =
    let rec onSorted : List<'a> -> bool =
        function
        | [] ->
            true
        | (_ :: []) ->
            true
        | (x :: y :: xs) ->
            x <> y && onSorted (y :: xs)
    
    ls
    |> List.sort
    |> onSorted

let validatePayoutNominee (env : Env) ((_, spends) as vote : payout) : Option<payout> =
    
    let subtractSpend (fund : Fund.T) (spend : Spend) : Option<Fund.T> =
        option {
            let! fundAmount =
                Map.tryFind spend.asset fund
            
            let! updatedAmount =
                check (fundAmount >= spend.amount)
                |<-- lazy (fundAmount - spend.amount)
            
            return Map.add spend.asset updatedAmount fund
        }
    
    let checkNonZero (spend : Spend) : Check = 
        check (spend.amount > 0UL)
    
    let checkSpendableFunds : Check =
        spends
        |> FSharpx.Option.foldM subtractSpend env.lastFund 
        |> ignoreResult
    
    let checkNonZeros : Check =
        spends
        |> FSharpx.Option.mapM checkNonZero
        |> ignoreResult
    
    let checkSize : Check =
        let len = List.length spends
        check (1 <= len && len <= 100)
    
    let checkUniqueness : Check =
        check (isUnique spends)
    
    Some ()
    *> checkNonZeros
    *> checkSpendableFunds
    *> checkSize
    *> checkUniqueness
    |<- vote

let addVote (env : Env) (tally:Map<payout, uint64>) (vote:payout) amount : Map<payout, uint64> =
    validatePayoutNominee env vote
    |@> fun votePayout -> accumulate votePayout amount tally
    |> Option.defaultValue tally

let computeCandidates (env : Env) : Candidates =
    
    let cgpNominee =
        if Map.isEmpty env.lastFund then
            None
        else
            (ContractRecipient env.cgpContractId
            , [{ asset = Asset.Zen ; amount = 1UL }]
            )
            |> Some
    
    let nomineesVotes = integrateBallots env.balances env.nomineesBallots
    
    let nomineesTally =
        Map.fold (addVote env) Map.empty nomineesVotes
    
    cgpNominee
    |> Option.map (fun nom -> Map.add nom env.threshold nomineesTally)
    |> Option.defaultValue nomineesTally
    |> Map.toList
    |> List.filter (fun (_ , weight) -> weight >= env.threshold)
    |> List.map fst

module Blockchain.Tally.Nomination

open Consensus
open Types
open Checked
open Consensus.Crypto
open Consensus.Serialization.Serialization
open Infrastructure.Functional
open Blockchain.Tally.Tally

let private (>>=) = FSharpx.Option.(>>=)

let private (|@>) x f = Option.map f x

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
    
    let checkNonZero (spend : Spend) : Option<unit> = 
        check (spend.amount > 0UL)
    
    let checkSpendableFunds : Option<unit> =
        spends
        |> FSharpx.Option.foldM subtractSpend env.lastFund 
        |> ignoreResult
    
    let checkNonZeros : Option<unit> =
        spends
        |> FSharpx.Option.mapM checkNonZero
        |> ignoreResult
    
    let checkSize : Option<unit> =
        let len = List.length spends
        check (1 <= len && len <= 100)
    
    Some ()
    *> checkNonZeros
    *> checkSpendableFunds
    *> checkSize
    |<- vote

let aggregateAssets spends =
    Fund.accumulateSpends Some spends
    |> Map.toList
    |> List.map (fun (asset, amount) -> { asset = asset ; amount = amount })

let addVote (env : Env) (tally:Map<payout, uint64>) (vote:payout) amount : Map<payout, uint64> =
    validatePayoutNominee env vote
    |@> fun votePayout -> accumulate votePayout amount tally
    |> Option.defaultValue tally

let computeNominees (env : Env) : Nominees =
    
    let cgpNominee =
        if Map.isEmpty env.lastFund then
            None
        else
            (ContractRecipient env.cgpContractId
            , [{ asset = Asset.Zen ; amount = 1UL }]
            )
            |> Some
    
    // TODO: this is a validation thing, move it to validation
    // Ensures that no nominees contains multiple spends of the same asset
    let uniqueNomineesBallots =
        Map.map (konst <| secondMap aggregateAssets) env.nomineesBallots
    
    let nomineesVotes = integrateBallots env.balances uniqueNomineesBallots
    
    let nomineesTally =
        Map.fold (addVote env) Map.empty nomineesVotes
    
    cgpNominee
    |> Option.map (fun nom -> Map.add nom env.nominationThreshold nomineesTally)
    |> Option.defaultValue nomineesTally
    |> Map.toList
    |> List.filter (fun (_ , weight) -> weight >= env.nominationThreshold)
    |> List.map fst

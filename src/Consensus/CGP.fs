module Consensus.CGP

open Chain
open Consensus
open Consensus.Types
open Logary.Message
open Infrastructure

type T = {
    allocation: byte
    amount: uint64
    tallies: Map<uint32, Tally.T>
    payout: Option<Types.Recipient * uint64>
}

let empty = {
    allocation = 0uy
    amount = 0UL
    tallies = Map.empty
    payout = None
}

let isEmpty = (=) empty

let pruneTallies interval cgp =
    { cgp with tallies = Map.filter (fun interval' _ -> interval' >= interval) cgp.tallies }

let getInterval chainParams blockNumber =
    (blockNumber - 1ul) / chainParams.intervalLength

let private isIntervalBlock chainParams blockNumber =
    (blockNumber - 1ul) % chainParams.intervalLength = 0ul

let isPayoutBlock chainParams blockNumber =
    blockNumber % chainParams.intervalLength = chainParams.coinbaseMaturity

let handleTransaction getUTXO (getTx:Hash.Hash -> Option<TransactionExtended>) (tx:Transaction) interval =
    let chooseVotes =
        List.choose (function
            | { spend = spend; lock = Vote (vote, interval', _) } -> 
                Some (vote, interval', spend.amount)
            | _ -> None)

    let handleVote adding cgp (vote:VoteData, interval, amount) =
        if adding then
            Map.tryFind interval cgp.tallies
            |> Option.defaultValue Tally.empty
            |> Tally.handleVote adding vote amount
            |> fun tally -> { cgp with tallies = Map.add interval tally cgp.tallies }
        else 
            Map.tryFind interval cgp.tallies
            |> Option.map (Tally.handleVote adding vote amount)
            |> Option.map (fun tally -> { cgp with tallies = Map.add interval tally cgp.tallies })
            |> Option.defaultValue cgp
        
    let handleTxInputs cgp =
        tx.inputs
        |> List.choose (function
            | Outpoint outpoint -> 
                match getTx outpoint.txHash with 
                | Some ex when ex.tx.version = Version1 -> Some outpoint
                | _ -> None         
            | _ -> None)
        |> List.map getUTXO
        |> chooseVotes
        |> List.fold (handleVote false) cgp

    let handleTxOutputs cgp =
        if tx.version = Version1 then
            tx.outputs
            |> chooseVotes
            |> List.fold (handleVote true) cgp
        else
            cgp

    handleTxInputs
    >> handleTxOutputs
    >> pruneTallies interval

let update chainParams blockNumber = 
    let setAllocation chainParams interval cgp =
        Map.tryFind interval cgp.tallies
        |> Option.map (fun tally -> tally.allocation)
        |> Option.bind Tally.getResult
        |> Option.map (fun allocation ->
            { cgp with
                allocation =
                allocation |>
                if cgp.allocation > allocation
                    && cgp.allocation - chainParams.allocationCorrectionCap < cgp.allocation // Overflow check
                    then max (cgp.allocation - chainParams.allocationCorrectionCap)
                elif cgp.allocation < allocation
                    && cgp.allocation + chainParams.allocationCorrectionCap > cgp.allocation // Overflow check
                    then min (cgp.allocation + chainParams.allocationCorrectionCap)
                else id
            })
        |> Option.defaultValue cgp
    
    let setPayout interval cgp =
        Map.tryFind interval cgp.tallies
        |> Option.map (fun tally -> tally.payout)
        |> Option.bind Tally.getResult
        |> Option.bind (fun (recipient, payoutAmount) -> 
            if payoutAmount > cgp.amount then
                eventX "Not enough tokens in CGP ({cgpAmount}) for payout ({payoutAmount})"
                >> setField "cgpAmount" cgp.amount
                >> setField "payoutAmount" payoutAmount
                |> Log.warning
                None
            else
                { cgp with payout = Some (recipient, payoutAmount); amount = cgp.amount - payoutAmount }
                |> Some)
        |> Option.defaultValue { cgp with payout = None }

    let accumulateAllocation blockNumber (cgp:T) =
        { cgp with amount = cgp.amount + blockAllocation blockNumber cgp.allocation }
        
    let log cgp =
        eventX "CGP interval update. Amount: {amount}; Allocation: {allocation}; Payout: {payout}"
        >> setField "amount" cgp.amount
        >> setField "allocation" (int cgp.allocation)
        >> setField "payout" (match cgp.payout with Some (_, amount) -> amount | _ -> 0UL)
        |> Log.warning
        cgp
        
    if isIntervalBlock chainParams blockNumber then 
        let interval = getInterval chainParams blockNumber
        setAllocation chainParams (interval - 1ul)
        >> setPayout (interval - 1ul)
        >> log
        >> pruneTallies interval
        >> accumulateAllocation blockNumber
    else
        accumulateAllocation blockNumber

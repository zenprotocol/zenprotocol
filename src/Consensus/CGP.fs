module Consensus.CGP

open Chain
open Consensus
open Consensus.Types
open Logary.Message
open Infrastructure


type T = {
    allocation: byte
    payout: Option<Types.Recipient * Spend list>
}

let empty = {
    allocation = 0uy
    payout = None
}

let isEmpty = (=) empty


let getInterval chainParams blockNumber =
    if blockNumber <> 0ul then (blockNumber - 1ul) / chainParams.intervalLength
    else blockNumber
    |> (+) 1u

let getSnapshotBlock chainParams interval =
    interval * (chainParams.snapshot)

let private isIntervalBlock chainParams blockNumber =
    (blockNumber - 1ul) % chainParams.intervalLength = 0ul

let isPayoutBlock chainParams blockNumber =
    blockNumber % chainParams.intervalLength = chainParams.coinbaseMaturity - 1ul

let isTallyBlock chainParams blockNumber =
    blockNumber % chainParams.intervalLength = chainParams.coinbaseMaturity / 2ul


let update (winner: Option<Winner>) cgp =

    let cgp = 
        match winner with
        | Some {allocation=Some allocation; payout=payout} ->
            {cgp with allocation=allocation; payout=payout}:T
        | _ -> cgp 
      
    let log cgp =
        eventX "CGP interval update. Allocation: {allocation}; Payout: {payout}"
        >> setField "allocation" (int cgp.allocation)
        >> setField "payout" (match cgp.payout with Some (_, spend) -> spend.ToString() | None -> "None")
        |> Log.warning
        cgp
        
        
    log cgp 
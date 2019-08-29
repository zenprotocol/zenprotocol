module Consensus.CGP

open System
open Chain
open Consensus
open Consensus.Types
open Logary.Message
open Infrastructure

module ZData = Zen.Types.Data

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
    blockNumber % chainParams.intervalLength = chainParams.coinbaseMaturity

let isTallyBlock chainParams blockNumber =
    blockNumber % chainParams.intervalLength = chainParams.coinbaseMaturity / 2ul


let update (winner: Option<Winner>) cgp =

    let cgp = 
        match winner with
        | Some {allocation=Some allocation; payout=payout} ->
            {cgp with allocation=allocation; payout=payout}:T
        | _ -> cgp

    let printPayout recipient (spend:Spend list) =
        let recipientString = 
            match recipient with
            | PKRecipient pk -> Hash.toString pk
            | ContractRecipient cId -> ContractId.toString cId
        let spendString =
            spend
            |> List.map (fun spend ->
                sprintf "Asset: %s, Amount: %s" (Asset.toString spend.asset) (string spend.amount))
            |> List.toSeq
        sprintf "Recipient: %s , Spend: %A" recipientString spendString
          
    let log cgp =
        eventX "CGP interval update. Allocation: {allocation}; Payout: {payout}"
        >> setField "allocation" (int cgp.allocation)
        >> setField "payout" (match cgp.payout with Some (recipient, spend) -> printPayout recipient spend  | None -> "None")
        |> Log.warning
        cgp
        
        
    log cgp

let recipientToLock : Recipient -> Lock =
    function
    | PKRecipient pk        -> PK pk
    | ContractRecipient cid -> Contract cid

let internalizeRecipient ((recip, spends) : Recipient * List<Spend>) : List<Output> =
        spends
        |> List.map ( fun spend -> { lock = recipientToLock recip; spend = spend } )

module Contract =

    let extractSpend (spend : Prims.list<ZData.data>) : Option<Spend> =
            match ZFStar.fstToFsList spend with
            | [ ZData.String asset ; ZData.U64 amount ] ->
                asset
                |> ZFStar.fstToFsString
                |> Asset.fromString
                |> Option.map (fun asset -> { asset = asset; amount = amount })
            | _ ->
                None
        
    let extractOutput (output : ZData.data) : Option<Output> =
         match output with
         | ZData.Collection (ZData.List xs) ->
            match ZFStar.fstToFsList xs with
            | [ ZData.Lock lock ; ZData.Collection (ZData.List spend) ] ->
                spend
                |> extractSpend
                |> Option.map (fun spend -> { lock = ZFStar.fstToFsLock lock; spend=spend })
            | _ ->
                None
         | _ ->
             None
    
    let extractPayoutOutputs (msgBody : Option<ZData.data>) : Result<List<Output>,unit> =
        match msgBody with
        |  Some (ZData.Collection (ZData.Dict (dict,_))) ->
            dict
            |> Map.tryFind ("Outputs" |> ZFStar.fsToFstString)
            |> function
               | Some (ZData.Collection (ZData.List outputs)) ->
                   outputs
                   |> ZFStar.fstToFsList
                   |> Result.traverseResultM (extractOutput >> Result.ofOption ())
               | _ ->
                   Error ()
        | _ ->
            Error ()
    
    let createSpend ({asset=asset; amount=amount} : Spend) : ZData.data =
        let asset = asset |> Asset.toString |> ZFStar.fsToFstString
        [ ZData.String asset ; ZData.U64 amount ]
        |> ZFStar.fsToFstList
        |> ZData.List
        |> ZData.Collection
        
    let createOutput ({lock=lock; spend=spend} : Output) : ZData.data =
        let lock  = lock  |> ZFStar.fsToFstLock |> ZData.Lock
        let spend = spend |> createSpend
        [ lock ; spend ]
        |> ZFStar.fsToFstList
        |> ZData.List
        |> ZData.Collection
    
    let createPayout (outputs : List<Output>) : ZData.data =
        outputs
        |> List.map createOutput
        |> ZFStar.fsToFstList
        |> ZData.List
        |> ZData.Collection
    
    let createPayoutMsgBody (outputs : List<Output>) : Option<ZData.data> =
        Map.empty
        |> Map.add "Outputs"B (createPayout outputs)
        |> fun m -> (m, 1u)
        |> ZData.Dict
        |> ZData.Collection
        |> Some
        
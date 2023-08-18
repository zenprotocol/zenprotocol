module Consensus.CGP

open System
open Chain
open Consensus
open Consensus.Types
open Logary.Message
open Infrastructure

module ZData = Zen.Types.Data

type T = {
    allocation : byte
    payout : Option<Types.Recipient * Spend list>
 }

let empty = {
    allocation = 0uy
    payout = None
 }

let isEmpty = (=) empty


let getInterval chainParams blockNumber =
    if blockNumber > 0ul then
        (blockNumber - 1ul) / chainParams.intervalLength + 1ul
    else
        1ul

let getSnapshotBlock chainParams interval =
    (interval - 1u) * chainParams.intervalLength + chainParams.snapshot

let isPayoutBlock chainParams blockNumber =
    blockNumber % chainParams.intervalLength = chainParams.coinbaseMaturity

let endOfNominationBlock chainParams interval =
    getSnapshotBlock chainParams interval + chainParams.nomination

let isNomineePhase chainParams blockNumber =
    let interval = getInterval chainParams blockNumber
    getSnapshotBlock chainParams interval < blockNumber && blockNumber <= endOfNominationBlock chainParams interval

let getLastIntervalBlock (chainParams : ChainParameters) (interval : uint32) : uint32 =
    chainParams.intervalLength * interval

let private printPayout recipient (spend : Spend list) =
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
    >> setField "payout" (match cgp.payout with | Some(recipient, spend) -> printPayout recipient spend | None -> "None")
    |> Log.warning

let update (winner : Option<Winner>) cgp =
    match winner with
    | Some { allocation = None; payout = payout } ->
        { cgp with payout = payout } : T
    | Some { allocation = Some allocation; payout = payout } ->
        {allocation = allocation; payout = payout } : T
    | _ -> { cgp with payout = None }

let recipientToLock : Recipient -> Lock =
    function
    | PKRecipient pk -> PK pk
    | ContractRecipient cid -> Contract cid

let internalizeRecipient ((recip, spends) : Recipient * List<Spend>) : List<Output> =
        spends
        |> List.map (fun spend -> { lock = recipientToLock recip; spend = spend })

module Contract =

    let extractSpend (spend : Prims.list<ZData.data>) : Option<Spend> =
            match ZFStar.fstToFsList spend with
            | [ ZData.String asset; ZData.U64 amount ] ->
                asset
                |> ZFStar.fstToFsString
                |> Asset.fromString
                |> Option.map (fun asset -> { asset = asset; amount = amount })
            | _ ->
                None

    let extractOutput (output : ZData.data) : Option<Output> =
         match output with
         | ZData.Collection(ZData.List xs) ->
            match ZFStar.fstToFsList xs with
            | [ ZData.Lock lock; ZData.Collection(ZData.List spend) ] ->
                spend
                |> extractSpend
                |> Option.map (fun spend -> { lock = ZFStar.fstToFsLock lock; spend = spend })
            | _ ->
                None
         | _ ->
             None

    let extractPayoutOutputs (msgBody : Option<ZData.data>) : Option<List<Output>> =
        match msgBody with
        | Some(ZData.Collection(ZData.Dict(dict, _))) ->
            dict
            |> Map.tryFind ("Outputs" |> ZFStar.fsToFstString)
            |> function
               | Some(ZData.Collection(ZData.List outputs)) ->
                   outputs
                   |> ZFStar.fstToFsList
                   |> Option.traverseM extractOutput
               | _ ->
                   None
        | _ ->
            None

    let createSpend ({ asset = asset; amount = amount } : Spend) : ZData.data =
        let asset =
            //TODO unnecessary for new zulib
            if asset = Asset.Zen then "000000000000000000000000000000000000000000000000000000000000000000000000" else asset |> Asset.toString
            |> ZFStar.fsToFstString
        [ ZData.String asset; ZData.U64 amount ]
        |> ZFStar.fsToFstList
        |> ZData.List
        |> ZData.Collection

    let createOutput ({ lock = lock; spend = spend } : Output) : ZData.data =
        let lock = lock |> ZFStar.fsToFstLock |> ZData.Lock
        let spend = spend |> createSpend
        [ lock; spend ]
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

module Connection =
    let private result = Result.ResultBuilder<string>()

    let internalizeRecipient
        ( (recip, spends) : Recipient * List<Spend> )
        : List<Output> =

        spends
        |> List.map ( fun spend -> { lock = recipientToLock recip; spend = spend } )

    let checkMessageBody
        ( payout  : Recipient * List<Spend>     )
        ( msgBody : Option<Zen.Types.Data.data> )
        : Result<unit, string> = result {

        let! outputs =
            msgBody
            |> Contract.extractPayoutOutputs
            |> Result.ofOption "Couldn't parse message body"

        let winner =
            internalizeRecipient payout

        if List.sort outputs = List.sort winner
            then return ()
            else return! Error "Contract outputs are not the same as the payout winner"
        }

    let extractPayoutWitnesses
        ( chainParams : ChainParameters     )
        ( ex          : TransactionExtended )
        : List<ContractWitness> =

        ex.tx.witnesses
        |> List.choose
               begin function
               | ContractWitness cw when cw.contractId = chainParams.cgpContractId ->
                   Some cw
               | _ ->
                   None
               end
    let checkPayoutWitness chainParams transactions cgp =
        let payoutWitnesses : List<ContractWitness> =
                transactions
                |> List.concatMap (extractPayoutWitnesses chainParams)

        match payoutWitnesses with
        | _ :: _ :: _ ->
            Error "Multiple payout Txs"
        | [] ->
            match cgp.payout with
            | None ->
                Ok ()
            | Some _ ->
                Error "No payout Tx"
        | [ cgpWitness ] ->
            match cgp.payout with
            | None ->
                Error "There shouldn't be a payout - there is no payout winner"
            | Some payout ->
                cgpWitness.messageBody
                |> checkMessageBody payout

    let isPayoutTransaction
        ( chainParams: ChainParameters     )
        ( tx         : TransactionExtended )
        : bool =
            not <| List.isEmpty (extractPayoutWitnesses chainParams tx)
    
    let isPayoutTransactionInBlock
        ( chainParams: ChainParameters )
        ( block         : Block        )
        : bool =
            block.transactions
            |> List.filter (isPayoutTransaction chainParams)
            |> List.isSingleton
    
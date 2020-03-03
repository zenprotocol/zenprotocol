module Blockchain.Tally.Handler
open Blockchain
open Blockchain.Tally.Repository
open Consensus
open Chain
open Infrastructure
open Logary.Message
open Types
open UtxoSet
open Functional
open Tally.Repository
open Environment

module ExtHeader = ExtendedBlockHeader

type ExtHeader = ExtHeader.T


module DA =

    module Fund =

        let get
            ( dataAccess: DatabaseContext.Session )
            ( interval  : Interval )
            : Fund.T =
            Fund.tryGet dataAccess dataAccess.session interval
            |> Option.defaultValue Map.empty

    module PKBalance =

        let get
            ( dataAccess: DatabaseContext.Session )
            ( interval  : Interval )
            : PKBalance =
            PKBalance.tryGet dataAccess dataAccess.session interval
            |> Option.defaultValue Map.empty

    module PKNominee =

        let get
            ( dataAccess: DatabaseContext.Session )
            ( interval  : Interval )
            : PKPayout =
            PKNominee.tryGet dataAccess dataAccess.session interval
            |> Option.defaultValue Map.empty

    module PKAllocation =

        let get
            ( dataAccess: DatabaseContext.Session )
            ( interval  : Interval )
            : PKAllocation =
            PKAllocation.tryGet dataAccess dataAccess.session interval
            |> Option.defaultValue Map.empty

    module PKPayout =

        let get
            ( dataAccess: DatabaseContext.Session )
            ( interval  : Interval )
            : PKPayout =
            PKPayout.tryGet dataAccess dataAccess.session interval
            |> Option.defaultValue Map.empty
    module Winner =
        let rec getFirstAllocation dataAccess interval : byte =
            if interval = 1ul then
                0uy
            else
               match Winner.tryGet dataAccess dataAccess.session interval with
               | Some {allocation = Some al; payout = _ } ->
                   al
               | _ ->
                   getFirstAllocation dataAccess (interval - 1ul)

    module Candidates =

        let get dataAccess (chainParams : ChainParameters) interval =

            let snapshotBlockNumber = CGP.getSnapshotBlock chainParams interval

            let threshold =
                Chain.getCurrentZPIssuance chainParams snapshotBlockNumber
                |> (*) (fst chainParams.thresholdFactor)
                |> (/) (snd chainParams.thresholdFactor)

            let env : Tally.Nomination.Env =
                {
                    cgpContractId       = chainParams.cgpContractId
                    threshold           = threshold
                    lastFund            = Fund.get dataAccess interval
                    nomineesBallots     = PKNominee.get dataAccess interval
                    balances            = PKBalance.get dataAccess interval
                }

            Nomination.computeCandidates env

    module Tally =

        let get dataAccess (chainParams : ChainParameters) interval lastAllocation =

            let env : Tally.Voting.Env =
                {
                    coinbaseCorrectionCap = Tally.allocationToCoinbaseRatio chainParams.allocationCorrectionCap
                    lowerCoinbaseBound    = Tally.allocationToCoinbaseRatio chainParams.upperAllocationBound
                    lastCoinbaseRatio     = Tally.allocationToCoinbaseRatio lastAllocation
                    candidates            = Candidates.get dataAccess chainParams interval
                    balances              = PKBalance.get dataAccess interval
                    allocationBallots     = PKAllocation.get dataAccess interval
                    payoutBallots         = PKPayout.get dataAccess interval
                }

            Voting.createTally env

let getWinner
    ( dataAccess: DatabaseContext.Session )
    ( interval  : Interval)
    : Winner option =
    Winner.tryGet dataAccess dataAccess.session interval

module Handler =

    let private updateMap
        ( op     : UpdateOperation   )
        ( key    : 'key              )
        ( amount : uint64            )
        ( map    : Map<'key, uint64> )
        : Map<'key, uint64> =

        let balance =
            map
            |> Map.tryFind key
            |> Option.defaultValue 0UL

        match op with
        | Add ->
            Map.add key (balance + amount)
        | Remove ->
            if balance > amount then
                Map.add key (balance - amount)
            elif balance < amount then
                failwithf "balance for key %A is %A and you tried to remove %A" key balance amount
            else
                Map.remove key
        <| map

    let votingWitnesses chainParams =
        function
        | ContractWitness {contractId=cid; command=cmd; messageBody=msgBody} when cid = chainParams.votingContractId ->
            Some (cmd, msgBody)
        | _ ->
            None

    let private add
        ( map : Map<Crypto.PublicKey,'a> )
        ( pk  : Crypto.PublicKey         )
        ( x   : 'a                       )
        : Map<Crypto.PublicKey,'a> =
        match Map.tryFind pk map with
        | Some _ -> map    // (only the first vote counts)
        | None   -> Map.add pk x map

    let private updatePK
        ( op : UpdateOperation           )
        ( x   : 'a                       )
        ( map : Map<Crypto.PublicKey,'a> )
        ( pk  : Crypto.PublicKey         )
        : Map<Crypto.PublicKey,'a> =
        match op with
        | Add    -> add map pk x
        | Remove -> Map.remove pk map

    let private set
        ( op : UpdateOperation                    )
        ( map  : Map<Crypto.PublicKey,'a>         )
        ( put  : Map<Crypto.PublicKey,'a> -> unit )
        ( x    : 'a                               )
        ( sigs : Crypto.PublicKey list            )
        : unit =
        sigs
        |> List.fold (updatePK op x) map
        |> put

    let getVoteUtxo dataAccess interval : VoteUtxo =
        VoteUtxoSet.tryGet dataAccess dataAccess.session interval
        |> Option.defaultValue Map.empty

    module VoteTip =
        let update op dataAccess block =

            lazy

            match op with
            | Add ->
                Block.hash block.header
            | Remove ->
                block.header.parent
            |> Tally.Repository.VoteTip.put dataAccess dataAccess.session

    module PKBalance =

        let private spendablePk validMaturity =
            function
            | Unspent {lock=PK pkHash; spend={asset=asset;amount=amount}}
                when asset = Asset.Zen ->
                    Some (pkHash,amount)
            | Unspent {lock=Coinbase (blockNumber,pkHash); spend={asset=asset;amount=amount}}
                when asset = Asset.Zen && validMaturity >= blockNumber ->
                    Some (pkHash,amount)
            | _ ->
                    None

        let private getSpendablePks validMaturity (utxos: Map<Outpoint,OutputStatus>) : (Hash.Hash * uint64) list =
            utxos
            |> Map.toList
            |> List.map snd
            |> List.choose (spendablePk validMaturity)

        let update op dataAccess interval chainParams =

            lazy

            let validMaturity = CGP.getSnapshotBlock chainParams interval - chainParams.coinbaseMaturity

            let updateBalance (pkHash : Hash.Hash, amount : uint64) =
                PKBalance.tryGet dataAccess dataAccess.session interval
                |> Option.defaultValue Map.empty
                |> updateMap op pkHash amount
                |> PKBalance.put dataAccess dataAccess.session interval

            getVoteUtxo dataAccess interval
            |> getSpendablePks validMaturity
            |> List.iter updateBalance

    module private PKBallot =

        type Setter =
            UpdateOperation
             -> DatabaseContext.Session
             -> Interval
             -> Ballot
             -> Crypto.PublicKey list
             -> unit

        let update (setter : Setter) op dataAccess interval chainParams block =

            lazy

            let blockNumber = block.header.blockNumber
            block.transactions
            |> List.concatMap (fun ex -> ex.tx.witnesses)
            |> List.choose (votingWitnesses chainParams)
            |> List.choose (uncurry <| VoteParser.parseMessageBody chainParams blockNumber)
            |> List.iter (uncurry <| setter op dataAccess interval)

    module PKVote =

        let private setAllocation op dataAccess interval =
            set op
                (PKAllocation.tryGet dataAccess dataAccess.session interval
                 |> Option.defaultValue Map.empty)
                (PKAllocation.put dataAccess dataAccess.session interval)

        let private setPayout op dataAccess interval =
            set op
                (PKPayout.tryGet dataAccess dataAccess.session interval
                 |> Option.defaultValue Map.empty)
                (PKPayout.put dataAccess dataAccess.session interval)

        let private setVote op dataAccess interval : Ballot -> Crypto.PublicKey list -> unit =
            function
            | Ballot.Allocation allocation ->
                setAllocation op dataAccess interval allocation
            | Ballot.Payout (recipient, spends) ->
                setPayout op dataAccess interval (recipient,spends)

        let update =
            PKBallot.update setVote

    module PKNominee =

        let private setNomine op dataAccess interval =
            set op
                (PKNominee.tryGet dataAccess dataAccess.session interval
                 |> Option.defaultValue Map.empty)
                (PKNominee.put dataAccess dataAccess.session interval)

        let private setNominees op dataAccess interval : Ballot -> Crypto.PublicKey list -> unit =
            function
            | Ballot.Payout (recipient, spends) ->
                setNomine op dataAccess interval (recipient,spends)
            | _ ->
                konst ()

        let update =
            PKBallot.update setNominees

    module Candidates =

        let update op dataAccess interval chainParams =

            lazy

            match op with
            | Add ->
                DA.Candidates.get dataAccess chainParams interval
                |> Candidates.put dataAccess dataAccess.session interval

            | Remove ->
                Candidates.delete dataAccess dataAccess.session interval

    module VoteUtxo =

        let private addBlock
            (getUTXO : Outpoint -> OutputStatus)
            (block : Block)
            (map : Map<Outpoint, OutputStatus>)
            : Map<Outpoint, OutputStatus> =
                block.transactions
                |> List.fold (fun map ex -> handleTransaction getUTXO ex.txHash ex.tx map) map

        let private getUTXO dataAccess interval (outpoint : Outpoint) : OutputStatus =
            getVoteUtxo dataAccess interval
            |> Map.tryFind outpoint
            |> Option.defaultValue NoOutput

        let private updateBlock =
            function
            | Add    -> addBlock
            | Remove -> undoBlock

        let copyToNext op dataAccess interval =

            lazy

            match op with
            | Add ->
                VoteUtxoSet.tryGet dataAccess dataAccess.session interval
                |> Option.defaultValue Map.empty
                |> VoteUtxoSet.put dataAccess dataAccess.session (interval + 1ul)
            | Remove ->
                VoteUtxoSet.delete dataAccess dataAccess.session (interval + 1ul)

        let update op dataAccess interval block =

            lazy

            getVoteUtxo dataAccess interval
            |> updateBlock op (getUTXO dataAccess interval) block
            |> VoteUtxoSet.put dataAccess dataAccess.session interval

    module Fund =

        let spendableCGP chainParams =
            function
            | Unspent {lock=Contract cid; spend={asset=asset;amount=amount}}
                when cid = chainParams.cgpContractId ->
                    Some (asset, amount)
            | _ ->
                    None

        let private getSpendableCGP chainParams (utxos: Map<Outpoint,OutputStatus>) : (Asset * uint64) list =
            utxos
            |> Map.toList
            |> List.map snd
            |> List.choose (spendableCGP chainParams)

        let update op dataAccess interval chainParams =

            lazy

            let updateFund (asset : Asset, amount : uint64) : unit =
                Fund.tryGet dataAccess dataAccess.session interval
                |> Option.defaultValue Map.empty
                |> updateMap op asset amount
                |> Fund.put dataAccess dataAccess.session interval

            getVoteUtxo dataAccess interval
            |> getSpendableCGP chainParams
            |> List.iter updateFund

    module Winner =

        let update op dataAccess interval chainParams =

            lazy

            match op with
            | Add ->
                let lastAllocation = DA.Winner.getFirstAllocation dataAccess interval
                let tally = DA.Tally.get dataAccess chainParams (interval - 1u) lastAllocation
                let winner = Voting.getWinner tally

                let withLastAllocation winner =
                    { winner with allocation = Some <| Option.defaultValue lastAllocation winner.allocation }

                winner
                |> Option.map withLastAllocation
                |> Option.defaultValue ({allocation= Some lastAllocation; payout= None}:Winner)
                |> Winner.put dataAccess dataAccess.session interval

            | Remove ->
                {allocation= None; payout= None}
                |> Winner.put dataAccess dataAccess.session interval


    let log op chainParams block =
        let operation = match op with | Add -> "adding" |Remove -> "removing"
        let interval = CGP.getInterval chainParams block.header.blockNumber
        eventX "Tally {operation} block #{blockNumber} of block {blockHash} for interval #{interval}"
        >> setField "operation" operation
        >> setField "blockNumber" block.header.blockNumber
        >> setField "blockHash" (Hash.toString (Block.hash block.header))
        >> setField "interval" interval
        |> Log.info;

    let private updateBlock op dataAccess chainParams (block : Block) : unit =
        let blockNumber   = block.header.blockNumber
        let interval      = CGP.getInterval          chainParams blockNumber
        let snapshot      = CGP.getSnapshotBlock     chainParams interval
        let endOfInterval = CGP.getLastIntervalBlock chainParams interval
        let beginOfInterval = CGP.getLastIntervalBlock chainParams (interval - 1ul) + 1ul
        let endOfNomination = CGP.endOfNominationBlock chainParams interval

        let tipHash = Tally.Repository.VoteTip.tryGet dataAccess dataAccess.session |> Option.defaultValue Hash.zero

        match op with
        | Add ->
            if tipHash <> block.header.parent then
                let tipHeader = BlockRepository.tryGetHeader dataAccess tipHash |> Option.defaultValue (ExtendedBlockHeader.empty)
                failwithf "trying to add a block to the tip but the tip is in different chain BlockHeader:\n %A\n tipHeader\n%A\n" block.header tipHeader.header
        | Remove ->
            if tipHash <> Block.hash block.header then
                let tipHeader = BlockRepository.tryGetHeader dataAccess tipHash |> Option.defaultValue (ExtendedBlockHeader.empty)
                failwithf "trying to remove a block from the tip but the tip is in different chain BlockHeader:\n %A\n tipHeader\n%A\n" block.header tipHeader.header

        let updates = seq {
            if blockNumber = beginOfInterval
                then yield Winner    .update op dataAccess interval chainParams
            if blockNumber <= snapshot
                then yield VoteUtxo  .update op dataAccess interval block
            if blockNumber = snapshot + 1u
                then yield VoteUtxo  .copyToNext op dataAccess interval
            if blockNumber > snapshot
                then yield VoteUtxo  .update op dataAccess (interval + 1u) block
            if blockNumber = snapshot
                then yield PKBalance .update op dataAccess interval chainParams
                     yield Fund      .update op dataAccess interval chainParams
            if snapshot < blockNumber && blockNumber <= endOfNomination
                then yield PKNominee .update op dataAccess interval chainParams block
            if blockNumber = endOfNomination
                then yield Candidates.update op dataAccess interval chainParams
            if endOfNomination < blockNumber && blockNumber <= endOfInterval
                then yield PKVote    .update op dataAccess interval chainParams block
            if true
                then yield VoteTip   .update op dataAccess block
        }

        match op with
        | Add ->
            updates
        | Remove ->
            // When removing - all the updates have to be done in reverse order
            Seq.rev updates
        |> Seq.iter (fun x -> x.Force())

        log op chainParams block


    let addBlock =
        updateBlock Add

    let removeBlock =
        updateBlock Remove

let addBlock dataAccess chainParams block =
    Handler.addBlock dataAccess chainParams block

let removeBlock dataAccess chainParams block =
    Handler.removeBlock dataAccess chainParams block

let updateTallyBlockFromHeaders op env headers =
    let headers =
        match op with
        | Add ->
            headers
        | Remove ->
            headers |> List.rev
    for header in headers do
            // Change status of the header from main to connected
            ExtHeader.markAsMain header
            |> BlockRepository.saveHeader env.session

            let block =
                BlockRepository.getFullBlock env.session header
            match op with
            | Add ->
                addBlock env.session env.chainParams block

            | Remove ->
                removeBlock env.session env.chainParams block


let addTallyBlockFromHeaders env headers =
    updateTallyBlockFromHeaders Add env headers

let removeTallyBlockFromHeaders env headers =
    updateTallyBlockFromHeaders Remove env headers

let removeTallyBlocks env forkBlock currentTip =
    Chain.getSubChain env.session forkBlock currentTip
    |> removeTallyBlockFromHeaders env

let addTallyBlocks env forkBlock tip =
    Chain.getSubChain env.session forkBlock tip
    |> addTallyBlockFromHeaders env

module Consensus.Block
open MBrace.FsPickler.Combinators
open Consensus
open Types
open Infrastructure
open Result
open Serialization
open Chain

[<Literal>]
let HeaderSize = 100

let TwoPow256 = bigint.Pow (2I, 256)

let MaxTimeInFuture = 15UL * 60UL * 1000UL // 15 minutes in milliseconds

let genesisParent = {version=Version0;parent=Hash.zero;blockNumber=0ul;commitments=Hash.zero;timestamp=0UL;difficulty=0ul;nonce=0UL,0UL}

let result = new Result.ResultBuilder<string>()

let private computeCommitmentsRoot = MerkleTree.computeRoot

let hash =
    Header.serialize
    >> Hash.compute

let toHex = Block.serialize >> FsBech32.Base16.encode

let fromHex hex =
    FsBech32.Base16.decode hex
    |> Option.bind Block.deserialize

let isGenesis (chain:Chain.ChainParameters) block =
    let blockHashHash =
        hash block.header
        |> Hash.computeOfHash

    chain.genesisHashHash = blockHashHash

let getChainWork (prevWork:bigint) header =
    let target =
        Difficulty.uncompress header.difficulty
        |> Hash.toBigInt

    let proof = bigint.Divide (TwoPow256, target + 1I)

    prevWork + proof

let createGenesis (chain:Chain.ChainParameters) transactions nonce =
    let txMerkleRoot =
        transactions
        |> List.map (fun tx-> tx.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun tx-> tx.witnessHash)
        |> MerkleTree.computeRoot

    let acsMerkleRoot = ActiveContractSet.root ActiveContractSet.empty

    let commitments =
        Block.createCommitments txMerkleRoot witnessMerkleRoot acsMerkleRoot []
        |> computeCommitmentsRoot

    let header =
        {
            version=Version0;
            parent=Hash.zero;
            blockNumber=1ul;
            commitments=commitments;
            timestamp=chain.genesisTime;
            difficulty=(EMA.create chain).difficulty;
            nonce=nonce;
        }

    { header=header;
      transactions=transactions;
      commitments=[];
      txMerkleRoot=txMerkleRoot;
      witnessMerkleRoot=witnessMerkleRoot;
      activeContractSetMerkleRoot=acsMerkleRoot; }

let getBlockSacrificeAmount chain acs =

    let computeContractSacrifice (contract:Contract.T) =
        (contract.code
         |> String.length
         |> uint64) * chain.contractSacrificePerBytePerBlock

    Seq.sumBy computeContractSacrifice (ActiveContractSet.getContracts acs)

let private getPayoutOutput = fun (recipient, amount) ->
    {
        lock =
            match recipient with
            | PKRecipient pkHash -> Lock.PK pkHash
            | ContractRecipient cId -> Lock.Contract cId
        spend =
        {
            amount = amount
            asset = Asset.Zen
        }
    }

let getBlockCoinbase chain acs blockNumber transactions coinbasePkHash (cgp:CGP.T) =
    // Get the coinbase outputs by summing the fees per asset and adding the block reward
    let coinbaseOutputs =
        let blockRewardAndFees =
            let blockFees =
                transactions
                |> List.map (fun ex->ex.tx)
                |> List.collect (fun tx -> tx.outputs)
                |> List.filter (fun output -> output.lock = Fee)
                |> List.fold (fun totals output ->
                      let amount = defaultArg (Map.tryFind output.spend.asset totals) 0UL
                      Map.add output.spend.asset (output.spend.amount + amount) totals) Map.empty
            let totalZen = defaultArg (Map.tryFind Asset.Zen blockFees) 0UL
            let blockSacrifice = getBlockSacrificeAmount chain acs
            let blockReward = blockReward blockNumber cgp.allocation

            Map.add Asset.Zen (totalZen + blockReward + blockSacrifice) blockFees

        let lock = Coinbase (blockNumber, coinbasePkHash)

        Map.toSeq blockRewardAndFees
        |> Seq.map (fun (asset,amount) ->
            {
                lock=lock
                spend={asset=asset;amount=amount}
            })
        |> Seq.toList

    let payoutOutput =
        if CGP.isPayoutBlock chain blockNumber then
            cgp.payout
            |> Option.map getPayoutOutput
            |> function
               | None   -> []
               | Some x -> [x]
        else []

    let tx = {
        version = Version0
        inputs = []
        outputs = payoutOutput @ coinbaseOutputs
        contract = None
        witnesses = []
    }

    Transaction.toExtended tx

let createTemplate chain (parent:BlockHeader) timestamp (ema:EMA.T) acs (cgp:CGP.T) transactions coinbasePkHash =
    let blockNumber = (parent.blockNumber + 1ul)

    let coinbase = getBlockCoinbase chain acs blockNumber transactions coinbasePkHash cgp

    let transactions = coinbase :: transactions

    let txMerkleRoot =
        transactions
        |> List.map (fun tx-> tx.txHash)
        |> MerkleTree.computeRoot

    let witnessMerkleRoot =
        transactions
        |> List.map (fun tx-> tx.witnessHash)
        |> MerkleTree.computeRoot

    let acs = ActiveContractSet.expireContracts blockNumber acs
    let acsMerkleRoot = ActiveContractSet.root acs

    let parentHash = hash parent

    let median = EMA.earliest ema
    let timestamp = if timestamp > median then timestamp else (median + 1UL)

    // TODO: add utxo commitments
    let commitments =
        Block.createCommitments txMerkleRoot witnessMerkleRoot acsMerkleRoot []
        |> computeCommitmentsRoot

    let header =
        {
            version=Version0;
            parent=parentHash;
            blockNumber=blockNumber;
            commitments=commitments;
            timestamp=timestamp;
            difficulty=ema.difficulty;
            nonce=0UL,0UL;
        }

    { header=header;
      transactions=transactions;
      commitments=[];
      txMerkleRoot=txMerkleRoot;
      witnessMerkleRoot=witnessMerkleRoot;
      activeContractSetMerkleRoot=acsMerkleRoot; }

let validateHeader chain (header:BlockHeader) =
    if header.timestamp > chain.versionExpiry then
        Error "expired node version, please upgrade"
    else

    let blockHash = hash header
    let blockHashHash = Hash.computeOfHash blockHash

    if blockHashHash = chain.genesisHashHash then Ok header else

    let difficulty = Difficulty.uncompress header.difficulty
    let proofOfWorkLimit = chain.proofOfWorkLimit

    if difficulty <= proofOfWorkLimit && blockHash <= difficulty then Ok header else Error "proof of work failed"

// TODO: Refactor to avoid chained state-passing style
let validate chain =
    let checkTxNotEmpty (block:Block) =
        if List.isEmpty block.transactions then Error "transactions is empty" else Ok block

    let checkHeader (block:Block) =
        validateHeader chain block.header
        |> Result.map (fun _ -> block)

    let checkCoinbase (block:Block) =
        if isGenesis chain block then
            Ok block
        else
            let coinbase = List.head block.transactions

            match TransactionValidation.validateCoinbase chain block.header.blockNumber coinbase.tx with
            | Error error -> Error <| sprintf "Block failed coinbase validation due to %A" error
            | Ok _ -> Ok block

    let checkTxBasic (block:Block) = result {
        // skip if genesis block
        if isGenesis chain block then
            return block
        else
            let withoutCoinbase = List.tail block.transactions

            // Fail if validateBasic fails on any transaction in the block.
            for ex in withoutCoinbase do
                let! _ =
                    TransactionValidation.validateBasic ex.tx
                    |> Result.mapError (sprintf "transaction %A failed validation due to %A" ex.txHash)
                ()
            return block
    }

    let checkCommitments (block:Block) =
        let txMerkleRoot =
            block.transactions
            |> List.map (fun tx-> tx.txHash)
            |> MerkleTree.computeRoot

        let witnessMerkleRoot =
            block.transactions
            |> List.map (fun tx-> tx.witnessHash)
            |> MerkleTree.computeRoot

        if txMerkleRoot = block.txMerkleRoot && witnessMerkleRoot = block.witnessMerkleRoot then
            let commitments =
                Block.createCommitments block.txMerkleRoot block.witnessMerkleRoot block.activeContractSetMerkleRoot block.commitments
                |> computeCommitmentsRoot

            if commitments = block.header.commitments then
                Ok block
            else
                Error "commitments mismatch"
        else
            Error "commitments mismatch"

    checkTxNotEmpty
    >=> checkHeader
    >=> checkCoinbase
    >=> checkTxBasic
    >=> checkCommitments

/// Apply block to UTXO, ACS and CGP; operation can fail
let connect chainParams getUTXO getTx contractsPath (parent:BlockHeader) timestamp utxoSet (cgp:CGP.T) acs contractCache ema getContractState contractStates =
    let checkBlockNumber (block:Block) =
        if parent.blockNumber + 1ul <> block.header.blockNumber then
            Error "blockNumber mismatch"
        else Ok block

    let checkDifficulty (block:Block) =
        let nextEma = EMA.add chainParams block.header.timestamp ema

        if block.header.difficulty <> ema.difficulty then
            Error "incorrect proof of work"
        elif isGenesis chainParams block then
            Ok (block,nextEma)
        elif block.header.timestamp <= EMA.earliest ema then
            Error "block's timestamp is too early"
        elif block.header.timestamp > timestamp + MaxTimeInFuture then
            Error "block timestamp too far in the future"
        else
            Ok (block,nextEma)

    let checkTxInputs (block,ema) =
        let getTx = fun txhash ->
            match List.tryFind (fun ex -> ex.txHash = txhash) block.transactions with 
            | Some ex -> Some ex
            | None -> getTx txhash
            
        if isGenesis chainParams block then
            let set = List.fold (fun set ex ->
                UtxoSet.handleTransaction getUTXO ex.txHash ex.tx set) utxoSet block.transactions
            Ok (block,set,acs,CGP.empty,ema,contractCache,contractStates)
        else
            let coinbase = List.head block.transactions
            let withoutCoinbase = List.tail block.transactions

            let set = UtxoSet.handleTransaction getUTXO coinbase.txHash coinbase.tx utxoSet

            let cgp = CGP.update chainParams block.header.blockNumber cgp

            List.fold (fun state ex -> result {
                let! block,set,acs,cgp,ema,contractCache,contractStates = state

                let! _,acs,contractCache,contractStates =
                    TransactionValidation.validateInContext chainParams getUTXO contractsPath block.header.blockNumber block.header.timestamp acs contractCache set getContractState contractStates ex
                    |> Result.mapError (sprintf "transactions failed inputs validation due to %A")

                let cgp = CGP.handleTransaction (UtxoSet.getOutput getUTXO set) getTx ex.tx (CGP.getInterval chainParams block.header.blockNumber) cgp
                let set = UtxoSet.handleTransaction getUTXO ex.txHash ex.tx set
                return block,set,acs,cgp,ema,contractCache,contractStates
            }) (Ok (block,set,acs,cgp,ema,contractCache,contractStates)) withoutCoinbase
            
            

    let checkCoinbase (block,set,acs,(cgp:CGP.T),ema,contractCache,contractStates) =
        if isGenesis chainParams block then
            Ok (block,set,cgp,acs,ema,contractCache,contractStates)
        else
            let coinbase = List.head block.transactions
            let transactions = List.tail block.transactions

            if CGP.isPayoutBlock chainParams block.header.blockNumber then
                match cgp.payout with
                | Some payout ->
                    if getPayoutOutput payout = List.head coinbase.tx.outputs then
                        Ok <| List.tail coinbase.tx.outputs
                    else
                        Error "CGP payout mismatch"
                | None ->
                        Ok <| coinbase.tx.outputs
            else
                Ok <| coinbase.tx.outputs
            |> Result.bind (fun outputs ->
                let folder totals output =
                    let amount = defaultArg (Map.tryFind output.spend.asset totals) 0UL
                    Map.add output.spend.asset (output.spend.amount + amount) totals

                // Compute the amount of reward per asset
                let coinbaseTotals = List.fold folder Map.empty outputs

                // Compute the block reward and fees together
                let blockRewardAndFees =
                    // Compute entire fees paid transactions in the block
                    let blockFees =
                        transactions
                        |> List.collect (fun ex -> ex.tx.outputs)
                        |> List.filter (fun output -> output.lock = Fee)
                        |> List.fold folder Map.empty

                    let totalZen = defaultArg (Map.tryFind Asset.Zen blockFees) 0UL

                    let blockSacrifice = getBlockSacrificeAmount chainParams acs
                    let blockReward = blockReward block.header.blockNumber cgp.allocation

                    Map.add Asset.Zen (totalZen + blockSacrifice + blockReward) blockFees

                if coinbaseTotals <> blockRewardAndFees then
                    Error "block reward is incorrect"
                else
                    Ok (block,set,cgp,acs,ema,contractCache,contractStates))

    let checkCommitments (block,set,cgp,acs,ema,contractCache,contractStates) =
        let acs = ActiveContractSet.expireContracts block.header.blockNumber acs
        let acsMerkleRoot = ActiveContractSet.root acs

        // we already validated txMerkleRoot and witness merkle root at the basic validation, re-calculate with acsMerkleRoot
        let commitments =
            Block.createCommitments block.txMerkleRoot block.witnessMerkleRoot acsMerkleRoot block.commitments
            |> computeCommitmentsRoot

        // We ignore the known commitments in the block as we already calculated them
        // Only check that the final commitment is correct
        if commitments = block.header.commitments then
            Ok (block,set,cgp,acs,contractCache,ema,contractStates)
        else
            Error "commitments mismatch"

    let checkWeight (block,nextEma) = result {
        let! weight = Weight.blockWeight getUTXO block utxoSet
        let maxWeight = chainParams.maxBlockWeight
        if weight <= maxWeight
        then
            return block,nextEma
        else
            return! Error "block weight exceeds maximum"
    }

    checkBlockNumber
    >=> checkDifficulty
    >=> checkWeight
    >=> checkTxInputs
    >=> checkCoinbase
    >=> checkCommitments

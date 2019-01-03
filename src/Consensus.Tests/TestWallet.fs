module Consensus.Tests.TestWallet

open Consensus
open Chain
open Hash
open Crypto
open Types
open Infrastructure
open Result
open Wallet

open Zen.Hash

module ZData = Zen.Types.Data
module Cost = Zen.Cost.Realized

let result = new ResultBuilder<string>()

[<Literal>]
let passphrase = ""

[<Literal>]
let rlimit = 2723280u

type Status<'a> =
    | Spent of 'a
    | Unspent of 'a

type OutputStatus = Status<Output>
type SpendStatus = Status<Spend>

type TxDelta = {
    txHash: Hash
    deltas: List<SpendStatus>
    blockNumber: Option<uint32>
}

type T = {
    deltas: List<TxDelta>
    outputs: Map<Outpoint, OutputStatus>
    mempool: (Hash*Transaction) list
    tip: Hash
    blockNumber: uint32
    publicKey: PublicKey
}

let private zenCoinType = ExtendedKey.Hardened 258
let private purpose = ExtendedKey.Hardened 44

let deriveZenKey = ExtendedKey.derivePath "m/44'/258'/0'/0/0"

let private create mnemonicPhrase password tipHash tipBlockNumber = result {
    let! extendedPrivateKey = ExtendedKey.fromMnemonicPhrase mnemonicPhrase

    let! zenPrivateKey = deriveZenKey extendedPrivateKey
    let! publicKey =ExtendedKey.getPublicKey zenPrivateKey

    let secured = Secured.create password mnemonicPhrase

    return {
        deltas = List.empty
        outputs = Map.empty
        mempool = List.empty
        tip = tipHash
        blockNumber = tipBlockNumber
        publicKey = publicKey
    }, secured
}

let import mnemonicPhrase =
    let mnemonicPhrase = String.concat " " mnemonicPhrase

    create mnemonicPhrase

let private isKeyMatch account address =
    PublicKey.hash account.publicKey = address

// update the outputs with transaction
let private handleTransaction txHash (tx:Transaction) account outputs txDeltas blockNumber =
    let handleOutput (outputs,deltas) (index,output) =
        let add = (
            Map.add { txHash = txHash; index = index } (Unspent output) outputs,
            List.add (Unspent output.spend) deltas)
        match output.lock with
        | Coinbase (_,pkHash) when isKeyMatch account pkHash -> add
        | PK pkHash when isKeyMatch account pkHash -> add
        | _ -> (outputs, deltas)

    let handleInput (outputs,deltas) outpoint =
        match Map.tryFind outpoint outputs with
        | Some (Unspent output) ->
            (Map.add outpoint (Spent output) outputs,
             List.add (Spent output.spend) deltas)
        | _ ->
            (outputs, deltas)

    let outputs, deltas =
        tx.inputs
        |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
        |> List.fold handleInput (outputs, List.empty)

    let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs
    let outputs, deltas = List.fold handleOutput (outputs, deltas) outputsWithIndex

    if List.length deltas > 0 then
        outputs, List.add { txHash = txHash; deltas = deltas; blockNumber = blockNumber } txDeltas
    else
        outputs, txDeltas

let private handleTransactions account transactions blockNumber =
    List.fold (fun (outputs,txDeltas) (txHash,tx) ->
       handleTransaction txHash tx account outputs txDeltas blockNumber
    ) (account.outputs, account.deltas) transactions

let private handleMempoolTransactions account =
    handleTransactions account account.mempool None

let getUnspentOutputs account =
    // we first update the account according to the mempool transactions
    let outputs, txDeltas = handleMempoolTransactions account

    let outputs =
        outputs
        |> Map.filter (fun _ output ->
            match output with
            | Unspent _-> true
            | _ -> false)
        |> Map.map (fun _ output ->
            match output with
            | Unspent output -> output
            | _ -> failwith "unexpected")

    outputs, txDeltas

let addTransaction txHash (tx:Transaction) account =
    // check if the transaction is relevant to the account
    let anyOutput = List.exists (fun output ->
        match output.lock with
        | PK pkHash when isKeyMatch account pkHash -> true
        | Coinbase (_,pkHash) when isKeyMatch account pkHash -> true
        | _ -> false) tx.outputs

    let unspentOutputs, _ = getUnspentOutputs account

    let anyInputs =
        tx.inputs
        |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
        |> List.exists (fun input -> Map.containsKey input unspentOutputs)

    if anyInputs || anyOutput then
        let mempool = List.add (txHash,tx) account.mempool

        {account with mempool = mempool}
    else
        account

let getBalance account =
    let unspent, _ = getUnspentOutputs account

    Map.fold (fun balance _ output ->
        match Map.tryFind output.spend.asset balance with
        | Some amount -> Map.add output.spend.asset (amount+output.spend.amount) balance
        | None -> Map.add output.spend.asset output.spend.amount balance) Map.empty unspent

let private collectInputs chainParams account amount asset secretKey =
    let unspent, _ = getUnspentOutputs account

    let collectInputs ((inputs, keys), collectedAmount) outpoint output =
        let isMature =
            match output.lock with
            | Coinbase (blockNumber,_) -> (account.blockNumber + 1ul) - blockNumber >= chainParams.coinbaseMaturity
            | _ -> true

        if isMature && amount > collectedAmount && asset = output.spend.asset then
            let keyPair = secretKey, account.publicKey
            (((outpoint,output) :: inputs, keyPair :: keys), output.spend.amount + collectedAmount)
        else
            ((inputs, keys), collectedAmount)

    let (inputs, keys), collectedAmount = Map.fold collectInputs (([],[]),0UL) unspent

    if collectedAmount >= amount then
        Ok (inputs,keys,collectedAmount)
    else
        Error "Not enough tokens"

let private addChange inputAmount asset account outputs =
    let outputAmount =
        List.filter (fun o -> o.spend.asset = asset) outputs
        |> List.sumBy (fun o -> o.spend.amount)

    if inputAmount > outputAmount then
        let changeOutput =
            {
                spend = { amount=(inputAmount-outputAmount);asset=asset};
                lock = PK (PublicKey.hash account.publicKey)
            }

        List.add changeOutput outputs
    else
        outputs

let createTransactionFromLock chainParams lk spend (account, extendedKey) = result {
    let! zenKey = deriveZenKey extendedKey
    let! secretKey = ExtendedKey.getPrivateKey zenKey

    let! (inputs, keys, inputAmount) = collectInputs chainParams account spend.amount spend.asset secretKey
    let inputPoints = List.map (fst >> Outpoint) inputs
    let outputs = addChange inputAmount spend.asset account [{ spend = spend; lock = lk }]
    return
        Transaction.sign keys TxHash {
                                version = Version0
                                inputs = inputPoints
                                outputs = outputs
                                witnesses = []
                                contract = None }
}

let createTransaction chainParams pkHash spend accoundData =
    createTransactionFromLock chainParams (PK pkHash) spend accoundData

let createContractRecord code =
    result {
        let contractId = Contract.makeContractId Version0 code

        let! hints = Measure.measure
                        (sprintf "recording hints for contract %A" contractId)
                        (lazy(Contract.recordHints code))
        let! queries = ZFStar.totalQueries hints

        return
            (contractId,
                {   code = code
                    hints = hints
                    rlimit = rlimit
                    queries = queries })
    }

let createActivationTransactionFromContract chainParams (contractId, ({queries=queries;code=code} as contractV0)) (numberOfBlocks:uint32) (account, extendedKey) =

    result {
        let codeLength = String.length code |> uint64

        let activationFee = queries * rlimit / 100ul |> uint64
        let activationSacrifice = chainParams.sacrificePerByteBlock * codeLength * (uint64 numberOfBlocks)

        let! zenKey = deriveZenKey extendedKey
        let! secretKey = ExtendedKey.getPrivateKey zenKey

        let! (inputs,keys,inputAmount) = collectInputs chainParams account (activationSacrifice + activationFee) Asset.Zen secretKey
        let inputPoints = List.map (fst >> Outpoint) inputs

        let outputs =
            [
                { spend = { amount = activationSacrifice; asset = Asset.Zen }; lock = ActivationSacrifice }
                { spend = { amount = activationFee; asset = Asset.Zen }; lock = Fee }
            ]
            |> addChange inputAmount Asset.Zen account

        return Transaction.sign
            keys TxHash
            {
                version = Version0
                inputs = inputPoints
                outputs = outputs
                witnesses = []
                contract = Some (V0 contractV0)
            }
    }

let createActivateContractTransaction chain code (numberOfBlocks:uint32) (account, extendedKey) =
    result {
        let! contractWithId = createContractRecord code
        return! createActivationTransactionFromContract chain contractWithId numberOfBlocks (account, extendedKey)

    }

let addReturnAddressToData publicKey data =
    let addReturnAddressToData' dict =
        let returnAddress = PK (PublicKey.hash publicKey)

        Zen.Dictionary.add "returnAddress"B (ZData.Lock (ZFStar.fsToFstLock returnAddress)) dict
        |> Cost.__force
        |> ZData.Dict
        |> ZData.Collection
        |> Some
        |> Ok

    match data with
    | Some (ZData.Collection (ZData.Dict dict)) -> addReturnAddressToData' dict
    | None -> addReturnAddressToData' Zen.Dictionary.empty
    | _ -> Error "data can only be empty or dict in order to add return address"

let private signFirstWitness signKey tx = result {
    match signKey with
    | Some signKey ->
        let! witnessIndex =
            List.tryFindIndex (fun witness ->
                match witness with
                | ContractWitness _ -> true
                | _ -> false) tx.witnesses
            |> ofOption "missing contact witness"
        let! wintess =
            match tx.witnesses.[witnessIndex] with
            | ContractWitness cw -> Ok cw
            | _ -> Error "missing contact witness"

        let txHash = Transaction.hash tx

        let! signature = ExtendedKey.sign txHash signKey
        let! publicKey = ExtendedKey.getPublicKey signKey

        let witness = {wintess with signature=Some (publicKey,signature)}
        let witnesses = List.update witnessIndex (ContractWitness witness) tx.witnesses

        return {tx with witnesses = witnesses}
    | None -> return tx
}

let createExecuteContractTransaction chainParams executeContract (contractId:ContractId) command data provideReturnAddress sign spends (account, extendedKey) = result {
    let Zen = Asset.Zen
    let mutable txSkeleton = TxSkeleton.empty
    let mutable keys = List.empty

    let! zenKey = deriveZenKey extendedKey
    let! secretKey = ExtendedKey.getPrivateKey zenKey
    let pkHash = PublicKey.hash account.publicKey

    if Map.isEmpty spends then
        // To avoid rejection of a valid contract transaction due to possible all-mint inputs
        // or same txhash, until we implement fees, we include a temp fee of one kalapa
        let tempFeeAmount = 1UL

        let! inputs, keys', collectedAmount =
            collectInputs chainParams account tempFeeAmount Zen secretKey
        let inputs = List.map TxSkeleton.Input.PointedOutput inputs

        txSkeleton <-
            TxSkeleton.addInputs inputs txSkeleton
            |> TxSkeleton.addChange Zen collectedAmount tempFeeAmount pkHash

        let feeOutput = { lock = Fee; spend = { amount = tempFeeAmount; asset = Zen } }
        txSkeleton <- TxSkeleton.addOutput feeOutput txSkeleton

        keys <- List.append keys keys'
    else
        for (asset, amount) in Map.toSeq spends do
            let! inputs, keys', collectedAmount =
                collectInputs chainParams account amount asset secretKey
            let inputs = List.map TxSkeleton.Input.PointedOutput inputs

            txSkeleton <-
                TxSkeleton.addInputs inputs txSkeleton
                |> TxSkeleton.addChange asset collectedAmount amount pkHash

            keys <- List.append keys keys'

    let! data =
        if provideReturnAddress then
            addReturnAddressToData account.publicKey data
        else
            Ok data

    let! signKey =
        match sign with
        | Some keyPath ->
            ExtendedKey.derivePath keyPath extendedKey
            <@> Some
        | None -> Ok None

    let! sender =
        match signKey with
        | Some signKey ->
            ExtendedKey.getPublicKey signKey
            <@> Some
        | None -> Ok None

    let! unsignedTx = executeContract contractId command sender data txSkeleton

    let sign tx = signFirstWitness signKey tx <@> Transaction.sign keys FollowingWitnesses

    return! sign unsignedTx
}
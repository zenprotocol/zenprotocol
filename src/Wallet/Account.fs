module Wallet.Account

open Consensus
open Chain
open Hash
open Crypto
open Types
open Infrastructure
open Result
open Messaging.Services
open Result
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

let private create password seed = result {
    let! extendedPrivateKey = ExtendedKey.create seed

    let! zenPrivateKey = deriveZenKey extendedPrivateKey
    let! publicKey =ExtendedKey.getPublicKey zenPrivateKey

    let secured = Secured.create password seed

    return {
        deltas = List.empty
        outputs = Map.empty
        mempool = List.empty
        tip = Hash.zero
        blockNumber = 0ul
        publicKey = publicKey
    }, secured
}

let private deriveSeed words =
    try
         let mnemonicSentence = new NBitcoin.Mnemonic(String.concat " " words, NBitcoin.Wordlist.English)
         Ok <| mnemonicSentence.DeriveSeed passphrase
     with _ as ex ->
         Error ex.Message

let import words password =
    deriveSeed words
    >>= create password

let private isKeyMatch account address =
    PublicKey.hash account.publicKey = address

// update the outputs with transaction
let private handleTransaction txHash (tx:Transaction) account outputs txDeltas =
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
        outputs, List.add { txHash = txHash; deltas = deltas } txDeltas
    else
        outputs, txDeltas

let private handleTransactions account transactions =
    List.fold (fun (outputs,txDeltas) (txHash,tx) ->
       handleTransaction txHash tx account outputs txDeltas
    ) (account.outputs, account.deltas) transactions

let handleBlock blockHash block account =
    let txHashes = List.map (fun tx -> Transaction.hash tx) block.transactions
    let transactions = List.zip txHashes block.transactions

    let outputs, deltas = handleTransactions account transactions

    // remove all transactions from list
    let set = Set.ofList txHashes
    let mempool = List.reject (fun (txHash,_) -> Set.contains txHash set) account.mempool

    { account with
        tip = blockHash;
        blockNumber = block.header.blockNumber;
        mempool = mempool;
        outputs = outputs;
        deltas = deltas }

let undoBlock block account =
    let undoTransaction tx (outputs,mempool) =
        let txHash = Transaction.hash tx

        let handleOutput outputs (index,output) =
            match output.lock with
            | Coinbase (_,pkHash) when isKeyMatch account pkHash ->
                let outpoint = {txHash=txHash;index=index;}
                Map.remove outpoint outputs
            | PK pkHash when isKeyMatch account pkHash ->
                let outpoint = {txHash=txHash;index=index;}
                Map.remove outpoint outputs
            | _ -> outputs

        let handleInput outputs input =
            match Map.tryFind input outputs with
            | Some (Spent output) -> Map.add input (Unspent output) outputs
            | _ -> outputs

        let outputs' =
            tx.inputs
            |> List.choose (function | Outpoint outpoint -> Some outpoint | Mint _ -> None)
            |> List.fold handleInput outputs

        let outputsWithIndex = List.mapi (fun i output -> (uint32 i,output)) tx.outputs
        let outputs' = List.fold handleOutput outputs' outputsWithIndex

        let mempool =
            // adding the transaction back to mempool only if the transaction affected the account
            if outputs' <> outputs then
                (txHash,tx) :: mempool
            else
                mempool

        outputs',mempool

    let outputs,mempool = List.foldBack undoTransaction block.transactions (account.outputs,account.mempool)

    {account with tip=block.header.parent;blockNumber=block.header.blockNumber - 1ul; outputs=outputs;mempool = mempool}

type private SyncAction =
    | Undo of Block
    | Add of Block * Hash

let private writer = new Writer.WriterBuilder<SyncAction>()

let sync tipBlockHash (getHeader:Hash -> BlockHeader) (getBlock:Hash -> Block) account =
    let blockNumber hash =
        if hash <> Hash.zero then
            (getHeader hash).blockNumber
        else
            0ul

    let log action = Writer.Writer([action],())

    // Find the fork block of the account and the blockchain, logging actions
    // to perform. Undo each block in the account's chain but not the blockchain,
    // and add each block in the blockchain but not the account's chain.
    let rec locate ((x,i),(y,j)) acc =

        if x = y && i = j then acc
        elif i > j
        then
            locate (((getHeader x).parent, i-1ul), (y,j)) (Add (getBlock x, x) :: acc)
        elif i < j
        then
            locate ((x,i), ((getHeader y).parent, j-1ul)) (Undo (getBlock y) :: acc)
        else
            locate (((getHeader x).parent, i-1ul), ((getHeader y).parent, j-1ul)) (Add (getBlock x, x) :: Undo (getBlock y) :: acc)

    let actions = locate ((tipBlockHash, blockNumber tipBlockHash), (account.tip, blockNumber account.tip)) []
    let toUndo, toAdd = List.partition (function | Undo _ -> true | _ -> false) actions
    let toUndo = List.rev toUndo     // Blocks to be undo were found backwards.
    let sortedActions = toUndo @ toAdd

    List.fold
        <|  fun accnt ->
                function
                | Undo blk -> undoBlock blk accnt
                | Add (blk,hash) -> handleBlock hash blk accnt
        <| account
        <| sortedActions

let getUnspentOutputs account =
    // we first update the account according to the mempool transactions
    let outputs, txDeltas = handleTransactions account account.mempool

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

let getHistory account =
    handleTransactions account account.mempool
    |> snd
    |> List.map (fun txDelta ->
        txDelta.txHash,
        txDelta.deltas
        |> List.fold (fun amounts spend ->
            let asset, amount =
                match spend with
                | Spent spend -> spend.asset, 0L - int64 spend.amount
                | Unspent spend -> spend.asset, int64 spend.amount
            let amount' =
                (match Map.tryFind asset amounts with
                | Some amount -> amount
                | None -> 0L)
            Map.add asset (amount' + amount) amounts
        ) Map.empty
    )

let private collectInputs account spend secretKey =
    let unspent, _ = getUnspentOutputs account

    let collectInputs ((inputs, keys), collectedAmount) outpoint output =
        let isMature =
            match output.lock with
            | Coinbase (blockNumber,_) -> (account.blockNumber + 1ul) - blockNumber >= CoinbaseMaturity
            | _ -> true

        if isMature && spend.amount > collectedAmount && spend.asset = output.spend.asset then
            let keyPair = secretKey, account.publicKey
            (((outpoint,output) :: inputs, keyPair :: keys), output.spend.amount + collectedAmount)
        else
            ((inputs, keys), collectedAmount)

    let (inputs, keys), collectedAmount = Map.fold collectInputs (([],[]),0UL) unspent

    if collectedAmount >= spend.amount then
        Ok (inputs,keys,collectedAmount)
    else
        Error "Not enough tokens"

let private addChange spend amount account output =
    List.add output (
        if amount = spend.amount then [] else
            // Create change if needed
            [{
                spend = { spend with amount=(amount-spend.amount) }
                lock = PK (PublicKey.hash account.publicKey)
            }]
        )

let createTransactionFromLock lk spend (account, extendedKey) = result {
    let! zenKey = deriveZenKey extendedKey
    let! secretKey = ExtendedKey.getPrivateKey zenKey

    let! (inputs, keys, amount) = collectInputs account spend secretKey
    let inputPoints = List.map (fst >> Outpoint) inputs
    let outputs = addChange spend amount account { spend = spend; lock = lk }
    return
        Transaction.sign keys { inputs = inputPoints
                                outputs = outputs
                                witnesses = []
                                contract = None }
}

let createTransaction pkHash spend accoundData =
    createTransactionFromLock (PK pkHash) spend accoundData

let createActivateContractTransaction chain code (numberOfBlocks:uint32) (account, extendedKey) =
    let codeLength = String.length code |> uint64

    let activationSacrifice = chain.sacrificePerByteBlock * codeLength * (uint64 numberOfBlocks)
    let spend = { amount = activationSacrifice; asset = Constants.Zen }

    result {
        let! zenKey = deriveZenKey extendedKey
        let! secretKey = ExtendedKey.getPrivateKey zenKey

        let! (inputs,keys,amount) = collectInputs account spend secretKey
        let inputPoints = List.map (fst >> Outpoint) inputs

        let cHash = Contract.computeHash code

        let! hints = Measure.measure
                        (sprintf "recording hints for contract %A" cHash)
                        (lazy(Contract.recordHints code))
        let! queries = ZFStar.totalQueries hints

        let outputs = addChange spend amount account { spend = spend; lock = ActivationSacrifice }
        return Transaction.sign
            keys
            {
                inputs = inputPoints
                outputs = outputs
                witnesses = []
                contract = Some { code = code
                                  hints = hints
                                  rlimit = rlimit
                                  queries = queries }
            }
    }

let addReturnAddressToData publicKey data =
    let addReturnAddressToData' dict =
        let returnAddress = PK (PublicKey.hash publicKey)

        Zen.Dictionary.add "returnAddress"B (ZData.Lock (ZFStar.fsToFstLock returnAddress)) dict
        |> Cost.__force
        |> ZData.DataDict
        |> ZData.Dict
        |> Some
        |> Ok

    match data with
    | Some (ZData.Dict (ZData.DataDict dict)) -> addReturnAddressToData' dict
    | None -> addReturnAddressToData' Zen.Dictionary.empty
    | _ -> Error "data can only be empty or dict in order to add return address"

let signFirstWitness signKey tx = result {
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

let createExecuteContractTransaction executeContract cHash command data provideReturnAddress sign spends (account, extendedKey) = result {
    let mutable txSkeleton = TxSkeleton.empty
    let mutable keys = List.empty

    let! zenKey = deriveZenKey extendedKey
    let! secretKey = ExtendedKey.getPrivateKey zenKey
    let pkHash = PublicKey.hash account.publicKey

    for (asset, amount) in Map.toSeq spends do
        let! inputs, keys', collectedAmount =
            collectInputs account { asset = asset; amount = amount } secretKey
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

    let! unsignedTx = executeContract cHash command sender data txSkeleton

    let sign tx = signFirstWitness signKey tx <@> Transaction.sign keys

    return! sign unsignedTx
}

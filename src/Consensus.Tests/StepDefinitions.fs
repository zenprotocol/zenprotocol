module Consensus.Tests.StepDefinitions

open Infrastructure
open TechTalk.SpecFlow
open NUnit.Framework
open TechTalk.SpecFlow.BindingSkeletons
open TechTalk.SpecFlow.Assist
open Platform
open Result
open System.Text.RegularExpressions
open Blockchain
open Consensus
open ZFStar
open Crypto
open Types
open System
open State
open System.IO
open FsUnit
open Messaging.Events

let private result = new ResultBuilder<string>()

let private (?=) expected actual =
    actual |> should equal expected

[<Literal>]
let rlimit = 2723280u

let defaultChain = { Chain.getChainParameters Chain.Local with proofOfWorkLimit = Difficulty.uncompress 553648127u; genesisTime = (Timestamp.now()) - 1_000_000UL }

let tempDir () =
    Path.Combine
        [| Path.GetTempPath(); Path.GetRandomFileName() |]

let dataPath = tempDir()
let mutable databaseContext = DatabaseContext.createEmpty dataPath
let mutable session = DatabaseContext.createSession databaseContext

let createDatabaseContext() =
    let contractsPath = Infrastructure.Platform.combine dataPath "contracts"
    let temp = tempDir()

    let copyDir sourcePath destPath =
        if not <| Directory.Exists sourcePath then
            Directory.CreateDirectory sourcePath |> ignore
        if not <| Directory.Exists destPath then
            Directory.CreateDirectory destPath |> ignore

        for newPath in Directory.GetFiles(sourcePath, "*.*", SearchOption.TopDirectoryOnly) do
            File.Copy(newPath, newPath.Replace(sourcePath, destPath), true)

    //backup dll's from old session
    copyDir contractsPath temp
    databaseContext <- DatabaseContext.createEmpty dataPath
    session <- DatabaseContext.createSession databaseContext
    copyDir temp session.context.contractPath
    Infrastructure.Platform.cleanDirectory temp

let clean() =
    cleanDirectory dataPath

[<Binding>]
module Binding =
    open Consensus.Chain
    open Consensus.Serialization

    type TestingState = {
        chain : ChainParameters
        blocks : Map<string, Block>
        acs: Map<BlockHeader, ActiveContractSet.T>
        cgp: Map<BlockHeader, CGP.T>
        missing : Map<string, Block>
        contracts : Map<string, ContractId * ContractV0>
        keys : Map<string, KeyPair>
        txs : Map<string, Transaction>
        txLists : Map<string, Transaction list>
        data : Map<string, Zen.Types.Data.data>
    }

    let mutable testingState = {
        chain = defaultChain
        blocks = Map.empty
        acs = Map.empty
        cgp = Map.empty
        missing = Map.empty
        contracts = Map.empty
        keys = Map.empty
        txs = Map.empty
        txLists = Map.empty
        data = Map.empty
    }

    let getEmptyState() =
        let ema : EMA.T = {
            difficulty = 0ul
            delayed = []
        }

        let tipState : TipState = {
            tip = ExtendedBlockHeader.empty
            activeContractSet = ActiveContractSet.empty
            ema = ema
            cgp = CGP.empty
        }

        let memoryState : MemoryState = {
            utxoSet = UtxoSet.asDatabase
            activeContractSet = ActiveContractSet.empty
            orphanPool = OrphanPool.create()
            mempool = MemPool.empty
            contractCache = ContractCache.empty
            contractStates = ContractStates.asDatabase
            invalidTxHashes = Set.empty
        }

        {
            tipState = tipState
            memoryState = memoryState
            initialBlockDownload = InitialBlockDownload.Inactive
            headers = 0ul
        }

    let mutable state : Blockchain.State.State = getEmptyState()
    let mutable contractExecutionCache : Map<ContractId, Contract.T> = Map.empty

    let getUTXO = UtxoSetRepository.get
    let getContractState = ContractStateRepository.get

    let split (value:string) =
        value.Split [|','|]
        |> Array.choose (fun value -> if String.IsNullOrWhiteSpace value then None else Some value)
        |> Array.toList

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let updateTx txLabel tx =
        testingState <- { testingState with txs = Map.add txLabel tx testingState.txs }
        tx

    let tryFindTx txLabel = Map.tryFind txLabel testingState.txs

    let tryFindTxByHash txHash =
        testingState.txs
        |> Map.tryFindKey (fun _ tx -> Transaction.hash tx = txHash)
        |> Option.bind tryFindTx

    let tryFindTxList txLabel = Map.tryFind txLabel testingState.txLists

    let tryFindContract contractLabel = Map.tryFind contractLabel testingState.contracts

    let findContract contractLabel =
        match tryFindContract contractLabel with
        | Some contract -> contract
        | _ -> failwithf "cannot resolve contract: %A" contractLabel

    let findBlock blockLabel = Map.find blockLabel testingState.blocks

    let findBlockLabel block =
        testingState.blocks
        |> Map.map (fun _ bk -> bk.header)
        |> Map.tryFindKey (fun _ block' -> block = block')

    let getContractId contractLabel =
        let path = Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location)
        let path = Path.Combine (path, "Contracts")
        let contract = Path.ChangeExtension (contractLabel,".fst")
        let code = Path.Combine (path, contract) |> File.ReadAllText
        let contractId = Contract.makeContractId Version0 code
        contractId, code

    let getContractRecord contractLabel =
        match Map.tryFind contractLabel testingState.contracts with
        | Some value -> value
        | None ->
            let contractId, code = getContractId contractLabel

            let hints =
                match Contract.recordHints code with
                | Ok hints -> hints
                | Error error -> failwith error

            let queries =
                match Infrastructure.ZFStar.totalQueries hints with
                | Ok queries -> queries
                | Error error -> failwith error

            let contract = {
               code = code
               hints = hints
               rlimit = rlimit
               queries = queries
            }

            testingState <-
                { testingState with contracts = Map.add contractLabel (contractId, contract) testingState.contracts }

            contractId, contract

    let getContractFunction contractLabel =
        let contractId, contract =
            getContractRecord contractLabel

        match Map.tryFind contractId contractExecutionCache with
        | Some contract -> contract
        | None ->
            Contract.compile session.context.contractPath contract
            |> Result.bind (fun _ -> Contract.load session.context.contractPath 1ul contract.code contractId)
            |> Result.map (fun contract -> contractExecutionCache <- Map.add contractId contract contractExecutionCache
                                           contract)
            |> Infrastructure.Result.get

    let getAsset value =
        if value = "Zen" then Asset.Zen
        else
            let contractId, _ = getContractRecord value
            Asset (contractId, Hash.zero)

    let getAmount asset amount =
        if asset = Asset.Zen then
            amount * 100_000_000UL
        else
            amount

    let findTx txLabel =
        match tryFindTx txLabel with
        | Some tx -> tx
        | None -> failwithf "Referenced tx missing: %A" txLabel

    let tryFindKey keyLabel = Map.tryFind keyLabel testingState.keys

    let findKey keyLabel =
        match tryFindKey keyLabel with
        | Some tx -> tx
        | None -> failwithf "Referenced key missing: %A" keyLabel

    let initTx txLabel =
        match tryFindTx txLabel with
        | Some tx ->
            tx
        | None ->
            { inputs = []; witnesses = []; outputs = []; version = Version0; contract = None }
            |> updateTx txLabel

    let initKey keyLabel =
        let context = Native.secp256k1_context_create (Native.SECP256K1_CONTEXT_SIGN ||| Native.SECP256K1_CONTEXT_VERIFY)

        let secretKey =
            (keyLabel:string)
            |> System.Text.Encoding.ASCII.GetBytes
            |> Hash.compute
            |> Hash.bytes

        let publicKey = Array.create 64 0uy

        let keyPair =
            match Native.secp256k1_ec_pubkey_create (context, publicKey, secretKey) with
            | Native.Result.Ok -> SecretKey secretKey, PublicKey.PublicKey publicKey
            | _ -> failwithf "Unexpected result generating test key"

        testingState <- { testingState with keys = Map.add keyLabel keyPair testingState.keys }

        keyPair

    let initData dataLabel data =
        if not <| Map.containsKey dataLabel testingState.data then
            testingState <- { testingState with data = Map.add dataLabel data testingState.data }

    let tryFindData dataLabel = Map.tryFind dataLabel testingState.data

    let getData dataType (value : string) =
        match dataType with
        | "string" ->
            value.Trim('"')
            |> fsToFstString
            |> Zen.Types.Data.String
        | "i64" ->
            Int64.Parse value
            |> Zen.Types.Data.I64
        | "u32" ->
            UInt32.Parse value
            |> Zen.Types.Data.U32
        | "u64" ->
            UInt64.Parse value
            |> Zen.Types.Data.U64
        | _ ->
            failwithf "Unrecognized data type: %A" dataType

    let getDataCollection collectionType dataType =
        split
        >> List.map (fun value ->
            match tryFindData value with
            | Some data -> data
            | None -> getData dataType value)
        >> match collectionType with
            | "list" -> fsToFstList >> Zen.Types.Data.List
            | "array" -> Array.ofList >> Zen.Types.Data.Array
            | _ -> failwithf "Unrecognized data collection type: %A" dataType
        >> Zen.Types.Data.Collection

    let getLock label =
        match tryFindContract label with
        | Some (contractId, _) ->
            contractId
            |> Lock.Contract
        | None ->
            initKey label
            |> snd
            |> PublicKey.hash
            |> Lock.PK

    let getOutput amount asset keyLabel = {
        spend = { amount = amount; asset = getAsset asset }
        lock = getLock keyLabel
    }

    let getSender sender =
        if sender = "anonymous" then
            None
        else
            initKey sender
            |> snd
            |> Some
            
    let getRecipient addressKeyLabel =
        match getLock addressKeyLabel with
        | Lock.PK pkHash -> PKRecipient pkHash
        | Lock.Contract cId -> ContractRecipient cId
        | _ -> failwith "unexpected"

    let [<BeforeTestRun>] beforeTestRun () =
        clean()
    let [<AfterTestRun>] afterTestRun () =
        clean()

    let [<BeforeScenario>] setupScenario () =
        createDatabaseContext()
        testingState <- {
            chain = defaultChain
            blocks = Map.empty
            acs = Map.empty
            cgp = Map.empty
            missing = Map.empty
            contracts = testingState.contracts
            keys = Map.empty
            txs = Map.empty
            txLists = Map.empty
            data = Map.empty
        }
        let contractCache = state.memoryState.contractCache
        state <- getEmptyState()
        state <- { state with memoryState = { state.memoryState with contractCache = contractCache } }

    // Setting chain params
    let [<Given>] ``chain params`` (table: Table) =
        for row in table.Rows do
            let key = row.GetString "Key"
            let value = row.GetString "Value"

            match key with
            | "interval" ->
                testingState <- { testingState with chain = { testingState.chain with intervalLength = UInt32.Parse value } }
            | "allocationCorrectionCap" ->
                testingState <- { testingState with chain = { testingState.chain with allocationCorrectionCap = Byte.Parse value } }
            | "coinbaseMaturity" ->
                testingState <- { testingState with chain = { testingState.chain with coinbaseMaturity = UInt32.Parse value } }
            | _ ->
                failwith "unidentified option"

    // Initializes a utxoset
    let [<Given>] ``utxoset`` (table: Table) =
        let mutable txs = Map.empty

        // init all mentioned txs with their outputs
        for row in table.Rows do
            let txLabel = row.GetString "Tx"
            let keyLabel = row.GetString "Key"
            let asset = row.GetString "Asset"
            let amount = row.GetInt64 "Amount" |> uint64

            let tx = initTx txLabel
            let output = getOutput (getAmount (getAsset asset) amount) asset keyLabel
            let tx = { tx with outputs = Infrastructure.List.add output tx.outputs }
                     |> updateTx txLabel

            txs <- Map.add txLabel tx txs

        // fold on mentioned txs, fold on their outputs
        let utxoSet =
            txs
            |> Map.fold (fun utxoset _ tx ->
                let txHash = Transaction.hash tx
                tx.outputs
                |> List.mapi (fun i output -> (uint32 i, output))
                |> List.fold (fun utxoset (index, output) ->
                    let outpoint = { txHash = txHash; index = index }
                    Map.add outpoint (UtxoSet.Unspent output) utxoset
                ) utxoset
            ) state.memoryState.utxoSet

        state <- { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    let executeContract contractLabel inputTxLabel outputTxLabel command sender messageBody =
        let contract = getContractFunction contractLabel
        let tx = initTx inputTxLabel
        let outputs =
            match UtxoSet.tryGetOutputs (getUTXO session) state.memoryState.utxoSet tx.inputs with
            | Some outputs -> outputs
            | None -> failwithf "unable to get utxos for tx %A" inputTxLabel

        let inputTx = TxSkeleton.fromTransaction tx outputs

        let state' = { state with memoryState = { state.memoryState with activeContractSet = ActiveContractSet.add contract.contractId contract state.memoryState.activeContractSet } }

        match TransactionHandler.executeContract session inputTx 0UL contract.contractId command sender messageBody state' false with
        | Ok tx -> updateTx outputTxLabel tx
        | Error error -> failwith error

    // Executes a contract
    let [<When>] ``executing (.*) on (.*) returning (.*)`` contractLabel inputTxLabel outputTxLabel =
        executeContract contractLabel inputTxLabel outputTxLabel "" None None

    // Executes a contract with additional arguments
    let [<When>] ``executing (.*) on (.*) returning (.*) with`` contractLabel inputTxLabel outputTxLabel (table: Table) =
        let mutable command = String.Empty
        let mutable messageBody = None
        let mutable sender = None

        for row in table.Rows do
            let value = row.Item(1)
            match row.Item(0) with
            | "Command" -> command <- value
            | "Sender" -> sender <- getSender value
            | "MessageBody" ->
                messageBody <-
                    match tryFindData value with
                    | Some data -> Some data
                    | _ -> failwithf "cannot resolve data: %A" value
            | "ReturnAddress" ->
                let _, publicKey = initKey value
                match TestWallet.addReturnAddressToData publicKey messageBody with
                | Ok messageBody' -> messageBody <- messageBody'
                | Error error -> failwithf "error initializing data with return address: %A" error
            | other -> failwithf "unexpected execute option %A" other

        executeContract contractLabel inputTxLabel outputTxLabel command sender messageBody

    // Adding a single output
    // or adding a change output for an asset
    let [<Given>] ``(.*) locks (.*) (.*) to (.*)`` txLabel amount asset keyLabel =
        let tx = initTx txLabel

        let amount =
            if amount = "change" then
                let foldOutputs =
                    List.fold (fun amount (output:Output) ->
                        amount +
                        if output.spend.asset = getAsset asset then
                            output.spend.amount
                        else
                            0UL) 0UL
                let inputsTotal =
                    match UtxoSet.tryGetOutputs (getUTXO session) state.memoryState.utxoSet tx.inputs with
                    | Some outputs -> outputs
                    | None ->
                        failwithf "calculating change, unable to get utxos for tx %A %A" txLabel tx
                    |> foldOutputs
                let outputsTotal =
                    tx.outputs
                    |> foldOutputs
                inputsTotal - outputsTotal
            else
                getAmount (getAsset asset) (UInt64.Parse amount)

        let output = getOutput amount asset keyLabel
        { tx with outputs = Infrastructure.List.add output tx.outputs }
        |> updateTx txLabel
        |> ignore

    let private getFirstPKAddress tx =
        let findOutput (outpoint:Outpoint) =
            tryFindTxByHash outpoint.txHash
            |> Option.map (fun tx -> tx.outputs.[int outpoint.index].lock)

        tx.inputs
        |> List.find (function | Outpoint _ -> true | _ -> false)
        |> function | Outpoint outpoint -> outpoint | _ -> failwith "expected outpoint"
        |> findOutput
        |> function
        | Some (PK pkHash) -> pkHash
        | Some (Vote (_,_,pkHash)) -> pkHash
        | Some other -> failwithf "expected PK lock, got %A" other
        | None -> failwithf "could not get first PK hash"
    
    let [<Given>] ``(.*) votes on nothing with (.*) Zen in interval (.*)`` txLabel amount interval =
        let tx = initTx txLabel
    
        let voteData = { allocation = None; payout = None }
        let pkHash = getFirstPKAddress tx
        let output = { spend = { amount = getAmount Asset.Zen amount; asset = Asset.Zen }; lock = (voteData, interval, pkHash) |> Lock.Vote}
    
        { tx with outputs = Infrastructure.List.add output tx.outputs; version = Version1 }
        |> updateTx txLabel
        |> ignore

    let [<Given>] ``(.*) votes on allocation of (.*) with (.*) Zen in interval (.*)`` txLabel allocation amount interval =
        let tx = initTx txLabel

        let voteData = { allocation = Some (byte allocation); payout = None }
        let pkHash = getFirstPKAddress tx
        let output = { spend = { amount = getAmount Asset.Zen amount; asset = Asset.Zen }; lock = (voteData, interval, pkHash) |> Lock.Vote}

        { tx with outputs = Infrastructure.List.add output tx.outputs; version = Version1 }
        |> updateTx txLabel
        |> ignore

    let [<Given>] ``(.*) votes on allocation of (.*) with (.*) Zen in interval (.*) with version (.*)`` txLabel allocation amount interval version =
        let tx = initTx txLabel

        let voteData = { allocation = Some (byte allocation); payout = None }
        let pkHash = getFirstPKAddress tx
        let output = { spend = { amount = getAmount Asset.Zen amount; asset = Asset.Zen }; lock = (voteData, interval, pkHash) |> Lock.Vote}

        { tx with outputs = Infrastructure.List.add output tx.outputs; version = version }
        |> updateTx txLabel
        |> ignore

    let [<Given>] ``(.*) votes on payout of (.*) for (.*) with (.*) Zen in interval (.*)`` txLabel payout addressKeyLabel amount interval =
        let tx = initTx txLabel

        let voteData = { allocation = None; payout = Some (getRecipient addressKeyLabel, getAmount Asset.Zen payout) }
        let pkHash = getFirstPKAddress tx
        let output = { spend = { amount = getAmount Asset.Zen amount; asset = Asset.Zen }; lock = (voteData, interval, pkHash) |> Lock.Vote}

        { tx with outputs = Infrastructure.List.add output tx.outputs; version = Version1 }
        |> updateTx txLabel
        |> ignore

    // Adding contract activation outputs (ActivationSacrifice and Fee)
    let [<Given>] ``(.*) activates (.*) for (.*) block(?:|s)`` txLabel contractLabel blocks =
        let tx = initTx txLabel

        let _, contract = getContractRecord contractLabel
        let codeLength = String.length contract.code |> uint64
        let activationFee = contract.queries * rlimit /100ul |> uint64
        let activationSacrifice = testingState.chain.sacrificePerByteBlock * codeLength * (uint64 blocks)

        let outputs =
            [ { spend = { amount = activationSacrifice; asset = Asset.Zen }; lock = ActivationSacrifice }
              { spend = { amount = activationFee;       asset = Asset.Zen }; lock = Fee } ]

        { tx with outputs = tx.outputs @ outputs; contract = Some (V0 contract) }
        |> updateTx txLabel
        |> ignore

    // Adding contract extension output
    let [<Given>] ``(.*) extends (.*) for (.*) block(?:|s)`` txLabel contractLabel blocks =
        let tx = initTx txLabel

        let contractId, contract = getContractRecord contractLabel
        let codeLength = String.length contract.code |> uint64
        let extensionSacrifice = testingState.chain.sacrificePerByteBlock * codeLength * (uint64 blocks)

        let outputs =
            [ { spend = { amount = extensionSacrifice; asset = Asset.Zen }; lock = ExtensionSacrifice contractId } ]

        { tx with outputs = tx.outputs @ outputs }
        |> updateTx txLabel
        |> ignore

    let addInput txLabel refTxLabel index =
        let refTx = findTx refTxLabel
        let outpoint = Outpoint { txHash = Transaction.hash refTx; index = index }
        let tx = initTx txLabel
        { tx with inputs = Infrastructure.List.add outpoint tx.inputs }
        |> updateTx txLabel
        |> ignore

    // Adds an outpoint to a tx
    let [<Given>] ``(.*) has the input (.*) index (.*)`` txLabel refTxLabel index =
        addInput txLabel refTxLabel index

    // Adds outpoint(s) to a tx
    let [<Given>] ``(.*) has inputs`` txLabel (table: Table) =
        for row in table.Rows do
            let refTxLabel = row.GetString "Tx"
            let index = row.GetInt64 "Index" |> uint32
            addInput txLabel refTxLabel index

    // Constructs an array of transactinos
    let [<Given>] ``(.*) is array of (.*) from (.*)`` txLabel count refTxLabel =
        let mutable txs = []
        let refTx = findTx refTxLabel

        if List.length refTx.inputs > 1 ||
           List.length refTx.outputs > 1 ||
           List.length refTx.witnesses > 1 then
            failwithf "Transaction array base tx should have a single input and a single output with amount dividable by tx count"

        let spend = { refTx.outputs.[0].spend with amount = refTx.outputs.[0].spend.amount (* / count *) }

        let findKeyByLock lock =
            testingState.keys
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.find (fun keyPair ->
                match lock with
                | PK pkHash -> PublicKey.hash (snd keyPair) = pkHash
                | _ -> false)

        let mutable keyPair = findKeyByLock refTx.outputs.[0].lock
        let mutable input = { txHash = Transaction.hash refTx; index = 0ul }

        for _ in [ 1UL .. count ] do
            let newKeyPair = KeyPair.create()

            let output = { lock = snd newKeyPair |> PublicKey.hash |> PK ; spend = spend }

            let tx =
                { inputs = [ Outpoint input ]; witnesses = []; outputs = [ output ]; version = Version0; contract = None }
                |> Transaction.sign [ keyPair ] TxHash

            input <- { txHash = Transaction.hash tx; index = 0ul }
            keyPair <- newKeyPair

            txs <- List.append txs [ tx ]

        testingState <- { testingState with txLists = Map.add txLabel txs testingState.txLists }

    let constructData dataType value =
        match dataType with
        | Regex @"(.+) (.+)" [ dataType; collectionType ] ->
            getDataCollection collectionType dataType value
        | _ ->
            getData dataType value

    // Defines a data
    let [<Given>] ``(.*) is (?:a|an) (.*) of (.*)`` dataLabel dataType value =
        constructData dataType value
        |> initData dataLabel

    // Defines a data dictionaty
    let [<Given>] ``(.*) is a dictionary of`` dataLabel (table: Table) =
        let mutable map = Map.empty

        for row in table.Rows do
            let key = row.GetString "Key"
            let dataType = row.GetString "Type"
            let value = row.GetString "Value"
            let data =
                if dataType = "dict" then
                    match tryFindData key with
                    | Some data -> data
                    | None -> failwithf "missing dictionary data %A" key
                else
                    constructData dataType value
            map <- Map.add (fsToFstString key) data map

        (map, Map.count map |> uint32)
        |> Zen.Types.Data.Dict
        |> Zen.Types.Data.Collection
        |> initData dataLabel

    let sign txLabel keyLabels tx =
        let keyPairs =
            keyLabels
            |> split
            |> List.map findKey
        tx
        |> Transaction.sign keyPairs TxHash
        |> updateTx txLabel
        |> ignore

    // Signs a tx
    let [<Given>] ``(.*) is signed with (.*)`` txLabel keyLabels =
        findTx txLabel
        |> sign txLabel keyLabels

    // Signs a tx
    let [<When>] ``signing (.*) with (.*)`` txLabel keyLabels =
        findTx txLabel
        |> sign txLabel keyLabels

    // Initializes a chain with a genesis block
    let [<Given>] ``genesis has (.*)`` rootTxLabels =
        let rootTxs =
            rootTxLabels
            |> split
            |> List.map findTx
            |> List.map Transaction.toExtended

        let genesisBlock = Block.createGenesis testingState.chain rootTxs (0UL,0UL)
        let genesisHash = Block.hash genesisBlock.header

        testingState <- { testingState with blocks = Map.add "genesis" genesisBlock testingState.blocks }

        let chain = { testingState.chain with genesisHashHash = Hash.computeOfHash genesisHash }
        let state' = { state with tipState = { state.tipState with ema = EMA.create chain }}

        let events, state' =
            BlockHandler.validateBlock chain session.context.contractPath session (Timestamp.now() + 1000_000_000UL) None genesisBlock false state'
            |> Infrastructure.Writer.unwrap

        events |> should contain (EffectsWriter.EventEffect (BlockAdded (genesisHash, genesisBlock)))
        events |> should contain (EffectsWriter.EventEffect (TipChanged (genesisBlock.header)))

        state <- state'

    let extendChain' newBlockLabel txs parentBlockLabel (includeInChain : bool) =
        let createBlock state =

            let parent =
                if parentBlockLabel = "tip" then
                    state.tipState.tip.header
                else
                    match Map.tryFind parentBlockLabel testingState.blocks with
                    | Some block -> block.header
                    | None ->
                        failwithf "could not resolve parent block %A" parentBlockLabel

            let initialAcs =
                match Map.tryFind parent testingState.acs with
                | Some acs -> acs
                | None -> ActiveContractSet.empty
            
            let initialCgp =
                match Map.tryFind parent testingState.cgp with
                | Some cgp -> cgp
                | None -> CGP.empty

            let (builderState:BlockTemplateBuilder.BuilderState), txs' =
                List.mapi (fun i tx -> Transaction.toExtended tx, bigint i) txs // fake weights
                |> BlockTemplateBuilder.selectOrderedTransactions testingState.chain session parent.blockNumber (Timestamp.now()) initialAcs initialCgp state.memoryState.contractCache

            // if some transactions could not make it into the block, force them in anyway
            let forceAddedTxs =
                let isPassedValidation tx = List.contains tx txs'

                txs
                |> List.map Transaction.toExtended
                |> List.fold (fun forceAddedTxs tx ->
                    if isPassedValidation tx then
                        forceAddedTxs
                    else
                        tx :: forceAddedTxs
                ) []

            let rawblock =
                Block.createTemplate
                    testingState.chain
                    parent
                    (Timestamp.now() + (uint64 parent.blockNumber)*1_000_000UL)
                    state.tipState.ema
                    builderState.activeContractSet
                    builderState.cgp
                    (txs' @ forceAddedTxs)
                    Hash.zero

            let rec addPow bk ctr =
                if ctr > 20 then failwith "You're testing with a real blockchain difficulty. Try setting the proofOfWorkLimit to the lowest value."
                match Block.validateHeader testingState.chain bk.header with
                | Ok _ -> bk
                | _ -> addPow {bk with header = {bk.header with nonce=(fst bk.header.nonce, snd bk.header.nonce + 1UL)}} (ctr+1)

            let block = addPow rawblock 0

            // Save the ACS, so that when extending a chain using a parent - the right amount of sacrifice in the coinbase would be calculated
            testingState <- { testingState with acs = Map.add block.header builderState.activeContractSet testingState.acs }
            
            // Save the CGP, so that when extending a chain using a parent - the right coinbase reward would be calculated
            testingState <- { testingState with cgp = Map.add block.header builderState.cgp testingState.cgp }

            block

        let block = createBlock state

        match newBlockLabel with
        | Some label ->
            testingState <- { testingState with blocks = Map.add label block testingState.blocks }
        | _ -> ()

      //  events |> should contain (EffectsWriter.EventEffect (BlockAdded (Block.hash block.header, block)))

        if includeInChain
            then
                let _, state' =
                    BlockHandler.validateBlock testingState.chain session.context.contractPath session (Timestamp.now() + 100_000_000UL) None block false state
                    |> Infrastructure.Writer.unwrap
            
                state <- state'
            else
                match newBlockLabel with
                | None -> ()
                | Some label ->
                    testingState <- { testingState with missing = Map.add label block testingState.missing }
        block
    
    let [<When>] ``reconnecting block (.*)`` blockLabel =
         let block =
             match Map.tryFind blockLabel testingState.missing with
             | Some block -> block
             | None -> failwithf "The block {%A} isn't declared as missing" blockLabel
         
         testingState <- { testingState with missing = Map.remove blockLabel testingState.missing }
    
         let _, state' =
             BlockHandler.validateBlock testingState.chain session.context.contractPath session (Timestamp.now() + 100_000_000UL) None block false state
             |> Infrastructure.Writer.unwrap
             
         state <- state'
         
         block
        
    let prepareTxs txLabels =
        match tryFindTxList txLabels with
            | Some txList -> txList
            | None ->
                txLabels
                |> split
                |> List.map findTx 

    let extendChain newBlockLabel txLabels parentBlockLabel =
        extendChain' newBlockLabel (prepareTxs txLabels) parentBlockLabel true
    
    let createMissing newBlockLabel txLabels parentBlockLabel =
        extendChain' newBlockLabel (prepareTxs txLabels) parentBlockLabel false

    let generateChain parentBlockLabel newBlockLabel blockLabelPrefix (chainLength : uint32) =
        assert (chainLength > 0ul)

        let range = [1ul .. chainLength-1ul]
        let bkIds = parentBlockLabel :: ((List.map (fun n -> sprintf "%s%i" blockLabelPrefix n) range) |> List.add newBlockLabel)
        let bkPairs = List.zip (List.take (List.length bkIds - 1) bkIds) (List.tail bkIds)

        List.fold (fun () (id1, id2) -> extendChain' (Some id2) [] id1 |> ignore) () bkPairs

    let [<When>] ``Chain for (.*) blocks from (.*) to (.*) with prefix (.*)`` numBlocks parentBlockLabel newBlockLabel blockLabelPrefix =
            generateChain parentBlockLabel newBlockLabel blockLabelPrefix <| numBlocks

    let [<When>] ``Chain an interval from (.*) to (.*) with prefix (.*)`` parentBlockLabel newBlockLabel blockLabelPrefix =
        generateChain parentBlockLabel newBlockLabel blockLabelPrefix <| testingState.chain.intervalLength

    // Extend a chain
    let [<When>] ``validating a block containing (.*) extending (.*)`` txLabels parentBlockLabel =
        extendChain None txLabels parentBlockLabel

    // Extend a chain (using block label)
    let [<When>] ``validating block (.*) containing (.*) extending (.*)`` newBlockLabel txLabels parentBlockLabel =
        extendChain (Some newBlockLabel) txLabels parentBlockLabel

    // Extend a chain (using block label, empty block)
    let [<When>] ``validating an empty block (.*) extending (.*)`` newBlockLabel parentBlockLabel =
        extendChain (Some newBlockLabel) "" parentBlockLabel

    // Extend a chain (using no block label, empty block)
    let [<When>] ``validating an empty block extending (.*)`` parentBlockLabel =
        extendChain None "" parentBlockLabel
    
    // Create a missing block (all of its children will be orphans)
    let [<When>] ``validating a detached block containing (.*) extending (.*)`` txLabels parentBlockLabel =
        createMissing None txLabels parentBlockLabel
    
    // Create a missing block (using block label)
    let [<When>] ``validating a detached block (.*) containing (.*) extending (.*)`` newBlockLabel txLabels parentBlockLabel =
        createMissing (Some newBlockLabel) txLabels parentBlockLabel

    // Create a missing block (using block label, empty block)
    let [<When>] ``validating a detached empty block (.*) extending (.*)`` newBlockLabel parentBlockLabel =
        createMissing (Some newBlockLabel) "" parentBlockLabel

    // Create a missing block (using no block label, empty block)
    let [<When>] ``validating a detached empty block extending (.*)`` parentBlockLabel =
        createMissing None "" parentBlockLabel

    let validate txLabel =
        let tx = findTx txLabel

        let acs =
            Map.fold (fun acs contractId contract -> ActiveContractSet.add contractId contract acs) ActiveContractSet.empty contractExecutionCache

        TransactionValidation.validateBasic tx
        <@> Transaction.toExtended
        >>= (TransactionValidation.validateInContext
            testingState.chain
            (getUTXO session)
            session.context.contractPath
            1ul
            1_000_000UL
            acs
            Map.empty
            state.memoryState.utxoSet
            (getContractState session)
            ContractStates.asDatabase)

    // Checks that tx passes validation
    let [<Then>] ``(.*) should pass validation`` txLabel =
        match validate txLabel with
        | Ok _ -> ()
        | Error e -> failwithf "Validation result: %A" e

    // Checks that tx doesn't passes validation
    let [<Then>] ``(.*) validation should yield (.*)`` txLabel (message:string) =
        match validate txLabel with
        | Ok _ -> failwithf "Unexpected Ok validation result"
        | Error (ValidationError.General error) -> message ?= error
        | Error error  -> message ?= (error.ToString())

    //Checks that a tx is locking an asset to an address or a contract
    let [<Then>] ``(.*) should lock (.*) (.*) to (.*)`` txLabel (expected:uint64) asset keyLabel =
        let tx = findTx txLabel
        let asset = getAsset asset

        let stepDown asset amount =
            if asset = Asset.Zen then
                amount / 100_000_000UL
            else
                amount

        expected ?=
            List.fold (fun state { lock = lock; spend = spend } ->
                if lock = getLock keyLabel && spend.asset = asset then
                    state + (stepDown spend.asset spend.amount)
                else
                    state) 0UL tx.outputs

    let checkContractState contractLabel expected =
        match tryFindContract contractLabel with
        | Some (contractId, _) ->
            let actual = ContractStates.tryGetState (getContractState session) contractId ContractStates.asDatabase
            expected ?= actual
        | None -> failwith "contract not resolved"

    // Checks a contract's state
    let [<Then>] ``state of (.*) should be (.*) of (.*)`` contractLabel dataType value =
        constructData dataType value
        |> Some
        |> checkContractState contractLabel

    // Checks that a contract's state is none
    let [<Then>] ``state of (.*) should be none`` contractLabel =
        checkContractState contractLabel None
    
    let private getTally (cgp:CGP.T) =
        let interval = CGP.getInterval testingState.chain state.tipState.tip.header.blockNumber

        cgp.tallies
        |> Map.tryFind interval 
        |> Option.defaultValue Tally.empty
        
    // Checks the total allocation votes in the tally
    let [<Then>] ``tally should have a total of (.*) allocation vote(?:|s)`` votes =
        getAmount Asset.Zen votes ?= Map.fold (fun total _ votes -> total + votes) 0UL (getTally state.tipState.cgp).allocation

    // Checks the total payout votes in the tally
    let [<Then>] ``tally should have a total of (.*) payout vote(?:|s)`` votes =
        getAmount Asset.Zen votes ?= Map.fold (fun total _ votes -> total + votes) 0UL (getTally state.tipState.cgp).payout

    // Checks the total votes for an allocation in the tally
    let [<Then>] ``tally should have a total of (.*) allocation vote(?:|s) for allocation of (.*)`` votes allocation =
        getAmount Asset.Zen votes ?= Map.fold (fun total allocation' votes -> total + (if allocation' = allocation then votes else 0UL)) 0UL (getTally state.tipState.cgp).allocation

    // Checks the total votes for a payout in the tally
    let [<Then>] ``tally should have a total of (.*) payout vote(?:|s) for payout of (.*) Zen to (.*)`` votes amount addressKeyLabel =
        getAmount Asset.Zen votes ?= Map.fold (fun total payout votes -> total + (if payout = (getRecipient addressKeyLabel, getAmount Asset.Zen amount) then votes else 0UL)) 0UL (getTally state.tipState.cgp).payout

    // Checks the chosen allocation in the tally
    let [<Then>] ``tally allocation result should be (.*)`` allocation =
        (if allocation = "none"
            then None
            else Some (byte allocation)  
        ) ?= Tally.getResult (getTally state.tipState.cgp).allocation

    // Checks the chosen payout in the tally
    let [<Then>] ``tally payout result should be of (.*) Zen to (.*)`` amount addressKeyLabel =
        Some (getRecipient addressKeyLabel, getAmount Asset.Zen amount) ?= Tally.getResult (getTally state.tipState.cgp).payout
    
    let [<Then>] ``tally payout result should be none(.*)`` (_ : string) =
        None ?= Tally.getResult (getTally state.tipState.cgp).payout

    // Checks the CGP's allocation
    let [<Then>] ``CGP allocation should be (.*)`` allocation =
       byte allocation ?= state.tipState.cgp.allocation

    // Checks the CGP's payout
    let [<Then>] ``CGP payout should be of (.*) Zen to (.*)`` amount addressKeyLabel =
        Some (getRecipient addressKeyLabel, getAmount Asset.Zen amount) ?= state.tipState.cgp.payout
    
    let [<Then>] ``CGP amount should be (.*) Zen`` amount =
        getAmount Asset.Zen amount ?= state.tipState.cgp.amount
        
    let [<Then>] ``CGP payout should be none(.*)`` (_ : string) =
        None ?= state.tipState.cgp.payout
        
    // Checks the tip
    let [<Then>] ``tip should be (.*)`` (blockLabel : string) =
        match findBlockLabel state.tipState.tip.header with
        | Some actualBlockLabel -> blockLabel ?= actualBlockLabel
        | _ -> failwithf "cannot resolve block: %A" blockLabel

    // Asserts that a contract is active
    let [<Then>] ``(.*) should be active for (.*) block(?:|s)`` contractLabel (blocks:uint32) =
        let contractId, _  = findContract contractLabel
        match ActiveContractSet.tryFind contractId state.tipState.activeContractSet with
        | Some contract ->
            blocks ?= contract.expiry - state.tipState.tip.header.blockNumber
        | None -> failwithf "expected contract to be activated"

    // Asserts that a contract is not active
    let [<Then>] ``(.*) should not be active`` contractLabel =
        let contractId, _  = findContract contractLabel
        match ActiveContractSet.tryFind contractId state.tipState.activeContractSet with
        | Some contract ->
            failwithf "expected contract to not be activated but is active for %A " (contract.expiry - state.tipState.tip.header.blockNumber)
        | None -> ()

    // Prints the number of blocks a contract active for
    let [<Then>] ``(.*) should be active for\?`` contractLabel =
        let contractId, _  = findContract contractLabel
        match ActiveContractSet.tryFind contractId state.tipState.activeContractSet with
        | Some contract ->
            printfn "%A is active for %A blocks" contractLabel (contract.expiry - state.tipState.tip.header.blockNumber)
        | None -> ()

    let malleateTx destTxLabel sourceTxLabel malleateTxFn =
        sourceTxLabel
        |> findTx
        |> malleateTxFn
        |> updateTx destTxLabel
        |> ignore

    let malleateListItem (list:List<'a>) malleateFn index =
        list.[ index + 1 .. Seq.length list - 1 ]
        |> (@) [ malleateFn list.[index] ]
        |> (@) list.[ 0..index - 1 ]

    let malleateList (list:List<'a>) (sourceIndex:int) (destIndex:int) =
        let prefix = list.[ 0 .. destIndex - 1 ]
        let suffix = list.[ destIndex .. Seq.length list - 1 ]
        let item = list.[sourceIndex]
        List.concat [ prefix; [ item ]; suffix ]

    let malleateTxOutput malleateFn index (tx:Transaction) =
        { tx with outputs = malleateListItem tx.outputs malleateFn index }

    let malleateTxWitnessItem malleateFn index (tx:Transaction) =
        { tx with witnesses = malleateListItem tx.witnesses malleateFn index }

    // re-signs a tx
    let [<When>] ``(.*) results by re-signing (.*) with (.*)`` destTxLabel sourceTxLabel keyLabels =
        { findTx sourceTxLabel with witnesses = [] }
        |> sign destTxLabel keyLabels

    let [<When>] ``(.*) results by changing asset of (.*) output (.*) to (.*)`` destTxLabel sourceTxLabel index value =
        let fn = fun output -> { output with spend = { output.spend with asset = getAsset value } }
        malleateTx destTxLabel sourceTxLabel (malleateTxOutput fn index)

    let [<When>] ``(.*) results by changing amount of (.*) output (.*) to (.*)`` destTxLabel sourceTxLabel index value =
        let fn = fun output -> { output with spend = { output.spend with amount = getAmount output.spend.asset value } }
        malleateTx destTxLabel sourceTxLabel (malleateTxOutput fn index)

    let [<When>] ``(.*) results by pushing (.*) (.*) of (.*) at index (.*)`` destTxLabel list sourceIndex sourceTxLabel destIndex =
        match list with
        | "witness" -> malleateTx destTxLabel sourceTxLabel (fun tx -> { tx with witnesses = malleateList tx.witnesses sourceIndex destIndex })
        | "output" -> malleateTx destTxLabel sourceTxLabel (fun tx -> { tx with outputs = malleateList tx.outputs sourceIndex destIndex })
        | "input" -> malleateTx destTxLabel sourceTxLabel (fun tx -> { tx with inputs = malleateList tx.inputs sourceIndex destIndex })
        | other -> failwithf "unexpected list %A" other

    let [<When>] ``(.*) results by changing (.*) of (.*) contract witness (.*) to (.*)`` destTxLabel field sourceTxLabel index value =
        let fn = fun (w:ContractWitness) ->
            match field with
            | "contractId"    -> { w with contractId = getContractId value |> fst }
            | "command"       -> { w with command = value }
            | "beginInputs"   -> { w with beginInputs = uint32 value }
            | "beginOutputs"  -> { w with beginOutputs = uint32 value }
            | "inputsLength"  -> { w with inputsLength = uint32 value }
            | "outputsLength" -> { w with outputsLength = uint32 value }
            | "cost"          -> { w with cost = uint64 value }
            // TODO:
            // messageBody: data option
            // stateCommitment: StateCommitment
            // signature: (PublicKey * Signature) option
            | other           -> failwithf "unexpected field %A" other

        let fn =
            function
            | ContractWitness w -> fn w |> ContractWitness
            | _ -> failwithf "unexpected witness type for index %A" index
        malleateTx destTxLabel sourceTxLabel (malleateTxWitnessItem fn index)

    let findBlockOutputs =
        findBlock
        >> fun bk -> bk.transactions
        >> List.head
        >> fun ex -> ex.tx.outputs

    // check miner reward
    let [<Then>] ``coinbase reward in block (.*) should be (.*) Zen`` blockLabel expectedAmount =
        blockLabel
        |> findBlockOutputs
        |> List.choose (function
            | { lock = Coinbase _; spend = spend } -> Some spend.amount
            | _ -> None)
        |> List.sum
        |> (?=) (uint64 <| getAmount Asset.Zen expectedAmount)
        
    let findPayout (outputs : Output list) : (Recipient * uint64) option =
        let extractRecipient output =
            match output.lock with
            | PK hash ->
                Error (PKRecipient hash, output.spend.amount)
            | Contract contractId ->
                Error (ContractRecipient contractId, output.spend.amount)
            | _ ->
                Ok ()
        let ofError res =
            match res with
            | Ok _      -> None
            | Error err -> Some err  
        Result.traverseResultM extractRecipient outputs |> ofError

    let [<Then>] ``there should be a payout on block (.*)`` blockLabel =
        blockLabel
        |> findBlockOutputs
        |> findPayout
        |> function
           | Some _ -> ()
           | None   -> failwithf "There is no payout in block %A" blockLabel
           
    let [<Then>] ``there shouldn't be a payout on block (.*)`` blockLabel =
        blockLabel
        |> findBlockOutputs
        |> findPayout
        |> function
           | Some _ -> failwithf "There is a payout in block %A" blockLabel
           | None   -> ()

    // check payout
    let [<Then>] ``coinbase payout in block (.*) should be (.*) Zen to (.*)`` blockLabel (expectedAmount : uint64) addressKeyLabel =
        blockLabel
        |> findBlockOutputs
        |> findPayout
        |> function
           | Some (recip, amount) ->
                recip ?= getRecipient addressKeyLabel
                amount ?= (getAmount Asset.Zen expectedAmount)
           | None -> failwithf "no payout on block %s" blockLabel
           
    let [<Then>] ``TODO(.*)`` (msg : string) : unit =
        match msg with
        | "" -> failwith "Not implemented"
        | _  -> failwithf "TODO: %s" msg
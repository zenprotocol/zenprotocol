module Consensus.Tests.StepDefinitionsModules

open Infrastructure
open TechTalk.SpecFlow
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


[<Literal>]
let rlimit = 2723280u

let (?=) expected actual =
    actual |> should equal expected

let tempDir () =
    Path.Combine
        [| Path.GetTempPath(); Path.GetRandomFileName() |]

let dataPath = tempDir()
let mutable databaseContext = DatabaseContext.createEmpty dataPath
let mutable session = DatabaseContext.createSession databaseContext

let createDatabaseContext() =
    let contractsPath = combine dataPath "contracts"
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
    cleanDirectory temp

let clean() =
    cleanDirectory dataPath

let getEmptyState() =
    let ema : EMA.T = {
        difficulty = 0ul
        delayed = []
    }

    let tipState : TipState = {
        tip = ExtendedBlockHeader.empty
        activeContractSet = ActiveContractSet.empty
        ema = ema
    }

    let memoryState : MemoryState = {
        utxoSet           = UtxoSet.asDatabase
        activeContractSet = ActiveContractSet.empty
        orphanPool        = OrphanPool.create()
        mempool           = MemPool.empty
        contractCache     = ContractCache.empty
        contractStates    = ContractStates.asDatabase
        invalidTxHashes   = Set.empty
    }

    {
        tipState             = tipState
        memoryState          = memoryState
        initialBlockDownload = InitialBlockDownload.Inactive
        headers              = 0ul
        cgp = CGP.empty
    }


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

let mutable state : Blockchain.State.State = getEmptyState()
let mutable contractExecutionCache : Map<ContractId, Contract.T> = Map.empty

type TestingState = {
        chain     : Chain.ChainParameters
        blocks    : Map<string, Block>
        acs       : Map<BlockHeader, ActiveContractSet.T>
        cgp       : Map<BlockHeader, CGP.T>
        missing   : Map<string, Block>
        contracts : Map<string, ContractId * ContractV0>
        keys      : Map<string, KeyPair>
        txs       : Map<string, Transaction>
        txLists   : Map<string, Transaction list>
        data      : Map<string, Zen.Types.Data.data>
        assets    : Map<string, Asset>
    }

let defaultChain = { Chain.getChainParameters Chain.Local with proofOfWorkLimit = Difficulty.uncompress 553648127u; genesisTime = (Timestamp.now()) - 1_000_000UL }
let mutable testingState = {
    chain     = defaultChain
    blocks    = Map.empty
    acs       = Map.empty
    cgp       = Map.empty
    missing   = Map.empty
    contracts = Map.empty
    keys      = Map.empty
    txs       = Map.empty
    txLists   = Map.empty
    data      = Map.empty
    assets    = Map.empty
}

    module Keys =
        let tryFindKey keyLabel = Map.tryFind keyLabel testingState.keys

        let findKey keyLabel =
            match tryFindKey keyLabel with
            | Some tx -> tx
            | None -> failwithf "Referenced key missing: %A" keyLabel


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
        
        let findKeyByLock lock =
            testingState.keys
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.find (fun keyPair ->
                match lock with
                | PK pkHash -> PublicKey.hash (snd keyPair) = pkHash
                | _ -> false)

    
    module Contract =

        module MessageBody =
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


            let constructData dataType value =
                match dataType with
                | Regex @"(.+) (.+)" [ dataType; collectionType ] ->
                    getDataCollection collectionType dataType value
                | _ ->
                    getData dataType value

        let tryFindContract contractLabel = Map.tryFind contractLabel testingState.contracts

        let checkContractState contractLabel expected =
            match tryFindContract contractLabel with
            | Some (contractId, _) ->
                let actual = ContractStates.tryGetState (getContractState session) contractId ContractStates.asDatabase
                expected ?= actual
            | None -> failwith "contract not resolved"

        let findContract contractLabel =
            match tryFindContract contractLabel with
            | Some contract -> contract
            | _ -> failwithf "cannot resolve contract: %A" contractLabel

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
                    match totalQueries hints with
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

        let getSender sender =
            if sender = "anonymous" then
                None
            else
                Keys.initKey sender
                |> snd
                |> Some


    module Transaction =

        let updateTx txLabel tx =
            testingState <- { testingState with txs = Map.add txLabel tx testingState.txs }
            tx

        let tryFindTx txLabel =
            Map.tryFind txLabel testingState.txs

        let tryFindTxByHash txHash =
            testingState.txs
            |> Map.tryFindKey (fun _ tx -> Transaction.hash tx = txHash)
            |> Option.bind tryFindTx

        let tryFindTxList txLabel = Map.tryFind txLabel testingState.txLists

        let findTx txLabel =
            match tryFindTx txLabel with
            | Some tx -> tx
            | None -> failwithf "Referenced tx missing: %A" txLabel

        let initTx txLabel =
            match tryFindTx txLabel with
            | Some tx ->
                tx
            | None ->
                { inputs = []; witnesses = []; outputs = []; version = Version0; contract = None }
                |> updateTx txLabel

        let malleateListItem (list:List<'a>) malleateFn index =
            list.[ index + 1 .. Seq.length list - 1 ]
            |> (@) [ malleateFn list.[index] ]
            |> (@) list.[ 0..index - 1 ]

        let malleateList (list:List<'a>) (sourceIndex:int) (destIndex:int) =
            let prefix = list.[ 0 .. destIndex - 1 ]
            let suffix = list.[ destIndex .. Seq.length list - 1 ]
            let item = list.[sourceIndex]
            List.concat [ prefix; [ item ]; suffix ]

        let malleateTx destTxLabel sourceTxLabel malleateTxFn =
            sourceTxLabel
            |> findTx
            |> malleateTxFn
            |> updateTx destTxLabel
            |> ignore

        let malleateTxOutput malleateFn index (tx:Transaction) =
           { tx with outputs = malleateListItem tx.outputs malleateFn index }

        let malleateTxWitnessItem malleateFn index (tx:Transaction) =
            { tx with witnesses = malleateListItem tx.witnesses malleateFn index }


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

        let sign txLabel keyLabels tx =
            let keyPairs =
                keyLabels
                |> split
                |> List.map Keys.findKey
            tx
            |> Transaction.sign keyPairs TxHash
            |> updateTx txLabel
            |> ignore

        let prepareTxs txLabels =
            match tryFindTxList txLabels with
                | Some txList -> txList
                | None ->
                    txLabels
                    |> split
                    |> List.map findTx


        let addInput txLabel refTxLabel index =
            let refTx = findTx refTxLabel
            let outpoint = Outpoint { txHash = Transaction.hash refTx; index = index }
            let tx = initTx txLabel
            { tx with inputs = Infrastructure.List.add outpoint tx.inputs }
            |> updateTx txLabel
            |> ignore
            
        
        let executeContract contractLabel inputTxLabel outputTxLabel command sender messageBody =
            let contract = Contract.getContractFunction contractLabel
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

        let activateContract contractLabel txLabel blocks =
            let tx = initTx txLabel

            let _, contract = Contract.getContractRecord contractLabel
            let codeLength = String.length contract.code |> uint64
            let activationFee = contract.queries * rlimit /100ul |> uint64
            let activationSacrifice = testingState.chain.sacrificePerByteBlock * codeLength * (uint64 blocks)

            let outputs =
                [ { spend = { amount = activationSacrifice; asset = Asset.Zen }; lock = ActivationSacrifice }
                  { spend = { amount = activationFee;       asset = Asset.Zen }; lock = Fee } ]

            { tx with outputs = tx.outputs @ outputs; contract = Some (V0 contract) }
            |> updateTx txLabel
        
        let extendContract contractLabel txLabel blocks =
            let tx = initTx txLabel

            let contractId, contract = Contract.getContractRecord contractLabel
            let codeLength = String.length contract.code |> uint64
            let extensionSacrifice = testingState.chain.sacrificePerByteBlock * codeLength * (uint64 blocks)

            let outputs =
                [ { spend = { amount = extensionSacrifice; asset = Asset.Zen }; lock = ExtensionSacrifice contractId } ]

            { tx with outputs = tx.outputs @ outputs }
            |> updateTx txLabel
    
    module Asset =

        let updateAsset assetLabel asset =
            testingState <- { testingState with assets = Map.add assetLabel asset testingState.assets }

        let tryFindAsset assetLabel =
            Map.tryFind assetLabel testingState.assets

        let findAsset assetLabel =
            match Map.tryFind assetLabel testingState.assets with
            | Some asset ->
                asset
            | None ->
                failwithf "Couldn't find asset: %s" assetLabel

        let initAsset assetLabel =
            let contractId =
                (assetLabel:string)
                |> System.Text.Encoding.ASCII.GetBytes
                |> Hash.compute
                |> fun hash -> ContractId (1u, hash)

            Asset.defaultOf contractId

        let mockAsset label =
            if label = "Zen" || label = "zen" || label = "ZEN"
                then Asset.Zen
                else
                    match tryFindAsset label with
                    | Some asset ->
                        asset
                    | None ->
                        initAsset label

        let getAsset value =
            if value = "Zen" then Asset.Zen
            else
                let contractId, _ = Contract.getContractRecord value
                Asset (contractId, Hash.zero)

    module Amount =
        let getAmount asset amount =
            if asset = Asset.Zen then
                amount * 100_000_000UL
            else
                amount
        let getChangeAmount txLabel asset tx =
            let foldOutputs =
                List.fold (fun amount (output:Output) ->
                    amount +
                    if output.spend.asset = Asset.getAsset asset then
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

    module Block =
        
        let createGenesis rootTxLabels =
            let rootTxs =
                rootTxLabels
                |> split
                |> List.map Transaction.findTx
                |> List.map Transaction.toExtended

            Block.createGenesis testingState.chain rootTxs (0UL,0UL)
        
        let createBlock parentBlockLabel txs state =
                // Set the parent of the block
                let parent =
                    if parentBlockLabel = "tip" then
                        state.tipState.tip.header
                    else
                        match Map.tryFind parentBlockLabel testingState.blocks with
                        | Some block -> block.header
                        | None ->
                            failwithf "could not resolve parent block %A" parentBlockLabel
                //Get Active Contract Set
                let initialAcs =
                    match Map.tryFind parent testingState.acs with
                    | Some acs -> acs
                    | None -> ActiveContractSet.empty
                // Set Transaction in the                 
                let builderState, txs' =
                    txs
                    |> List.mapi (fun i tx -> Transaction.toExtended tx, bigint i) // fake weights
                    |> BlockTemplateBuilder.selectOrderedTransactions testingState.chain session parent.blockNumber (Timestamp.now()) initialAcs state.memoryState.contractCache
                
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
                        CGP.empty
                        (txs' @ forceAddedTxs)
                        Hash.zero
                
                let rec addPow bk ctr =
                    if ctr > 20 then failwith "You're testing with a real blockchain difficulty. Try setting the proofOfWorkLimit to the lowest value."
                    match BlockValidation.Header.validate testingState.chain bk.header with
                    | Ok _ -> bk
                    | _ -> addPow {bk with header = {bk.header with nonce=(fst bk.header.nonce, snd bk.header.nonce + 1UL)}} (ctr+1)

                let block = addPow rawblock 0

                // Save the ACS, so that when extending a chain using a parent - the right amount of sacrifice in the coinbase would be calculated
                testingState <- { testingState with acs = Map.add block.header builderState.activeContractSet testingState.acs }

                // Save the CGP, so that when extending a chain using a parent - the right coinbase reward would be calculated
                testingState <- { testingState with cgp = Map.add block.header CGP.empty testingState.cgp }

                block

        let findBlock blockLabel = Map.find blockLabel testingState.blocks

        let findBlockLabel block =
            testingState.blocks
            |> Map.map (fun _ bk -> bk.header)
            |> Map.tryFindKey (fun _ block' -> block = block')

    module Lock =
        let getLock label =
            match Contract.tryFindContract label with
            | Some (contractId, _) ->
                contractId
                |> Lock.Contract
            | None ->
                Keys.initKey label
                |> snd
                |> PublicKey.hash
                |> Lock.PK
                
    module UtxoSet =
        
        /// fold on mentioned txs, fold on their outputs
        let fromTxs txs =
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

    module Spend =
        let getSpend amount asset =
            [{ amount = amount; asset = asset }]

    module Output =
        let getOutput amount asset keyLabel = {
            spend = List.head (Spend.getSpend amount (Asset.getAsset asset))
            lock = Lock.getLock keyLabel
        }

    module Chain =
        let extendChain' newBlockLabel txs parentBlockLabel (includeInChain : bool) =

            let block = Block.createBlock parentBlockLabel txs state

            match newBlockLabel with
            | Some label ->

                testingState <- { testingState with blocks = Map.add label block testingState.blocks }
            | _ -> ()

            if includeInChain then
                    let env : Environment.Env =
                        {
                            chainParams   = testingState.chain
                            contractsPath = session.context.contractPath
                            timestamp     = Timestamp.now() + 100_000_000UL
                            session       = session
                        }

                    let _, state' =
                        BlockHandler.validateBlock env None block state
                        |> Writer.unwrap

                    state <- state'
                else
                    match newBlockLabel with
                    | None -> ()
                    | Some label ->
                        testingState <- { testingState with missing = Map.add label block testingState.missing }
            block

        let extendChain newBlockLabel txLabels parentBlockLabel =
            extendChain' newBlockLabel (Transaction.prepareTxs txLabels) parentBlockLabel true

        let createMissing newBlockLabel txLabels parentBlockLabel =
            extendChain' newBlockLabel (Transaction.prepareTxs txLabels) parentBlockLabel false

        let generateChain parentBlockLabel newBlockLabel blockLabelPrefix (chainLength : uint32) =
            assert (chainLength > 0ul)

            let range = [1ul .. chainLength-1ul]
            let bkIds = parentBlockLabel :: ((List.map (fun n -> sprintf "%s%i" blockLabelPrefix n) range) |> List.add newBlockLabel)
            let bkPairs = List.zip (List.take (List.length bkIds - 1) bkIds) (List.tail bkIds)

            List.fold (fun () (id1, id2) -> extendChain' (Some id2) [] id1 |> ignore) () bkPairs

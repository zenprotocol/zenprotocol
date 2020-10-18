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

[<Binding>]
module Binding =
    open Consensus.Chain
    open Consensus.Serialization
    open StepDefinitionsModules

    let [<BeforeTestRun>] beforeTestRun () =
        clean()
    let [<AfterTestRun>] afterTestRun () =
        clean()

    let [<BeforeScenario>] setupScenario () =
        createDatabaseContext()
        testingState <- {
            chain     = defaultChain
            blocks    = Map.empty
            acs       = Map.empty
            cgp       = Map.empty
            missing   = Map.empty
            contracts = testingState.contracts
            keys      = Map.empty
            txs       = Map.empty
            txLists   = Map.empty
            data      = Map.empty
            assets    = Map.empty
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

            let tx = Transaction.initTx txLabel
            let output = Output.getOutput (Amount.getAmount (Asset.getAsset asset) amount) asset keyLabel
            let tx = { tx with outputs = Infrastructure.List.add output tx.outputs }
                     |> Transaction.updateTx txLabel

            txs <- Map.add txLabel tx txs

        let utxoSet =
            UtxoSet.fromTxs txs

        state <- { state with memoryState = { state.memoryState with utxoSet = utxoSet } }

    // Executes a contract
    let [<When>] ``executing (.*) on (.*) returning (.*)`` contractLabel inputTxLabel outputTxLabel =
        Transaction.executeContract contractLabel inputTxLabel outputTxLabel "" None None

    // Executes a contract with additional arguments
    let [<When>] ``executing (.*) on (.*) returning (.*) with`` contractLabel inputTxLabel outputTxLabel (table: Table) =
        let mutable command = String.Empty
        let mutable messageBody = None
        let mutable sender = None

        for row in table.Rows do
            let value = row.Item(1)
            match row.Item(0) with
            | "Command" -> command <- value
            | "Sender" -> sender <- Contract.getSender value
            | "MessageBody" ->
                messageBody <-
                    match Contract.MessageBody.tryFindData value with
                    | Some data -> Some data
                    | _ -> failwithf "cannot resolve data: %A" value
            | "ReturnAddress" ->
                let _, publicKey = Keys.initKey value
                match TestWallet.addReturnAddressToData publicKey messageBody with
                | Ok messageBody' -> messageBody <- messageBody'
                | Error error -> failwithf "error initializing data with return address: %A" error
            | other -> failwithf "unexpected execute option %A" other

        Transaction.executeContract contractLabel inputTxLabel outputTxLabel command sender messageBody

    // Adding a single output
    // or adding a change output for an asset
    let [<Given>] ``(.*) locks (.*) (.*) to (.*)`` txLabel amount asset keyLabel =
        let tx = Transaction.initTx txLabel

        let amount =
            if amount = "change" then
                Amount.getChangeAmount txLabel asset tx
            else
                Amount.getAmount (Asset.getAsset asset) (UInt64.Parse amount)

        let output = Output.getOutput amount asset keyLabel
        { tx with outputs = Infrastructure.List.add output tx.outputs }
        |> Transaction.updateTx txLabel
        |> ignore

    // Adding contract activation outputs (ActivationSacrifice and Fee)
    let [<Given>] ``(.*) activates (.*) for (.*) block(?:|s)`` txLabel contractLabel blocks =
        Transaction.activateContract contractLabel txLabel blocks 
        |> ignore

    // Adding contract extension output
    let [<Given>] ``(.*) extends (.*) for (.*) block(?:|s)`` txLabel contractLabel blocks =
        Transaction.extendContract contractLabel txLabel blocks
        |> ignore

    // Adds an outpoint to a tx
    let [<Given>] ``(.*) has the input (.*) index (.*)`` txLabel refTxLabel index =
        Transaction.addInput txLabel refTxLabel index

    // Adds outpoint(s) to a tx
    let [<Given>] ``(.*) has inputs`` txLabel (table: Table) =
        for row in table.Rows do
            let refTxLabel = row.GetString "Tx"
            let index = row.GetInt64 "Index" |> uint32
            Transaction.addInput txLabel refTxLabel index

    // Constructs an array of transactinos
    let [<Given>] ``(.*) is array of (.*) from (.*)`` txLabel count refTxLabel =
        let mutable txs = []
        let refTx = Transaction.findTx refTxLabel

        if List.length refTx.inputs > 1 ||
           List.length refTx.outputs > 1 ||
           List.length refTx.witnesses > 1 then
            failwithf "Transaction array base tx should have a single input and a single output with amount dividable by tx count"

        let spend = { refTx.outputs.[0].spend with amount = refTx.outputs.[0].spend.amount (* / count *) }

        let mutable keyPair = Keys.findKeyByLock refTx.outputs.[0].lock
        let mutable input = { txHash = Transaction.hash refTx; index = 0ul }

        for _ in [ 1UL .. count ] do
            let newKeyPair = KeyPair.create()

            let output = { lock = snd newKeyPair |> PublicKey.hash |> PK ; spend = spend }

            let tx =
                { inputs = [ Outpoint input ]; witnesses = []; outputs = [ output ]; version = Version0; contract = None }
                |> Consensus.Transaction.sign [ keyPair ] TxHash

            input <- { txHash = Transaction.hash tx; index = 0ul }
            keyPair <- newKeyPair

            txs <- List.append txs [ tx ]

        testingState <- { testingState with txLists = Map.add txLabel txs testingState.txLists }

    // Defines a data
    let [<Given>] ``(.*) is (?:a|an) (.*) of (.*)`` dataLabel dataType value =
        Contract.MessageBody.constructData dataType value
        |> Contract.MessageBody.initData dataLabel

    // Defines a data dictionaty
    let [<Given>] ``(.*) is a dictionary of`` dataLabel (table: Table) =
        let mutable map = Map.empty

        for row in table.Rows do
            let key = row.GetString "Key"
            let dataType = row.GetString "Type"
            let value = row.GetString "Value"
            let data =
                if dataType = "dict" then
                    match Contract.MessageBody.tryFindData key with
                    | Some data -> data
                    | None -> failwithf "missing dictionary data %A" key
                else
                    Contract.MessageBody.constructData dataType value
            map <- Map.add (fsToFstString key) data map

        (map, Map.count map |> uint32)
        |> Zen.Types.Data.Dict
        |> Zen.Types.Data.Collection
        |> Contract.MessageBody.initData dataLabel

    // Signs a tx
    let [<Given>] ``(.*) is signed with (.*)`` txLabel keyLabels =
        Transaction.findTx txLabel
        |> Transaction.sign txLabel keyLabels

    // Signs a tx
    let [<When>] ``signing (.*) with (.*)`` txLabel keyLabels =
        Transaction.findTx txLabel
        |> Transaction.sign txLabel keyLabels

    // Initializes a chain with a genesis block
    let [<Given>] ``genesis has (.*)`` rootTxLabels =
        let genesisBlock = Block.createGenesis rootTxLabels
        let genesisHash = Block.hash genesisBlock.header

        testingState <- { testingState with blocks = Map.add "genesis" genesisBlock testingState.blocks }

        let chain = { testingState.chain with genesisHashHash = Hash.computeOfHash genesisHash }
        let state' = { state with tipState = { state.tipState with ema = EMA.create chain }}

        let env : Environment.Env =
            {
                chainParams   = chain
                contractsPath = session.context.contractPath
                timestamp     = Timestamp.now() + 1000_000_000UL
                session       = session
            }

        let events, state' =
            BlockHandler.validateBlock env None genesisBlock state'
            |> Infrastructure.Writer.unwrap

        events |> should contain (EffectsWriter.EventEffect (BlockAdded (genesisHash, genesisBlock)))
        events |> should contain (EffectsWriter.EventEffect (TipChanged (genesisBlock.header)))

        state <- state'

    let [<When>] ``reconnecting block (.*)`` blockLabel =
         let block =
             match Map.tryFind blockLabel testingState.missing with
             | Some block -> block
             | None -> failwithf "The block {%A} isn't declared as missing" blockLabel

         testingState <- { testingState with missing = Map.remove blockLabel testingState.missing }

         let env : Environment.Env =
            {
                chainParams   = testingState.chain
                contractsPath = session.context.contractPath
                timestamp     = Timestamp.now() + 100_000_000UL
                session       = session
            }

         let _, state' =
             BlockHandler.validateBlock env None block state
             |> Infrastructure.Writer.unwrap

         state <- state'

         block


    let [<When>] ``Chain for (.*) blocks from (.*) to (.*) with prefix (.*)`` numBlocks parentBlockLabel newBlockLabel blockLabelPrefix =
        Chain.generateChain parentBlockLabel newBlockLabel blockLabelPrefix <| numBlocks

    let [<When>] ``Chain an interval from (.*) to (.*) with prefix (.*)`` parentBlockLabel newBlockLabel blockLabelPrefix =
        Chain.generateChain parentBlockLabel newBlockLabel blockLabelPrefix <| testingState.chain.intervalLength

    // Extend a chain
    let [<When>] ``validating a block containing (.*) extending (.*)`` txLabels parentBlockLabel =
        Chain.extendChain None txLabels parentBlockLabel

    // Extend a chain (using block label)
    let [<When>] ``validating block (.*) containing (.*) extending (.*)`` newBlockLabel txLabels parentBlockLabel =
        Chain.extendChain (Some newBlockLabel) txLabels parentBlockLabel

    // Extend a chain (using block label, empty block)
    let [<When>] ``validating an empty block (.*) extending (.*)`` newBlockLabel parentBlockLabel =
        Chain.extendChain (Some newBlockLabel) "" parentBlockLabel

    // Extend a chain (using no block label, empty block)
    let [<When>] ``validating an empty block extending (.*)`` parentBlockLabel =
        Chain.extendChain None "" parentBlockLabel

    // Create a missing block (all of its children will be orphans)
    let [<When>] ``validating a detached block containing (.*) extending (.*)`` txLabels parentBlockLabel =
        Chain.createMissing None txLabels parentBlockLabel

    // Create a missing block (using block label)
    let [<When>] ``validating a detached block (.*) containing (.*) extending (.*)`` newBlockLabel txLabels parentBlockLabel =
        Chain.createMissing (Some newBlockLabel) txLabels parentBlockLabel

    // Create a missing block (using block label, empty block)
    let [<When>] ``validating a detached empty block (.*) extending (.*)`` newBlockLabel parentBlockLabel =
        Chain.createMissing (Some newBlockLabel) "" parentBlockLabel

    // Create a missing block (using no block label, empty block)
    let [<When>] ``validating a detached empty block extending (.*)`` parentBlockLabel =
        Chain.createMissing None "" parentBlockLabel

    // Checks that tx passes validation
    let [<Then>] ``(.*) should pass validation`` txLabel =
        match Transaction.validate txLabel with
        | Ok _ -> ()
        | Error e -> failwithf "Validation result: %A" e

    // Checks that tx doesn't passes validation
    let [<Then>] ``(.*) validation should yield (.*)`` txLabel (message:string) =
        match Transaction.validate txLabel with
        | Ok _ -> failwithf "Unexpected Ok validation result"
        | Error (ValidationError.General error) -> message ?= error
        | Error error  -> message ?= (error.ToString())

    //Checks that a tx is locking an asset to an address or a contract
    let [<Then>] ``(.*) should lock (.*) (.*) to (.*)`` txLabel (expected:uint64) asset keyLabel =
        let tx = Transaction.findTx txLabel
        let asset = Asset.getAsset asset

        let stepDown asset amount =
            if asset = Asset.Zen then
                amount / 100_000_000UL
            else
                amount

        expected ?=
            List.fold (fun state { lock = lock; spend = spend } ->
                if lock = Lock.getLock keyLabel && spend.asset = asset then
                    state + (stepDown spend.asset spend.amount)
                else
                    state) 0UL tx.outputs


    // Checks a contract's state
    let [<Then>] ``state of (.*) should be (.*) of (.*)`` contractLabel dataType value =
        Contract.MessageBody.constructData dataType value
        |> Some
        |> Contract.checkContractState contractLabel

    // Checks that a contract's state is none
    let [<Then>] ``state of (.*) should be none`` contractLabel =
        Contract.checkContractState contractLabel None

    // Checks the tip
    let [<Then>] ``tip should be (.*)`` (blockLabel : string) =
        match Block.findBlockLabel state.tipState.tip.header with
        | Some actualBlockLabel -> blockLabel ?= actualBlockLabel
        | _ -> failwithf "cannot resolve block: %A" blockLabel

    // Asserts that a contract is active
    let [<Then>] ``(.*) should be active for (.*) block(?:|s)`` contractLabel (blocks:uint32) =
        let contractId, _  = Contract.findContract contractLabel
        match ActiveContractSet.tryFind contractId state.tipState.activeContractSet with
        | Some contract ->
            blocks ?= contract.expiry - state.tipState.tip.header.blockNumber
        | None -> failwithf "expected contract to be activated"

    // Asserts that a contract is not active
    let [<Then>] ``(.*) should not be active`` contractLabel =
        let contractId, _  = Contract.findContract contractLabel
        match ActiveContractSet.tryFind contractId state.tipState.activeContractSet with
        | Some contract ->
            failwithf "expected contract to not be activated but is active for %A " (contract.expiry - state.tipState.tip.header.blockNumber)
        | None -> ()

    // Prints the number of blocks a contract active for
    let [<Then>] ``(.*) should be active for\?`` contractLabel =
        let contractId, _  = Contract.findContract contractLabel
        match ActiveContractSet.tryFind contractId state.tipState.activeContractSet with
        | Some contract ->
            printfn "%A is active for %A blocks" contractLabel (contract.expiry - state.tipState.tip.header.blockNumber)
        | None -> ()

    // re-signs a tx
    let [<When>] ``(.*) results by re-signing (.*) with (.*)`` destTxLabel sourceTxLabel keyLabels =
        { Transaction.findTx sourceTxLabel with witnesses = [] }
        |> Transaction.sign destTxLabel keyLabels

    let [<When>] ``(.*) results by changing asset of (.*) output (.*) to (.*)`` destTxLabel sourceTxLabel index value =
        let fn = fun output -> { output with spend = { output.spend with asset = Asset.getAsset value } }
        Transaction.malleateTx destTxLabel sourceTxLabel (Transaction.malleateTxOutput fn index)

    let [<When>] ``(.*) results by changing amount of (.*) output (.*) to (.*)`` destTxLabel sourceTxLabel index value =
        let fn = fun output -> { output with spend = { output.spend with amount = Amount.getAmount output.spend.asset value } }
        Transaction.malleateTx destTxLabel sourceTxLabel (Transaction.malleateTxOutput fn index)

    let [<When>] ``(.*) results by pushing (.*) (.*) of (.*) at index (.*)`` destTxLabel list sourceIndex sourceTxLabel destIndex =
        match list with
        | "witness" -> Transaction.malleateTx destTxLabel sourceTxLabel (fun tx -> { tx with witnesses = Transaction.malleateList tx.witnesses sourceIndex destIndex })
        | "output" -> Transaction.malleateTx destTxLabel sourceTxLabel (fun tx -> { tx with outputs = Transaction.malleateList tx.outputs sourceIndex destIndex })
        | "input" -> Transaction.malleateTx destTxLabel sourceTxLabel (fun tx -> { tx with inputs = Transaction.malleateList tx.inputs sourceIndex destIndex })
        | other -> failwithf "unexpected list %A" other

    let [<When>] ``(.*) results by changing (.*) of (.*) contract witness (.*) to (.*)`` destTxLabel field sourceTxLabel index value =
        let fn = fun (w:ContractWitness) ->
            match field with
            | "contractId"    -> { w with contractId = Contract.getContractId value |> fst }
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
        Transaction.malleateTx destTxLabel sourceTxLabel (Transaction.malleateTxWitnessItem fn index)

    let [<Then>] ``TODO(.*)`` (msg : string) : unit =
        match msg with
        | "" -> failwith "Not implemented"
        | _  -> failwithf "TODO: %s" msg

module Consensus.Tests.StepDefinitions

open TechTalk.SpecFlow
open NUnit.Framework
open TechTalk.SpecFlow.BindingSkeletons
open TechTalk.SpecFlow.Assist
open Consensus.Crypto

let private (?=) expected actual = Assert.AreEqual (expected, actual)

[<Binding>]
module Binding =
    open Consensus
    open Consensus.Crypto
    open Consensus.Types
    open FStar.UInt

    type State = {
        keys : Map<string, KeyPair>
        txs : Map<string, Transaction>
        utxoset: UtxoSet.T
    }

    let mutable state = {
        keys = Map.empty
        txs = Map.empty
        utxoset = Map.empty
    }

    let getAsset = function
    | "Zen" -> Asset.Zen
    | other -> failwithf "Unrecognized asset: %A" other

    let updateTx txLabel tx =
        state <- { state with txs = Map.add txLabel tx state.txs }
        tx

    let tryFindTx txLabel = Map.tryFind txLabel state.txs

    let findTx txLabel =
        match tryFindTx txLabel with
        | Some tx -> tx
        | None -> failwithf "Referenced tx missing: %A" txLabel

    let tryFindKey keyLabel = Map.tryFind keyLabel state.keys

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
        if not <| Map.containsKey keyLabel state.keys then
            let keyPair = KeyPair.create()
            state <- { state with keys = Map.add keyLabel keyPair state.keys }

        Map.find keyLabel state.keys

    let getLock keyLabel =
        initKey keyLabel
        |> snd
        |> PublicKey.hash
        |> Lock.PK

    let getOutput amount asset keyLabel = {
        spend = { amount = amount; asset = getAsset asset }
        lock = getLock keyLabel
    }

    // Init a utxoset
    let [<Given>] ``utxoset`` (table: Table) =
        let mutable txs = Map.empty

        // init all mentioned txs with their outputs
        for row in table.Rows do
            let txLabel = row.GetString "Tx"
            let keyLabel = row.GetString "Key"
            let asset = row.GetString "Asset"
            let amount = row.GetInt64 "Amount" |> uint64

            let tx = initTx txLabel
            let output = getOutput amount asset keyLabel
            let tx = { tx with outputs = Infrastructure.List.add output tx.outputs }
                     |> updateTx txLabel

            txs <- Map.add txLabel tx txs

        // fold on mentioned txs, fold on their outputs
        let utxoset =
            txs
            |> Map.fold (fun utxoset _ tx ->
                let txHash = Transaction.hash tx
                tx.outputs
                |> List.mapi (fun i output -> (uint32 i, output))
                |> List.fold (fun utxoset (index, output) ->
                    let outpoint = { txHash = txHash; index = index }
                    Map.add outpoint (UtxoSet.Unspent output) utxoset
                ) utxoset
            ) state.utxoset

        state <- { state with utxoset = utxoset }

    // Adding a single output
    let [<Given>] ``(.*) locks (.*) (.*) to (.*)`` txLabel amount asset keyLabel =
        let output = getOutput amount asset keyLabel
        let tx = initTx txLabel
        { tx with outputs = Infrastructure.List.add output tx.outputs }
        |> updateTx txLabel
        |> ignore        
        
    // Adds an outpoint to a tx
    let [<Given>] ``(.*) has the input (.*) index (.*)`` txLabel refTxLabel index =
        let refTx = findTx refTxLabel
        let outpoint = Outpoint { txHash = Transaction.hash refTx; index = index }
        let tx = initTx txLabel
        { tx with inputs = Infrastructure.List.add outpoint tx.inputs }
        |> updateTx txLabel
        |> ignore

    // Signes a tx
    let [<When>] ``(.*) is signed with (.*)`` txLabel keyLabels =
        let tx = findTx txLabel
        let keyPairs =
            (keyLabels:string).Split [|','|]
            |> Array.toList
            |> List.map findKey
        let tx = Transaction.sign keyPairs tx
        updateTx txLabel tx
        |> ignore

    // Checks that tx passes in-context validation
    let [<Then>] ``(.*) should pass validation`` txLabel =
        let tx = findTx txLabel
        let chainParams = Chain.getChainParameters Chain.Test
        let getUTXO outpoint =
            match Map.tryFind outpoint state.utxoset with
            | Some output -> output
            | None -> failwithf "Missing UTXO: %A" outpoint

        match TransactionValidation.validateInContext
            chainParams
            getUTXO
            "./test"
            1ul
            1_000_000UL
            ActiveContractSet.empty
            Map.empty
            state.utxoset
            (Transaction.hash tx)
            tx with
        | Ok _ -> true
        | Error e -> failwithf "Validation result: %A" e
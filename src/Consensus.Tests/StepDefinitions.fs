module Consensus.Tests.StepDefinitions

open TechTalk.SpecFlow
open NUnit.Framework
open TechTalk.SpecFlow.BindingSkeletons
open TechTalk.SpecFlow.Assist
open Consensus.Crypto
open Infrastructure.Result
open Consensus.ZFStar
open System.Text.RegularExpressions

let private result = new ResultBuilder<string>()

let private (?=) expected actual = Assert.AreEqual (expected, actual)

[<Literal>]
let rlimit = 2723280u

let private contractPath = "./test"

[<Binding>] 
module Binding =
    open Consensus
    open Consensus.Crypto
    open Consensus.Types
    open FStar.UInt
    open System
    open Wallet

    type Cache = {
        contracts : Map<string, Contract.T>
    }

    type State = {
        keys : Map<string, KeyPair>
        txs : Map<string, Transaction>
        utxoset: UtxoSet.T
        data : Map<string, Zen.Types.Data.data>
    }

    let mutable cache = {
        contracts = Map.empty
    }
    
    let mutable state = {
        keys = Map.empty
        txs = Map.empty
        utxoset = Map.empty
        data = Map.empty
    }
    
    let split (value:string) =
        value.Split [|','|]
        |> Array.toList

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let updateTx txLabel tx =
        state <- { state with txs = Map.add txLabel tx state.txs }
        tx

    let tryFindTx txLabel = Map.tryFind txLabel state.txs

    let tryFindContract contractLabel = Map.tryFind contractLabel cache.contracts

    let getAsset value =
        if value = "Zen" then Asset.Zen
        else 
            match tryFindContract value with
            | Some contract ->
                let contractId = Contract.makeContractId Version0 contract.code
                Asset (contractId, Hash.zero)
            | None ->
                failwithf "Unrecognized asset: %A" value

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

    let initContract contractLabel =
        match tryFindContract contractLabel with
        | Some contract ->
            contract
        | None ->
            let activateContract contract = 
                let compile code = result {
                    let! hints = Contract.recordHints code
                    let! queries = Infrastructure.ZFStar.totalQueries hints
                
                    let contract = {
                        code = code
                        hints = hints
                        rlimit = rlimit
                        queries = queries
                    }
                
                    return! 
                        Contract.compile contractPath contract
                        |> Result.bind (Contract.load contractPath 100ul code)
                }
        
                let path = System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location)
                let path = System.IO.Path.Combine (path, "Contracts")
                let contract = System.IO.Path.ChangeExtension (contract,".fst")
                
                System.IO.Path.Combine (path, contract)
                |> System.IO.File.ReadAllText
                |> compile
                |> Infrastructure.Result.get
        
            let contract = activateContract contractLabel
            cache <- { cache with contracts = Map.add contractLabel contract cache.contracts }
            contract

    let initKey keyLabel =
        if not <| Map.containsKey keyLabel state.keys then
            let keyPair = KeyPair.create()
            state <- { state with keys = Map.add keyLabel keyPair state.keys }

        Map.find keyLabel state.keys


    let initData dataLabel data =
        if not <| Map.containsKey dataLabel state.data then
            state <- { state with data = Map.add dataLabel data state.data }

    let tryFindData dataLabel = Map.tryFind dataLabel state.data

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
        | Some contract ->
            Contract.makeContractId Version0 contract.code
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
            Zen.Types.Main.Anonymous
        else
            match tryFindContract sender with
            | Some contract -> 
                Contract.makeContractId Version0 contract.code
                |> Consensus.Types.ContractSender
            | None ->
                initKey sender
                |> snd
                |> Consensus.Types.PKSender

    let [<BeforeScenario>] SetupScenario () = 
        state <- {
            keys = Map.empty
            txs = Map.empty
            utxoset = Map.empty
            data = Map.empty
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

    let executeContract contractLabel inputTxLabel outputTxLabel command sender data =
        let contract = initContract contractLabel
        let tx = initTx inputTxLabel
        let outputs = 
            tx.inputs
            |> UtxoSet.tryGetOutputs (state.utxoset.TryFind >> Option.get) state.utxoset
            |> Option.get
        let inputTx = TxSkeleton.fromTransaction tx outputs

        //copied from TransactionHandler
        let rec run (txSkeleton:TxSkeleton.T) (contractId:ContractId) command sender data witnesses totalCost =
            match tryFindContract contractLabel with
            | Some contract ->
                let contractWallet =
                    txSkeleton.pInputs
                    |> List.choose (fun input ->
                        match input with
                        | TxSkeleton.PointedOutput (outpoint,output) when output.lock = Consensus.Types.Contract contractId ->
                            Some (outpoint,output)
                        | _ -> None
                    )

                let contractContext = 
                    { blockNumber=1u; timestamp=1UL }

                Contract.run contract txSkeleton contractContext command sender data contractWallet
                |> Result.bind (fun (tx, message) ->

                    TxSkeleton.checkPrefix txSkeleton tx
                    |> Result.bind (fun finalTxSkeleton ->
                        let witness = TxSkeleton.getContractWitness contract.contractId command data txSkeleton finalTxSkeleton 0L
    
                        // To commit to the cost we need the real contract wallet
                        let contractWallet = Contract.getContractWallet tx witness
                        let cost = Contract.getCost contract txSkeleton contractContext command sender data contractWallet
                        let totalCost = cost + totalCost
    
                        // We can now commit to the cost, so lets alter it with the real cost
                        let witness = {witness with cost = uint32 cost}
    
                        let witnesses = Infrastructure.List.add (ContractWitness witness) witnesses
    
                        match message with
                        | Some {contractId=contractId; command=command; data=data} ->
                            run finalTxSkeleton contractId command (ContractSender contract.contractId) data witnesses totalCost
                        | None ->
                            Ok (finalTxSkeleton, witnesses)
                    )
                )
            | _ -> Error "Contract not active"

        let contractId = Contract.makeContractId Version0 contract.code
        
        run inputTx contractId command sender data [] 0L
        |> Result.mapError failwith
        <@> (fun (finalTxSkeleton, witnesses) -> 
            Transaction.fromTxSkeleton finalTxSkeleton
            |> Transaction.addWitnesses witnesses)
        <@> updateTx outputTxLabel
        |> ignore

    // Executes a contract
    let [<When>] ``(.*) executes on (.*) returning (.*)`` contractLabel inputTxLabel outputTxLabel =
        executeContract contractLabel inputTxLabel outputTxLabel "" Zen.Types.Main.Anonymous None

    // Executes a contract with additional arguments
    let [<When>] ``(.*) executes on (.*) returning (.*) with`` contractLabel inputTxLabel outputTxLabel (table: Table) =
        let mutable command = String.Empty
        let mutable data = None
        let mutable sender = Zen.Types.Main.Anonymous
        
        for row in table.Rows do
            let value = row.Item(1)
            match row.Item(0) with
            | "Command" -> command <- value
            | "Sender" -> sender <- getSender value
            | "Data" -> data <- match tryFindData value with
                                | Some data -> Some data
                                | _ -> failwithf "cannot resolve data: %A" value
            | "ReturnAddress" ->
                let _, publicKey = initKey value
                match Account.addReturnAddressToData publicKey data with
                | Ok data' -> data <- data'
                | Error error -> failwithf "error initializing data with return address: %A" error
            | other -> failwithf "unexpected execute option %A" other

        executeContract contractLabel inputTxLabel outputTxLabel command sender data
        
    // Adding a single output
    let [<Given>] ``(.*) locks (.*) (.*) to (.*)`` txLabel amount asset keyLabel =
        let output = getOutput amount asset keyLabel
        let tx = initTx txLabel
        { tx with outputs = Infrastructure.List.add output tx.outputs }
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

    // Signes a tx
    let [<When>] ``(.*) is signed with (.*)`` txLabel keyLabels =
        let tx = findTx txLabel
        let keyPairs =
            keyLabels
            |> split
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

        let acs = 
            Map.fold (fun acs _ (contract:Contract.T) -> 
                let contractId = Contract.makeContractId Version0 contract.code
                ActiveContractSet.add contractId contract acs
            ) ActiveContractSet.empty cache.contracts

        match TransactionValidation.validateInContext
            chainParams
            getUTXO
            "./test"
            1ul
            1_000_000UL
            acs
            Map.empty
            state.utxoset
            (Transaction.hash tx)
            tx with
        | Ok _ -> ()
        | Error e -> failwithf "Validation result: %A" e
        
    //Checks that a tx is locking an asset to an address or a contract
    let [<Then>] ``(.*) should lock (.*) (.*) to (.*)`` txLabel (amount:uint64) asset keyLabel =
        let tx = findTx txLabel
        let asset = getAsset asset

        Assert.AreEqual (amount, List.fold (fun state { lock = lock; spend = spend } -> 
            if lock = getLock keyLabel && spend.asset = asset then
                state + spend.amount
            else
                state) 0UL tx.outputs)
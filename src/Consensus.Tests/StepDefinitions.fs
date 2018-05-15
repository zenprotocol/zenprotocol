module Consensus.Tests.StepDefinitions

open TechTalk.SpecFlow
open NUnit.Framework
open TechTalk.SpecFlow.BindingSkeletons
open TechTalk.SpecFlow.Assist
open Consensus.Crypto
open Infrastructure.Result

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

    type State = {
        keys : Map<string, KeyPair>
        txs : Map<string, Transaction>
        utxoset: UtxoSet.T
        contracts : Map<string, Contract.T>
    }

    let mutable state = {
        keys = Map.empty
        txs = Map.empty
        utxoset = Map.empty
        contracts = Map.empty
    }

    let updateTx txLabel tx =
        state <- { state with txs = Map.add txLabel tx state.txs }
        tx

    let tryFindTx txLabel = Map.tryFind txLabel state.txs

    let tryFindContract contractLabel = Map.tryFind contractLabel state.contracts

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
            state <- { state with contracts = Map.add contractLabel contract state.contracts }
            contract

    let initKey keyLabel =
        if not <| Map.containsKey keyLabel state.keys then
            let keyPair = KeyPair.create()
            state <- { state with keys = Map.add keyLabel keyPair state.keys }

        Map.find keyLabel state.keys

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

    let getSender senderType address =
        match senderType with 
        | "anonymous" -> Zen.Types.Main.Anonymous
        | other -> failwithf "Undexpected sender type %A" other

    let [<BeforeScenario>] SetupScenario () = 
        state <- {
            keys = Map.empty
            txs = Map.empty
            utxoset = Map.empty
            contracts = Map.empty
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

    // Executes a contract
    let [<When>] ``(.*) executes on (.*) returning (.*)`` contractLabel inputTxLabel outputTxLabel (*command senderType senderAddress returnAddess*) =
        let contract = initContract contractLabel
        let tx = initTx inputTxLabel
        let outputs = 
            tx.inputs
            |> UtxoSet.tryGetOutputs (state.utxoset.TryFind >> Option.get) state.utxoset
            |> Option.get
        let inputTx = TxSkeleton.fromTransaction tx outputs
        let command = ""
        let sender = getSender "anonymous" "" //senderAddress

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
        
        run inputTx contractId command sender None [] 0L
        |> Result.mapError failwith
        <@> (fun (finalTxSkeleton, witnesses) -> 
            Transaction.fromTxSkeleton finalTxSkeleton
            |> Transaction.addWitnesses witnesses)
        <@> updateTx outputTxLabel
        |> ignore

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

        let acs = 
            Map.fold (fun acs _ (contract:Contract.T) -> 
                let contractId = Contract.makeContractId Version0 contract.code
                ActiveContractSet.add contractId contract acs
            ) ActiveContractSet.empty state.contracts

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

        Assert.AreEqual (List.fold (fun state { lock = lock; spend = spend } -> 
            if lock = getLock keyLabel && spend.asset = asset then
                state + spend.amount
            else
                state) 0UL tx.outputs, amount)
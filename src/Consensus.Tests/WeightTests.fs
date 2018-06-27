module Consensus.Tests.WeightTests
#nowarn "25"

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open Crypto
open Consensus.Weight
open TransactionNunitHelpers
open TestsInfrastructure.Nunit
open TestsInfrastructure.Constraints
open FsUnit
open Helper

let acs = ActiveContractSet.empty   // Not used due to mocks
let getUTXO _ = UtxoSet.NoOutput    // Not testing disk access

let testInput1 = getInput 1uy 0ul
let keys = getKeys 10


let transactionWeight set tx =
    match UtxoSet.tryGetOutputs getUTXO set tx.inputs with 
    | None -> failwith ""
    | Some outputs ->
        let txSkel = TxSkeleton.fromTransaction tx outputs
        Weight.transactionWeight tx txSkel

[<Test>]
let ``Transaction with one PK lock in inputs should have corresponding cost``() =
    let oneKeyPair = List.take 1 keys
    let _, publicKey = keys.[0]
    let outputLock = PK (PublicKey.hash publicKey)
    let output = { lock = outputLock; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    let sTx = Transaction.sign oneKeyPair TxHash tx
    let txWeight = transactionWeight utxos sTx
    shouldEqual (txWeight, (Ok pkWitnessWeight : Result<bigint,string>))

[<Test>]
let ``Transaction with many PK locks in inputs should have right cost``() =
    let outpoints = List.map2 getInput [1uy..10uy] [0ul..9ul]
    let inputs = List.map Outpoint outpoints
    let outputLocks =
        [ for publicKey in List.map snd keys ->
            PK (PublicKey.hash publicKey) ]
    let outputs =
        [ for lk in outputLocks ->
            { lock = lk; spend = { asset = Asset.Zen; amount = 1UL} } ]
    let tx = { version=Version0;inputs=inputs;witnesses=[];outputs=outputs;contract=None }
    let utxos = Map.ofList <| List.zip outpoints (List.map Unspent outputs)
    let sTx = Transaction.sign keys TxHash tx
    let txWeight = transactionWeight utxos sTx
    shouldEqual (txWeight, (Ok (pkWitnessWeight * bigint (List.length keys)) : Result<bigint,string>))

// The weight of a contract witness is just computed from the cost to which
// it commits. The cost is validated in TransactionValidation.

[<Test>]
let ``Contract validated transaction should have the right cost``() =
    let cHash = testHash
    let cAsset = Asset (ContractId (Version0, testHash), Hash.zero)
    let cLock = Contract <| ContractId (Version0,cHash)
    let cSpend = { asset = Asset.Zen; amount = 1UL }
    let mintInput = Types.Input.Mint { asset = cAsset; amount = 100UL }
    let outpoints = List.map2 getInput [1uy..10uy] [0ul..9ul]
    let inputs = List.map Outpoint outpoints
    let utxos =
        Map [ for p in outpoints -> (p, Unspent { lock=cLock; spend=cSpend }) ]
    let outputSpends = [
        {asset=cAsset;amount=50UL};
        {asset=cAsset;amount=50UL};
        {asset=Asset.Zen;amount=10UL}
    ]
    let outputs = [
        for spend in outputSpends -> {lock=cLock;spend=spend}
    ]
    let cWitness = {
        contractId=ContractId (Version0,cHash);
        command = "foo";
        messageBody = None;
        stateCommitment = NotCommitted
        beginInputs = 0u;       //
        beginOutputs = 0u;      //  Consumes entire transaction
        inputsLength = 11u;     //
        outputsLength = 3u;     //
        signature = None;
        cost = 200UL;
    }
    let tx = {
        version = Version0
        inputs = mintInput::inputs;
        outputs = outputs;
        contract = None;
        witnesses = [ ContractWitness cWitness ]
    }
    let txWeight = transactionWeight utxos tx
    shouldEqual (txWeight,(Ok 20_000I : Result<bigint,string>))

[<Test>]
let ``Two contracts in sequence should have the right cost``() =
    let cHash = testHash
    let cAsset = Asset (ContractId (Version0, testHash), Hash.zero)
    let cLock = Contract <| ContractId (Version0,cHash)
    let cSpend = { asset = Asset.Zen; amount = 1UL }
    let outpoints = List.map2 getInput [1uy..10uy] [0ul..9ul]
    let inputs = List.map Outpoint outpoints
    let utxos =
        Map [ for p in outpoints -> (p, Unspent { lock=cLock; spend=cSpend }) ]
    let outputSpends = [
        {asset=cAsset;amount=50UL};
        {asset=cAsset;amount=50UL};
        {asset=Asset.Zen;amount=10UL}
    ]
    let outputs = [
        for spend in outputSpends -> {lock=cLock;spend=spend}
    ]
    let cWitness1 = {
        contractId=ContractId (Version0,cHash);
        command = "foo";
        messageBody = None;
        stateCommitment = NotCommitted
        beginInputs = 0u;
        beginOutputs = 0u;
        inputsLength = 5u;
        outputsLength = 2u;
        signature = None;
        cost = 200UL;
    }
    let cWitness2 = {
        cWitness1 with
            messageBody = None;
            beginInputs = 5u;
            beginOutputs = 2u;
            inputsLength = 5u;
            outputsLength = 1u;
            cost = 50UL;
    }
    let tx = {
        version = Version0
        inputs = inputs;
        outputs = outputs;
        contract = None;
        witnesses = [ ContractWitness cWitness1; ContractWitness cWitness2 ]
    }
    let txWeight = transactionWeight utxos tx
    shouldEqual (txWeight, (Ok 25_000I : Result<bigint,string>))

[<Test>]
let ``Transaction with too many witnesses should fail``() =
    let twoKeyPairs = List.take 2 keys
    let _, publicKey = keys.[0]
    let outputLock = PK (PublicKey.hash publicKey)
    let output = { lock = outputLock; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    let sTx = Transaction.sign twoKeyPairs TxHash tx
    let txWeight = transactionWeight utxos sTx
    shouldEqual (txWeight, (Error "expecting a contract 0 witness" : Result<bigint,string>))

[<Test>]
let ``Transaction with too few witnesses should fail``() =
    let _, publicKey = keys.[0]
    let outputLock = PK (PublicKey.hash publicKey)
    let output = { lock = outputLock; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    let txWeight = transactionWeight utxos tx
    shouldEqual (txWeight, (Error "expecting a public key witness" : Result<bigint,string>))

[<Test>]
let ``Contract activation weight should be positive``() =
    let (Ok contractWithId) = SampleContract.contractWithId
    let rootAccount = createTestAccount()
    let tx =
        TestWallet.createActivationTransactionFromContract
                            (Chain.getChainParameters (Chain.Local)) contractWithId 1ul rootAccount
    let actWeight = Result.map (fun {Transaction.contract=Some contract} -> activationWeight contract) tx

    actWeight |> should be ok
    let (Ok wt) = actWeight
    wt |> should be (greaterThan 0I)

[<Test>]
let ``Transaction with large amount of inputs should not fail weight calculation``() =
    let mutable keys = []
    let mutable inputs = []
    let mutable utxos = UtxoSet.asDatabase

    let size = 5000u
    for i in [ 1u .. size ] do
        let bytes =
            Infrastructure.BigEndianBitConverter.uint32ToBytes i
        let bytes =
            bytes
            |> Array.append (Array.zeroCreate (Consensus.Hash.Length - (Array.length bytes)))
        let input =
            {
               txHash = Hash bytes
               index = 0u
            }
        let privateKey, publicKey = KeyPair.create()
        let output = { lock = publicKey |> PublicKey.hash |> PK ; spend = { asset = Asset.Zen; amount = 1UL } }

        keys <- Infrastructure.List.add (privateKey, publicKey) keys
        inputs <- Infrastructure.List.add (Outpoint input) inputs
        utxos <- Map.add input (Unspent output) utxos

    let _, publicKey = KeyPair.create()
    let output = { lock = publicKey |> PublicKey.hash |> PK ; spend = { asset = Asset.Zen; amount = 1UL * (uint64 size) } }
    let tx = {
        version = Version0
        inputs = inputs
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let signedTx = Transaction.sign keys TxHash tx
    let txWeight = transactionWeight utxos signedTx
    shouldEqual (txWeight, (Ok 500000000I : Result<bigint,string>))

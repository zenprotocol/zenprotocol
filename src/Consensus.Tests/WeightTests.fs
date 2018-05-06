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
    let sTx = Transaction.sign oneKeyPair tx
    let txWeight = transactionWeight getUTXO utxos sTx
    let pkWeight, _, _ = pkWitnessWeight
    shouldEqual (txWeight, (Ok pkWeight : Result<bigint,string>))

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
    let sTx = Transaction.sign keys tx
    let txWeight = transactionWeight getUTXO utxos sTx
    let pkWeight, _, _ = pkWitnessWeight
    shouldEqual (txWeight, (Ok (pkWeight * bigint (List.length keys)) : Result<bigint,string>))

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
        data = None;
        beginInputs = 0u;       //
        beginOutputs = 0u;      //  Consumes entire transaction
        inputsLength = 11u;     //
        outputsLength = 3u;     //
        signature = None;
        cost = 200u;
    }
    let tx = {
        version = Version0
        inputs = mintInput::inputs;
        outputs = outputs;
        contract = None;
        witnesses = [ ContractWitness cWitness ]
    }
    let txWeight = transactionWeight getUTXO utxos tx
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
        data = None;
        beginInputs = 0u;
        beginOutputs = 0u;
        inputsLength = 5u;
        outputsLength = 2u;
        signature = None;
        cost = 200u;
    }
    let cWitness2 = {
        cWitness1 with
            data = None;
            beginInputs = 5u;
            beginOutputs = 2u;
            inputsLength = 5u;
            outputsLength = 1u;
            cost = 50u;
    }
    let tx = {
        version = Version0
        inputs = inputs;
        outputs = outputs;
        contract = None;
        witnesses = [ ContractWitness cWitness1; ContractWitness cWitness2 ]
    }
    let txWeight = transactionWeight getUTXO utxos tx
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
    let sTx = Transaction.sign twoKeyPairs tx
    let txWeight = transactionWeight getUTXO utxos sTx
    shouldEqual (txWeight, (Error "Too many witnesses" : Result<bigint,string>))

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
    let txWeight = transactionWeight getUTXO utxos tx
    shouldEqual (txWeight, (Error "Too few witnesses" : Result<bigint,string>))

[<Test>]
let ``Contract activation weight should be positive``() =
    let code = SampleContract.sampleContractCode
    let rootAccount = createTestAccount()
    let tx =
        Wallet.Account.createActivateContractTransaction
                            (Chain.getChainParameters (Chain.Local)) code 1ul rootAccount
    let actWeight = Result.map (fun {Transaction.contract=Some contract} -> activationWeight contract) tx

    actWeight |> should be ok
    let (Ok wt) = actWeight
    wt |> should equal 62_635_440I
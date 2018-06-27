module Consensus.Tests.TransactionTests

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open Crypto
open TransactionValidation

open TransactionNunitHelpers
open TransactionHelpers
open TestsInfrastructure.Nunit
open FsUnit
open Hopac.Extensions.Seq
open TestsInfrastructure.Constraints

let acs = ActiveContractSet.empty

let testInput1 = getInput 1uy 0ul
let testInput2 = getInput 2uy 0ul
let keys = getKeys 1

[<Test>]
let ``Transaction should be orphan``() =
    let orphanInput = {
        txHash = Hash (Array.create 32 100uy)
        index = 0ul
    }
    let output = { lock = PK testHash; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1; Outpoint orphanInput ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    inputsValidationOrphan 1ul 1_000_000UL acs utxos tx keys
    |> shouldEqual

[<Test>]
let ``Transaction basic validation should be Ok``() =
    let publicKey = Crypto.PublicKey <| Array.create 64 1uy
    let signature = Crypto.Signature <| Array.create 64 1uy
    let witness = PKWitness (TxHash, publicKey, signature)
    let output = { lock = PK testHash; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1 ]
        witnesses = [ witness ]
        outputs = [ output ]
        contract = None
    }
    basicValidationOk tx
    |> shouldEqual

let checkBasicValidation msg tx =
    tx
    |> basicValidationMsg msg
    |> shouldEqual

let checkSerialization tx =
    try
        Serialization.Transaction.serialize tx |> ignore
        false
    with _  ->
        true


let tx = { version = Version0; inputs = []; witnesses = []; outputs = []; contract = None }

[<Test>]
let ``Transaction validation should fail with outputs invalid error``() =
    let check = checkBasicValidation "structurally invalid output(s)"

    check { tx with inputs = [ Outpoint testInput1 ];
                    outputs = [ { lock = PK testHash; spend = { asset = Asset.Zen; amount = 0UL } } ] }

[<Test>]
let ``Transaction validation should fail with duplicate inputs error``() =
    let check = checkBasicValidation "inputs duplicated"

    let publicKey = Crypto.PublicKey <| Array.create 64 1uy
    let signature = Crypto.Signature <| Array.create 64 1uy
    let witness = PKWitness (TxHash, publicKey, signature)

    check { tx with inputs = [ Outpoint testInput1; Outpoint testInput1 ]
                    outputs = [ { lock = PK testHash; spend = { asset = Asset.Zen; amount = 1UL } } ]
                    witnesses = [ witness ] }

let emptyArr = [||]
let nonEmptyArr = [| byte 0 |]
let nullArr = null

[<Test>]
let ``Transaction validation should fail with structurally invalid contract data error``() =
    let check = checkBasicValidation "structurally invalid contract data"
    let checkV0 contract = check { tx with contract = Some (V0 contract) }
    let checkHighV contract = check { tx with contract = Some (HighV contract) }

    checkV0 { code="x";hints="";rlimit=10u;queries=10u }
    checkV0 { code="";hints="x";rlimit=10u;queries=10u }
    checkV0 { code=null;hints="x";rlimit=10u;queries=10u }
    checkV0 { code="x";hints=null;rlimit=10u;queries=10u }
    checkV0 { code="x";hints="x";rlimit=0u;queries=10u }
    checkV0 { code="x";hints="x";rlimit=10u;queries=0u }

    checkHighV (0ul, nonEmptyArr)
    checkHighV (1ul, emptyArr)
    checkHighV (1ul, nullArr)

let structurallyInvalidHash = Hash (Array.create (Hash.Length - 1) 1uy)
let validHash = Hash (Array.create Hash.Length 1uy)
let emptyHash = Hash [||]
let nullHash = Hash null

[<Test>]
let ``Transaction validation should fail with structurally invalid input data error``() =
    let check = checkBasicValidation "structurally invalid input(s)"

    let tx = { tx with outputs = [ { lock = PK validHash; spend = { asset = Asset.Zen; amount = 10UL } } ] }
    // empty imputs
    check { tx with inputs = [] }
    // zero amount
    check { tx with inputs = [ Mint { asset = Asset (ContractId (Version0, validHash), Hash.zero); amount = 0UL } ] }

[<Test>]
let ``Transaction validation should fail with structurally invalid output data``() =
    let check = checkBasicValidation "structurally invalid output(s)"

    let publicKey = Crypto.PublicKey <| Array.create 64 1uy
    let signature = Crypto.Signature <| Array.create 64 1uy
    let witness = PKWitness (TxHash, publicKey, signature)
    let tx = { tx with witnesses = [ witness ]; inputs = [ Outpoint { txHash = validHash; index = 0ul } ] }


    // illegal identifier
    for identifier in [ 1u .. 7u ] do
        check { tx with outputs = [ { lock = HighVLock (identifier, [| byte 0 |]); spend = { asset = Asset.Zen; amount = 10UL } } ] }

    // missing highvlock bytes
    check { tx with outputs = [ { lock = HighVLock (100u, [||]); spend = { asset = Asset.Zen; amount = 10UL } } ] }

    check { tx with outputs = [ { lock = PK Hash.zero; spend = { asset = Asset (ContractId (Version0, Hash.zero), validHash); amount = 10UL } } ] }

    // zero amount
    check { tx with outputs = [ { lock = PK validHash; spend = { asset = Asset.Zen; amount = 0UL } } ] }

    // test with malformatted HighV lock
    for invalidArr in [ emptyArr; nullArr ] do
        check { tx with outputs = [ { lock = HighVLock (0ul, invalidArr); spend = { asset = Asset.Zen; amount = 10UL } } ] }

[<Test>]
let ``Transaction validation should fail with structurally invalid witness data``() =
    let check witness =
        { tx with witnesses = [ witness ];
                  inputs = [ Mint { asset = Asset (ContractId (Version0, validHash), Hash.zero); amount = 10UL } ]
                  outputs = [ { lock = PK validHash; spend = { asset = Asset.Zen; amount = 10UL } } ] }
        |> checkBasicValidation "structurally invalid witness(es)"

    // test with malformatted HighVWitness
    check <| HighVWitness (1ul, nullArr)
    check <| HighVWitness (1ul, emptyArr)


    let validContractWitness = {
        contractId=ContractId (Version0,validHash)
        command = "command"
        messageBody = None
        stateCommitment = NotCommitted
        beginInputs = 0ul
        beginOutputs = 0ul
        inputsLength = 0ul
        outputsLength = 0ul
        signature = None
        cost = 1UL
    }

    let check = ContractWitness >> check

    check <| { validContractWitness with command = null }
    check <| { validContractWitness with cost = 0UL }

[<Test>]
let ``Signed transaction should be valid``() =
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
    inputsValidationOk 1ul 1_000_000UL acs utxos tx keys
    |> shouldEqual

[<Test>]
let ``Signed transaction validation result should be invalid witness``() =
    let outputLock = PK testHash // testHash will not match keypair
    let output = { lock = outputLock; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]
    inputsValidationMsg "PK witness mismatch" 1ul 1_000_000UL acs utxos tx keys
    |> shouldEqual
[<Test>]
let ``Transaction validation should fail when inputs consist only of mints``() =
    let asset = Asset (ContractId (Version0, validHash), Hash.zero)
    let output = { lock = PK testHash; spend = { asset = asset; amount = 10UL } }

    let contractWitness = {
        contractId = ContractId (Version0, validHash)
        command = ""
        messageBody = None
        stateCommitment = NotCommitted
        beginInputs = 0ul
        beginOutputs = 0ul
        inputsLength = 0ul
        outputsLength = 0ul
        signature = None
        cost = 1UL
    }
    {
        version = Version0
        inputs = [ Mint { asset = asset; amount = 10UL } ]
        witnesses = [ ContractWitness contractWitness ]
        outputs = [ output ]
        contract = None
    }
    |> basicValidationMsg "inputs consist of mints only"
    |> shouldEqual

[<Test>]
let ``sign with FollowingWitnesses sighash``() =
    let keys = getKeys 2

    let secret, publicKey = keys.[0]
    let secret2, publicKey2 = keys.[1]

    let outputLock1 = PK (PublicKey.hash publicKey)
    let output1 = { lock = outputLock1; spend = { asset = Asset.Zen; amount = 1UL } }

    let outputLock2 = PK (PublicKey.hash publicKey2)
    let output2 = {lock = outputLock2; spend = { asset = Asset.Zen; amount = 1UL } }

    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1;Outpoint testInput2]
        witnesses = []
        outputs = [ {lock = PK Hash.zero; spend = {asset = Asset.Zen; amount = 2UL}} ]
        contract = None
    }

    let utxos = Map.ofSeq [ testInput1, Unspent output1; testInput2, Unspent output2 ]

    let signedTx =
        Transaction.sign [(secret2, publicKey2)] TxHash tx
        |> Transaction.sign [(secret, publicKey)] FollowingWitnesses

    let txHash = Transaction.hash signedTx

    let sigHash = signedTx.witnesses.[0] |> function PKWitness (sigHash,_,_) -> sigHash
    sigHash |> should equal FollowingWitnesses

    inputsValidation 1ul 0UL acs utxos signedTx txHash
    |> should be ok

[<Test>]
let ``sign with FollowingWitnesses sighash last input in tx``() =
    let keys = getKeys 2

    let secret, publicKey = keys.[0]
    let secret2, publicKey2 = keys.[1]

    let outputLock1 = PK (PublicKey.hash publicKey)
    let output1 = { lock = outputLock1; spend = { asset = Asset.Zen; amount = 1UL } }

    let outputLock2 = PK (PublicKey.hash publicKey2)
    let output2 = {lock = outputLock2; spend = { asset = Asset.Zen; amount = 1UL } }

    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1;Outpoint testInput2]
        witnesses = []
        outputs = [ {lock = PK Hash.zero; spend = {asset = Asset.Zen; amount = 2UL}} ]
        contract = None
    }

    let utxos = Map.ofSeq [ testInput1, Unspent output1; testInput2, Unspent output2 ]

    let signedTx =
        Transaction.sign [(secret2, publicKey2)] FollowingWitnesses tx
        |> Transaction.sign [(secret, publicKey)] TxHash

    let txHash = Transaction.hash signedTx

    let sigHash = signedTx.witnesses.[1] |> function PKWitness (sigHash,_,_) -> sigHash
    sigHash |> should equal FollowingWitnesses

    inputsValidation 1ul 0UL acs utxos signedTx txHash
    |> should be ok

[<Test>]
let ``sign with FollowingWitnesses sighash and modify following witness``() =
    let keys = getKeys 2

    let secret, publicKey = keys.[0]
    let secret2, publicKey2 = keys.[1]

    let outputLock1 = PK (PublicKey.hash publicKey)
    let output1 = { lock = outputLock1; spend = { asset = Asset.Zen; amount = 1UL } }

    let outputLock2 = PK (PublicKey.hash publicKey2)
    let output2 = {lock = outputLock2; spend = { asset = Asset.Zen; amount = 1UL } }

    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1;Outpoint testInput2]
        witnesses = []
        outputs = [ {lock = PK Hash.zero; spend = {asset = Asset.Zen; amount = 2UL}} ]
        contract = None
    }

    let utxos = Map.ofSeq [ testInput1, Unspent output1; testInput2, Unspent output2 ]

    let signedTx =
        Transaction.sign [(secret, publicKey)] FollowingWitnesses tx
        |> Transaction.sign [(secret2, publicKey2)] TxHash

    let signedTx = {signedTx with witnesses = List.rev signedTx.witnesses}

    let txHash = Transaction.hash signedTx

    let expected:Result<Transaction,ValidationError.ValidationError> = Error (ValidationError.General "invalid PK witness signature")

    inputsValidation 1ul 0UL acs utxos signedTx txHash |> should equal expected

[<Test>]
let ``sign last input with FollowingWitnesses sighash and add another witness``() =
    let keys = getKeys 2

    let secret, publicKey = keys.[0]
    let secret2, publicKey2 = keys.[1]

    let outputLock1 = PK (PublicKey.hash publicKey)
    let output1 = { lock = outputLock1; spend = { asset = Asset.Zen; amount = 1UL } }

    let outputLock2 = PK (PublicKey.hash publicKey2)
    let output2 = {lock = outputLock2; spend = { asset = Asset.Zen; amount = 1UL } }

    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1;Outpoint testInput2]
        witnesses = []
        outputs = [ {lock = PK Hash.zero; spend = {asset = Asset.Zen; amount = 2UL}} ]
        contract = None
    }

    let utxos = Map.ofSeq [ testInput1, Unspent output1; testInput2, Unspent output2 ]

    let signedTx =
        Transaction.sign [(secret2, publicKey2)] FollowingWitnesses tx
        |> Transaction.sign [(secret, publicKey)] TxHash

    let witnesses = List.append signedTx.witnesses [signedTx.witnesses.[0]]
    let inputs = List.append signedTx.inputs [signedTx.inputs.[0]]
    let outputs = List.append signedTx.outputs [{lock = PK Hash.zero; spend = {asset = Asset.Zen; amount = 1UL}}]

    let signedTx = {signedTx with witnesses = witnesses; inputs = inputs; outputs = outputs}

    let txHash = Transaction.hash signedTx

    let expected:Result<Transaction,ValidationError.ValidationError> = Error (ValidationError.General "invalid PK witness signature")

    inputsValidation 1ul 0UL acs utxos signedTx txHash
    |> should equal expected




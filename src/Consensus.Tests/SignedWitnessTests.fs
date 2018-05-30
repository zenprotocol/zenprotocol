module Consensus.Tests.SignedWitnessTests

open System.Security.Cryptography.X509Certificates
open NUnit.Framework
open FsUnit
open Consensus
open Consensus.Crypto
open Consensus.Types
open Consensus.ValidationError
open Consensus.TransactionValidation
open Consensus.Tests.Helper
open Wallet
open Infrastructure
open TestsInfrastructure.Constraints
open TestsInfrastructure.Nunit


let chain = Chain.getChainParameters Chain.Local
let contractPath =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]


let account = createTestAccount ()
let publicKey = ExtendedKey.getPublicKey (snd account) |> Result.get

let serializePK = PublicKey.toString publicKey

type TxResult = Result<Transaction * ActiveContractSet.T * ContractCache.T * ContractStates.T,ValidationError>

let code = sprintf
            """
            open Zen.Types
            open Zen.Base
            open Zen.Cost
            open Zen.Asset

            module RT = Zen.ResultT
            module Tx = Zen.TxSkeleton
            module Crypto = Zen.Crypto
            module C = Zen.Cost

            let main txSkeleton _ contractId command sender messageBody wallet state =
                let! pk = Crypto.parsePublicKey "%s" in

                match sender with
                | PK pk' ->
                    if (Some pk') = pk then
                    begin
                        let! contractAsset = getDefault contractId in

                        let! txSkeleton =
                            Tx.mint 1UL contractAsset txSkeleton
                            >>=Tx.lockToContract contractAsset 1UL contractId in

                        RT.ok @ { tx = txSkeleton; message = None; state = NoChange}
                    end
                        else RT.autoFailw "expected different pk"
                | _ -> RT.autoFailw "expected pk"

            let cf _ _ _ _ _ _ _ =
                120 + (64 + (64 + 64 + 0)) + 30
                |> cast nat
                |> C.ret
            """ serializePK

let contractId = Contract.makeContractId Version0 code

let mutable acs = ActiveContractSet.empty

[<OneTimeSetUp>]
let setup () =
    let hints = Contract.recordHints code |> Result.get
    let contractActivation =
        {
            code=code;
            hints=hints;
            rlimit = TestWallet.rlimit;
            queries = ZFStar.totalQueries hints |> Result.get
        }
    let contract =
        Contract.compile contractPath contractActivation
        |> Result.bind (Contract.load contractPath 100ul code)
        |> Result.get

    acs <- ActiveContractSet.add contractId contract ActiveContractSet.empty

[<Test>]
let ``contract witness with valid signature``() =
    let spend = {asset=Asset.defaultOf contractId;amount=1UL}
    let tx =
        {
            version = Version0
            inputs=[Mint spend]
            outputs=[{lock=Contract contractId;spend=spend}]
            witnesses=[]
            contract=None
        }

    let txHash = Transaction.hash tx

    let signature = ExtendedKey.sign txHash (snd account) |> Result.get

    let contractWintess = ContractWitness {
                                              contractId=contractId;
                                              command="";
                                              messageBody=None;
                                              stateCommitment = NotCommitted;
                                              beginInputs = 0ul;
                                              beginOutputs = 0ul;
                                              inputsLength=1ul;
                                              outputsLength=1ul;
                                              signature=Some (publicKey,signature)
                                              cost = 342ul
                                          }
    let tx = {tx with witnesses= [contractWintess]}

    let result = TransactionValidation.validateInContext chain (fun _ -> UtxoSet.NoOutput) contractPath 2u 1_000_000UL acs UtxoSet.asDatabase ContractCache.empty ContractStates.asDatabase txHash tx

    result |> should be ok

[<Test>]
let ``contract witness with invalid publickey``() =
    let spend = {asset=Asset.defaultOf contractId;amount=1UL}
    let tx =
        {
            version = Version0
            inputs=[Mint spend]
            outputs=[{lock=Contract contractId;spend=spend}]
            witnesses=[]
            contract=None
        }

    let txHash = Transaction.hash tx

    let publicKey = ExtendedKey.derivePath "m/0'" (snd account) |> Result.get |> ExtendedKey.getPublicKey |> Result.get
    let signature = ExtendedKey.sign txHash (snd account) |> Result.get

    let contractWintess = ContractWitness {
                                              contractId=contractId;
                                              command="";
                                              messageBody=None;
                                              stateCommitment = NotCommitted;
                                              beginInputs = 0ul;
                                              beginOutputs = 0ul;
                                              inputsLength=1ul;
                                              outputsLength=1ul;
                                              signature=Some (publicKey,signature)
                                              cost = 338u
                                          }
    let tx = {tx with witnesses= [contractWintess]}

    let result = TransactionValidation.validateInContext chain (fun _ -> UtxoSet.NoOutput) contractPath 2u 1_000_000UL acs UtxoSet.asDatabase ContractCache.empty ContractStates.asDatabase txHash tx

    let expected:TxResult = Error (General "invalid contract witness signature")

    (result, expected)
    |> shouldEqual
    
[<Test>]
let ``contract witness with no signature``() =
    let spend = {asset=Asset.defaultOf contractId;amount=1UL}
    let tx =
        {
            version = Version0
            inputs=[Mint spend]
            outputs=[{lock=Contract contractId;spend=spend}]
            witnesses=[]
            contract=None
        }

    let txHash = Transaction.hash tx


    let contractWintess = ContractWitness {
                                              contractId=contractId;
                                              command="";
                                              messageBody=None;
                                              stateCommitment = NotCommitted;
                                              beginInputs = 0ul;
                                              beginOutputs = 0ul;
                                              inputsLength=1ul;
                                              outputsLength=1ul;
                                              signature=None
                                              cost = 342ul
                                          }
    let tx = {tx with witnesses= [contractWintess]}

    let result = TransactionValidation.validateInContext chain (fun _ -> UtxoSet.NoOutput) contractPath 2u 1_000_000UL acs UtxoSet.asDatabase ContractCache.empty ContractStates.asDatabase txHash tx

    let expected:TxResult = Error (General "expected pk")

    (result, expected)
    |> shouldEqual

[<Test>]
let ``contract witness with unexpcected public key``() =
    let spend = {asset=Asset.defaultOf contractId;amount=1UL}
    let tx =
        {
            version = Version0
            inputs=[Mint spend]
            outputs=[{lock=Contract contractId;spend=spend}]
            witnesses=[]
            contract=None
        }

    let txHash = Transaction.hash tx

    let key = ExtendedKey.derivePath "m/0'" (snd account) |> Result.get

    let publicKey = key |> ExtendedKey.getPublicKey |> Result.get
    let signature = ExtendedKey.sign txHash key |> Result.get

    let contractWintess = ContractWitness {
                                              contractId=contractId;
                                              command="";
                                              messageBody=None;
                                              stateCommitment = NotCommitted;
                                              beginInputs = 0ul;
                                              beginOutputs = 0ul;
                                              inputsLength=1ul;
                                              outputsLength=1ul;
                                              signature=Some (publicKey,signature)
                                              cost = 342ul
                                          }
    let tx = {tx with witnesses= [contractWintess]}

    let result = TransactionValidation.validateInContext chain (fun _ -> UtxoSet.NoOutput) contractPath 2u 1_000_000UL acs UtxoSet.asDatabase ContractCache.empty ContractStates.asDatabase txHash tx

    let expected:TxResult = Error (General "expected different pk")

    (result, expected)
    |> shouldEqual

[<Test>]
let ``contract witness with invalid execution cost``() =
    let spend = {asset=Asset.defaultOf contractId;amount=1UL}
    let tx =
        {
            version = Version0
            inputs=[Mint spend]
            outputs=[{lock=Contract contractId;spend=spend}]
            witnesses=[]
            contract=None
        }

    let txHash = Transaction.hash tx

    let signature = ExtendedKey.sign txHash (snd account) |> Result.get

    let contractWintess = ContractWitness {
                                              contractId=contractId;
                                              command="";
                                              messageBody=None;
                                              stateCommitment = NotCommitted;
                                              beginInputs = 0ul;
                                              beginOutputs = 0ul;
                                              inputsLength=1ul;
                                              outputsLength=1ul;
                                              signature=Some (publicKey,signature)
                                              cost = 1000ul
                                          }
    let tx = {tx with witnesses= [contractWintess]}

    let result = TransactionValidation.validateInContext chain (fun _ -> UtxoSet.NoOutput) contractPath 2u 1_000_000UL acs UtxoSet.asDatabase ContractCache.empty ContractStates.asDatabase txHash tx

    let expected:TxResult = Error (General "execution cost commitment mismatch")

    (result, expected)
    |> shouldEqual

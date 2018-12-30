module Consensus.Tests.CoinbaseTests

open Consensus
open Consensus.Types
open Consensus.Hash
open Consensus.UtxoSet
open NUnit.Framework
open Crypto
open ValidationError
open TransactionValidation
open FsUnit
open TransactionNunitHelpers
open TransactionHelpers
open TestsInfrastructure.Nunit

let acs = ActiveContractSet.empty

let testInput1 = getInput 1uy 0ul
let testInput2 = getInput 2uy 0ul
let keys = getKeys 1

[<Test>]
let ``coinbase cannot have any locks other than coinbase lock``() =
    let tx =
      {
         version = Version0
         inputs = [];
         outputs=[{lock= PK Hash.zero;spend={amount=1UL;asset=Asset.Zen}}]
         witnesses=[]
         contract=None
      }

    let expected:Result<Transaction,ValidationError> = Error (General "within coinbase transaction all outputs must use coinbase lock")

    TransactionValidation.validateCoinbase Chain.localParameters 15ul tx |> should equal expected

[<Test>]
let ``coinbase with wrong block nubmer should fail``() =
    let tx =
      {
         version = Version0
         inputs = [];
         outputs=[{lock= Coinbase (15ul, Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
         witnesses=[]
         contract=None
      }

    let expected:Result<Transaction,ValidationError> = Error (General "within coinbase transaction all outputs must use coinbase lock")

    TransactionValidation.validateCoinbase Chain.localParameters 14ul tx |> should equal expected

[<Test>]
let ``coinbase with inputs should fail``() =
    let tx =
      {
         version = Version0
         inputs = [Outpoint {txHash=Hash.zero;index=1ul}];
         outputs=[{lock= Coinbase (15ul, Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
         witnesses=[]
         contract=None
      }

    let expected:Result<Transaction,ValidationError> = Error (General "coinbase transaction must not have any inputs")

    TransactionValidation.validateCoinbase Chain.localParameters 15ul tx |> should equal expected

[<Test>]
let ``coinbase with witnesses fail``() =
    let tx =
      {
         version = Version0
         inputs = []
         outputs=[{lock= Coinbase (15ul, Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
         witnesses=[PKWitness (TxHash, Consensus.Tests.Helper.rootPublicKey ,Signature Array.empty)]
         contract=None
      }

    let expected:Result<Transaction,ValidationError> = Error (General  "coinbase transaction must not have any witnesses")

    TransactionValidation.validateCoinbase Chain.localParameters 15ul tx |> should equal expected

[<Test>]
let ``coinbase with contract should fail``() =
    let tx =
      {
         version = Version0
         inputs = [];
         outputs=[{lock= Coinbase (15ul, Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
         witnesses=[]
         contract=Some (V0 { code="ad";hints="ad";rlimit=0u;queries=0u })
      }

    let expected:Result<Transaction,ValidationError> = Error (General "coinbase transaction cannot activate a contract")

    TransactionValidation.validateCoinbase Chain.localParameters 15ul tx |> should equal expected

[<Test>]
let ``valid coinbase should pass``() =
    let tx =
      {
         version = Version0
         inputs = [];
         outputs=[{lock= Coinbase (15ul, Hash.zero);spend={amount=1UL;asset=Asset.Zen}}]
         witnesses=[]
         contract=None
      }

    let expected:Result<Transaction,ValidationError> = Ok tx

    TransactionValidation.validateCoinbase Chain.localParameters 15ul tx |> should equal expected

[<Test>]
let ``coinbase with two outputs should pass``() =
    let tx =
      {
         version = Version0
         inputs = [];
         outputs=
            [
                {lock= Coinbase (15ul, Hash.zero);spend={amount=1UL;asset=Asset.Zen}}
                {lock= Coinbase (15ul, Hash.zero);spend={amount=1UL;asset=Asset.Zen}}
            ]
         witnesses=[]
         contract=None
      }

    let expected:Result<Transaction,ValidationError> = Ok tx

    TransactionValidation.validateCoinbase Chain.localParameters 15ul tx |> should equal expected

[<Test>]
let ``coinbase with no outputs``() =
    let tx =
      {
         version = Version0
         inputs = []
         outputs= []
         witnesses=[]
         contract=None
      }

    let expected:Result<Transaction,ValidationError> = Error (General "outputs empty")

    TransactionValidation.validateCoinbase Chain.localParameters 15ul tx |> should equal expected

[<Test>]
let ``transaction spending coinbase with maturity should be valid``() =
    let _, publicKey = keys.[0]
    let outputLock = Coinbase (15ul,PublicKey.hash publicKey)
    let output = { lock = outputLock; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]

    inputsValidationOk 115ul 1_000_000UL acs utxos tx keys
    |> shouldEqual

[<Test>]
let ``transaction spending coinbase with no maturity should fail``() =
    let _, publicKey = keys.[0]
    let outputLock = Coinbase (100ul, PublicKey.hash publicKey)
    let output = { lock = outputLock; spend = { asset = Asset.Zen; amount = 1UL } }
    let tx = {
        version = Version0
        inputs = [ Outpoint testInput1 ]
        witnesses = []
        outputs = [ output ]
        contract = None
    }
    let utxos = Map.ofSeq [ testInput1, Unspent output ]

    inputsValidationMsg "Coinbase not mature enough" (100ul + Chain.testParameters.coinbaseMaturity - 1ul) 1_000_000UL acs utxos tx keys
    |> shouldEqual
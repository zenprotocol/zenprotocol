module Consensus.Tests.Helper

open Consensus
open Crypto
open Wallet
open Wallet.Account
open Infrastructure
open Consensus.Types

let rootMnemonicPhrase = "feel muffin volcano click mercy abuse bachelor ginger limb tomorrow okay input spend athlete boring security document exclude liar dune usage camera ranch thought"

let rootExtendedKey = ExtendedKey.fromMnemonicPhrase rootMnemonicPhrase |> Result.get

let rootPublicKey = Account.deriveZenKey rootExtendedKey |> Result.get |> ExtendedKey.getPublicKey |> Result.get

let rootPKHash = PublicKey.hash rootPublicKey

let private rng = new System.Security.Cryptography.RNGCryptoServiceProvider()

let create() =

    let seed = Array.create 32 0uy
    rng.GetBytes (seed)

    let extendedKey = ExtendedKey.create seed |> Result.get
    let zenKey = Account.deriveZenKey extendedKey |> Result.get

    {
        deltas = List.empty
        outputs = Map.empty
        publicKey = ExtendedKey.getPublicKey zenKey |> Result.get
        mempool = List.empty
        tip = Hash.zero
        blockNumber = 0ul
    }, extendedKey

let rootAccountData =
    {
        deltas = List.empty
        outputs = Map.empty
        publicKey = rootPublicKey
        tip = Hash.zero
        mempool= List.empty
        blockNumber=0ul
    }, rootExtendedKey

let addTransaction txHash tx accountData =
    let account = addTransaction txHash tx (fst accountData)
    account, snd accountData

let rootAmount = 100_000_000_000UL

let rootTx =
    {
        version = Version0
        inputs=[];
        outputs=[{lock = PK rootPKHash; spend= {asset = Asset.Zen;amount=rootAmount}}];
        witnesses=[]
        contract=None
    }

let rootTxHash = Transaction.hash rootTx

let createTestAccount () =
    let account =
        rootAccountData
        |> addTransaction rootTxHash rootTx

    account

let publicKeyHash account =
    PublicKey.hash account.publicKey

let getSecretKey extendedKey = Account.deriveZenKey extendedKey |> Result.get |> ExtendedKey.getPrivateKey |> Result.get

let rootKeyPair =
    (Account.deriveZenKey rootExtendedKey |> Result.get |> ExtendedKey.getPrivateKey |> Result.get, rootPublicKey)


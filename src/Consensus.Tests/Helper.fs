module Consensus.Tests.Helper

open Consensus
open Crypto
open Wallet
open Wallet.Account
open Infrastructure
open Consensus.Types

let rootSeed = [|189uy; 140uy; 82uy; 12uy; 79uy; 140uy; 35uy; 59uy; 11uy; 41uy; 199uy;
                           58uy; 23uy; 63uy; 112uy; 239uy; 45uy; 147uy; 51uy; 246uy; 34uy; 16uy;
                           156uy; 2uy; 111uy; 184uy; 140uy; 218uy; 136uy; 240uy; 57uy; 24uy |]

let rootExtendedKey = ExtendedKey.create rootSeed |> Result.get

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

let rootTx =
    {
        inputs=[];
        outputs=[{lock = PK rootPKHash; spend= {asset = Constants.Zen;amount=100000000UL}}];
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


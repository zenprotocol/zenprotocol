module Consensus.Tests.Helper

open Consensus
open Crypto
open Wallet
open Wallet.Account

let rootSecretKey = SecretKey [|189uy; 140uy; 82uy; 12uy; 79uy; 140uy; 35uy; 59uy; 11uy; 41uy; 199uy;
                           58uy; 23uy; 63uy; 112uy; 239uy; 45uy; 147uy; 51uy; 246uy; 34uy; 16uy;
                           156uy; 2uy; 111uy; 184uy; 140uy; 218uy; 136uy; 240uy; 57uy; 24uy |]

let create() = 
    let keyPair = KeyPair.create()
    {
        deltas = List.empty
        outputs = Map.empty
        publicKey = snd keyPair
        mempool = List.empty
        tip = Hash.zero
        blockNumber = 0ul
    }, fst keyPair 

let rootAccountData =
    {
        deltas = List.empty
        outputs = Map.empty
        publicKey = SecretKey.getPublicKey rootSecretKey |> Option.get
        tip = Hash.zero
        mempool= List.empty
        blockNumber=0ul
    }, rootSecretKey

let addTransaction txHash tx accountData =
    let account = addTransaction txHash tx (fst accountData)
    account, snd accountData
    
let createTestAccount () =
    let account =
        rootAccountData
        |> addTransaction Transaction.rootTxHash Transaction.rootTx

    account
    
let publicKeyHash account =
    PublicKey.hash account.publicKey 
    
let keyPair account = 
    (rootSecretKey, account.publicKey)
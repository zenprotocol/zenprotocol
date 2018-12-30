namespace Api.Types

open FSharp.Data
open System

type BalanceResponseJson = JsonProvider<"""
[
    {
        "asset": "asset",
        "balance": 2147483649
    }
]
""">

type ContractActivateResponseJson = JsonProvider<"""
{
    "address" : "address",
    "contractId" : "contractId"
}
""">

type CreateRawTransactionJson = JsonProvider<"""
{
    "outputs": [{
        "address": "address",
        "asset": "asset",
        "amount": 2147483649
    }]
}
""">

type RawTransactionResultJson = JsonProvider<"""
{
    "txHash" : "abcd123456",
    "tx": "abcd123456"
}
""">

type SignRawTransactionJson = JsonProvider<"""
{
    "tx":"abcdef1234",
    "password":"ab1234"
}
""">

type SendRequestJson = JsonProvider<"""
{
    "outputs": [{
        "address": "address",
        "asset": "asset",
        "amount": 2147483649
    }],
    "password": "password"
}
""">

type ContractActivateRequestJson = JsonProvider<"""
{
    "code": "string",
    "numberOfBlocks": 1,
    "password": "password"
}
""">

type ContractExtendRequestJson = JsonProvider<"""
{
    "address": "address",
    "numberOfBlocks": 1,
    "password": "password"
}
""">

type ContractExecuteRequestJson = JsonProvider<"""
{
    "address": "address",
    "command": "command",
    "messageBody": "message body",
    "options": {
        "returnAddress": true,
        "sign": "m/0/9'"
    },
    "spends": [
        {
            "asset": "asset",
            "amount": 2147483649
        }
    ],
    "password": "password"
}
""">

type PublishBlockJson = JsonProvider<"""
{
    "block": "somehex"
}
""">

type SubmitBlockHeaderJson = JsonProvider<"""
{
    "header": "somehex"
}
""">

type ActiveContractsResponseJson = JsonProvider<"""
[
    {
        "contractId": "contractId",
        "address":"address",
        "expire": 555,
        "code": "printfn hello world"
    }
]
""">

type BlockChainInfoJson = JsonProvider<"""
{
    "chain": "string",
    "blocks":100,
    "headers":100,
    "difficulty":0.1,
    "medianTime":2147483648,
    "initialBlockDownload":true,
    "tip": "hash"
}""">

type ImportSeedJson = JsonProvider<"""
{
    "password": "secret",
    "words" : [ "word" ]
}
""">

type TransactionsResponseJson = JsonProvider<"""
[
    {
        "txHash": "hash",
        "asset":"asset",
        "amount":2147483453648,
        "confirmations": 0,
        "lock": "lock"
    }
]
""">

type HeadersResponseJson = JsonProvider<"""
[
    {
        "hash":"ab1234",
        "timestamp":2147483648,
        "date":"date",
        "blockNumber":1,
        "difficulty":"0x12345678",
        "target":"abcd1234"
    }
]
""">

type GetPublicKeyJson = JsonProvider<"""
{
    "path": "path",
    "password": "secret"
}
""">

type SignJson = JsonProvider<"""
{
    "message": "txhash",
    "path": "path",
    "password": "secret"
}
""">

type CheckPasswordJson = JsonProvider<"""
{
    "password": "secret"
}
""">

type BlockHeaderJson = JsonProvider<"""
{
    "version": 2,
    "parent": "0000ab12ab120123ab12ab120123ab12ab120123ab12ab120123ab12ab120123",
    "blockNumber": 4,
    "commitments": "0abc...0000",
    "timestamp": 1234567890000,
    "difficulty": 1234567890,
    "nonce": [1234567890000,1234567890000]
}
""">

type PKLockJson = JsonProvider<"""
{
  "hash": "12..34",
  "address":"zen1adb"
}
""">

type ContractLockJson = JsonProvider<"""
{
  "id":"c234234",
  "address":"czen1adb"
}
""">

type CoinbaseLockJson = JsonProvider<"""
{
  "blockNumber":100, 
  "pkHash":"12..34", 
  "address":"zen1adb"
}
""">

type ExtensionSacrificeLockJson = JsonProvider<"""
{
  "id":"c234234",
  "address":"czen1adb"
}
""">

type HighVLockLockJson = JsonProvider<"""
{
  "identifier":56, 
  "data":"12..45"
}
""">

type PayoutRequestJson = JsonProvider<"""
{
    "payout": {},
    "password": "password"
}
""">

type PayoutResultJson = JsonProvider<"""
{
    "recipient": "abcd1234",
     "amount": 1234567890000
}
""">

type AllocationRequestJson = JsonProvider<"""
{
    "allocation": 50,
    "password": "password"
}
""">

type SpendJson = JsonProvider<"""
{
    "asset": "c234",
    "amount": 1234567890000
}
""">

type OutpointJson = JsonProvider<"""
{
    "txHash": "12..34",
    "index": 1234
}
""">

type ImportAddressResultJson = JsonProvider<"""
{
    "address":"address",
    "index":1234
}""">

type ReceivedByAddressJson = JsonProvider<"""
{
    "address":"address",
    "asset":"asset",
    "amount":1234567890000
}
""">

type AddressOutputJson = JsonProvider<"""
{
    "outpoint":{
        "txHash":"abbbb",
        "index": 0
    },
    "asset":"abcd",
    "amount":1234567890000,
    "confirmations":0,
    "spent":true
}
""">

type BlockTemplateJson = JsonProvider<"""
{
    "header": "abcdef012346789",
    "body": "abcdef012346789",
    "target": "abcdef012346789",
    "parent": "abcdef012346789",
    "blockNumber": 1
}
""">

type AddressJson = JsonProvider<"""
{
    "address":"address"
}
""">

type TxHexJson = JsonProvider<"""
{
    "tx":"abcde1234"
}
""">

type RestoreNewAddressesJson = JsonProvider<"""
{
    "max":1234
}
""">

type ImportZenPublicKey = JsonProvider<"""
{
    "publicKey":"abcd1234"
}""">

type GetBalanceJson = JsonProvider<"""
{
    "addresses": ["abcd1234", "abcd1234" ]
}""">

type GetHistoryJson = JsonProvider<"""
{
    "addresses": ["abcd1234", "abcd1234" ],
    "skip": 1000,
    "take": 1000
}""">

type GetOutputsJson = JsonProvider<"""
{
    "addresses": ["abcd1234", "abcd1234" ],
    "mode": "all"
}""">

type ContractExecuteFromTransactionJson = JsonProvider<"""
{
    "address": "address",
    "command": "command",
    "messageBody": "message body",
    "options": {
        "sender": "publickey"
    },
    "tx": "abcdef012346789"
}
""">

type GetContractHistoryJson = JsonProvider<"""
{
    "contractId": "abcd1234",
    "skip": 1000,
    "take": 1000
}""">

type ContractCommandHistoryResultJson = JsonProvider<"""
[{
    "command": "command",
    "messageBody": "abcdef012346789",
    "txHash": "abcdef012346789",
    "confirmations": 0
}]""">

type PublicKeyDataJson = JsonProvider<"""
{
    "publicKey": "address",
    "path": "path"
}
""">

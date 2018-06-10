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

type SendRequestJson = JsonProvider<"""
{
    "address": "address",
    "asset": "asset",
    "amount": 2147483649,
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
    "data": "data",
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
    "initialBlockDownload":true
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
        "confirmations": 0
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

type LockJson = JsonProvider<"""
[
    {
        "PK": {"hash": "12..34"}
    },
    {
        "Contract": {"id":"c234234"}
    },
    {
        "Coinbase": {"blockNumber":100, "pkHash":"12..34"}
    },
    "Fee",
    "ActivationSacrifice",
    {
        "ExtensionSacrifice": {"id":"c234234"}
    },
    "Destroy",
    {
        "HighVLock": {"identifier":56, "data":"12..45"}
    }
]
""", SampleIsList=true>

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
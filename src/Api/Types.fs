namespace Api.Types

open FSharp.Data
open System

type BalanceResponseJson = JsonProvider<"""
[
    {
        "asset": "hash",
        "assetType": "hash",
        "balance": 2147483649
    }
]
""">

type ContractActivateResponseJson = JsonProvider<"""
{
    "address" : "address",
    "hash" : "hash"
}
""">

type SendRequestJson = JsonProvider<"""
{
    "address": "address",
    "spend": {
        "asset": "hash",
        "assetType": "hash",
        "amount": 2147483649
    },
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
            "asset": "hash",
            "assetType": "hash",
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
        "contractHash": "hash",
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
    "medianTime":2147483648
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
        "deltas": [
            {
                "asset": "hash",
                "assetType": "hash",
                "amount": -2147483453648
            }
        ]
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

type CheckPasswordJson = JsonProvider<"""
{
    "password": "secret"
}
""">

namespace Api.Types

open FSharp.Data

type BalanceResponseJson = JsonProvider<"""
[
    {
        "asset": "hash",
        "assetType": "hash",
        "balance": 2147483649
    }
]
""">

type AddressJson = JsonProvider<"""
{
    "address" : "address"
}
""">

type ContractActivateResponseJson = JsonProvider<"""
{
    "address" : "address",
    "hash" : "hash"
}
""">

type SpendRequestJson = JsonProvider<"""
{
    "address": "address",
    "spend": {
        "asset": "hash",
        "assetType": "hash",
        "amount": 2147483649
    }
}
""">

type ContractActivateRequestJson = JsonProvider<"""
{
    "code": "string",
    "numberOfBlocks": 1
}
""">

type ContractExecuteRequestJson = JsonProvider<"""
{
    "address": "address",
    "command": "command",
    "data": "data",
    "options": {
        "returnAddress": true
    },
    "spends": [
        {
            "asset": "hash",
            "assetType": "hash",
            "amount": 2147483649
        }
    ]
}
""">

type PublishBlockJson = JsonProvider<"""
{
    "block": "somehex"
}
""">

type ActiveContractsJson = JsonProvider<"""
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
    "key": "secret",
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

type UnlockAccountJson = JsonProvider<"""
{
    "key": "secret"
}
""">

type AccountExistsResponseJson = JsonProvider<"""
{
    "accountExists" : false
}
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

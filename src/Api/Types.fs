namespace Api.Types

open FSharp.Data

type BalanceResponseJson = JsonProvider<"""
[
    {
        "asset": "hash",
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
        "amount": 2147483649
    }
}
""">

type ContractActivateRequestJson = JsonProvider<"""
{
    "code": "string"
}
""">

type ContractExecuteRequestJson = JsonProvider<"""
{
    "address": "address",
    "command": "command",
    "data": "data",
    "spends": [
        {
            "asset": "hash",
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
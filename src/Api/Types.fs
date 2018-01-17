namespace Api.Types

open FSharp.Data

type BalanceJson = JsonProvider<"""
[
    {
        "asset": "hash",
        "balance": 2147483649
    }
]
""">

type AddressJson = JsonProvider<"""
{
    "address":"address"
}
""">   
 
type SpendJson = JsonProvider<"""
{
    "address": "address",
    "spend": {
        "asset": "hash",
        "amount": 2147483649
    }
}
""">

type ContractActivateJson = JsonProvider<"""
{
    "code": "string"
}
""">

type ContractExecuteJson = JsonProvider<"""
{
    "address": "address",
    "spends": [
        {
            "asset": "hash",
            "amount": 2147483649
        }
    ]
}
""">

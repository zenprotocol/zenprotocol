namespace Api.Types

open FSharp.Data

type BalanceJson = JsonProvider<"""[{"asset": "hash", "balance": 2147483649}]""">
type AddressJson = JsonProvider<"""{"address":"address"}""">    
type TransactionSendJson = JsonProvider<"""{"asset": "hash", "amount": 2147483649, "to": "address"}""">
type ContractActivationJson = JsonProvider<"""{"code": "string"}""">
type ContractMessageSendJson = JsonProvider<"""{"asset": "hash", "amount": 2147483649, "to": "address"}""">

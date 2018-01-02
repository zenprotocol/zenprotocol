module Messaging.Services

open Consensus
open Consensus.Types
open Consensus.TxSkeleton
open Infrastructure.ServiceBus.Client

module Blockchain = 
    let serviceName = "blockchain"

    type Command = 
        | ValidateTransaction of Types.Transaction
        | GetMemPool of peerId:byte[]
        | GetTransaction of peerId:byte[] * txHash:Hash.Hash
        | HandleMemPool of peerId:byte[] * Hash.Hash list

    type Request = 
        | ExecuteContract of (TxSkeleton * Hash.Hash)

    type Response = unit
        
    let validateTransaction client tx = 
        Command.send client serviceName (ValidateTransaction tx)
    
    let getMemPool client peerId = 
        Command.send client serviceName (GetMemPool peerId)
        
    let getTransaction client peerId txHash = 
        Command.send client serviceName (GetTransaction (peerId,txHash))
    
    let handleMemPool client peerId txHashes =
        Command.send client serviceName (HandleMemPool (peerId,txHashes))                                

    let executeContract client cHash txSkeleton = 
        ExecuteContract (txSkeleton, cHash)
        |> Request.send<Request, Result<TxSkeleton, string>> client serviceName

module Network =
    type Command = 
        | SendMemPool of peerId:byte[] * Hash.Hash list
        | SendTransaction of peerId:byte[] * Transaction 
        | GetTransaction of peerId:byte[] * Hash.Hash
                   
    type Request = unit
                
    type Response = unit        

    let serviceName = "network"       
    
module Wallet = 
    type Command = unit
    
    type Request = 
        | GetAddress
        | GetBalance
        | CreateTransaction of address:string * asset:Hash.Hash * amount:uint64
        | CreateContractActivationTransaction of code:string
                              
    type CreateTransactionResult =
        | Created of Transaction
        | Error of string                             
                                         
    let serviceName = "wallet"
    
    let getBalance client =
        Request.send<Request, Map<Hash.Hash,uint64>> client serviceName GetBalance
        
    let getAddress client =
        Request.send<Request, string> client serviceName GetAddress
        
    let createTransaction client address asset amount =
        Request.send<Request, CreateTransactionResult> client serviceName (CreateTransaction (address,asset,amount))

    let createContractActivationTransaction client code =
        Request.send<Request, CreateTransactionResult> client serviceName (CreateContractActivationTransaction (code))
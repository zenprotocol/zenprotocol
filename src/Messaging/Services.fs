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
        | GetBlock of peerId:byte[] * blockHash:Hash.Hash
        | GetTip of peerId:byte[]
        | HandleMemPool of peerId:byte[] * Hash.Hash list
        | HandleBlockHeader of Types.BlockHeader
        | ValidateBlock of Types.Block

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
        
    let validateBlock client block = 
        ValidateBlock block 
        |> Command.send client serviceName
        
    let handleBlockHeader client header = 
        HandleBlockHeader header 
        |> Command.send client serviceName 
                
    let getBlock client peerId blockHash = 
        GetBlock (peerId,blockHash)
        |> Command.send client serviceName
        
    let getTip client peerId =
        GetTip peerId         
        |> Command.send client serviceName
        
module Network =
    type Command = 
        | SendMemPool of peerId:byte[] * Hash.Hash list
        | SendTransaction of peerId:byte[] * Transaction 
        | SendTip of peerId:byte[] * BlockHeader
        | SendBlock of peerId:byte[] * Block
        | GetTransaction of peerId:byte[] * Hash.Hash
        | GetBlock of Hash.Hash 
                   
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
        | CreateSendMessageTranscation of address:string * asset:Hash.Hash * amount:uint64
                              
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

    let createSendMessageTranscation client address asset amount =
        Request.send<Request, CreateTransactionResult> client serviceName (CreateSendMessageTranscation (address,asset,amount))

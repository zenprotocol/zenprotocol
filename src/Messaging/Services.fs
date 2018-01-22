module Messaging.Services

open Consensus
open Types
open Hash
open TxSkeleton
open Infrastructure.ServiceBus.Client

type TransactionResult =
    | Ok of Transaction
    | Error of string                             

module Blockchain = 
    let serviceName = "blockchain"

    type Command = 
        | ValidateTransaction of Types.Transaction
        | RequestMemPool of peerId:byte[]
        | RequestTransaction of peerId:byte[] * txHash:Hash.Hash
        | RequestBlock of peerId:byte[] * blockHash:Hash.Hash
        | RequestTip of peerId:byte[]
        | HandleMemPool of peerId:byte[] * Hash.Hash list
        | HandleTip of Types.BlockHeader
        | ValidateNewBlockHeader of peerId:byte[] * Types.BlockHeader
        | ValidateBlock of Types.Block
        | ValidateMinedBlock of Types.Block

    type Request = 
        | ExecuteContract of TxSkeleton * Hash.Hash
        | GetBlockTemplate

    type Response = unit
        
    let validateTransaction client tx = 
        Command.send client serviceName (ValidateTransaction tx)
    
    let requestMemPool client peerId = 
        Command.send client serviceName (RequestMemPool peerId)
        
    let requestTransaction client peerId txHash = 
        Command.send client serviceName (RequestTransaction (peerId,txHash))
    
    let handleMemPool client peerId txHashes =
        Command.send client serviceName (HandleMemPool (peerId,txHashes))                                                

    let executeContract client cHash txSkeleton = 
        ExecuteContract (txSkeleton, cHash)
        |> Request.send<Request, TransactionResult> client serviceName
        
    let validateBlock client block = 
        ValidateBlock block 
        |> Command.send client serviceName
    
    let validateMinedBlock client block = 
        ValidateMinedBlock block 
        |> Command.send client serviceName
    
    let handleTip client header = 
        HandleTip header 
        |> Command.send client serviceName
        
    let validateNewBlockHeader client peerId header = 
        ValidateNewBlockHeader (peerId,header)
        |> Command.send client serviceName 
                
    let requestBlock client peerId blockHash = 
        RequestBlock (peerId,blockHash)
        |> Command.send client serviceName
        
    let requestTip client peerId =
        RequestTip peerId         
        |> Command.send client serviceName
        
    let getBlockTemplate client = 
        Request.send<Request,Block option> client serviceName GetBlockTemplate // TODO: we return an option because for now we cannot mine empty blocks        
        
module Network =
    type Command = 
        | SendMemPool of peerId:byte[] * Hash.Hash list
        | SendTransaction of peerId:byte[] * Transaction 
        | SendTip of peerId:byte[] * BlockHeader
        | SendBlock of peerId:byte[] * Block
        | GetTransaction of peerId:byte[] * Hash.Hash
        | GetBlock of Hash.Hash 
        | GetNewBlock of peerId:byte[] * Hash.Hash
        | PublishBlock of BlockHeader
                   
    type Request = unit
                
    type Response = unit        

    let serviceName = "network"       
    
module Wallet = 
    type Command = unit
    
    type Request = 
        | GetAddress
        | GetBalance
        | Spend of Hash * Spend
        | ActivateContract of string
        | ExecuteContract of Hash * Map<Hash.Hash, uint64>

    let serviceName = "wallet"
    
    let getBalance client =
        Request.send<Request, Map<Hash.Hash,uint64>> client serviceName GetBalance
        
    let getAddress client =
        Request.send<Request, string> client serviceName GetAddress
        
    let createTransaction client address spend =
        Request.send<Request, TransactionResult> client serviceName (Spend (address, spend))

    let activateContract client code =
        Request.send<Request, TransactionResult> client serviceName (ActivateContract (code))

    let executeContract client address spends =
        Request.send<Request, TransactionResult> client serviceName (ExecuteContract (address, spends))

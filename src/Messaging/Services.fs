module Messaging.Services

open Consensus
open Types
open Hash
open TxSkeleton
open Infrastructure.ServiceBus.Client


type TransactionResult = Result<Transaction,string>

type ActivateContractTransactionResult = Result<Transaction * Hash, string>

module Blockchain =
    let serviceName = "blockchain"

    type ActiveContract = {
        contractHash:Hash
        code:string
        expiry:uint32
    }

    type BlochChainInfo = {
        chain:string
        blocks:uint32
        headers:uint32
        difficulty:float
        medianTime:uint64
    }

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
        | ExecuteContract of Hash.Hash * string * Data * Lock * TxSkeleton.T
        | GetBlockTemplate of pkHash:Hash.Hash
        | GetTip
        | GetBlock of Hash.Hash
        | GetBlockHeader of Hash.Hash
        | GetActiveContracts
        | GetBlockChainInfo

    type Response = unit

    let validateTransaction client tx =
        Command.send client serviceName (ValidateTransaction tx)

    let requestMemPool client peerId =
        Command.send client serviceName (RequestMemPool peerId)

    let requestTransaction client peerId txHash =
        Command.send client serviceName (RequestTransaction (peerId,txHash))

    let handleMemPool client peerId txHashes =
        Command.send client serviceName (HandleMemPool (peerId,txHashes))

    let executeContract client cHash command data returnAddress txSkeleton =
        ExecuteContract (cHash,command, data, returnAddress,txSkeleton)
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

    let getBlockTemplate client pkHash =
        GetBlockTemplate pkHash
        |> Request.send<Request,Block> client serviceName

    let getBlockHeader client blockHash =
        Request.send<Request,BlockHeader option> client serviceName (GetBlockHeader blockHash)

    let getBlock client blockHash =
        Request.send<Request,Block option> client serviceName (GetBlock blockHash)

    let getTip client =
        Request.send<Request,(Hash.Hash*BlockHeader) option> client serviceName GetTip

    let getActiveContracts client =
        Request.send<Request,ActiveContract list> client serviceName GetActiveContracts

    let getBlockChainInfo client =
        Request.send<Request,BlochChainInfo> client serviceName GetBlockChainInfo

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

    type Request =
        GetConnectionCount

    type Response = unit

    let serviceName = "network"

    let getConnectionCount client =
        Request.send<Request, uint32> client serviceName GetConnectionCount

module Wallet =
    type Command = unit

    type Request =
        | GetAddressPKHash
        | GetAddress
        | GetBalance
        | Spend of Hash * Spend
        | ActivateContract of string*uint32
        | ExecuteContract of Hash * string * Data * Map<Asset, uint64>

    let serviceName = "wallet"

    let getBalance client =
        Request.send<Request, Map<Asset,uint64>> client serviceName GetBalance

    let getAddressPKHash client =
        Request.send<Request, Hash.Hash> client serviceName GetAddressPKHash

    let getAddress client =
        Request.send<Request, string> client serviceName GetAddress


    let createTransaction client address spend =
        Request.send<Request, TransactionResult> client serviceName (Spend (address, spend))

    let activateContract client code numberOfBlocks =
        Request.send<Request, ActivateContractTransactionResult> client serviceName (ActivateContract (code,numberOfBlocks))

    let executeContract client address command data spends =
        Request.send<Request, TransactionResult> client serviceName (ExecuteContract (address,command,data,spends))

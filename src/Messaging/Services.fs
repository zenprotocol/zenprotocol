module Messaging.Services

open Consensus
open Types
open Hash
open Zen.Types.Data
open Infrastructure.ServiceBus.Client

module Blockchain =
    let serviceName = "blockchain"

    type ActiveContract = {
        contractId:ContractId
        code:string
        expiry:uint32
    }

    type BlockchainInfo = {
        chain:string
        blocks:uint32
        headers:uint32
        difficulty:float
        medianTime:uint64
        initialBlockDownload:bool
    }

    type Command =
        | ValidateTransaction of Types.Transaction
        | RequestMemPool of peerId:byte[]
        | RequestTransaction of peerId:byte[] * txHash:Hash
        | RequestBlock of peerId:byte[] * blockHash:Hash
        | RequestTip of peerId:byte[]
        | HandleMemPool of peerId:byte[] * Hash list
        | HandleTip of peerId:byte[] * Types.BlockHeader
        | ValidateNewBlockHeader of peerId:byte[] * Types.BlockHeader
        | ValidateBlock of peerId:byte[] * Types.Block
        | ValidateMinedBlock of Types.Block
        | RequestHeaders of peerId:byte[] * startBlockHashes:Hash list * endBlockHash :Hash
        | HandleHeaders of peerId:byte[] * BlockHeader list
        | HandleNewTransaction of peerId:byte[] * txHash:Hash

    type Request =
        | ExecuteContract of ContractId * string * Crypto.PublicKey option * data option * TxSkeleton.T
        | GetBlockTemplate of pkHash:Hash
        | GetTip
        | GetBlock of Hash
        | GetBlockByNumber of uint32
        | GetBlockHeader of Hash
        | GetActiveContracts
        | GetActiveContract of ContractId
        | GetBlockChainInfo
        | GetHeaders
        | GetMempool
        | GetTransaction of Hash

    type Response = unit

    let validateTransaction client tx =
        Command.send client serviceName (ValidateTransaction tx)

    let requestMemPool client peerId =
        Command.send client serviceName (RequestMemPool peerId)

    let requestTransaction client peerId txHash =
        Command.send client serviceName (RequestTransaction (peerId,txHash))

    let handleMemPool client peerId txHashes =
        Command.send client serviceName (HandleMemPool (peerId,txHashes))

    let executeContract client contractId command sender data txSkeleton =
        ExecuteContract (contractId,command, sender, data, txSkeleton)
        |> Request.send<Request, Result<Transaction,string>> client serviceName

    let validateBlock client peerId block =
        ValidateBlock (peerId, block)
        |> Command.send client serviceName

    let validateMinedBlock client block =
        ValidateMinedBlock block
        |> Command.send client serviceName

    let handleTip client peerId header =
        HandleTip (peerId, header)
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

    let getTransaction client txHash =
        Request.send<Request,(Transaction*uint32) option> client serviceName (GetTransaction txHash)

    let getBlockByNumber client blockNumber =
        Request.send<Request,Block option> client serviceName (GetBlockByNumber blockNumber)

    let getTip client =
        Request.send<Request,(Hash*BlockHeader) option> client serviceName GetTip

    let getActiveContracts client =
        Request.send<Request,ActiveContract list> client serviceName GetActiveContracts

    let getActiveContract client contractId =
        Request.send<Request,ActiveContract option> client serviceName (GetActiveContract contractId)

    let getBlockChainInfo client =
        Request.send<Request,BlockchainInfo> client serviceName GetBlockChainInfo

    let requestHeaders client peerId startBlockHash endBlockHash=
        RequestHeaders (peerId,startBlockHash,endBlockHash) |> Command.send client serviceName

    let handleHeaders client peerId headers =
        HandleHeaders (peerId,headers) |> Command.send client serviceName

    let getHeaders client =
        GetHeaders |> Request.send<Request, BlockHeader list> client serviceName

    let getMempool client =
        GetMempool |> Request.send<Request, (Hash.Hash * Transaction) list> client serviceName

    let handleNewTransaction client peerId txHash =
        HandleNewTransaction (peerId,txHash)
        |> Command.send client serviceName

module Network =
    type Command =
        | SendMemPool of peerId:byte[] * Hash list
        | SendTransaction of peerId:byte[] * Transaction
        | SendTip of peerId:byte[] * BlockHeader
        | SendBlock of peerId:byte[] * Block
        | GetTransaction of peerId:byte[] * Hash
        | GetBlock of Hash
        | GetBlockFrom of peerId:byte[] * Hash
        | PublishBlock of BlockHeader
        | GetHeaders of peerId:byte[] * from:Hash list * toHash:Hash
        | SendHeaders of peerId:byte[] * BlockHeader list
        | DisconnectPeer of peerId:byte[]
        | GetTipFromAllPeers

    type Request =
        GetConnectionCount

    type Response = unit

    let serviceName = "network"

    let getConnectionCount client =
        Request.send<Request, uint32> client serviceName GetConnectionCount

module Wallet =
    type BalanceResponse = Map<Asset,uint64>

    type TransactionDirection =
        | In
        | Out

    type TransactionsResponse = List<Hash*TransactionDirection*Spend*uint32>
    type ActivateContractResponse = Transaction * ContractId

    type Command =
        | Resync

    type Request =
        | GetAddressPKHash
        | GetAddress
        | GetTransactions of skip: int * take: int
        | GetBalance
        | ImportSeed of string list * password:string
        | Send of Hash * Spend * password:string
        | ActivateContract of string * uint32 * password:string
        | ExtendContract of ContractId * uint32 * password:string
        | ExecuteContract of ContractId * string * data option * provideReturnAddress:bool * sign:string option * Map<Asset, uint64> * password:string
        | AccountExists
        | CheckPassword of password:string
        | GetPublicKey of path:string * password:string
        | GetMnemonicPhrase of password:string
        | Sign of Hash * path:string * password:string
        | ImportWatchOnlyAddress of string
        | GetNewAddress
        | GetReceivedByAddress of confirmations:uint32
        | GetAddressOutputs of address:string
        | GetAddressBalance of address:string * confirmations:uint32

    let serviceName = "wallet"

    //TODO: apply same convention to other services
    let private send<'a> = Request.send<Request, Result<'a,string>>

    let getBalance client =
        send<BalanceResponse> client serviceName GetBalance

    let getAddressPKHash client =
        send<Hash> client serviceName GetAddressPKHash

    let getAddress client =
        send<string> client serviceName GetAddress

    let createTransaction client address spend password =
        send<Transaction> client serviceName (Send (address, spend, password))

    let activateContract client code numberOfBlocks password =
        send<ActivateContractResponse> client serviceName (ActivateContract (code, numberOfBlocks, password))

    let extendContract client address numberOfBlocks password =
        send<Transaction> client serviceName (ExtendContract (address, numberOfBlocks, password))

    let executeContract client address command data provideReturnAddress sign spends password =
        send<Transaction> client serviceName (ExecuteContract (address, command, data, provideReturnAddress, sign, spends, password))

    let importSeed client words password =
        send<unit> client serviceName (ImportSeed (words, password))

    let getTransactions client skip take =
        send<TransactionsResponse> client serviceName (GetTransactions (skip, take))

    let accountExists client =
        send<bool> client serviceName AccountExists

    let checkPassword client password =
        send<bool> client serviceName (CheckPassword password)

    let resyncAccount client =
        Command.send client serviceName Resync

    let getPublicKey client path password =
        Request.send<Request, Result<Crypto.PublicKey,string>> client serviceName (GetPublicKey (path, password))

    let sign client message path password =
        Request.send<Request, Result<Crypto.Signature,string>> client serviceName (Sign (message, path, password))

    let getMnemonicPhrase client password =
        Request.send<Request, Result<string, string>> client serviceName (GetMnemonicPhrase password)

    let importWatchOnlyAddress client address =
        Request.send<Request, Result<unit,string>> client serviceName (ImportWatchOnlyAddress address)

    let getNewAddress client =
        Request.send<Request, Result<string * int,string>> client serviceName GetNewAddress

    let getReceivedByAddress client confirmations =
        Request.send<Request, Result<Map<(string*Asset), uint64>,string>> client serviceName (GetReceivedByAddress confirmations)

    let getAddressOutputs client address =
        Request.send<Request, Result<List<(Outpoint*Spend*uint32*bool)>,string>> client serviceName (GetAddressOutputs address)

    let getAddressBalance client address confirmations =
        Request.send<Request, Result<Map<Asset, uint64>,string>> client serviceName (GetAddressBalance (address,confirmations))
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
        tipBlockHash:Hash.Hash
    }

    type Command =
        | ValidateTransaction of Types.TransactionExtended
        | RequestMemPool of peerId:byte[]
        | RequestTransactions of peerId:byte[] * txHashes:Hash list
        | RequestBlock of peerId:byte[] * blockHash:Hash
        | RequestTip of peerId:byte[]
        | HandleMemPool of peerId:byte[] * Hash list
        | HandleTip of peerId:byte[] * Types.BlockHeader
        | ValidateNewBlockHeader of peerId:byte[] * Types.BlockHeader
        | ValidateBlock of peerId:byte[] * Types.Block
        | ValidateMinedBlock of Types.Block
        | RequestHeaders of peerId:byte[] * startBlockHashes:Hash list * endBlockHash :Hash
        | HandleHeaders of peerId:byte[] * BlockHeader list
        | HandleNewTransactions of peerId:byte[] * txHashes:Hash list

    type Request =
        | ExecuteContract of ContractId * string * Crypto.PublicKey option * data option * TxSkeleton.T
        | GetBlockTemplate of pkHash:Hash
        | GetTip
        | GetBlock of mainChain:bool * Hash
        | GetBlockByNumber of uint32
        | GetBlockHeader of Hash
        | GetAllBlocks of from: int
        | GetBlocks of blockNumber: int * take: int
        | GetActiveContracts
        | GetActiveContract of ContractId
        | GetBlockChainInfo
        | GetAllHeaders
        | GetHeaders of blockNumber: int * take: int
        | GetMempool
        | GetTransaction of Hash
        | CheckTransaction of TransactionExtended
        | GetTotalZP
        | GetCandidates of uint32
        | GetBlockReward of uint32
        | GetCGP
        | GetWinner
        | GetCgpHistory

    type Response = unit
    
    let private sendCommand client command =
        Command.send client serviceName command
        
    let private sendRequest<'a> client =
        Request.send<Request, 'a> client serviceName

    let validateTransaction client tx =
        sendCommand client (ValidateTransaction tx)

    let requestMemPool client peerId =
        sendCommand client (RequestMemPool peerId)

    let requestTransactions client peerId txHashes =
        sendCommand client (RequestTransactions (peerId,txHashes))

    let handleMemPool client peerId txHashes =
        sendCommand client (HandleMemPool (peerId,txHashes))

    let executeContract client contractId command sender messageBody txSkeleton =
        ExecuteContract (contractId,command, sender, messageBody, txSkeleton)
        |> sendRequest<Result<Transaction,string>> client

    let validateBlock client peerId block =
        ValidateBlock (peerId, block)
        |> sendCommand client

    let validateMinedBlock client block =
        ValidateMinedBlock block
        |> sendCommand client

    let handleTip client peerId header =
        HandleTip (peerId, header)
        |> sendCommand client

    let validateNewBlockHeader client peerId header =
        ValidateNewBlockHeader (peerId,header)
        |> sendCommand client

    let requestBlock client peerId blockHash =
        RequestBlock (peerId,blockHash)
        |> sendCommand client

    let requestTip client peerId =
        RequestTip peerId
        |> sendCommand client

    let getBlockTemplate client pkHash =
        GetBlockTemplate pkHash
        |> sendRequest<Block> client

    let tryGetBlockTemplate client pkHash =
        GetBlockTemplate pkHash
        |> Request.trySend<Request,Block> client serviceName

    let getAllBlocks client from =
        GetAllBlocks from
        |> sendRequest<Map<Hash.Hash, byte array>> client
    
    let getBlocks client take blockNumber  =
        GetBlocks (blockNumber, take)
        |> sendRequest<List<uint32 * byte array>> client
    
    let getBlockHeader client blockHash =
        GetBlockHeader blockHash
        |> sendRequest<BlockHeader option> client

    let getBlock client mainChain blockHash =
        GetBlock (mainChain,blockHash)
        |> sendRequest<Block option> client

    let getTransaction client txHash =
        GetTransaction txHash
        |> sendRequest<(Transaction*uint32) option> client

    let getBlockByNumber client blockNumber =
        GetBlockByNumber blockNumber
        |> sendRequest<Block option> client

    let getTip client =
        GetTip
        |> sendRequest<(Hash*BlockHeader) option> client

    let getActiveContracts client =
        GetActiveContracts
        |> sendRequest<ActiveContract list> client

    let getActiveContract client contractId =
        GetActiveContract contractId
        |> sendRequest<ActiveContract option> client

    let getBlockChainInfo client =
        GetBlockChainInfo
        |> sendRequest<BlockchainInfo> client

    let tryGetBlockChainInfo client =
        GetBlockChainInfo
        |> Request.trySend<Request,BlockchainInfo> client serviceName

    let requestHeaders client peerId startBlockHash endBlockHash=
        RequestHeaders (peerId,startBlockHash,endBlockHash)
        |> sendCommand client

    let handleHeaders client peerId headers =
        HandleHeaders (peerId,headers)
        |> sendCommand client

    let getAllHeaders client =
        GetAllHeaders
        |> sendRequest<BlockHeader list> client
        
    let getHeaders client take blockNumber  =
        GetHeaders (blockNumber, take)
        |> sendRequest<BlockHeader list> client

    let getMempool client =
        GetMempool
        |> sendRequest<(Hash.Hash * Transaction) list> client

    let handleNewTransactions client peerId txHashes =
        HandleNewTransactions (peerId,txHashes)
        |> sendCommand client

    let checkTransaction client transaction =
        CheckTransaction transaction
        |> sendRequest<Result<Hash,ValidationError.ValidationError>> client

    let getTotalZP client =
        GetTotalZP
        |> sendRequest<uint64> client
        
    let getCandidates client interval =
        GetCandidates interval
        |> sendRequest<List<Recipient * Spend list>> client

    let getBlockReward client blockNumber =
        GetBlockReward blockNumber
        |> sendRequest<uint64> client
        
    let getCgp client =
        GetCGP 
        |> sendRequest<CGP.T> client
        
    let getWinner client =
        GetWinner
        |> sendRequest<Winner option> client
        
    let getCgpHistory client =
        GetCgpHistory
        |> sendRequest<(uint32 * CGP.T) list> client

module Network =
    type Command =
        | SendMemPool of peerId:byte[] * Hash list
        | SendTransactions of peerId:byte[] * byte[] list
        | SendTip of peerId:byte[] * BlockHeader
        | SendBlock of peerId:byte[] * Block // TODO: send bytes instead
        | GetTransactions of peerId:byte[] * Hash list
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

    type TransactionsResponse = List<Hash*TransactionDirection*Spend*uint32*Lock>
    type ActivateContractResponse = Transaction * ContractId

    type Command =
        | Resync
        | RestoreNewAddresses of int32

    type Request =
        | GetAddressPKHash
        | GetAddress
        | GetTransactionCount
        | GetTransactions of skip: int * take: int
        | GetBalance
        | ImportSeed of string list * password:string
        | Send of publish:bool*outputs:List<Lock * Spend> * password:string
        | ActivateContract of publish:bool* code:string * numberOfBlocks:uint32 * rlimit:uint32 option * password:string
        | ExtendContract of publish:bool*ContractId * uint32 * password:string
        | ExecuteContract of publish:bool*ContractId * string * data option * provideReturnAddress:bool * sign:string option * Map<Asset, uint64> * password:string
        | ExecuteCGP of publish:bool * password:string
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
        | ExportZenPublicKey
        | ImportZenPublicKey of string
        | RemoveAccount of password:string
        | ChangePassword of oldPassword: string * newPassword: string 
        | RawTransactionCreate of outputs:List<Hash * Spend>
        | RawTransactionSign of RawTransaction * password:string
        | GetKeys of password:string
        | GetUtxo

        
    let serviceName = "wallet"

    let private sendCommand client command =
        Command.send client serviceName command
        
    let private sendRequest<'a> client =
        Request.send<Request, Result<'a,string>> client serviceName

    let getBalance client =
        GetBalance
        |> sendRequest<BalanceResponse> client

    let getAddressPKHash client =
        GetAddressPKHash
        |> sendRequest<Hash> client

    let getAddress client =
        GetAddress
        |> sendRequest<string> client

    let createRawTransaction client outputs =
        RawTransactionCreate outputs
        |> sendRequest<RawTransaction> client

    let signRawTransaction client rawTx password =
        RawTransactionSign (rawTx, password)
        |> sendRequest<RawTransaction> client

    let createTransaction client publish outputs password =
        Send (publish, outputs, password)
        |> sendRequest<Transaction> client

    let activateContract client publish code numberOfBlocks rlimit password =
        ActivateContract (publish, code, numberOfBlocks, rlimit, password)
        |> sendRequest<ActivateContractResponse> client

    let extendContract client publish address numberOfBlocks password =
        ExtendContract (publish, address, numberOfBlocks, password)
        |> sendRequest<Transaction> client

    let executeContract client publish address command messageBody provideReturnAddress sign spends password =
        ExecuteContract (publish, address, command, messageBody, provideReturnAddress, sign, spends, password)
        |> sendRequest<Transaction> client 

    let executeCGP client publish password =
        ExecuteCGP (publish, password)
        |> sendRequest<Transaction> client

    let importSeed client words password =
        ImportSeed (words, password)
        |> sendRequest<unit> client
        
    let getTransactionCount client =
        GetTransactionCount
        |> sendRequest<int> client

    let getTransactions client skip take =
        GetTransactions (skip, take)
        |> sendRequest<TransactionsResponse> client

    let accountExists client =
        AccountExists
        |> sendRequest<bool> client

    let checkPassword client password =
        CheckPassword password
        |> sendRequest<bool> client

    let resyncAccount client =
        Resync
        |> sendCommand client

    let getPublicKey client path password =
        GetPublicKey (path, password)
        |> sendRequest<Crypto.PublicKey> client

    let sign client message path password =
        Sign (message, path, password)
        |> sendRequest<Crypto.Signature> client

    let getMnemonicPhrase client password =
        GetMnemonicPhrase password
        |> sendRequest<string> client
        
    let changeSecure client oldPass newPass =
        ChangePassword (oldPass,newPass)
        |> sendRequest<unit> client

    let importWatchOnlyAddress client address =
        ImportWatchOnlyAddress address
        |> sendRequest<unit> client

    let getNewAddress client =
        GetNewAddress
        |> sendRequest<string * int> client

    let restoreNewAddresses client maxIndex =
        RestoreNewAddresses maxIndex
        |> sendCommand client

    let getReceivedByAddress client confirmations =
        GetReceivedByAddress confirmations
        |> sendRequest<Map<(string*Asset), uint64>> client

    let getAddressOutputs client address =
        GetAddressOutputs address
        |> sendRequest<List<(Outpoint*Spend*uint32*bool)>> client

    let getAddressBalance client address confirmations =
        GetAddressBalance (address,confirmations)
        |> sendRequest<Map<Asset, uint64>> client

    let exportZenPublicKey client =
        ExportZenPublicKey
        |> sendRequest<string> client

    let importZenPublicKey client publicKey =
        ImportZenPublicKey publicKey
        |> sendRequest<unit> client

    let removeAccount client password =
        RemoveAccount password
        |> sendRequest<unit> client

    let getKeys client password =
        GetKeys password
        |> sendRequest<Map<Crypto.PublicKey, string>> client
        
    let getUtxo client =
        GetUtxo
        |> sendRequest<List<PointedOutput>> client

module AddressDB =
    open Wallet
    type TransactionsResponse = List<Hash*TransactionDirection*Spend*uint32*uint64*Lock>
    
    type DiscoveryResponse = List<Recipient*int*List<Asset*uint64>> // txs, balance by address, asset, amount

    type ContractHistoryResponse = List<string * data option * Hash * uint32> // command, messageBody, txHash, confirmations

    type Mode =
        | All
        | UnspentOnly

    type Command =
        | Resync

    type Request =
        | GetBalance of addresses:string list * blockNumber:uint32 option
        | GetOutputs of addresses:string list * Mode
        | GetTransactions of addresses:string list * skip: int * take: int
        | GetTransactionsByBlockNumber of addresses:string list * start: uint32 * stop: uint32
        | GetContractHistory of contractId : ContractId * skip: int * take: int
        | GetContractHistoryByBlockNumber of contractId : ContractId * start: uint32 * stop: uint32
        | GetTransactionCount of addresses:string list * uint32
        | GetContractAssets of asset: Asset
        | GetContractInfo of code: string * rlimit: uint32 option
        | GetDiscovery of addresses: string list

    let serviceName = "addressDB"
    let resyncAccount client =
        Command.send client serviceName Resync

    let private sendRequest<'a> client = Request.send<Request, Result<'a,string>> client serviceName

    let getBalance client addresses blockNumber =
        GetBalance (addresses, blockNumber)
        |> sendRequest<BalanceResponse> client

    let getOutputs client args =
        GetOutputs args
        |> sendRequest<List<PointedOutput>> client

    let getTransactions client args =
        GetTransactions args
        |> sendRequest<TransactionsResponse> client
        
    let getDiscovery client args =
        GetDiscovery args
        |> sendRequest<DiscoveryResponse> client

    let getContractHistory client args =
        GetContractHistory args
        |> sendRequest<ContractHistoryResponse> client
        
    let getTransactionCount client args =
        GetTransactionCount args
        |> sendRequest<int> client
        
    let getContractAssets client args =
        GetContractAssets args
        |> sendRequest<option<uint32 * string option * string * Zen.Types.Data.data option>> client

    let getContractInfo client args =
        GetContractInfo args
        |> sendRequest<ContractId * ContractV0> client
        
    let getContractHistoryByBlockNumber client args =
        GetContractHistoryByBlockNumber args
        |> sendRequest<ContractHistoryResponse> client
    
    let getTransactionsByBlockNumber client addresses start stop =
        GetTransactionsByBlockNumber (addresses, start, stop)
        |> sendRequest<TransactionsResponse> client

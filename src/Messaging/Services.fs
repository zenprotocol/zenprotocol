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
        | GetActiveContracts
        | GetActiveContract of ContractId
        | GetBlockChainInfo
        | GetHeaders
        | GetMempool
        | GetTransaction of Hash
        | CheckTransaction of TransactionExtended
        | GetTotalZP
        | GetBlockReward of uint32
        | GetCGP
        | GetCgpHistory

    type Response = unit

    let validateTransaction client tx =
        Command.send client serviceName (ValidateTransaction tx)

    let requestMemPool client peerId =
        Command.send client serviceName (RequestMemPool peerId)

    let requestTransactions client peerId txHashes =
        Command.send client serviceName (RequestTransactions (peerId,txHashes))

    let handleMemPool client peerId txHashes =
        Command.send client serviceName (HandleMemPool (peerId,txHashes))

    let executeContract client contractId command sender messageBody txSkeleton =
        ExecuteContract (contractId,command, sender, messageBody, txSkeleton)
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

    let tryGetBlockTemplate client pkHash =
        GetBlockTemplate pkHash
        |> Request.trySend<Request,Block> client serviceName

    let getBlockHeader client blockHash =
        Request.send<Request,BlockHeader option> client serviceName (GetBlockHeader blockHash)

    let getBlock client mainChain blockHash =
        Request.send<Request,Block option> client serviceName (GetBlock (mainChain,blockHash))

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

    let tryGetBlockChainInfo client =
        Request.trySend<Request,BlockchainInfo> client serviceName GetBlockChainInfo

    let requestHeaders client peerId startBlockHash endBlockHash=
        RequestHeaders (peerId,startBlockHash,endBlockHash) |> Command.send client serviceName

    let handleHeaders client peerId headers =
        HandleHeaders (peerId,headers) |> Command.send client serviceName

    let getHeaders client =
        GetHeaders |> Request.send<Request, BlockHeader list> client serviceName

    let getMempool client =
        GetMempool |> Request.send<Request, (Hash.Hash * Transaction) list> client serviceName

    let handleNewTransactions client peerId txHashes =
        HandleNewTransactions (peerId,txHashes)
        |> Command.send client serviceName

    let checkTransaction client transaction =
        CheckTransaction transaction
        |> Request.send<Request, Result<Hash,ValidationError.ValidationError>> client serviceName

    let getTotalZP client =
        GetTotalZP
        |> Request.send<Request, uint64> client serviceName

    let getBlockReward client blockNumber =
        GetBlockReward blockNumber
        |> Request.send<Request, uint64> client serviceName
        
    let getCgp client =
        GetCGP 
        |> Request.send<Request, CGP.T> client serviceName
        
    let getCgpHistory client =
        GetCgpHistory
        |> Request.send<Request, CGP.T list> client serviceName

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
        | GetVoteUtilization
        | ImportSeed of string list * password:string
        | Send of publish:bool*outputs:List<Hash * Spend> * password:string
        | Vote of publish:bool * vote:VoteData * password:string 
        | ActivateContract of publish:bool*string * uint32 * password:string
        | ExtendContract of publish:bool*ContractId * uint32 * password:string
        | ExecuteContract of publish:bool*ContractId * string * data option * provideReturnAddress:bool * sign:string option * Map<Asset, uint64> * password:string
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
        | RawTransactionCreate of outputs:List<Hash * Spend>
        | RawTransactionSign of RawTransaction * password:string
        | GetKeys of password:string
        
    let serviceName = "wallet"

    //TODO: apply same convention to other services
    let private send<'a> = Request.send<Request, Result<'a,string>>

    let getBalance client =
        send<BalanceResponse> client serviceName GetBalance

    let getAddressPKHash client =
        send<Hash> client serviceName GetAddressPKHash

    let getAddress client =
        send<string> client serviceName GetAddress

    let createRawTransaction client outputs =
        RawTransactionCreate outputs
        |> send<RawTransaction> client serviceName

    let signRawTransaction client rawTx password =
        RawTransactionSign (rawTx, password)
        |> send<RawTransaction> client serviceName

    let createTransaction client publish outputs password =
        send<Transaction> client serviceName (Send (publish, outputs, password))
    
    let createVoteTransaction client publish voteData password =
        send<Transaction> client serviceName (Vote (publish, voteData,password))
        
    let getVoteUtilization client =
        send<uint64 * uint64 * VoteData option> client serviceName GetVoteUtilization

    let activateContract client publish code numberOfBlocks password =
        send<ActivateContractResponse> client serviceName (ActivateContract (publish, code, numberOfBlocks, password))

    let extendContract client publish address numberOfBlocks password =
        send<Transaction> client serviceName (ExtendContract (publish, address, numberOfBlocks, password))

    let executeContract client publish address command messageBody provideReturnAddress sign spends password =
        send<Transaction> client serviceName (ExecuteContract (publish, address, command, messageBody, provideReturnAddress, sign, spends, password))

    let importSeed client words password =
        send<unit> client serviceName (ImportSeed (words, password))
        
    let getTransactionCount client =
        send<int> client serviceName GetTransactionCount

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

    let restoreNewAddresses client maxIndex =
        RestoreNewAddresses maxIndex
        |> Command.send client serviceName

    let getReceivedByAddress client confirmations =
        Request.send<Request, Result<Map<(string*Asset), uint64>,string>> client serviceName (GetReceivedByAddress confirmations)

    let getAddressOutputs client address =
        Request.send<Request, Result<List<(Outpoint*Spend*uint32*bool)>,string>> client serviceName (GetAddressOutputs address)

    let getAddressBalance client address confirmations =
        Request.send<Request, Result<Map<Asset, uint64>,string>> client serviceName (GetAddressBalance (address,confirmations))

    let exportZenPublicKey client =
        Request.send<Request,Result<string,string>> client serviceName ExportZenPublicKey

    let importZenPublicKey client publicKey =
        ImportZenPublicKey publicKey
        |> Request.send<Request,Result<unit,string>> client serviceName

    let removeAccount client password =
        RemoveAccount password
        |> Request.send<Request,Result<unit,string>> client serviceName

    let getKeys client password =
        Request.send<Request, Result<Map<Crypto.PublicKey, string>,string>> client serviceName (GetKeys password)

module AddressDB =
    open Wallet

    type ContractHistoryResponse = List<string * data option * Hash * uint32> // command, messageBody, txHash, confirmations

    type Mode =
        | All
        | UnspentOnly

    type Command =
        | Resync

    type Request =
        | GetBalance of addresses:string list
        | GetOutputs of addresses:string list * Mode
        | GetTransactions of addresses:string list * skip: int * take: int
        | GetContractHistory of contractId : ContractId * skip: int * take: int

    let serviceName = "addressDB"

    //TODO: apply same convention to other services
    let private send<'a> client = Request.send<Request, Result<'a,string>> client serviceName

    let getBalance client args =
        GetBalance args
        |> send<BalanceResponse> client

    let getOutputs client args =
        GetOutputs args
        |> send<List<PointedOutput>> client

    let getTransactions client args =
        GetTransactions args
        |> send<TransactionsResponse> client

    let getContractHistory client args =
        GetContractHistory args
        |> send<ContractHistoryResponse> client
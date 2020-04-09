module Blockchain.DatabaseContext

open Blockchain
open Blockchain.Serialization
open DataAccess
open Consensus
open Types
open UtxoSet
open FStar
open Infrastructure
open Result
open Serialization
open Serialization
open Blockchain.Tally.Types
open Logary.Message

[<Literal>]
let DbVersion = 2

type T =
    {
        databaseContext:DataAccess.DatabaseContext
        tip:SingleValue<Hash.Hash>
        genesis:SingleValue<Hash.Hash>
        utxoSet:Collection<Outpoint, OutputStatus>
        contractStates:Collection<ContractId, Zen.Types.Data.data>
        activeContractSet:Collection<Hash.Hash,ActiveContractSet.ContractKey>
        contractUtxo:MultiCollection<ContractId,PointedOutput>
        blocks:Collection<Hash.Hash,ExtendedBlockHeader.T>
        blockChildrenIndex: Index<Hash.Hash,ExtendedBlockHeader.T,Hash.Hash>
        blockState:Collection<Hash.Hash,BlockState.T>
        blockTransactions: Collection<Hash.Hash, Hash.Hash seq>
        transactions:Collection<Hash.Hash, TransactionExtended>
        transactionBlocks:MultiCollection<Hash.Hash, Hash.Hash>
        contractPath:string
        dbVersion:SingleValue<int>
        //TODO move to contract context
        pkbalance: Collection<Interval, PKBalance>
        allocationVoters: Collection<Interval, PKAllocation>
        payoutVoters: Collection<Interval, PKPayout>
        nomineesVoters: Collection<Interval,PKPayout>
        nomineesWinner: Collection<Interval, Candidates>
        winner: Collection<Interval, Winner>
        funds: Collection<Interval, Fund.T>
        voteTip: SingleValue<Hash.Hash>
    }
    interface System.IDisposable with
        member x.Dispose () =
            Disposables.dispose x.utxoSet
            Disposables.dispose x.contractUtxo
            Disposables.dispose x.blocks
            Disposables.dispose x.blockChildrenIndex
            Disposables.dispose x.blockState
            Disposables.dispose x.blockTransactions
            Disposables.dispose x.transactions
            Disposables.dispose x.transactionBlocks
            Disposables.dispose x.databaseContext


type Session =
    {
        session: DataAccess.Session
        context: T
    }
    interface System.IDisposable with
        member x.Dispose () = Disposables.dispose x.session

let createSession context : Session =
    let session = DataAccess.DatabaseContext.createSession context.databaseContext
    {
        session=session
        context=context
    }
    
/// Use this for wipe clean the DB and resync from genesis
let updateVersion (t:T) session =
    Collection.truncate t.utxoSet session
    Collection.truncate t.contractStates session
    Collection.truncate t.activeContractSet session
    Collection.truncate t.blocks session
    Collection.truncate t.blockState session
    Collection.truncate t.blockTransactions session
    Collection.truncate t.transactions session
    MultiCollection.truncate t.contractUtxo session
    MultiCollection.truncate t.transactionBlocks session
    SingleValue.delete t.tip session
    SingleValue.put t.dbVersion session DbVersion
    
/// Use this if contract version was updated
let updateVersionClean t session =
    Platform.cleanDirectory t.contractPath
    Collection.getAll t.activeContractSet session
    |> List.iter (fun contractKey ->
        let (ContractId (_, contractHash)) = contractKey.contractId
        let moduleName = Contract.getModuleName contractHash
        
        Infrastructure.ZFStar.recordHints contractKey.code moduleName
        <@> (fun hints -> 
            Infrastructure.ZFStar.compile t.contractPath contractKey.code hints (2723280u * 10u) moduleName
        )
        |> Result.mapError (failwithf "could not recompile contract %A due to %A. try using 'wipe'" contractKey.contractId)
        |> ignore
    )
let create dataPath =
    let contractPath = Platform.combine dataPath "contracts"
    let databaseContext = DataAccess.DatabaseContext.create DataAccess.DatabaseContext.Large (Platform.combine dataPath "blockchain")
    use session = DatabaseContext.createSession databaseContext

    let tip = SingleValue.create databaseContext "tip" Hash.bytes (Hash.Hash >> Some)
    let genesis = SingleValue.create databaseContext "genesisHash" Hash.bytes (Hash.Hash >> Some)

    let blocks =
        Collection.create session "blocks" Hash.bytes
            ExtendedBlockHeader.serialize
            ExtendedBlockHeader.deserialize

    let blockChildrenIndex =
        Index.create session blocks "blockChildren" Hash.Length Hash.bytes (fun _ key (value:ExtendedBlockHeader.T) ->
            value.header.parent,key)

    let blockState =
        Collection.create session "blockState" Hash.bytes
            BlockState.serialize
            BlockState.deserialize

    let blockTransactions =
        Collection.create session "blockTransactions" Hash.bytes
            Hashes.serialize
            (Hashes.deserialize >> Some)

    let transactions = Collection.create session "transactions" Hash.bytes
                        TransactionExtended.serialize TransactionExtended.deserialize

    let transactionBlocks = MultiCollection.create session "transactionBlocks"
                                Hash.bytes Hash.bytes (Hash.Hash >> Some)

    let blocks =
        blocks
        |> Collection.addIndex blockChildrenIndex

    let utxoSet =
        Collection.create session "utxoSet"
            Outpoint.serialize
            OutputStatus.serialize
            OutputStatus.deserialize

    let activeContractSet =
        Collection.create session "activeContractset"
            Hash.bytes
            ContractKey.serialize
            ContractKey.deserialize

    let contractStates =
        Collection.create session "contractStates"
            ContractId.serialize
            Data.serialize
            Data.deserialize

    let contractUtxo =
        MultiCollection.create session "contractUtxo" ContractId.toBytes
            PointedOutput.serialize
            PointedOutput.deserialize

    let dbVersion =
        SingleValue.create databaseContext "dbVersion"
            Version.serialize
            Version.deserialize
    
    let pkbalance =
        Collection.create session "pkbalance" VarInt.serialize
            PKBalance.serialize
            PKBalance.deserialize
            
    let allocationVoters =
        Collection.create session "allocationVoters" VarInt.serialize
            PKAllocation.serialize
            PKAllocation.deserialize
            
    let payoutVoters =
        Collection.create session "payoutVoters" VarInt.serialize
            PKPayout.serialize
            PKPayout.deserialize
            
    let nomineesVoters =
        Collection.create session "nomineesVoters" VarInt.serialize
            PKPayout.serialize
            PKPayout.deserialize
            
    let nomineesWinner =
        Collection.create session "nomineesWinner" VarInt.serialize
            Nominees.serialize
            Nominees.deserialize
    
    let winner =
         Collection.create session "winners" VarInt.serialize
             Winner.serialize
             Winner.deserialize
    let funds =
        Collection.create session "funds" VarInt.serialize
            Fund.serialize
            Fund.deserialize

    let voteTip =
        SingleValue.create databaseContext "voteTip" Hash.bytes
            (Hash.Hash >> Some)
    
    let t = 
        {
            databaseContext = databaseContext
            tip=tip
            genesis=genesis
            utxoSet=utxoSet
            activeContractSet=activeContractSet
            contractStates=contractStates
            contractUtxo=contractUtxo
            blocks=blocks
            blockChildrenIndex=blockChildrenIndex
            blockState=blockState
            blockTransactions=blockTransactions
            transactions=transactions
            transactionBlocks = transactionBlocks
            contractPath=contractPath
            dbVersion = dbVersion
            pkbalance = pkbalance
            allocationVoters = allocationVoters
            payoutVoters = payoutVoters
            nomineesVoters = nomineesVoters
            nomineesWinner = nomineesWinner
            winner = winner
            funds = funds
            voteTip = voteTip
        }

    Session.commit session
    t

let createEmpty pathToFolder =
    if System.IO.Directory.Exists pathToFolder then
       System.IO.Directory.Delete (pathToFolder,true)

    create pathToFolder

let createTemporary prefix =
    let tempPath = System.IO.Path.GetTempPath()
    let unique = String.concat "" [ prefix; System.Guid.NewGuid().ToString() ]
    let uniquePath = System.IO.Path.Combine(tempPath, unique)

    createEmpty uniquePath

let createChildSession (session:Session) =
    let childSession = DataAccess.DatabaseContext.createChildSession session.session
    {
        session=childSession
        context=session.context
    }
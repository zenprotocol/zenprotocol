module Blockchain.DatabaseContext

open Blockchain.Serialization
open Blockchain.Serialization
open DataAccess
open Consensus
open Types
open UtxoSet
open FStar
open ContractStates
open Infrastructure
open Serialization

[<Literal>]
let DbVersion = 0

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
        transactions:Collection<Hash.Hash, Transaction>
        transactionBlocks:MultiCollection<Hash.Hash, Hash.Hash>
        contractPath:string
        dbVersion:SingleValue<int>
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

let create dataPath =
    let databaseContext = DataAccess.DatabaseContext.create (Platform.combine dataPath "blockchain")
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
                        (Transaction.serialize Full) (Transaction.deserialize Full)

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

    match SingleValue.tryGet dbVersion session with
    | Some version when version <> DbVersion ->
        failwithf "Blockchain: wrong db version, expected %d but got %d" DbVersion version
    | None ->
        SingleValue.put dbVersion session DbVersion
    | _ -> () // TODO: in the future we should have here db upgrade script

    Session.commit session

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
        contractPath=(Platform.combine dataPath "contracts")
        dbVersion = dbVersion
    }

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
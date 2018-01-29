module Blockchain.DatabaseContext

open DataAccess
open Consensus
open Consensus.Types
open Consensus.UtxoSet
open FStar
open Infrastructure
open MBrace.FsPickler

// TODO:Move to own file
// TODO:Implement serialize and deserialize for persistence
type BlockState = 
    {
        ema:EMA.T        
        activeContractSet:Hash.Hash list
    }

type T = 
    {        
        databaseContext:DataAccess.DatabaseContext
        tip:SingleValue<Hash.Hash>
        utxoSet:Collection<Outpoint, OutputStatus>
        contractUtxo:MultiCollection<Hash.Hash,PointedOutput>
        blocks:Collection<Hash.Hash,ExtendedBlockHeader.T>
        blockChildrenIndex: Index<Hash.Hash,ExtendedBlockHeader.T,Hash.Hash> 
        blockState:Collection<Hash.Hash,BlockState>
        blockTransactions: Collection<Hash.Hash,Hash.Hash seq>                
        transactions:Collection<Hash.Hash, Transaction>
        contractPath:string        
    }
    interface System.IDisposable with   
        member x.Dispose () =
            Disposables.dispose x.blockChildrenIndex
            Disposables.dispose x.blocks 
            Disposables.dispose x.blockState
            Disposables.dispose x.blockTransactions
            Disposables.dispose x.transactions
            Disposables.dispose x.databaseContext
    

type Session = 
    {
        session: DataAccess.Session
        context: T
    }
    interface System.IDisposable with   
        member x.Dispose () = Disposables.dispose x.session
        
let binarySerializer = FsPickler.CreateBinarySerializer()  
    
let private serializeHashes hs =
    hs
    |> Seq.map Hash.bytes
    |> Array.concat 
        
let private deserializeHashes bytes = 
    bytes
    |> Array.chunkBySize Hash.Length
    |> Array.toSeq
    |> Seq.map Hash.Hash
                  
let createSession context : Session = 
    let session = DataAccess.DatabaseContext.createSession context.databaseContext  
    {
        session=session
        context=context
    }        
            
let create dataPath = 
    let databaseContext = DataAccess.DatabaseContext.create (Platform.combine dataPath "blockchain")
    use session = DatabaseContext.createSession databaseContext
    
    let tip = SingleValue.create databaseContext "tip" Hash.bytes Hash.Hash
    
    let blocks = Collection.create session "blocks" Hash.bytes 
                    binarySerializer.Pickle<ExtendedBlockHeader.T> binarySerializer.UnPickle<ExtendedBlockHeader.T>
                           
    let blockChildrenIndex = 
        Index.create session blocks "blockChildren" Hash.Length Hash.bytes (fun _ key (value:ExtendedBlockHeader.T) ->
            value.header.parent,key)
            
    let blockState = Collection.create session "blockState" Hash.bytes 
                        binarySerializer.Pickle<BlockState> binarySerializer.UnPickle<BlockState> 
                        
    let blockTransactions = Collection.create session "blockTransactions" Hash.bytes serializeHashes deserializeHashes
    
    let transactions = Collection.create session "transactions" Hash.bytes 
                        (Transaction.serialize Transaction.Full) (Transaction.deserialize >> Option.get)                                                                 
    let blocks = 
        blocks
        |> Collection.addIndex blockChildrenIndex
        
    let utxoSet = 
        Collection.create session "utxoSet"
            binarySerializer.Pickle<Outpoint> 
            binarySerializer.Pickle<OutputStatus>
            binarySerializer.UnPickle<OutputStatus>  
            
    let contractUtxo =
        MultiCollection.create session "contractUtxo" Hash.bytes
            binarySerializer.Pickle<PointedOutput>                         
            binarySerializer.UnPickle<PointedOutput>
        
    Session.commit session
    
    {
        databaseContext = databaseContext        
        tip=tip
        utxoSet=utxoSet
        contractUtxo=contractUtxo
        blocks=blocks
        blockChildrenIndex=blockChildrenIndex
        blockState=blockState
        blockTransactions=blockTransactions
        transactions=transactions
        contractPath=(Platform.combine dataPath "contracts")
    }  

let createEmpty pathToFolder =
    if System.IO.Directory.Exists pathToFolder then 
       System.IO.Directory.Delete (pathToFolder,true)

    create pathToFolder       
    
    
    
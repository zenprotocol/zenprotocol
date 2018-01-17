module Miner.Main

open Consensus
open Messaging.Events
open Infrastructure
open Consensus.Types
open Consensus.Difficulty
open System

type Command = 
    | NewBlockTemplate of Block
    | Stop       
    | Exit
      
type Queue = System.Collections.Concurrent.BlockingCollection<Command>

let random = new System.Random()

let getRandomNonce () =
    let array = 
        Array.zeroCreate 64
    
    random.NextBytes (array) 
        
    System.BitConverter.ToUInt64 (array,0) 

let minerTask chain busName (collection:Queue) =
    let client = ServiceBus.Client.create busName
 
    let rec findNonce target (block:Block) =
        let n1,n2 = block.header.nonce
        
        let header = 
            if n1 = UInt64.MaxValue then
                {block.header with nonce=getRandomNonce(), 0UL }
            else
                {block.header with nonce=n1, (n2 + 1UL) }
                
        let block = {block with header=header}
        
        match BlockHeader.validate chain header with
        | Ok header ->
            Log.info "new block mined"
        
            // We found a block            
            Messaging.Services.Blockchain.validateBlock client block
            ()
        | Error _ ->
            // lets continue looking for a block, but first check if there is any message
            // If message waits, exit function
            // TODO: don't check every time, only once every X iterations
            if collection.Count = 0 then
                findNonce target block                                                      
               
    let mutable shouldStop = false       
    
    async {     
        while not shouldStop do
            match collection.Take () with
            | NewBlockTemplate block -> 
                let target = Difficulty.uncompress block.header.difficulty
                findNonce target block
            | Stop -> () // do nothing, we will block on next take call and wait for new block
            | Exit -> shouldStop <- true
    }

let acsSerializer _ = ActiveContractSet.set
        
type State = 
    {
        parent: BlockHeader option
        transactions:List<Transaction>
        activeContractSet:SparseMerkleTree.T<Hash.Hash>
        ema:EMA.T
    } 
                                                   
let handleEvent chain (collection:Queue) event state =
    let createBlock parent transactions =
        Log.info "New block to mine"
    
        // as we don't have difficulty yet we sleep for random time to simulate difficulty
        // TODO: remove
        System.Threading.Thread.Sleep (random.Next (1000,10000))
    
        let block = Block.createTemplate parent (Timestamp.now ()) state.ema state.activeContractSet transactions
        let header = {block.header with nonce=getRandomNonce(),0UL}
        {block with header=header} 
 
    match event with
    | ContractActivated cHash ->
        // Contract activated happen before TransactionAddedToMemPool, so we only change the state without create a new block templete
        // TransactionAddedToMemPool will create the new block template and feed it to miner thread
        let acs = SparseMerkleTree.add cHash cHash state.activeContractSet
        {state with activeContractSet=acs}
         
    | TransactionAddedToMemPool (txHash,tx) ->
        let transactions = state.transactions @ [tx]
        
        match state.parent with 
        | Some parent ->
            let block = createBlock parent transactions
                                    
            collection.Add (NewBlockTemplate block)
        | None -> ()
        
        {state with transactions=transactions}
    | BlockAdded block ->
        let parent = block.header
        let transactions = List.filter (fun tx -> not <| List.contains tx block.transactions) state.transactions
        let ema = EMA.add chain parent.timestamp state.ema
        
        match transactions with 
        | [] -> collection.Add (Stop)
        | _ ->             
            let block = createBlock parent transactions              
            collection.Add (NewBlockTemplate block)
        
        {state with ema=ema;transactions=transactions; parent=Some parent}
    | _ -> state
        
    // TODO: undoblock
    
            
let main busName chain =
    Actor.create<unit,unit,Event,State> busName "Miner" (fun poller sbObservable ebObservable  ->  
        let client = ServiceBus.Client.create busName
        let collection = new Queue()
        
        Async.Start (minerTask chain busName collection)               
        
        let state = 
            {
                transactions=[]
                activeContractSet=SparseMerkleTree.create ActiveContractSet.cwt acsSerializer
                ema = EMA.create chain
                parent = None
            }
        
        let ebObservable = 
            ebObservable
            |> Observable.map (handleEvent chain collection) 
            
        let observable =                      
            ebObservable
            |> Observable.scan (fun state handler -> handler state) state  
                                                                                      
        Disposables.empty, observable
    )
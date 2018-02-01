module Miner.Main

open Consensus
open Messaging.Events
open Messaging.Services
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
        | Result.Ok header ->
            Log.info "new block mined"
        
            // We found a block            
            Messaging.Services.Blockchain.validateMinedBlock client block
            ()
        | Result.Error _ ->
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
                                                   
let handleEvent chain client (collection:Queue) event =    
    match event with    
    | TransactionAddedToMemPool _ ->                
        match Blockchain.getBlockTemplate client with 
        | Some block -> collection.Add (NewBlockTemplate block)
        | None -> collection.Add (Stop)                
    | TipChanged _ ->
        match Blockchain.getBlockTemplate client with 
        | Some block -> collection.Add (NewBlockTemplate block)
        | None -> collection.Add (Stop) 
    | _ -> ()                         
            
let main busName chain =
    Actor.create<unit,unit,Event,unit> busName "Miner" (fun poller sbObservable ebObservable  ->  
        let client = ServiceBus.Client.create busName
        let collection = new Queue()
        
        Async.Start (minerTask chain busName collection)                              
        
        let observable = 
            ebObservable
            |> Observable.map (handleEvent chain client collection)               
                                                                                      
        Disposables.empty, observable
    )
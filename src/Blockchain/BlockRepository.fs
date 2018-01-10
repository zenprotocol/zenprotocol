module Blockchain.BlockRepository

open Infrastructure
open Blockchain
open Consensus
open Consensus
open Consensus.Types
open PersistentBlock

type T = {
        mutable blocks: Map<Hash.Hash, PersistentBlock.T>
    }
        
let create () = 
    {
        blocks=Map.empty
    }        
        
let update (blockRepository:T) block =
    blockRepository.blocks <- Map.add block.hash block blockRepository.blocks
    Writer.ret ()
        
let insert = update
    
let tryFind (blockRepository:T) hash : PersistentBlock.T option = 
    Map.tryFind hash blockRepository.blocks

let findParent (blockRepository:T) block : PersistentBlock.T = 
    Map.find block.header.parent blockRepository.blocks 

//let findTip (blockRepository:T) : PersistentBlock.T  = failwith ""

let getBlockChildren (blockRepository:T) (block:PersistentBlock.T) : PersistentBlock.T list = 
    Map.filter (fun _ b -> b.header.parent = block.hash) blockRepository.blocks
    |> Map.toList 
    |> List.map snd

let exist (blockRepository:T) blockHash = 
    Map.containsKey blockHash blockRepository.blocks

let findHeader blockRepository (hash:Hash.Hash) : BlockHeader = 
    let block = Map.find hash blockRepository.blocks
    block.header
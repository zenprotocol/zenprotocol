module Blockchain.Chain


open Blockchain
open Blockchain.ChainConnection
open Consensus
open Types
open Infrastructure
open Blockchain.EffectsWriter
open Consensus.Chain
open Messaging
open Events
open State
open Logary.Message
open Environment

module ExtHeader = ExtendedBlockHeader

type ExtHeader = ExtHeader.T


let updateTip
    ( session   : DatabaseContext.Session )
    ( connState : BlockConnection.State   )
    ( header    : ExtHeader               )
    : ActiveContractSet.T =
    BlockRepository.updateTip session header.hash

    UtxoSetRepository           .save session connState.utxoSet
    ContractStateRepository     .save session connState.contractStates
    ActiveContractSetRepository .save session connState.acs
    
    ActiveContractSet.clearChanges connState.acs


// Change the status of the entire chain from the root orphan block up to all tips from Orphan to Connected
let unorphanChain
    ( session : DatabaseContext.Session )
    ( root    : ExtHeader               )
    : unit =
    let unorphanBlock (block : ExtHeader) =
        if root.header <> block.header then
            let parent = BlockRepository.getHeader session block.header.parent
            let parentChainWork = ExtHeader.chainWork parent
            let chainWork = Block.getChainWork parentChainWork block.header

            ExtHeader.unorphan block chainWork
            |> BlockRepository.saveHeader session

    Tree.iter session unorphanBlock root

// find chains that are longer than the minChainWork
// this is not final as we also need to check if the chain is connectable
let findLongerChains
    ( session        : DatabaseContext.Session)        
    ( extendedHeader : ExtHeader )               
    ( minChainWork   : bigint    )
    : ExtHeader list =
    let rec getContinuation continuation headers f =
        
        if Seq.isEmpty headers then
            continuation
        else
            let head = Seq.head headers
            let tail = Seq.tail headers

            fun acc -> f head acc (getContinuation continuation tail f)
        
    let rec findLongerChains' extendedHeader acc continuation =
        let children = BlockRepository.getBlockChildren session extendedHeader

        if Seq.isEmpty children then
            if ExtHeader.chainWork extendedHeader > minChainWork then
                continuation (extendedHeader :: acc)
            else continuation acc
        else
            let head = Seq.head children
            let tail = Seq.tail children

            findLongerChains' head acc (getContinuation continuation tail findLongerChains')

    findLongerChains' extendedHeader [] id
    |> List.sortByDescending ExtHeader.chainWork

// Find the fork block of two chains
let rec findForkBlock session (tip1:ExtHeader) (tip2:ExtHeader) =
    if tip1.header = tip2.header then
        tip1
    else
        if tip1.header.blockNumber > tip2.header.blockNumber then
            let tip1 = BlockRepository.getHeader session tip1.header.parent
            findForkBlock session tip1 tip2
        else if tip2.header.blockNumber > tip1.header.blockNumber then
            let tip2 = BlockRepository.getHeader session tip2.header.parent
            findForkBlock session tip1 tip2
        else
            let tip1 = BlockRepository.getHeader session tip1.header.parent
            let tip2 = BlockRepository.getHeader session tip2.header.parent
            findForkBlock session tip1 tip2

let getSubChain session (start:ExtHeader) (tip:ExtHeader) =
    let rec getSubChain' (current:ExtHeader) acc =
        if current.header = start.header then
            acc
        else
            let parent = BlockRepository.getHeader session current.header.parent

            getSubChain' parent (current :: acc)

    getSubChain' tip []

// find orphan chain root
let requestOrphanChainRoot session get (tip:ExtHeader) (state:State) =
    let rec findRoot (header:ExtHeader) =
        match BlockRepository.tryGetHeader session header.header.parent with
        | Some parent -> findRoot parent
        | None -> header

    let root = findRoot tip

    effectsWriter {
        eventX "Request root of orphan chain from network block {blockNumber} {parent}"
        >> setField "blockNumber" (root.header.blockNumber - 1ul)
        >> setField "parent" (Hash.toString root.header.parent)
        |> Log.info

        do! get root.header.parent

        return state
    }

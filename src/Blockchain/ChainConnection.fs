module Blockchain.ChainConnection

open Blockchain
open Consensus
open Types
open Infrastructure
open Logary.Message
open Environment
open Blockchain.Tally

module ExtHeader = ExtendedBlockHeader

type Operation =
    | Main
    | Fork

type ExtHeader = ExtHeader.T

type OriginState =
    {
        header     : ExtHeader
        connection : BlockConnection.State
        mempool    : MemPool.T
    }
    member this.utxoSet         = this.connection.utxoSet
    member this.acs             = this.connection.acs
    member this.cgp             = this.connection.cgp
    member this.ema             = this.connection.ema
    member this.contractCache   = this.connection.contractCache
    member this.contractStates  = this.connection.contractStates


let private connectTip
    ( env       : Env         )
    ( op        : Operation   )
    ( origState : OriginState )
    ( tip       : ExtHeader   )
    : OriginState =
        
        if not (ExtHeader.isValid tip) then
            
            origState
            
        elif origState.header.hash <> tip.header.parent then
            
            // parent block is invalid, so are we
            BlockRepository.saveHeader env.session (ExtHeader.invalid tip)

            origState
            
        else
            
            let block =
                BlockRepository.getFullBlock env.session tip
            
            let connEnv : BlockConnection.Env = {
                chainParams      = env.chainParams
                timestamp        = env.timestamp
                getUTXO          = UtxoSetRepository.get env.session
                getContractState = ContractStateRepository.get env.session
                contractsPath    = env.contractsPath
                parent           = origState.header.header
                block            = block
            }

            let cgp =
                match op with
                | Main ->
                    if CGP.isPayoutBlock env.chainParams block.header.blockNumber then
                        let interval = CGP.getInterval  env.chainParams block.header.blockNumber
                        let winner   = Tally.Handler.getWinner env.session interval
                        CGP.update winner origState.cgp
                    else
                        origState.cgp
                | Fork ->
                    origState.cgp
                        
            let connection = {origState.connection with cgp = cgp }
            
            match BlockConnection.connect connection connEnv with
            
            | Error error ->
                
                BlockRepository.saveHeader env.session (ExtHeader.invalid tip)

                eventX "Failed connecting block #{blockNumber} {hash} due to {error}"
                >> setField "blockNumber" block.header.blockNumber
                >> setField "hash" (Hash.toString tip.hash)
                >> setField "error" error
                |> Log.info

                origState
                
            | Ok (block, connState) ->
                match op with
                | Main -> 
                    Tally.Handler.addBlock env.session env.chainParams connState.utxoSet block
                | Fork ->
                    ()

                BlockRepository.saveBlockState env.session tip.hash
                    {
                        ema =
                            connState.ema
                        cgp =
                            connState.cgp
                        activeContractSetUndoData =
                            ActiveContractSet.getUndoData connState.acs origState.acs
                        contractStatesUndoData =
                            ContractStates.getUndoData (ContractStateRepository.get env.session) connState.contractStates origState.contractStates
                    }
                
                {
                    header =
                        tip
                    connection =
                        connState
                    mempool =
                        MemPool.handleBlock block origState.mempool
                }

let rec private connectTips
    ( env       : Env             )
    ( op        : Operation       )
    ( origState : OriginState     )
    ( tips      : ExtHeader list  )
    : OriginState
    =
    let mutable state = origState
    for tip in tips do
            state <- connectTip env op state tip
    state

let private getHeaders (env : Env) (origHeader : ExtHeader) (tipHeader : ExtHeader) : ExtHeader list =
    let rec getHeadersAux (tipHeader : ExtHeader) (headers : ExtHeader list) : ExtHeader list =
        if tipHeader.header = origHeader.header then
            headers
        else
            let parent = BlockRepository.getHeader env.session tipHeader.header.parent

            getHeadersAux parent (tipHeader :: headers)
    getHeadersAux tipHeader []

// Connect the entire chain, returning the valid tip along with state
let private connectChain
    ( env       : Env         )
    ( op        : Operation   )
    ( origState : OriginState )
    ( tip       : ExtHeader   )
    : OriginState =
    
    let headers = getHeaders env origState.header tip
    
    connectTips env op origState headers
//if the chains is singelton then just add without over checking the different chains
let connectLongestChain
    ( env          : Env            )
    ( origState    : OriginState    )
    ( chains       : ExtHeader list )
    ( minChainWork : bigint         )
    ( op           : Operation      )
    : OriginState option =
    
    let connect (bestState : OriginState) (tip : ExtHeader) : OriginState =
        // We are checking twice if current is longer than the bestChain, once before connecting and once after
        if ExtHeader.chainWork tip > ExtHeader.chainWork bestState.header then
            let state =
                connectChain env op origState tip
            
            if ExtHeader.chainWork state.header > ExtHeader.chainWork bestState.header then
                state
            else
                { state with header = bestState.header }
        else
            bestState
    
    let bestState =
        List.fold connect origState chains
    
    if ExtHeader.chainWork bestState.header > minChainWork then
        Some bestState
    else
        None

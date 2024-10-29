module Consensus.BlockValidation

open Consensus
open Types
open Infrastructure
open Result
open Serialization
open Chain
open Block
open ReaderResult

type Check = Block -> ReaderResult<ChainParameters, string, Block>

module TxNotEmpty =
    
    let check : Check = fun block _ ->
        if List.isEmpty block.transactions then Error "transactions is empty" else Ok block

module Header =
    
    let validate
        ( chainParams : ChainParameters )
        ( header      : BlockHeader     )
        : Result<BlockHeader, string> =
        
        let blockHash     = hash header
        let blockHashHash = Hash.computeOfHash blockHash
    
        if blockHashHash = chainParams.genesisHashHash then
            Ok header
        else
            let difficulty       = Difficulty.uncompress header.difficulty
            let proofOfWorkLimit = chainParams.proofOfWorkLimit
    
            if difficulty <= proofOfWorkLimit && blockHash <= difficulty then
                Ok header
            else
                Error "proof of work failed"
    
    let check : Check = fun block chainParams ->
        validate chainParams block.header
        |> Result.map (fun _ -> block)

module Coinbase =
    
    let check : Check = fun block chainParams ->
        if isGenesis chainParams block then
            Ok block
        else
            let coinbase = List.head block.transactions

            match TransactionValidation.validateCoinbase block.header.blockNumber coinbase.tx with
            | Error error -> Error <| sprintf "Block failed coinbase validation due to %A" error
            | Ok _ -> Ok block

module TxBasic =
    
    let check : Check = fun block chainParams -> result {
        // skip if genesis block
        if isGenesis chainParams block then
            return block
        else
            let withoutCoinbase = List.tail block.transactions

            // Fail if validateBasic fails on any transaction in the block.
            for ex in withoutCoinbase do
                let! _ =
                    TransactionValidation.validateBasic ex.tx
                    |> Result.mapError (sprintf "transaction %A failed validation due to %A" ex.txHash)
                ()
            return block
        }

module Commitments =
    
    let check : Check = fun block _ ->
        
        let txMerkleRoot =
            block.transactions
            |> List.map (fun tx-> tx.txHash)
            |> MerkleTree.computeRoot
        
        let witnessMerkleRoot =
            block.transactions
            |> List.map (fun tx-> tx.witnessHash)
            |> MerkleTree.computeRoot
        
        if txMerkleRoot = block.txMerkleRoot && witnessMerkleRoot = block.witnessMerkleRoot then
            let commitments =
                Block.createCommitments
                    block.txMerkleRoot
                    block.witnessMerkleRoot
                    block.activeContractSetMerkleRoot
                    block.commitments
                |> computeCommitmentsRoot
            
            if commitments = block.header.commitments then
                Ok block
            else
                Error "commitments mismatch"
        else
            Error "commitments mismatch"

// TODO: Refactor to avoid chained state-passing style
let validate
    : Block -> ReaderResult<ChainParameters, string, Block> =
    
    ReaderResult.ret
    >=> TxNotEmpty  .check
    >=> Header      .check
    >=> Coinbase    .check
    >=> TxBasic     .check
    >=> Commitments .check

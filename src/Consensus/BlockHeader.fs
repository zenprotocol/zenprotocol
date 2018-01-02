module Consensus.BlockHeader

open Consensus.Types
open Consensus.Crypto
open Consensus.ChainParameters
open FsNetMQ.Stream
open FsNetMQ.Stream.Reader

open MBrace.FsPickler.Combinators

[<Literal>]
let Size = 100 

let serialize header =
    let writeHash hash = writeBytes (Hash.bytes hash) Hash.Length
    let writeNonce (n1,n2) = writeNumber8 n1 >> writeNumber8 n2                

    let bytes = 
        create Size
        |> writeNumber4 header.version 
        |> writeHash header.parent
        |> writeNumber4 header.blockNumber
        |> writeHash header.commitments
        |> writeNumber8 header.timestamp
        |> writeNumber4 header.difficulty
        |> writeNonce header.nonce
        |> getBuffer
        
    if Array.length bytes <> Size then
        failwithf "Header serailized size is different than BlockHeaderSize"
        
    bytes                

let deserialize header =
    let readHash = 
        reader {
            let! bytes = readBytes Hash.Length
            return (Hash.Hash bytes)
        }
        
    let readNonce = 
        reader {
            let! nonce1 = readNumber8
            let! nonce2 = readNumber8
            
            return (nonce1,nonce2)
        }

    if Array.length header <> Size then
        None
    else
        let stream = Stream (header,0)
        
        let read = reader {
            let! version = readNumber4
            let! parent = readHash
            let! blockNumber = readNumber4
            let! commitments = readHash
            let! timestamp = readNumber8
            let! difficulty = readNumber4
            let! nonce = readNonce
            
            return {
                version=version;
                parent=parent;
                blockNumber=blockNumber;
                commitments=commitments;
                timestamp=timestamp;
                difficulty=difficulty;
                nonce=nonce
            }
        }
        
        run read stream
                  
let hash = serialize >> Hash.compute 

let validate chain header  =
    let h = hash header
    
    let difficulty = Difficulty.uncompress header.difficulty
    let proofOfWorkLimit = ChainParameters.proofOfWorkLimit chain
    
    if difficulty <= proofOfWorkLimit && h <= difficulty then Ok header else Error "proof of work failed"        
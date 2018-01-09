module Consensus.Block

open Consensus
open Consensus.Types

open MBrace.FsPickler.Combinators

[<Literal>]
let Version = 0ul

let TwoPow256 = bigint.Pow (2I, 256)

let genesisParent = {version=Version;parent=Hash.zero;blockNumber=0ul;commitments=Hash.zero;timestamp=0UL;difficulty=0ul;nonce=0UL,0UL}

let pickler = Pickler.auto<Block>
    
type Commitments = { 
    commitments:Hash.Hash;
    txMerkleRoot:Hash.Hash;
    acsMerkleRoot:Hash.Hash;    
}

let private (>=>) f1 f2 x = Result.bind f2 (f1 x)

let private createCommitments txMerkleRoot acsMerkleRoot witnessMerkleRoot =
    MerkleTree.computeRoot [ txMerkleRoot; acsMerkleRoot; witnessMerkleRoot ]

let hash (block:Block) = BlockHeader.hash block.header

let serialize block =   

    Binary.pickle pickler block     

let deserialize block =
    try
        Some (Binary.unpickle pickler block) with
    | _ -> None

let isGenesis chain block = 
    let blockHash = hash block
    
    ChainParameters.getGenesisHash chain = blockHash
        
let getChainWork (prevWork:bigint) block = 
    let target = 
        Difficulty.uncompress block.difficulty
        |> Hash.toBigInt         
        
    let proof = bigint.Divide (TwoPow256, target + 1I)
    
    prevWork + proof         

let createGenesis chain timestamp transactions nonce = 
    let txMerkleRoot = 
        transactions
        |> List.map Transaction.hash
        |> MerkleTree.computeRoot 
        
    let witnessMerkleRoot = 
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot    
        
    let acsMerkleRoot = SparseMerkleTree.root ActiveContractSet.empty
    
    let commitments = createCommitments txMerkleRoot acsMerkleRoot witnessMerkleRoot
        
    let header = 
        {
            version=Version;
            parent=Hash.zero;
            blockNumber=1ul;
            commitments=commitments;
            timestamp=timestamp;
            difficulty=EMA.get (EMA.create chain);
            nonce=nonce;
        }
        
    {header=header;transactions=transactions}

let createTemplate (parent:BlockHeader) timestamp ema acs transactions = 
    let txMerkleRoot = 
        transactions
        |> List.map Transaction.hash
        |> MerkleTree.computeRoot                
        
    let witnessMerkleRoot = 
        transactions
        |> List.map Transaction.witnessHash
        |> MerkleTree.computeRoot
        
    let acsMerkleRoot = SparseMerkleTree.root acs        
        
    let parentHash = BlockHeader.hash parent        
                
    // TODO: add utxo commitments   
    let commitments = createCommitments txMerkleRoot acsMerkleRoot witnessMerkleRoot
    
    let header = 
        {
            version=Version;
            parent=parentHash;
            blockNumber=parent.blockNumber + 1ul;
            commitments=commitments;
            timestamp=timestamp;
            difficulty=EMA.get ema;
            nonce=0UL,0UL;
        }
        
    {header=header;transactions=transactions}

let validate chain =     
    let checkTxNotEmpty (block:Block) =
        if List.isEmpty block.transactions then Error "transactions is empty" else Ok block
            
    let checkHeader (block:Block) =        
        BlockHeader.validate chain block.header
        |> Result.map (fun _ -> block)  
        
    let checkTxBasic (block:Block) = 
        // we skip this if this is the genesis
        if isGenesis chain block then
            Ok block
        else             
            List.fold (fun state tx->
                match state with
                | Error e -> Error e
                | ok -> 
                    match TransactionValidation.validateBasic tx with
                    | Error err -> Error (sprintf "transaction %A failed validation due to %A" (Transaction.hash tx) err)
                    | _ -> ok) (Ok block) block.transactions
   
    checkTxNotEmpty    
    >=> checkHeader
    >=> checkTxBasic

/// Apply block to UTXO and ACS, operation can fail
let connect chain parent set acs ema =    
    let checkBlockNumber (block:Block) = 
        if parent.blockNumber + 1ul <> block.header.blockNumber then
            Error "blockNumber mismatch"
        else Ok block 

    let checkDifficulty (block:Block) =
        let nextEma = EMA.add block.header.timestamp ema 
        if block.header.difficulty = ema.difficulty then Ok (block,nextEma) else Error "incorrect proof of work"               
        
    let checkCommitments ((block:Block),ema) =
        let acs =
            // TODO: reject uncompiled contracts 
            block.transactions
            |> List.choose (fun tx -> tx.contract)        
            |> List.map (fun contract -> Contract.compile contract)
            |> List.choose (fun contract ->
                match contract with
                | Ok c -> Some c
                | _ -> None)
            |> List.fold (fun acs contract -> SparseMerkleTree.add (Contract.hash contract) contract acs) acs
            
        let txRoot = 
            block.transactions
            |> List.map Transaction.hash            
            |> MerkleTree.computeRoot
            
        let witnessMerkleRoot = 
            block.transactions
            |> List.map Transaction.witnessHash
            |> MerkleTree.computeRoot                
     
        let acsRoot = SparseMerkleTree.root acs
        
        let commitments = createCommitments txRoot acsRoot witnessMerkleRoot
        
        if commitments = block.header.commitments then
            Ok (block,{commitments=commitments;txMerkleRoot=txRoot;acsMerkleRoot=acsRoot},acs,ema)
        else
            Error "commitments mismatch"
            
    let checkTxInputs (block, commitments,acs,ema) =
        if isGenesis chain block then
            let set = List.fold (fun set tx ->
                let txHash = (Transaction.hash tx)
                UtxoSet.handleTransaction txHash tx set) set block.transactions
            Ok (commitments,set,acs,ema) 
        else                       
            List.fold (fun state tx->
                match state with
                | Error e -> Error e
                | Ok (block,set,acs,ema) -> 
                    let txHash = (Transaction.hash tx)
                
                    match TransactionValidation.validateInputs acs set txHash tx with
                    | Error err -> Error (sprintf "transactions failed inputs validation due to %A" err)
                    | _ -> 
                        let set = UtxoSet.handleTransaction txHash tx set
                        Ok (commitments,set,acs,ema) 
                    ) (Ok (commitments,set,acs,ema)) block.transactions

    checkBlockNumber
    >=> checkDifficulty
    >=> checkCommitments
    >=> checkTxInputs
       
    
                
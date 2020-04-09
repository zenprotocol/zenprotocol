module Blockchain.Tests.TallyChainTest


open NUnit.Framework
open FsUnit
open Consensus
open Consensus.Types
open Consensus.Chain
open Infrastructure
open Blockchain
open Blockchain.State
open FsCheck
open Messaging.Events
open Messaging.Services.Network
open Wallet
open Blockchain.DatabaseContext
open Consensus.Tests.SampleContract
open Consensus.Contract
open Consensus.Tests
open Tests.ContractCode
open Api.Types

open System
open Blockchain.Tally
open Blockchain.Tests
open Consensus
open Consensus.Tests
open Consensus.Tests
open Consensus.Tests
open Consensus.Types
open TestsInfrastructure.Constraints
open Messaging.Services
open Helper
open Tests.ValidateBlockTests


type IntervalCGPMap = Map<uint32, CGP.T>

type BlockNumberEXMap = Map<uint32, TransactionExtended list>

let contractId = Contract.makeContractId Version0 votingContractCode

let contractPath =
    NUnit.Framework.TestContext.CurrentContext.TestDirectory

let getContractV0 code =
    result {
        let! hints = Contract.recordHints code
        let! queries = Infrastructure.ZFStar.totalQueries hints
    
        let contract = {
            code = code
            hints = hints
            rlimit = TestWallet.rlimit
            queries = queries
        }
        return contract
    }

let compile code = lazy (result {
    let! contract = getContractV0 code
    let expiry = 200ul
    return!
        Contract.compile contractPath contract
        >>>= Contract.load contractPath expiry code
})


let state = {
    memoryState =
        {
            utxoSet = utxoSet
            mempool = mempool
            orphanPool = orphanPool
            activeContractSet = acs|> ActiveContractSet.add chain.votingContractId ((compile votingContractCode).Force() |> function | Ok x -> x | _ -> failwith "no")
            contractCache = ContractCache.empty
            contractStates = ContractStates.asDatabase
            invalidTxHashes = Set.empty
        }
    tipState =
        {
            tip = ExtendedBlockHeader.empty
            activeContractSet = acs |> ActiveContractSet.add chain.votingContractId ((compile votingContractCode).Force() |> function | Ok x -> x | _ -> failwith "no")
            ema = ema
        }
    initialBlockDownload = InitialBlockDownload.Inactive
    headers=0ul
    cgp = {allocation = 0uy; payout= None}
}

let addBlock (env:Environment.Env) utxoSet block =
    Tally.Handler.addBlock env.session env.chainParams utxoSet block
let removeBlock (env:Environment.Env) utxoSet block =
     Tally.Handler.removeBlock env.session env.chainParams utxoSet block

let private sign message path : Result<Crypto.Signature,string> =
    result {
        let! secretKey =
                rootExtendedKey
                |> ExtendedKey.derivePath path
                >>>= ExtendedKey.getPrivateKey
    
        return Crypto.sign secretKey message
    }    
let private parserPublicKey string =
    PublicKeyDataJson.Parse string
    |> Array.map (fun data -> data.PublicKey,data.Path)
    |> Array.toList

let createVoteData ballot blockNumber =
    let ballotString =
        match ballot with
        |Ballot.Payout _ -> "Payout"
        |Ballot.Allocation _ -> "Allocation"
        |> ZFStar.fsToFstString

    let signatureString =
        "Signature"
        |> ZFStar.fsToFstString

    let ballotSer = Serialization.Serialization.Ballot.serialize ballot |> FsBech32.Base16.encode |> ZFStar.fsToFstString

    let message = Blockchain.Tally.VoteParser.hashBallot chain blockNumber ballotSer
    // get keys
    let keys = parserPublicKey publicKeyRoot

    //Create dictionary
    let signatureData =
        let map =
            keys
            |> List.map (fun (publicKeyAsString, pathAsString) ->
                // sign message
                let publicKey = publicKeyAsString |> ZFStar.fsToFstString
                let signature = sign message pathAsString |> Result.get

                publicKey, signature |> ZFStar.fsToFstSignature |> Zen.Types.Data.Signature
                )
            |> Map.ofList
        Zen.Types.Data.Collection (Zen.Types.Data.Dict (map, Map.count map |> uint32))
    let signatures =
        Zen.Dictionary.add signatureString signatureData Zen.Dictionary.empty
        |> Zen.Cost.Realized.__force

    Zen.Dictionary.add ballotString (Zen.Types.Data.String ballotSer) signatures
    |> Zen.Cost.Realized.__force
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection

let createCGPData (winner : Consensus.Types.Recipient * Consensus.Types.Spend list) : byte array =
    let outputs = Consensus.CGP.internalizeRecipient winner
    match Consensus.CGP.Contract.createPayoutMsgBody outputs with
    | Some msgBody -> Consensus.Serialization.Data.serialize msgBody
    | None -> [||]




let createVoteTransaction session (account:TestWallet.T) blockNumber timestamp ballot =

    let executeContract (contractId:ContractId) command sender (messageBody: Zen.Types.Data.data option) (txSkel : TxSkeleton.T) =
        TransactionHandler.executeContract session txSkel timestamp contractId command sender messageBody state false
        
    let messageBody =
        createVoteData ballot blockNumber
        |> Some
        
    let command =
        match ballot with
        | Allocation _ -> "Allocation"
        | Payout _ -> "Payout"
    
    Result.get <| TestWallet.createExecuteContractTransaction chain executeContract contractId command messageBody false None Map.empty (account, rootExtendedKey)


let private getTxsFromBlockNumber blockNumber txs =
    match Map.tryFind blockNumber txs with
    | Some txs -> txs
    | None -> []
    
    
let private addTransactionsToAccount blockNumber (txs: BlockNumberEXMap) account =
    txs
    |> getTxsFromBlockNumber blockNumber
    |> List.fold (fun account ex -> TestWallet.addTransaction ex.txHash ex.tx account) account

let createChainWithSession (length:int) nonce start ema account (cgps: IntervalCGPMap ) (txs: BlockNumberEXMap)  =

    let blocks, _, account =
        [start.header.blockNumber..(start.header.blockNumber + (uint32 length) - 1ul)]
        |> Seq.fold
            begin fun (blocks,ema,(account:TestWallet.T)) i ->
                let parent = List.head blocks
                let timestamp = timestamp + ((uint64 i) * 1000UL * 60UL * 1UL)
                let blockNumber   = parent.header.blockNumber + 1ul
                let interval      = CGP.getInterval          chain blockNumber
                let cgp =
                    if interval <> 1ul && CGP.isPayoutBlock chain blockNumber then
                         cgps
                         |> Map.tryFind interval
                         |> Option.defaultValue CGP.empty
                    else
                        account.cgp

                let account = addTransactionsToAccount blockNumber txs account

                let txs = getTxsFromBlockNumber blockNumber txs

                let block = Block.createTemplate chain parent.header timestamp ema acs cgp txs rootPKHash
                let block = {block with header ={ block.header with nonce = uint64 nonce,0UL}}

                let coinbaseTx =
                    List.head block.transactions |> fun ex ->ex.tx

                let account = TestWallet.addTransaction (Transaction.hash coinbaseTx) coinbaseTx account
                let account = {account with blockNumber = blockNumber; cgp = cgp }


                let ema = EMA.add chain timestamp ema

                let blocks = block :: blocks

                blocks, ema, account
            end  ([start],ema,account)

    let blocks =
        blocks
        |> List.rev

    blocks,account

let createChainFromGenesisWithSession length nonce =
    let ema = EMA.add chain genesisBlock.header.timestamp ema
    createChainWithSession length nonce genesisBlock ema rootAccount

[<Test>]
let ``tally correctly find winner and change state`` () =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext
    
    let ballot = Allocation 5uy
     
    let cgps : IntervalCGPMap = Map.add 2ul {allocation = 5uy; payout= None} Map.empty
    let txs : BlockNumberEXMap = Map.add 96ul [(createVoteTransaction session rootAccount 96ul timestamp ballot |> Transaction.toExtended)] Map.empty
    let mainChain, _ = createChainFromGenesisWithSession 120 0 cgps txs
    

    // validate all chains
    let _, state = validateChain session mainChain state
    
    state.cgp |> should equal ({allocation = 5uy; payout= None}:CGP.T)

[<Test>]
let ``2 chain, tally correctly find winner from the stronger chain and change state`` () =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext
    
    let ballot = Allocation 5uy
     
    let cgps : IntervalCGPMap = Map.add 2ul {allocation = 5uy; payout= None} Map.empty
    let txs : BlockNumberEXMap = Map.add 97ul [(createVoteTransaction session rootAccount 97ul timestamp ballot |> Transaction.toExtended)] Map.empty
    let mainChain, _ = createChainFromGenesisWithSession 95 0 cgps txs

    let ballot = Allocation 4uy
    
    let forkedBlock = mainChain.[95]
    let cgps : IntervalCGPMap = Map.add 2ul {allocation = 4uy; payout= None} Map.empty
    let txs : BlockNumberEXMap = Map.add 97ul [(createVoteTransaction session rootAccount 97ul timestamp ballot |> Transaction.toExtended)] Map.empty
    let ema = EMA.add chain forkedBlock.header.timestamp ema
    let strongerChain, _ = createChainWithSession 20 1 forkedBlock ema rootAccount cgps txs
    
    

    // validate all chains
    let _, state = validateChain session mainChain state
    let _, state = validateChain session strongerChain state 
    
    state.cgp |> should equal ({allocation = 4uy; payout= None}:CGP.T)

[<Test>]
let ``Tally modules are reverted to its previous state `` () =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext
    let ballot = (Allocation 5uy)
     
    let cgps : IntervalCGPMap = Map.add 1ul {allocation = 5uy; payout= None} Map.empty
    let txs : BlockNumberEXMap = Map.add 91ul [(createVoteTransaction session rootAccount 91ul timestamp ballot |> Transaction.toExtended)] Map.empty
    let mainChainBlocks, _ = createChainFromGenesisWithSession 101 0 cgps txs
    let env : Environment.Env =
        {
            chainParams   = chain
            contractsPath = session.context.contractPath
            timestamp     = timestamp
            session       = session
        }
    
    
    let snapshot, mainChain =
        let blocks = mainChainBlocks |> List.take 90
        List.head (blocks|>List.rev), blocks |> List.take 89

    for block in mainChain do
        addBlock env utxoSet block
        Tally.Repository.VoteTip.put session session.session (Block.hash block.header)

    let pkBalanceBefore =
        Tally.Repository.PKBalance.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    let fundBefore =
        Tally.Repository.Fund.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    addBlock env utxoSet snapshot
    Tally.Repository.VoteTip.put session session.session (Block.hash snapshot.header)
    removeBlock env utxoSet snapshot
    
    let pkBalanceAfter =
        Tally.Repository.PKBalance.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue [] 
    
    let fundAfter =
        Tally.Repository.Fund.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    

    
    pkBalanceBefore |> should equal pkBalanceAfter
    fundBefore |> should equal fundAfter

[<Test>]
let ``Tally modules in the voting interval are reverted to its previous state `` () =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ballot = (Allocation 5uy)
    let tx = createVoteTransaction session rootAccount 91ul timestamp ballot
    let cgps : IntervalCGPMap = Map.add 1ul {allocation = 5uy; payout= None} Map.empty
    let txs : BlockNumberEXMap = Map.add 91ul [tx|> Transaction.toExtended] Map.empty
    let mainChainBlocks, _ = createChainFromGenesisWithSession 101 0 cgps txs
    
    let env : Environment.Env =
        {
            chainParams   = chain
            contractsPath = session.context.contractPath
            timestamp     = timestamp
            session       = session
        }
    
    
    let endOfInterval, mainChain =
        let blocks = mainChainBlocks |> List.take 100
        List.head (blocks|>List.rev), blocks |> List.take 99
        
    let utxoSet =
        validateChain session mainChain state
        |> fun (_,s) -> s.memoryState.utxoSet

    let pkBalanceBefore =
        Tally.Repository.PKBalance.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    
    let fundBefore =
        Tally.Repository.Fund.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    let pkAllocBefore =
        Tally.Repository.PKAllocation.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    let pkPayoutBefore =
        Tally.Repository.PKPayout.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    addBlock env utxoSet endOfInterval

    removeBlock env utxoSet endOfInterval
    
    let pkBalanceAfter =
        Tally.Repository.PKBalance.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue [] 
    
    
    let fundAfter =
        Tally.Repository.Fund.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    let pkAllocAfter =
        Tally.Repository.PKAllocation.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
    let pkPayoutAfter =
        Tally.Repository.PKPayout.tryGet session session.session 1ul
        |> Option.map Map.toList
        |> Option.map List.sort
        |> Option.defaultValue []
    
        
    pkBalanceBefore |> should equal pkBalanceAfter
    fundBefore |> should equal fundAfter
    pkAllocBefore |> should equal pkAllocAfter
    pkPayoutBefore |> should equal pkPayoutAfter

[<Test>]
let ``edge cases of allocation bounds`` () =
    
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext
    let mutable account = rootAccount
    
    let voteTx blockNumber alloc =
        let tx =
            createVoteTransaction session account blockNumber timestamp (Allocation alloc)
            |> Transaction.toExtended
        account <- TestWallet.addTransaction tx.txHash tx.tx account
        
        tx
    
    // [15, 27, 38, 47, 55, 62, 67, 72, 76, 80, 83, 85, 87, 89, 90]
    let cgps : IntervalCGPMap =
        Map.empty
        |> Map.add 2ul  ({allocation = 15uy; payout= None} : CGP.T)
        |> Map.add 3ul  ({allocation = 27uy; payout= None} : CGP.T)
        |> Map.add 4ul  ({allocation = 38uy; payout= None} : CGP.T)
        |> Map.add 5ul  ({allocation = 47uy; payout= None} : CGP.T)
        |> Map.add 6ul  ({allocation = 55uy; payout= None} : CGP.T)
        |> Map.add 7ul  ({allocation = 62uy; payout= None} : CGP.T)
        |> Map.add 8ul  ({allocation = 67uy; payout= None} : CGP.T)
        |> Map.add 9ul  ({allocation = 72uy; payout= None} : CGP.T)
        |> Map.add 10ul ({allocation = 76uy; payout= None} : CGP.T)
        |> Map.add 11ul ({allocation = 80uy; payout= None} : CGP.T)
        |> Map.add 12ul ({allocation = 83uy; payout= None} : CGP.T)
        |> Map.add 13ul ({allocation = 85uy; payout= None} : CGP.T)
        |> Map.add 14ul ({allocation = 87uy; payout= None} : CGP.T)
        |> Map.add 15ul ({allocation = 89uy; payout= None} : CGP.T)
        |> Map.add 16ul ({allocation = 90uy; payout= None} : CGP.T)
        |> Map.add 17ul ({allocation = 90uy; payout= None} : CGP.T)

    
    let voteBlock interval = (interval - 1ul) * 100ul + 96ul
    
    let txs : BlockNumberEXMap =
        Map.empty
        |> Map.add (voteBlock 1ul )  [ voteTx (voteBlock 1ul ) 15uy ]
        |> Map.add (voteBlock 2ul )  [ voteTx (voteBlock 2ul ) 27uy ]
        |> Map.add (voteBlock 3ul )  [ voteTx (voteBlock 3ul ) 38uy ]
        |> Map.add (voteBlock 4ul )  [ voteTx (voteBlock 4ul ) 47uy ]
        |> Map.add (voteBlock 5ul )  [ voteTx (voteBlock 5ul ) 55uy ]
        |> Map.add (voteBlock 6ul )  [ voteTx (voteBlock 6ul ) 62uy ]
        |> Map.add (voteBlock 7ul )  [ voteTx (voteBlock 7ul ) 67uy ]
        |> Map.add (voteBlock 8ul )  [ voteTx (voteBlock 8ul ) 72uy ]
        |> Map.add (voteBlock 9ul )  [ voteTx (voteBlock 9ul ) 76uy ]
        |> Map.add (voteBlock 10ul)  [ voteTx (voteBlock 10ul) 80uy ]
        |> Map.add (voteBlock 11ul)  [ voteTx (voteBlock 11ul) 83uy ]
        |> Map.add (voteBlock 12ul)  [ voteTx (voteBlock 12ul) 85uy ]
        |> Map.add (voteBlock 13ul)  [ voteTx (voteBlock 13ul) 87uy ]
        |> Map.add (voteBlock 14ul)  [ voteTx (voteBlock 14ul) 89uy ]
        |> Map.add (voteBlock 15ul)  [ voteTx (voteBlock 15ul) 90uy ; voteTx (voteBlock 15ul) 91uy ]
        |> Map.add (voteBlock 16ul)  [ voteTx (voteBlock 16ul) 91uy ]
    
    let (blocks, account) = createChainFromGenesisWithSession 1611 1 cgps txs
    
    let _, state' = validateChain session blocks state
    
    state'.cgp.allocation
    |> should equal 90ul
    
    
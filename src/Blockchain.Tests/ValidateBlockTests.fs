module Blockchain.Tests.ValidateBlockTests

open NUnit.Framework
open FsUnit
open Consensus
open Consensus.Types
open Consensus.ChainParameters
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
open TestsInfrastructure.Constraints
open Messaging.Services

let (>>=) = Writer.bind
let chain = Chain.Local
let timestamp = 1515594186383UL + 1UL

module Result =
    let get =
        function
        | Ok x -> x
        | Error y -> failwithf "%A" y

let getTxOutpoints tx =
    let txHash = Transaction.hash tx
    [ for i in 0 .. List.length tx.outputs - 1 -> {txHash=txHash;index= uint32 i} ]

let rootTxOutpoints = getTxOutpoints Transaction.rootTx
let areOutpointsInSet session outpoints set =
    Option.isSome <| UtxoSet.getUtxos (UtxoSetRepository.get session) outpoints set
let isAccountInSet session (account:Account.T) =
    let outpoints =
        Account.getUnspentOutputs account
        |> Map.toList
        |> List.map fst

    areOutpointsInSet session outpoints

let rootAccount = Account.createTestAccount ()

let createTransaction account =
    match Account.createTransaction chain account account.publicKeyHash {asset=Constants.Zen;amount=1UL} with
    | Ok tx -> tx
    | Error error -> failwith error


// Default initial state of mempool and utxoset
let utxoSet = UtxoSet.asDatabase
let mempool = MemPool.empty
let orphanPool = OrphanPool.create()
let acs = ActiveContractSet.empty
let ema = EMA.create chain
let genesisBlock = Block.createGenesis chain [Transaction.rootTx] (0UL,0UL)
let genesisBlockHash = Block.hash genesisBlock

let hashBlock b = Block.hash b, b

let state = {
    memoryState =
        {
            utxoSet=utxoSet
            mempool=mempool
            orphanPool=orphanPool
            activeContractSet=acs
        }
    tipState =
        {
           tip = ExtendedBlockHeader.empty
           activeContractSet=acs
           ema=ema
        }
    blockRequests= Map.empty
}

let createChain (length:int) nonce start ema account =

    let blocks, _, account =
        [start.header.blockNumber..(start.header.blockNumber + (uint32 length) - 1ul)]
        |> Seq.fold (fun (blocks,ema,account) i ->
            let parent = List.head blocks
            let timestamp = timestamp + ((uint64 i) * 1000UL * 60UL * 10UL)
            let tx = createTransaction account
            let block = Block.createTemplate parent.header timestamp ema acs [tx] Hash.zero
            let block = {block with header ={ block.header with nonce = uint64 nonce,0UL}}

            let account = Account.addTransaction (Transaction.hash tx) tx account
            let ema = EMA.add chain timestamp ema

            let blocks = block :: blocks

            blocks, ema, account) ([start],ema,account)

    let blocks =
        blocks
        |> List.rev
        |> List.tail // Remove genesis block

    blocks,account

let createChainFromGenesis length nonce =
    let ema = EMA.add chain genesisBlock.header.timestamp ema
    createChain length nonce genesisBlock ema rootAccount

let getGenesisState session =
    BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
    |> Writer.unwrap
    |> snd

let validateChain session blocks state =
    List.fold (fun (_,state) block ->
        BlockHandler.validateBlock chain session.context.contractPath session block.header.timestamp block false state
        |> Writer.unwrap) ([], state) blocks

[<Test>]
let ``genesis block accepted``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let block = Block.createGenesis chain [Transaction.rootTx] (0UL,0UL)
    let blockHash = Block.hash block

    let events, state' =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    events |> should haveLength 2
    events |> should contain (EffectsWriter.EventEffect (BlockAdded (blockHash,block)))

    BlockRepository.tryGetTip session
    |> should equal (Some (state'.tipState.tip,
                           state'.tipState.activeContractSet,
                           state'.tipState.ema))

    state'.tipState.tip.status |> should equal ExtendedBlockHeader.MainChain
    state'.tipState.tip.header |> should equal block.header
    state'.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state'.tipState.activeContractSet |> should equal state'.memoryState.activeContractSet
    areOutpointsInSet session rootTxOutpoints UtxoSet.asDatabase |> should equal true

[<Test>]
let ``wrong genesis block should be rejected``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let block = Block.createGenesis chain [Transaction.rootTx] (0UL,1UL)
    let events, state' =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    events |> should haveLength 0
    state'.tipState.tip |> should equal ExtendedBlockHeader.empty
    state'.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state'.tipState.activeContractSet |> should equal state'.memoryState.activeContractSet
    areOutpointsInSet session rootTxOutpoints UtxoSet.asDatabase |> should equal false

[<Test>]
let ``validate new valid block which extended main chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let _, state =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap

    let tx = createTransaction rootAccount
    let block = Block.createTemplate genesisBlock.header timestamp state.tipState.ema acs [tx] Hash.zero
    let blockHash = Block.hash block


    // Mark the block as new so we will also have network command
    let _, state =
        BlockHandler.handleNewBlockHeader chain session (Array.empty) block.header state
        |> Writer.unwrap

    let events, state' =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    events |> should haveLength 3
    events |> should contain (EffectsWriter.EventEffect (BlockAdded (blockHash,block)))
    events |> should contain (EffectsWriter.NetworkCommand (PublishBlock block.header))

    BlockRepository.tryGetTip session
    |> should equal (Some (state'.tipState.tip,
                           state'.tipState.activeContractSet,
                           state'.tipState.ema))

    state'.tipState.tip.status |> should equal ExtendedBlockHeader.MainChain
    state'.tipState.tip.header |> should equal block.header
    state'.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state'.tipState.activeContractSet |> should equal state'.memoryState.activeContractSet
    areOutpointsInSet session rootTxOutpoints UtxoSet.asDatabase |> should equal false // SHOULD be spent
    areOutpointsInSet session (getTxOutpoints tx) UtxoSet.asDatabase |> should equal true

[<Test>]
let ``validate new invalid block which try to extended main chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let _, state =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap

    let tx = createTransaction rootAccount
    let block = Block.createTemplate genesisBlock.header timestamp state.tipState.ema acs [tx] Hash.zero
    let block = {block with txMerkleRoot = Hash.zero}

    // Mark the block as new so we will also have network command
    let _, state =
        BlockHandler.handleNewBlockHeader chain session (Array.empty) block.header state
        |> Writer.unwrap

    let events, state' =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    events |> should haveLength 0

    state'.tipState.tip.header |> should equal genesisBlock.header
    state'.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state'.tipState.activeContractSet |> should equal state'.memoryState.activeContractSet
    areOutpointsInSet session rootTxOutpoints UtxoSet.asDatabase |> should equal true
    areOutpointsInSet session (getTxOutpoints tx) UtxoSet.asDatabase |> should equal false

[<Test>]
let ``validating orphan block yield a request for block from network``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext

    let tx = createTransaction rootAccount
    let block = Block.createTemplate genesisBlock.header timestamp state.tipState.ema acs [tx] Hash.zero

    let events, state' =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    events |> should haveLength 1
    events |> should contain (EffectsWriter.NetworkCommand (GetBlock block.header.parent))

    state'.tipState.tip |> should equal ExtendedBlockHeader.empty
    state'.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state'.tipState.activeContractSet |> should equal state'.memoryState.activeContractSet
    areOutpointsInSet session rootTxOutpoints UtxoSet.asDatabase |> should equal false
    areOutpointsInSet session (getTxOutpoints tx) UtxoSet.asDatabase |> should equal false

[<Test>]
let ``validate new block which connect orphan chain which extend main chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext

    let tx = createTransaction rootAccount

    let ema = EMA.add chain genesisBlock.header.timestamp ema
    let block = Block.createTemplate genesisBlock.header timestamp ema acs [tx] Hash.zero
    let blockHash = Block.hash block

    // Sending orphan block first
    let _, state =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    // Now sending the genesis, which should roll forward the chain
    let events, state' =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap

    events |> should haveLength 3
    events |> should contain (EffectsWriter.EventEffect (BlockAdded (genesisBlockHash,genesisBlock)))
    events |> should contain (EffectsWriter.EventEffect (BlockAdded (blockHash,block)))

    BlockRepository.tryGetTip session
    |> should equal (Some (state'.tipState.tip,
                           state'.tipState.activeContractSet,
                           state'.tipState.ema))

    state'.tipState.tip.status |> should equal ExtendedBlockHeader.MainChain
    state'.tipState.tip.header |> should equal block.header
    state'.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state'.tipState.activeContractSet |> should equal state'.memoryState.activeContractSet
    areOutpointsInSet session rootTxOutpoints UtxoSet.asDatabase |> should equal false
    areOutpointsInSet session (getTxOutpoints tx) UtxoSet.asDatabase |> should equal true

[<Test>]
let ``validate new block which connect orphan chain which is not long enough to become main chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let mainChain,account = createChainFromGenesis 3 0
    let alternativeChain, sideChainAccount = createChainFromGenesis 2 1

    // validate main chain first
    let _,state = validateChain session mainChain state

    // now validating orphan chain which is not long enough
    // we reverse the order of block in order to make it orphan first
    let events ,state = validateChain session (List.rev alternativeChain) state

    let tip = List.last mainChain

    events |> should haveLength 0
    state.tipState.tip.header |> should equal tip.header
    state.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state.tipState.activeContractSet |> should equal state.memoryState.activeContractSet
    isAccountInSet session account UtxoSet.asDatabase |> should equal true
    isAccountInSet session sideChainAccount UtxoSet.asDatabase |> should equal false

[<Test>]
let ``orphan chain become longer than main chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let mainChain, account = createChainFromGenesis 3 0
    let alternativeChain, sideChainAccount = createChainFromGenesis 2 1

    // validate main chain first
    let _,state = validateChain session alternativeChain state

    // Check that the alternative chain, which is now main, is marked as main
    List.iter (fun block ->
        let blockHash = Block.hash block
        let extendedHeader = BlockRepository.getHeader session blockHash
        extendedHeader.status |> should equal ExtendedBlockHeader.MainChain) alternativeChain

    // now validating orphan chain which is longer
    // we reverse the order of block in order to make it orphan first
    let events,state = validateChain session (List.rev mainChain) state

    let tip = List.last mainChain

    // Making sure all the alternative chain status changed to Connected
    List.iter (fun block ->
        let blockHash = Block.hash block
        let extendedHeader = BlockRepository.getHeader session blockHash
        extendedHeader.status |> should equal ExtendedBlockHeader.Connected) alternativeChain

    // Making sure all the new main chain status changed to Main
    List.iter (fun block ->
        let blockHash = Block.hash block
        let extendedHeader = BlockRepository.getHeader session blockHash
        extendedHeader.status |> should equal ExtendedBlockHeader.MainChain) mainChain

    BlockRepository.tryGetTip session
    |> should equal (Some (state.tipState.tip,
                           state.tipState.activeContractSet,
                           state.tipState.ema))

    events |> should haveLength 6
    events.[0] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock alternativeChain.[0])))
    events.[1] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock alternativeChain.[1])))
    events.[2] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock mainChain.[0])))
    events.[3] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock mainChain.[1])))
    events.[4] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock mainChain.[2])))

    state.tipState.tip.status |> should equal ExtendedBlockHeader.MainChain
    state.tipState.tip.header |> should equal tip.header
    state.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state.tipState.activeContractSet |> should equal state.memoryState.activeContractSet
    isAccountInSet session account UtxoSet.asDatabase |> should equal true
    isAccountInSet session sideChainAccount UtxoSet.asDatabase |> should equal false

[<Test>]
let ``new block extend fork chain which become longest``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let mainChain, account = createChainFromGenesis 2 0
    let alternativeChain, sideChainAccount = createChainFromGenesis 1 1

    // validate main chain first
    let _,state = validateChain session alternativeChain state

    // now validating orphan chain which is longer
    // we reverse the order of block in order to make it orphan first
    let events,state = validateChain session mainChain state

    let tip = List.last mainChain

    events |> should haveLength 4

    events.[0] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock alternativeChain.[0])))
    events.[1] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock mainChain.[0])))
    events.[2] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock mainChain.[1])))
    events.[3] |> should equal (EffectsWriter.EventEffect (TipChanged state.tipState.tip.header))

    state.tipState.tip.header |> should equal tip.header
    state.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state.tipState.activeContractSet |> should equal state.memoryState.activeContractSet
    isAccountInSet session account UtxoSet.asDatabase |> should equal true
    isAccountInSet session sideChainAccount UtxoSet.asDatabase |> should equal false

[<Test>]
let ``2 orphan chains, one become longer than main chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session
    let orphanChain, orphanAccount =
        createChainFromGenesis 1 1
    let orphanBlock = List.head orphanChain

    let orphanEMA = EMA.add chain orphanBlock.header.timestamp state.tipState.ema

    let mainChain, account = createChainFromGenesis 2 0
    let sideChain1, _ = createChain 1 2 orphanBlock orphanEMA orphanAccount
    let sideChain2, sideChainAccount = createChain 2 3 orphanBlock orphanEMA orphanAccount

    // validate all chains
    let _,state = validateChain session mainChain state
    let _,state = validateChain session sideChain1 state
    let _,state = validateChain session sideChain2 state
    let events, state = validateChain session orphanChain state

    let tip = List.last sideChain2

    events |> should haveLength 6
    events.[0] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock mainChain.[0])))
    events.[1] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock mainChain.[1])))
    events.[2] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock orphanBlock)))
    events.[3] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock sideChain2.[0])))
    events.[4] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock sideChain2.[1])))
    events.[5] |> should equal (EffectsWriter.EventEffect (TipChanged state.tipState.tip.header))

    state.tipState.tip.header |> should equal tip.header
    state.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state.tipState.activeContractSet |> should equal state.memoryState.activeContractSet
    isAccountInSet session account UtxoSet.asDatabase |> should equal false
    isAccountInSet session sideChainAccount UtxoSet.asDatabase |> should equal true

[<Test>]
let ``2 orphan chains, two longer than main, one is longer``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session
    let orphanChain, orphanAccount =
        createChainFromGenesis 1 1
    let orphanBlock = List.head orphanChain

    let orphanEMA = EMA.add chain orphanBlock.header.timestamp state.tipState.ema

    let mainChain, account = createChainFromGenesis 2 0
    let sideChain1, _ = createChain 2 2 orphanBlock orphanEMA orphanAccount
    let sideChain2, sideChainAccount = createChain 3 3 orphanBlock orphanEMA orphanAccount

    // validate all chains
    let _,state = validateChain session mainChain state
    let _,state = validateChain session sideChain1 state
    let _,state = validateChain session sideChain2 state
    let events, state = validateChain session orphanChain state

    let tip = List.last sideChain2

    events |> should haveLength 7
    events.[0] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock mainChain.[0])))
    events.[1] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock mainChain.[1])))
    events.[2] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock orphanBlock)))
    events.[3] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock sideChain2.[0])))
    events.[4] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock sideChain2.[1])))
    events.[5] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock sideChain2.[2])))
    events.[6] |> should equal (EffectsWriter.EventEffect (TipChanged state.tipState.tip.header))

    state.tipState.tip.header |> should equal tip.header
    state.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state.tipState.activeContractSet |> should equal state.memoryState.activeContractSet
    isAccountInSet session account UtxoSet.asDatabase |> should equal false
    isAccountInSet session sideChainAccount UtxoSet.asDatabase |> should equal true

[<Test>]
let ``2 orphan chains, two longer than main, longest is invalid, shuld pick second long``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session
    let orphanChain, orphanAccount =
        createChainFromGenesis 1 1
    let orphanBlock = List.head orphanChain

    let orphanEMA = EMA.add chain orphanBlock.header.timestamp state.tipState.ema

    let mainChain, account = createChainFromGenesis 2 0

    // we are creatig invalid sidchain by giving sideChain1 the wrong account which will create invalid chain
    let sideChain1, _ = createChain 3 2 orphanBlock orphanEMA account
    let sideChain2, sideChainAccount = createChain 2 3 orphanBlock orphanEMA orphanAccount

    // validate all chains
    let _,state = validateChain session mainChain state
    let _,state = validateChain session sideChain1 state
    let _,state = validateChain session sideChain2 state
    let events, state = validateChain session orphanChain state

    let tip = List.last sideChain2

    events |> should haveLength 6
    events.[0] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock mainChain.[0])))
    events.[1] |> should equal (EffectsWriter.EventEffect (BlockRemoved (hashBlock mainChain.[1])))
    events.[2] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock orphanBlock)))
    events.[3] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock sideChain2.[0])))
    events.[4] |> should equal (EffectsWriter.EventEffect (BlockAdded (hashBlock sideChain2.[1])))
    events.[5] |> should equal (EffectsWriter.EventEffect (TipChanged state.tipState.tip.header))

    state.tipState.tip.header |> should equal tip.header
    state.memoryState.utxoSet |> should equal UtxoSet.asDatabase
    state.tipState.activeContractSet |> should equal state.memoryState.activeContractSet
    isAccountInSet session account UtxoSet.asDatabase |> should equal false
    isAccountInSet session sideChainAccount UtxoSet.asDatabase |> should equal true

[<Test>]
let ``transaction in mempool and not in next block stays in mempool``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session
    let chain,account = createChainFromGenesis 1 0
    let tx1 = createTransaction rootAccount
    let tx2 = createTransaction account
    let txHash1 = Transaction.hash tx1
    let txHash2 = Transaction.hash tx2

    let mempool =
        state.memoryState.mempool
        |> MemPool.add txHash1 tx1
        |> MemPool.add txHash2 tx2

    let utxoSet =
        state.memoryState.utxoSet
        |> UtxoSet.handleTransaction (UtxoSetRepository.get session) txHash1 tx1
        |> UtxoSet.handleTransaction (UtxoSetRepository.get session) txHash2 tx2

    let state = {
        state with memoryState = {state.memoryState with utxoSet=utxoSet;mempool=mempool}
    }

    let _, state = validateChain session chain state

    Map.containsKey txHash2 state.memoryState.mempool  |> should equal true
    Map.containsKey txHash1 state.memoryState.mempool  |> should equal false
    areOutpointsInSet session (getTxOutpoints tx1) state.memoryState.utxoSet |> should equal false
    areOutpointsInSet session (getTxOutpoints tx2) state.memoryState.utxoSet |> should equal true
    areOutpointsInSet session (getTxOutpoints tx1) UtxoSet.asDatabase |> should equal true
    areOutpointsInSet session (getTxOutpoints tx2) UtxoSet.asDatabase |> should equal false

[<Test>]
let ``orphan transactions added to mempool after origin tx found in block``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session
    let chain,account = createChainFromGenesis 1 0
    let tx1 = createTransaction account
    let txHash1 = Transaction.hash tx1
    let tx2 =
        Account.addTransaction txHash1 tx1 account
        |> createTransaction
    let txHash2 = Transaction.hash tx2

    let orphanPool =
        state.memoryState.orphanPool
        |> OrphanPool.add txHash2 tx2
        |> OrphanPool.add txHash1 tx1

    let state = {
        state with memoryState = {state.memoryState with orphanPool=orphanPool}
    }

    let _, state = validateChain session chain state

    Map.containsKey txHash1 state.memoryState.mempool |> should equal true
    Map.containsKey txHash2 state.memoryState.mempool |> should equal true
    state.memoryState.orphanPool |> should haveCount 0
    areOutpointsInSet session (getTxOutpoints tx1) state.memoryState.utxoSet |> should equal false
    areOutpointsInSet session (getTxOutpoints tx1) UtxoSet.asDatabase |> should equal false
    areOutpointsInSet session (getTxOutpoints tx2) state.memoryState.utxoSet |> should equal true
    areOutpointsInSet session (getTxOutpoints tx2) UtxoSet.asDatabase |> should equal false

[<Test>]
let ``block with a contract activation is added to chain``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let cHash = Contract.computeHash sampleContractCode
    let tx =
        Account.createActivateContractTransaction rootAccount sampleContractCode
        |>  function
            | Ok tx -> tx
            | Error error -> failwith error

    let contract = {
        hash = cHash
        fn = fun _ _ _ _ tx -> Ok (tx,None)
        costFn = fun _ _ _ tx -> Ok 1I
    }

    let acs = ActiveContractSet.add cHash contract state.tipState.activeContractSet

    let block = Block.createTemplate genesisBlock.header timestamp state.tipState.ema acs [tx] Hash.zero

    let events, state' =
            BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
            |> Writer.unwrap

    events |> should haveLength 2
    events |> should contain (EffectsWriter.EventEffect (BlockAdded (hashBlock block)))

    let tip = BlockRepository.tryGetTip session

    tip |> should be some

    let _,acs,_ = (Option.get tip)

    SparseMerkleTree.root acs |> should equal (SparseMerkleTree.root state'.tipState.activeContractSet)

[<Test>]
let ``validate new block header should ask for block``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let block =
        createChainFromGenesis 1 0
        |> fst
        |> List.head

    let events, _ =
        BlockHandler.handleNewBlockHeader chain session Array.empty block.header state
        |> Writer.unwrap

    events |> should haveLength 1
    events.[0] |> should equal (EffectsWriter.NetworkCommand (GetNewBlock (Array.empty,(Block.hash block))))

[<Test>]
let ``validate new block header which we already asked for``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let block =
        createChainFromGenesis 1 0
        |> fst
        |> List.head

    let _, state =
        BlockHandler.handleNewBlockHeader chain session Array.empty block.header state
        |> Writer.unwrap
    let events, _ =
        BlockHandler.handleNewBlockHeader chain session Array.empty block.header state
        |> Writer.unwrap

    events |> should haveLength 0

[<Test>]
let ``new block should publish to network``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let block =
        createChainFromGenesis 1 0
        |> fst
        |> List.head

    let _, state =
        BlockHandler.handleNewBlockHeader chain session Array.empty block.header state
        |> Writer.unwrap
    let events, _ =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    events |> should contain (EffectsWriter.NetworkCommand (PublishBlock block.header))

[<Test>]
let ``mined block should publish to network``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let block =
        createChainFromGenesis 1 0
        |> fst
        |> List.head

    let events, _ =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block true state
        |> Writer.unwrap

    events |> should contain (EffectsWriter.NetworkCommand (PublishBlock block.header))

[<Test>]
let ``validate new tip should ask for block``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let block =
        createChainFromGenesis 1 0
        |> fst
        |> List.head

    let events, _ =
        BlockHandler.handleTip chain session block.header state
        |> Writer.unwrap

    events |> should haveLength 1
    events.[0] |> should equal (EffectsWriter.NetworkCommand (GetBlock (Block.hash block)))

[<Test>]
let ``validate new tip which we already asked for``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let block =
        createChainFromGenesis 1 0
        |> fst
        |> List.head

    let _, state =
        BlockHandler.handleTip chain session block.header state
        |> Writer.unwrap
    let events, _ =
        BlockHandler.handleTip chain session block.header state
        |> Writer.unwrap

    events |> should haveLength 0

[<Test>]
let ``tip block should not be publish to network``() =
    use databaseContext = DatabaseContext.createEmpty "test"

    use session = DatabaseContext.createSession databaseContext
    let state = getGenesisState session

    let block =
        createChainFromGenesis 1 0
        |> fst
        |> List.head

    let _, state =
        BlockHandler.handleTip chain session block.header state
        |> Writer.unwrap
    let events, _ =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp block false state
        |> Writer.unwrap

    events |> should not' (contain (EffectsWriter.NetworkCommand (PublishBlock block.header)))

[<Test>]
let ``Valid template for two transactions which don't depend on each other``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let _, genesisState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap
    
    let balances = Account.getBalance rootAccount
    let asset, amount = balances |> Map.toList |> List.head
    let firstAmount = amount / 4UL
    let secondAmount = amount - firstAmount
    let splitTx =
        Account.createTransaction chain rootAccount rootAccount.publicKeyHash {asset=asset;amount=firstAmount}
        |>  Result.get
    let ema' = EMA.add chain genesisBlock.header.timestamp ema
    let splitBlock = Block.createTemplate genesisBlock.header timestamp ema' acs [splitTx] Hash.zero

    let account = Account.addTransaction (Transaction.hash splitTx) splitTx rootAccount
    let ema'' = EMA.add chain timestamp ema'
    ema''.delayed.Length |> should equal 2
  
    let _, splitState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp splitBlock false genesisState
        |> Writer.unwrap
    let splitOutputs = Account.getUnspentOutputs account

    let txOfPOutput (outpoint, output) =
        let spend = output.spend
        let outputs = [{spend=spend;lock=PK rootAccount.publicKeyHash}]
        Transaction.sign
            [ rootAccount.keyPair ]
            {
                inputs = [outpoint];
                outputs = outputs;
                witnesses=[];
                contract=None
            }
    let tx1, tx2 =
        splitOutputs |>
        Map.toList |>
        List.map txOfPOutput |>
        function
        | [t1;t2] -> (t1,t2)
        | _ -> failwith "Wrong number of outputs"

    let twoTxBlock = Block.createTemplate splitBlock.header (timestamp+100UL) splitState.tipState.ema acs [tx1;tx2] Hash.zero
    let oldHash = splitState.tipState.tip.hash
    let _, withTxsState =
        BlockHandler.validateBlock chain session.context.contractPath session (timestamp+100UL) twoTxBlock false splitState
        |> Writer.unwrap
    withTxsState.tipState.tip.hash |> should not' (equal oldHash)
    withTxsState.tipState.tip.hash |> should equal (Block.hash twoTxBlock)

[<Test>]
let ``Two transactions in the same block which depend on each other are valid``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let _, genesisState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap
    
    let balances = Account.getBalance rootAccount
    let asset, amount = balances |> Map.toList |> List.head
    let firstAmount = amount
    let firstTx =
        Account.createTransaction chain rootAccount rootAccount.publicKeyHash {asset=asset;amount=firstAmount}
        |>  Result.get
    let account = Account.addTransaction (Transaction.hash firstTx) firstTx rootAccount
    let secondTx =
        Account.createTransaction chain account rootAccount.publicKeyHash {asset=asset;amount=firstAmount}
        |>  Result.get
    let ema' = EMA.add chain genesisBlock.header.timestamp ema
    let twoTxBlock = Block.createTemplate genesisBlock.header timestamp ema' acs [firstTx;secondTx] Hash.zero
    let oldHash = genesisState.tipState.tip.hash

    let _, twoTxState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp twoTxBlock false genesisState
        |> Writer.unwrap
    twoTxState.tipState.tip.hash |> should not' (equal oldHash)
    twoTxState.tipState.tip.hash |> should equal (Block.hash twoTxBlock)

[<Test>]
let ``Two transactions in the same block which depend on each other are invalid if in reversed order``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let _, genesisState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap
    
    let balances = Account.getBalance rootAccount
    let asset, amount = balances |> Map.toList |> List.head
    let firstAmount = amount
    let firstTx =
        Account.createTransaction chain rootAccount rootAccount.publicKeyHash {asset=asset;amount=firstAmount}
        |>  Result.get
    let account = Account.addTransaction (Transaction.hash firstTx) firstTx rootAccount
    let secondTx =
        Account.createTransaction chain account rootAccount.publicKeyHash {asset=asset;amount=firstAmount}
        |>  Result.get
    let ema' = EMA.add chain genesisBlock.header.timestamp ema
    let twoTxBlock = Block.createTemplate genesisBlock.header timestamp ema' acs [secondTx;firstTx] Hash.zero
    let oldHash = genesisState.tipState.tip.hash

    let _, twoTxState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp twoTxBlock false genesisState
        |> Writer.unwrap
    twoTxState.tipState.tip.hash |> should not' (equal (Block.hash twoTxBlock))
    twoTxState.tipState.tip.hash |> should equal oldHash

[<Test>]
let ``Template builder uses two transactions in the same block which depend on each other``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let _, genesisState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap
    
    let balances = Account.getBalance rootAccount
    let asset, amount = balances |> Map.toList |> List.head
    let firstAmount = amount
    let firstTx =
        Account.createTransaction chain rootAccount rootAccount.publicKeyHash {asset=asset;amount=firstAmount}
        |>  Result.get
    let account = Account.addTransaction (Transaction.hash firstTx) firstTx rootAccount
    let secondTx =
        Account.createTransaction chain account rootAccount.publicKeyHash {asset=asset;amount=firstAmount}
        |>  Result.get
    let _, updatedState = Writer.unwrap <| Handler.handleCommand chain (Blockchain.Command.ValidateTransaction firstTx) session (timestamp+1UL) genesisState
    let _, updatedState = Writer.unwrap <| Handler.handleCommand chain (Blockchain.Command.ValidateTransaction secondTx) session (timestamp+1UL) updatedState
    let ema' = EMA.add chain genesisBlock.header.timestamp ema
    let memState, validatedTransactions = BlockTemplateBuilder.makeTransactionList session updatedState

    let twoTxBlock = Block.createTemplate genesisBlock.header (timestamp+1UL) ema' memState.activeContractSet validatedTransactions Hash.zero
    let oldHash = genesisState.tipState.tip.hash
    let _, twoTxState =
        BlockHandler.validateBlock chain session.context.contractPath session (timestamp+1UL) twoTxBlock false genesisState
        |> Writer.unwrap
    twoTxState.tipState.tip.hash |> should not' (equal oldHash)
    twoTxState.tipState.tip.hash |> should equal (Block.hash twoTxBlock)
    List.length twoTxBlock.transactions |> should equal 3       // includes coinbase tx

[<Test>]
let ``Out of order dependent transactions are rearranged``() =
    use databaseContext = DatabaseContext.createEmpty "test"
    use session = DatabaseContext.createSession databaseContext

    let _, genesisState =
        BlockHandler.validateBlock chain session.context.contractPath session timestamp genesisBlock false state
        |> Writer.unwrap
    
    let balances = Account.getBalance rootAccount
    let asset, amount = balances |> Map.toList |> List.head
    let firstTx =
        Account.createTransaction chain rootAccount rootAccount.publicKeyHash {asset=asset;amount=amount}
        |>  Result.get
    let account = Account.addTransaction (Transaction.hash firstTx) firstTx rootAccount
    let secondTx =
        Account.createTransaction chain account rootAccount.publicKeyHash {asset=asset;amount=amount}
        |>  Result.get
    let _, updatedState = Writer.unwrap <| Handler.handleCommand chain (Blockchain.Command.ValidateTransaction firstTx) session (timestamp+1UL) genesisState
    let _, updatedState = Writer.unwrap <| Handler.handleCommand chain (Blockchain.Command.ValidateTransaction secondTx) session (timestamp+1UL) updatedState
    let blockNumber = updatedState.tipState.tip.header.blockNumber
    let acs = updatedState.tipState.activeContractSet
    let _, validatedTransactions = BlockTemplateBuilder.selectOrderedTransactions session blockNumber acs [firstTx;secondTx]
    let _, validatedTransactions_ = BlockTemplateBuilder.selectOrderedTransactions session blockNumber acs [secondTx;firstTx]

    List.length validatedTransactions |> should equal 2
    validatedTransactions |> should equal validatedTransactions_

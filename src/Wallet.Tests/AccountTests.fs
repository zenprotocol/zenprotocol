module Wallet.Tests.AccountTests

open NUnit.Framework
open FsUnit
open Consensus
open Consensus.Chain
open Consensus.Types
open Wallet
open TestsInfrastructure.Constraints

let chain = Chain.Local
let chainParams = Chain.localParameters

let balanceShouldBe asset expected account =
    let balance = Account.getBalance account

    let actual =
        match Map.tryFind asset balance with
        | Some value -> value
        | None -> 0UL

    actual |> should equal expected

let anotherAsset = Hash.compute "anotherasset"B, Hash.compute "anotherasset"B

[<Test>]
let ``received tokens``() =
    let account = Account.create()

    let output = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}

    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}

    let account' = Account.addTransaction (Transaction.hash tx) tx account

    account' |> balanceShouldBe Constants.Zen 10UL

[<Test>]
let ``tokens spent``() =
    let account = Account.create()

    let output = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}

    let tx = {inputs=[];outputs=[output;output];witnesses=[];contract=None}
    let txHash = (Transaction.hash tx)

    let tx' = {inputs=[ Outpoint {txHash=txHash; index=0ul}];outputs=[];witnesses=[];contract=None}

    let account' =
        Account.addTransaction txHash tx account

    let account'' =
        Account.addTransaction (Transaction.hash tx') tx' account'

    account' |> balanceShouldBe Constants.Zen 20UL
    account'' |> balanceShouldBe Constants.Zen 10UL

[<Test>]
let ``creating, not enough tokens``() =
    let account = Account.create()

    let output = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}

    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}

    let account' = Account.addTransaction (Transaction.hash tx) tx account

    let result = Account.createTransaction account account.publicKeyHash { asset = Constants.Zen; amount = 11UL }

    let expected:Result<Transaction,string> = Error "Not enough tokens"

    result |> should equal expected

[<Test>]
let ``creating, no change``() =
    let bob = Account.create ()
    let alice = Account.create ()

    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction bob' alice.publicKeyHash { asset = Constants.Zen; amount = 10UL }

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob

        alice' |> balanceShouldBe Constants.Zen 10UL
        bob'' |> balanceShouldBe Constants.Zen 0UL

[<Test>]
let ``creating, with change``() =
    let bob = Account.create ()
    let alice = Account.create ()

    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction bob' alice.publicKeyHash { asset = Constants.Zen; amount = 7UL }

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob

        alice' |> balanceShouldBe Constants.Zen 7UL
        bob'' |> balanceShouldBe Constants.Zen 3UL

[<Test>]
let ``picking the correct asset``() =
    let bob = Account.create ()
    let alice = Account.create ()

    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=anotherAsset;amount=10UL}}
    let output2 = {lock = PK bob.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}

    let tx = {inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    Account.getUnspentOutputs bob' |> fst |> should haveCount 2
    bob' |> balanceShouldBe anotherAsset 10UL

    // sending money to alice
    let result = Account.createTransaction bob' alice.publicKeyHash { asset = Constants.Zen; amount = 7UL }

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        List.length tx.inputs |> should equal 1

        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob'

        alice' |> balanceShouldBe Constants.Zen 7UL
        bob'' |> balanceShouldBe Constants.Zen 3UL

        alice' |> balanceShouldBe anotherAsset 0UL
        bob'' |> balanceShouldBe anotherAsset 10UL

[<Test>]
let ``picking from multiple inputs``() =
    let bob = Account.create ()
    let alice = Account.create ()

    // giving some money to bob
    let output = {lock = PK bob.publicKeyHash; spend={asset=Constants.Zen;amount=5UL}}
    let output2 = {lock = PK bob.publicKeyHash; spend={asset=Constants.Zen;amount=7UL}}

    let tx = {inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction bob' alice.publicKeyHash { asset = Constants.Zen; amount = 10UL }

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob'

        alice' |> balanceShouldBe Constants.Zen 10UL
        bob'' |> balanceShouldBe Constants.Zen 2UL

[<Test>]
let ``create execute contract transaction``() =
    let account = Account.createTestAccount ()

    let executeContract _ _ _ txSkeleton =
        let tx =
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract Hash.zero;spend={asset=Constants.Zen;amount=1UL}}
            |> Transaction.fromTxSkeleton

        tx |> Ok

    let spends = Map.add Constants.Zen 1UL Map.empty

    let result = Account.createExecuteContractTransaction account executeContract Hash.zero Zen.Types.Data.Empty Contract.EmptyData true spends

    result |> should be ok

    let tx =
        match result with
        | Ok tx -> tx
        | Error error -> failwith error

    tx.inputs |> should haveLength 1


[<Test>]
let ``account sync up``() =
    let startBlockHeader = {
        version = 0ul
        parent = chainParams.genesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let account =
        {Account.create () with tip = Block.hash startBlockHeader}

    let output = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let txHash = Transaction.hash tx

    let account = Account.addTransaction txHash tx account

    List.exists (fst >> (=) txHash) account.mempool |> should equal true

    let header = {
        version = 0ul
        parent = Block.hash startBlockHeader
        blockNumber = 3ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let block = {
        header = header
        transactions = [tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot= Hash.zero
        commitments= []
    }

    let blockHash = Block.hash block.header

    let account' = Account.sync chainParams blockHash (fun _ -> startBlockHeader) (fun _ -> block) account

    account'.tip |> should equal blockHash
    account'.mempool |> should haveLength 0

[<Test>]
let ``sync up from empty wallet``() =
    let account = Account.create ()

    let output = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}

    let header = {
        version = 0ul
        parent = chainParams.genesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let block = {
        header = header
        transactions = [tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot= Hash.zero
        commitments = []
    }

    let blockHash = Block.hash block.header

    let account = Account.sync chainParams blockHash (fun _ -> failwith "unexpected")
                    (fun blockHash ->
                        if blockHash = chainParams.genesisHash then
                            Block.createGenesis chainParams [Transaction.rootTx] (0UL,0UL)
                        else
                            block) account

    account.tip |> should equal blockHash
    account.mempool |> should haveLength 0
    Account.getUnspentOutputs account |> fst |> should haveCount 1

[<Test>]
let ``account reorg``() =
    let startBlockHeader = {
        version = 0ul
        parent = (Chain.getChainParameters chain).genesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let account =
        {Account.create () with tip = Block.hash startBlockHeader}

    let output = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}
    let tx = {inputs=[];outputs=[output];witnesses=[];contract=None}
    let txHash = Transaction.hash tx

    let account = Account.addTransaction txHash tx account

    let header = {
        version = 0ul
        parent = Block.hash startBlockHeader
        blockNumber = 3ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let block = {
        header = header
        transactions = [tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot= Hash.zero
        commitments= []
    }

    let blockHash = Block.hash block.header

    let account = Account.sync chainParams blockHash (fun _ -> startBlockHeader) (fun _ -> block) account
    account.tip |> should equal blockHash

    let header2 = {
       version = 0ul
       parent = Block.hash startBlockHeader
       blockNumber = 3ul
       commitments = Hash.zero
       timestamp = 0UL
       difficulty = 0ul
       nonce = 1UL,0UL
    }

    let block2 = {
       header = header2
       transactions = []
       txMerkleRoot = Hash.zero
       witnessMerkleRoot = Hash.zero
       activeContractSetMerkleRoot= Hash.zero
       commitments= []
    }

    let blockHash2 = Block.hash block2.header

    let account = Account.sync chainParams blockHash2 (fun _ -> block.header)
                        (fun bh ->
                            if bh = blockHash then
                                block
                            else if bh = blockHash2 then
                                block2
                            else
                                failwithf "unexpected %A" bh) account

    account.tip |> should equal blockHash2
    account.mempool |> should haveLength 1
    List.exists (fst >> (=) txHash) account.mempool |> should equal true

[<Test>]
let ``wallet won't spend coinbase if not mature enough``() =
    let rootAccount = {Account.rootAccount with blockNumber=99ul}
    let origin =
            {
                inputs=[]
                outputs=[{lock =  Coinbase (1ul, rootAccount.publicKeyHash); spend= {asset = Constants.Zen;amount=100000000UL}}]
                witnesses=[]
                contract=None
            }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    let expected: Result<Transaction,string>= Error "Not enough tokens"

    Account.createTransaction rootAccount rootAccount.publicKeyHash { asset = Constants.Zen; amount = 1UL }
    |> should equal expected

[<Test>]
let ``wallet spend coinbase with coinbase mature enough``() =
    let rootAccount = {Account.rootAccount with blockNumber=100ul}
    let origin =
            {
                inputs=[]
                outputs=[{lock =  Coinbase (1ul, rootAccount.publicKeyHash); spend= {asset = Constants.Zen;amount=100000000UL}}]
                witnesses=[]
                contract=None
            }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    Account.createTransaction rootAccount rootAccount.publicKeyHash { asset = Constants.Zen; amount = 1UL }
    |> should be ok

[<Test>]
let ``wallet spend coinbase when come from block``() =
    let rootAccount = Account.rootAccount
    let origin =
            {
                inputs=[]
                outputs=[{lock =  Coinbase (1ul, rootAccount.publicKeyHash); spend= {asset = Constants.Zen;amount=100000000UL}}]
                witnesses=[]
                contract=None
            }

    let header =
        {
            version = Block.Version
            parent = Hash.zero
            blockNumber = 100ul
            difficulty = 0x20fffffful;
            commitments=Hash.zero;
            timestamp = 0UL
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=[origin];commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    let rootAccount = Account.handleBlock (Block.hash block.header) block rootAccount

    Account.createTransaction rootAccount rootAccount.publicKeyHash { asset = Constants.Zen; amount = 1UL }
    |> should be ok

[<Test>]
let ``Should get expected deltas``() =
    let account = Account.create ()

    let output1 = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=10UL}}
    let tx1 = {inputs=[];outputs=[output1];witnesses=[];contract=None}
    let tx1Hash = Transaction.hash tx1
    let output2A = {lock = PK Hash.zero; spend={asset=Constants.Zen;amount=2UL}}
    let output2B = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=8UL}}
    let tx2 = {inputs=[Outpoint { txHash = tx1Hash; index = 0ul } ];outputs=[output2A;output2B];witnesses=[];contract=None}

    let header = {
        version = 0ul
        parent = chainParams.genesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let block = {
        header = header
        transactions = [tx1;tx2]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot= Hash.zero
        commitments = []
    }

    let blockHash = Block.hash block.header

    let account = Account.sync chainParams blockHash (fun _ -> failwith "unexpected")
                    (fun blockHash ->
                        if blockHash = chainParams.genesisHash then
                            Block.createGenesis chainParams [Transaction.rootTx] (0UL,0UL)
                        else
                            block) account

    let tx2Hash = Transaction.hash tx2
    let output3A = {lock = PK Hash.zero; spend={asset=Constants.Zen;amount=3UL}}
    let output3B = {lock = PK account.publicKeyHash; spend={asset=Constants.Zen;amount=5UL}}
    let tx3 = {inputs=[Outpoint { txHash = tx2Hash; index = 1ul } ];outputs=[output3A;output3B];witnesses=[];contract=None}

    let expected = [ (tx1Hash, Map.add Constants.Zen 10L Map.empty) ]
    let expected = expected @ [ (tx2Hash, Map.add Constants.Zen -2L Map.empty) ]

    should equal expected (Account.getHistory account)

    let tx3Hash = Transaction.hash tx3

    // add tx3 to mempool
    let account' = Account.addTransaction tx3Hash tx3 account

    let expected = expected @ [ (tx3Hash, Map.add Constants.Zen -3L Map.empty) ]

    should equal expected (Account.getHistory account')
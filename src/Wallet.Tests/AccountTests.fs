module Wallet.Tests.AccountTests

open NUnit.Framework
open FsUnit
open Consensus
open Chain
open Types
open Wallet
open TestsInfrastructure.Constraints
open Consensus.Tests.Helper
open Infrastructure

let chain = Chain.Local
let chainParams = Chain.localParameters

let balanceShouldBe asset expected account =
    let balance = Account.getBalance account

    let actual =
        match Map.tryFind asset balance with
        | Some value -> value
        | None -> 0UL

    actual |> should equal expected

let anotherAsset = Asset (ContractId (Version0,Hash.compute "anotherasset"B), Hash.compute "anotherasset"B)

[<Test>]
let ``received tokens``() =
    let account, _ = create()

    let output = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

    let account' = Account.addTransaction (Transaction.hash tx) tx account

    account' |> balanceShouldBe Asset.Zen 10UL

[<Test>]
let ``tokens spent``() =
    let account, _ = create()

    let output = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output;output];witnesses=[];contract=None}
    let txHash = (Transaction.hash tx)

    let tx' = {version=Version0;inputs=[ Outpoint {txHash=txHash; index=0ul}];outputs=[];witnesses=[];contract=None}

    let account' =
        Account.addTransaction txHash tx account

    let account'' =
        Account.addTransaction (Transaction.hash tx') tx' account'

    account' |> balanceShouldBe Asset.Zen 20UL
    account'' |> balanceShouldBe Asset.Zen 10UL

[<Test>]
let ``creating, not enough tokens``() =
    let accountData = create()
    let account, _ = accountData

    let output = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

    let account' = Account.addTransaction (Transaction.hash tx) tx account

    let result = Account.createTransaction (publicKeyHash account) { asset = Asset.Zen; amount = 11UL } accountData

    let expected:Result<Transaction,string> = Error "Not enough tokens"

    result |> should equal expected

[<Test>]
let ``creating, no change``() =
    let bob, bobKey = create()
    let alice, _ = create()

    // giving some money to bob
    let output = {lock = PK (publicKeyHash bob); spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction (publicKeyHash alice) { asset = Asset.Zen; amount = 10UL } (bob', bobKey)

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob

        alice' |> balanceShouldBe Asset.Zen 10UL
        bob'' |> balanceShouldBe Asset.Zen 0UL

[<Test>]
let ``creating, with change``() =
    let bob, bobKey = create()
    let alice, _ = create()

    // giving some money to bob
    let output = {lock = PK (publicKeyHash bob); spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction (publicKeyHash alice) { asset = Asset.Zen; amount = 7UL } (bob', bobKey)

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob

        alice' |> balanceShouldBe Asset.Zen 7UL
        bob'' |> balanceShouldBe Asset.Zen 3UL

[<Test>]
let ``picking the correct asset``() =
    let bob, bobKey = create()
    let alice, _ = create()

    // giving some money to bob
    let output = {lock = PK (publicKeyHash bob); spend={asset=anotherAsset;amount=10UL}}
    let output2 = {lock = PK (publicKeyHash bob); spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    Account.getUnspentOutputs bob' |> fst |> should haveCount 2
    bob' |> balanceShouldBe anotherAsset 10UL

    // sending money to alice
    let result = Account.createTransaction (publicKeyHash alice) { asset = Asset.Zen; amount = 7UL } (bob', bobKey)

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        List.length tx.inputs |> should equal 1

        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob'

        alice' |> balanceShouldBe Asset.Zen 7UL
        bob'' |> balanceShouldBe Asset.Zen 3UL

        alice' |> balanceShouldBe anotherAsset 0UL
        bob'' |> balanceShouldBe anotherAsset 10UL

[<Test>]
let ``picking from multiple inputs``() =
    let bob, bobKey = create()
    let alice, _ = create()

    // giving some money to bob
    let output = {lock = PK (publicKeyHash bob); spend={asset=Asset.Zen;amount=5UL}}
    let output2 = {lock = PK (publicKeyHash bob); spend={asset=Asset.Zen;amount=7UL}}

    let tx = {version=Version0;inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob' = Account.addTransaction (Transaction.hash tx) tx bob

    // sending money to alice
    let result = Account.createTransaction (publicKeyHash alice) { asset = Asset.Zen; amount = 10UL } (bob', bobKey)

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let alice' = Account.addTransaction (Transaction.hash tx) tx alice
        let bob'' = Account.addTransaction (Transaction.hash tx) tx bob'

        alice' |> balanceShouldBe Asset.Zen 10UL
        bob'' |> balanceShouldBe Asset.Zen 2UL

[<Test>]
let ``create execute contract transaction``() =
    let account = createTestAccount ()

    let executeContract _ _ _ _ txSkeleton =
        let tx =
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract <| ContractId  (Version0,Hash.zero);spend={asset=Asset.Zen;amount=1UL}}
            |> Transaction.fromTxSkeleton

        tx |> Ok

    let spends = Map.add Asset.Zen 1UL Map.empty

    let result = Account.createExecuteContractTransaction executeContract (ContractId (Version0,Hash.zero)) "" None true None spends account

    result |> should be ok

    let tx =
        match result with
        | Ok tx -> tx
        | Error error -> failwith error

    tx.inputs |> should haveLength 1

[<Test>]
let ``create execute contract transaction without explicitly spending any Zen should allocate fee``() =
    let account = createTestAccount ()

    let executeContract _ _ _ _ txSkeleton =
        let tx =
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract <| ContractId  (Version0,Hash.zero);spend={asset=Asset.Zen;amount=1UL}}
            |> Transaction.fromTxSkeleton

        tx |> Ok

    let spends = Map.empty

    let result = Account.createExecuteContractTransaction executeContract (ContractId (Version0,Hash.zero)) "" None true None spends account

    result |> should be ok

    let tx = Result.get result
    
    tx.outputs
    |> List.exists (fun output -> output.lock = Fee && output.spend = { asset = Asset.Zen; amount = 1UL })
    |> should equal true

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

    let accountData = create()
    let account =
        { (fst accountData)  with tip = Block.hash startBlockHeader}

    let output = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
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

    let account' = Account.sync blockHash (fun _ -> startBlockHeader) (fun _ -> block) account

    account'.tip |> should equal blockHash
    account'.mempool |> should haveLength 0

[<Test>]
let ``sync up from empty wallet``() =
    let account, _ = create()

    let output = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

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

    let genesisBlock = Block.createGenesis chainParams [rootTx] (0UL,0UL)
    let genesisHeader = genesisBlock.header

    let getHeader = function
    | h when h = chainParams.genesisHash -> genesisHeader
    | h when h = blockHash -> header
    | h -> failwithf "unexpected block hash %A" h

    let getBlock = function
    | h when h = chainParams.genesisHash -> genesisBlock
    | h when h = blockHash -> block
    | h -> failwithf "unexpected block hash %A" h

    let account = Account.sync blockHash getHeader getBlock account

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

    let startHash = Block.hash startBlockHeader
    let account =
        {(create() |> fst) with tip = startHash}

    let output = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
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

    let getHeader = function
    | h when h = startHash -> startBlockHeader
    | h when h = blockHash -> header
    | h when h = blockHash2 -> header2
    | bh -> failwithf "unexpected %A" bh

    let getBlock = function
    | h when h = startHash -> failwithf "Unexpected deref of start block"
    | h when h = blockHash -> block
    | h when h = blockHash2 -> block2
    | bh -> failwithf "unexpected %A" bh

    let account = Account.sync blockHash getHeader getBlock account
    account.tip |> should equal blockHash

    let account = Account.sync blockHash2 getHeader getBlock account

    account.tip |> should equal blockHash2
    account.mempool |> should haveLength 1
    List.exists (fst >> (=) txHash) account.mempool |> should equal true

[<Test>]
let ``wallet won't spend coinbase if not mature enough``() =
    let rootAccount, rootSecretKey = rootAccountData
    let rootAccount = {rootAccount with blockNumber=99ul}
    let origin =
            {
                version=Version0
                inputs=[]
                outputs=[{lock =  Coinbase (1ul, publicKeyHash rootAccount); spend= {asset = Asset.Zen;amount=100000000UL}}]
                witnesses=[]
                contract=None
            }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    let expected: Result<Transaction,string>= Error "Not enough tokens"

    Account.createTransaction (publicKeyHash rootAccount) { asset = Asset.Zen; amount = 1UL } rootAccountData
    |> should equal expected

[<Test>]
let ``wallet spend coinbase with coinbase mature enough``() =
    let rootAccount, rootSecretKey = rootAccountData
    let rootAccount = {rootAccount with blockNumber=100ul}
    let origin =
            {
                version=Version0
                inputs=[]
                outputs=[{lock =  Coinbase (1ul, publicKeyHash rootAccount); spend= {asset = Asset.Zen;amount=100000000UL}}]
                witnesses=[]
                contract=None
            }
    let originHash = Transaction.hash origin

    let rootAccount = Account.addTransaction originHash origin rootAccount

    Account.createTransaction (publicKeyHash rootAccount) { asset = Asset.Zen; amount = 1UL } (rootAccount, rootSecretKey)
    |> should be ok

[<Test>]
let ``wallet spend coinbase when come from block``() =
    let rootAccount, rootSecretKey = rootAccountData
    let origin =
            {
                version=Version0
                inputs=[]
                outputs=[{lock =  Coinbase (1ul, publicKeyHash rootAccount); spend= {asset = Asset.Zen;amount=100000000UL}}]
                witnesses=[]
                contract=None
            }

    let header =
        {
            version = Version0
            parent = Hash.zero
            blockNumber = 100ul
            difficulty = 0x20fffffful;
            commitments=Hash.zero;
            timestamp = 0UL
            nonce = 0UL,0UL
        }

    let block = {header=header;transactions=[origin];commitments=[];txMerkleRoot=Hash.zero; witnessMerkleRoot=Hash.zero;activeContractSetMerkleRoot=Hash.zero;}

    let rootAccount = Account.handleBlock (Block.hash block.header) block rootAccount

    Account.createTransaction (publicKeyHash rootAccount) { asset = Asset.Zen; amount = 1UL } (rootAccount, rootSecretKey)
    |> should be ok

[<Test>]
let ``Should get expected deltas``() =
    let account, _ = create()

    let output1 = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=10UL}}
    let tx1 = {version=Version0;inputs=[];outputs=[output1];witnesses=[];contract=None}
    let tx1Hash = Transaction.hash tx1
    let output2A = {lock = PK Hash.zero; spend={asset=Asset.Zen;amount=2UL}}
    let output2B = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=8UL}}
    let tx2 = {version=Version0;inputs=[Outpoint { txHash = tx1Hash; index = 0ul } ];outputs=[output2A;output2B];witnesses=[];contract=None}

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

    let genesisBlock = Block.createGenesis chainParams [rootTx] (0UL,0UL)
    let genesisHeader = genesisBlock.header

    let getHeader = function
    | h when h = chainParams.genesisHash -> genesisHeader
    | h when h = blockHash -> header
    | h -> failwithf "unexpected block hash %A" h

    let getBlock = function
    | h when h = chainParams.genesisHash -> genesisBlock
    | h when h = blockHash -> block
    | h -> failwithf "unexpected block hash %A" h

    let account = Account.sync blockHash getHeader getBlock account

    let tx2Hash = Transaction.hash tx2
    let output3A = {lock = PK Hash.zero; spend={asset=Asset.Zen;amount=3UL}}
    let output3B = {lock = PK (publicKeyHash account); spend={asset=Asset.Zen;amount=5UL}}
    let tx3 = {version=Version0;inputs=[Outpoint { txHash = tx2Hash; index = 1ul } ];outputs=[output3A;output3B];witnesses=[];contract=None}

    let expected = [ (tx1Hash, Map.add Asset.Zen 10L Map.empty, 2u) ]
    let expected = (tx2Hash, Map.add Asset.Zen -2L Map.empty, 2u) :: expected

    should equal expected (Account.getHistory 0 10 account)

    let tx3Hash = Transaction.hash tx3

    // add tx3 to mempool
    let account' = Account.addTransaction tx3Hash tx3 account

    let expected = (tx3Hash, Map.add Asset.Zen -3L Map.empty, 0u) :: expected

    should equal expected (Account.getHistory 0 10 account')

[<Test>]
let ``sign contract wintess``() =
    let account = createTestAccount ()

    let executeContract _ _ _ _ txSkeleton =
        let tx =
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract <| ContractId  (Version0,Hash.zero);spend={asset=Asset.Zen;amount=1UL}}
            |> Transaction.fromTxSkeleton

        let tx = {tx with witnesses=[
                                        ContractWitness {
                                            contractId = ContractId (Version0,Hash.zero)
                                            command = ""
                                            data=None
                                            beginInputs=1ul
                                            beginOutputs = 0ul
                                            inputsLength = 0ul
                                            outputsLength = 1ul
                                            signature = None
                                            cost = 8ul
                                        }]
        }

        tx |> Ok

    let spends = Map.add Asset.Zen 1UL Map.empty

    let result = Account.createExecuteContractTransaction executeContract (ContractId (Version0,Hash.zero)) "" None true (Some "m/0'") spends account

    result |> should be ok

    let tx:Transaction = Result.get result

    let txHash = Transaction.hash tx

    let publicKey = ExtendedKey.derivePath "m/0'" (snd account) |> Result.get |> ExtendedKey.getPublicKey |> Result.get

    match tx.witnesses.[1] with
    | ContractWitness cw ->
        match cw.signature with
        | Some (publicKey',signature) ->
            Crypto.verify publicKey signature txHash |> should equal Crypto.Valid
            publicKey' |> should equal publicKey
        | None ->
            failwith "expected signature"
    | _ -> failwith "expected contract witness"
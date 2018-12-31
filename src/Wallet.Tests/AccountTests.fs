module Wallet.Tests.AccountTests

open NUnit.Framework
open FsUnit
open Consensus
open Consensus.Types
open Chain

open Wallet
open TestsInfrastructure.Constraints
open Consensus.Tests.Helper
open Infrastructure
open Messaging.Services.Wallet
open Wallet.Serialization
open Wallet.Types
open Infrastructure.Result

let chain = Chain.Local
let chainParams = Chain.localParameters

let ema = EMA.create chainParams

let tempDir () = System.IO.Path.Combine
                    [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]

let databaseContext = DataAccess.createContext (tempDir())
let dataAccess = DataAccess.init databaseContext
let password = "1234"

let mnemonicPhrase = ["one";"one";"one";"one";"one";"one";"one";"one";"one";"one";"one";"one"]

let privateKey =
    (String.concat " " mnemonicPhrase)
    |> ExtendedKey.fromMnemonicPhrase
    |> Result.get

// Initialize the wallet and get the public key
let accountPKHash =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    Account.import dataAccess session ["one";"one";"one";"one";"one";"one";"one";"one";"one";"one";"one";"one"] password Hash.zero 0ul
    |> Result.get

    let pkHash = Account.getPKHash dataAccess session

    DataAccess.Session.commit session

    pkHash

let balanceShouldBe session asset expected view =
    let balance = Account.getBalance dataAccess session view 0ul

    let actual =
        match Map.tryFind asset balance with
        | Some value -> value
        | None -> 0UL

    actual |> should equal expected

let anotherAsset = Asset (ContractId (Version0,Hash.compute "anotherasset"B), Hash.compute "anotherasset"B)

let fundAccount session =
    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10000000UL}}

    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
    View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

[<Test>]
let ``received tokens - mempool``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

    let view' = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

    view' |> balanceShouldBe session Asset.Zen 10UL

[<Test>]
let ``received tokens - block``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

    let block = Block.createGenesis chainParams [Transaction.toExtended tx] (0UL,0UL)

    Account.addBlock dataAccess session (Block.hash block.header) block

    balanceShouldBe session Asset.Zen 10UL View.empty

[<Test>]
let ``tokens spent - mempool``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output;output];witnesses=[];contract=None}
    let txHash = (Transaction.hash tx)

    let view = View.addMempoolTransaction dataAccess session txHash tx View.empty

    let tx' = {version=Version0;inputs=[ Outpoint {txHash=txHash; index=0ul}];outputs=[];witnesses=[];contract=None}

    let view' = View.addMempoolTransaction dataAccess session  (Transaction.hash tx') tx' view

    balanceShouldBe session Asset.Zen 10UL view'

[<Test>]
let ``tokens spent - block``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output;output];witnesses=[];contract=None}
    let txHash = (Transaction.hash tx)

    let tx' = {version=Version0;inputs=[ Outpoint {txHash=txHash; index=0ul}];outputs=[];witnesses=[];contract=None}

    let block = Block.createGenesis chainParams [Transaction.toExtended tx;Transaction.toExtended tx'] (0UL,0UL)
    Account.addBlock dataAccess session (Block.hash block.header) block

    balanceShouldBe session Asset.Zen 10UL View.empty


[<Test>]
let ``creating, not enough tokens``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

    let view = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

    let result = TransactionCreator.createTransaction testParameters dataAccess session view password 0ul [ { lock = (PK Hash.zero); spend = { asset = Asset.Zen; amount = 11UL } } ]

    let expected:Result<Transaction,string> = Error "Not enough tokens"

    result |> should equal expected

[<Test>]
let ``creating, no change``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let lockToBob = Account.getNewPKHash dataAccess session |> Result.get |> fst |> PK
    let lockToAlice = Hash.zero  |> PK

    // giving some money to bob
    let output = {lock = lockToBob; spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

    // sending money to alice
    let result = TransactionCreator.createTransaction testParameters dataAccess session bob password 0ul [ { lock = lockToAlice; spend = { asset = Asset.Zen; amount = 10UL } } ]

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx bob

        bob |> balanceShouldBe session Asset.Zen 0UL

[<Test>]
let ``creating, with change``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let lockToBob = Account.getNewPKHash dataAccess session |> Result.get |> fst |> PK
    let lockToAlice = Hash.zero  |> PK

    // giving some money to bob
    let output = {lock = lockToBob; spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

    // sending money to alice
    let result = TransactionCreator.createTransaction testParameters dataAccess session bob password 0ul [ { lock = lockToAlice; spend = { asset = Asset.Zen; amount = 7UL } } ]

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx bob

        bob |> balanceShouldBe session Asset.Zen 3UL

[<Test>]
let ``picking the correct asset``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let lockToBob = Account.getNewPKHash dataAccess session |> Result.get |> fst |> PK
    let lockToAlice = Hash.zero  |> PK

    // giving some money to bob
    let output = {lock = lockToBob; spend={asset=anotherAsset;amount=10UL}}
    let output2 = {lock = lockToBob; spend={asset=Asset.Zen;amount=10UL}}

    let tx = {version=Version0;inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

    bob |> balanceShouldBe session anotherAsset 10UL
    bob |> balanceShouldBe session Asset.Zen 10UL

    // sending money to alice
    let result = TransactionCreator.createTransaction testParameters dataAccess session bob password 0ul [ { lock = lockToAlice; spend = { asset = Asset.Zen; amount = 7UL } } ]

    match result with
    | Error x -> failwithf "expected transaction %s" x
    | Ok tx ->
        List.length tx.inputs |> should equal 1

        let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx bob

        bob |> balanceShouldBe session Asset.Zen 3UL
        bob |> balanceShouldBe session anotherAsset 10UL

[<Test>]
let ``picking from multiple inputs``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let lockToBob = Account.getNewPKHash dataAccess session |> Result.get |> fst |> PK
    let lockToAlice = Hash.zero  |> PK

    // giving some money to bob
    let output = {lock = lockToBob; spend={asset=Asset.Zen;amount=5UL}}
    let output2 = {lock = lockToBob; spend={asset=Asset.Zen;amount=7UL}}

    let tx = {version=Version0;inputs=[];outputs=[output; output2];witnesses=[];contract=None}
    let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

    // sending money to alice
    let result = TransactionCreator.createTransaction testParameters dataAccess session bob password 0ul [ { lock = lockToAlice; spend = { asset = Asset.Zen; amount = 10UL } } ]

    match result with
        | Error x -> failwithf "expected transaction %s" x
        | Ok tx ->
            List.length tx.inputs |> should equal 2

            let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx bob

            bob |> balanceShouldBe session Asset.Zen 2UL

[<Test>]
let ``create execute contract transaction``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext
    let view = fundAccount session

    let executeContract _ _ _ _ txSkeleton =
        let tx =
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract <| ContractId  (Version0,Hash.zero);spend={asset=Asset.Zen;amount=1UL}}
            |> Transaction.fromTxSkeleton

        tx |> Ok

    let spends = Map.add Asset.Zen 1UL Map.empty

    let result = TransactionCreator.createExecuteContractTransaction testParameters dataAccess session view executeContract password (ContractId (Version0,Hash.zero)) "" None true None spends 0ul

    result |> should be ok

    let tx = Result.get result

    tx.inputs |> should haveLength 1

    // last pkwitness should use FollowingWitnesses
    List.findBack (function | PKWitness _ -> true | _ -> false) tx.witnesses
    |> function PKWitness (sigHash,_,_) -> sigHash
    |> should equal FollowingWitnesses


[<Test>]
let ``create execute contract transaction without explicitly spending any Zen should allocate fee``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext
    let view = fundAccount session

    let executeContract _ _ _ _ txSkeleton =
        let tx =
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract <| ContractId  (Version0,Hash.zero);spend={asset=Asset.Zen;amount=1UL}}
            |> Transaction.fromTxSkeleton

        tx |> Ok

    let spends = Map.empty

    let result = TransactionCreator.createExecuteContractTransaction testParameters dataAccess session view executeContract password (ContractId (Version0,Hash.zero)) "" None true None spends 0ul

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

    // last pkwitness should use FollowingWitnesses
    List.findBack (function | PKWitness _ -> true | _ -> false) tx.witnesses
    |> function PKWitness (sigHash,_,_) -> sigHash
    |> should equal FollowingWitnesses



[<Test>]
let ``account sync up``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let startBlockHeader = {
        version = 0ul
        parent = localGenesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }
    let startBlockHash = Block.hash startBlockHeader

    let account = DataAccess.Account.get dataAccess session

    {account with blockHash = Block.hash startBlockHeader;blockNumber = startBlockHeader.blockNumber}
    |> DataAccess.Account.put dataAccess session

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

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
        transactions = [Transaction.toExtended tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot = Hash.zero
        commitments= []
    }

    let blockHash = Block.hash block.header

    Account.sync dataAccess session blockHash block.header (fun hash ->
        if hash = startBlockHash then
            startBlockHeader
        elif hash = blockHash then
            header
        else
            failwithf "invalid block %A" hash
        ) (fun _ -> block)

    let account = DataAccess.Account.get dataAccess session

    account.blockHash |> should equal blockHash
    account.blockNumber |> should equal block.header.blockNumber

[<Test>]
let ``sync up from empty wallet``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}

    let header = {
        version = 0ul
        parent = localGenesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let block = {
        header = header
        transactions = [Transaction.toExtended tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot = Hash.zero
        commitments = []
    }

    let blockHash = Block.hash block.header

    let genesisBlock = Block.createGenesis chainParams [Transaction.toExtended rootTx] (0UL,0UL)
    let genesisHeader = genesisBlock.header

    let getHeader = function
    | h when h = localGenesisHash -> genesisHeader
    | h when h = blockHash -> header
    | h -> failwithf "unexpected block hash %A" h

    let getBlock = function
    | h when h = localGenesisHash -> genesisBlock
    | h when h = blockHash -> block
    | h -> failwithf "unexpected block hash %A" h

    Account.sync dataAccess session blockHash header getHeader getBlock

    let account = DataAccess.Account.get dataAccess session

    account.blockHash |> should equal blockHash
    account.blockNumber |> should equal block.header.blockNumber

    Account.getUnspentOutputs dataAccess session View.empty 0ul |> should haveLength 1

[<Test>]
let ``account reorg``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let startBlockHeader = {
        version = 0ul
        parent = localGenesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let startHash = Block.hash startBlockHeader

    let account = DataAccess.Account.get dataAccess session

    {account with blockHash = startHash;blockNumber = startBlockHeader.blockNumber}
    |> DataAccess.Account.put dataAccess session

    let output = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
    let txHash = Transaction.hash tx

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
        transactions = [Transaction.toExtended tx]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot = Hash.zero
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
       activeContractSetMerkleRoot = Hash.zero
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

    Account.sync dataAccess session blockHash header getHeader getBlock
    let account = DataAccess.Account.get dataAccess session

    account.blockHash |> should equal blockHash
    account.blockNumber |> should equal header.blockNumber

    Account.sync dataAccess session blockHash2 header2 getHeader getBlock
    let account = DataAccess.Account.get dataAccess session

    account.blockHash |> should equal blockHash2
    account.blockNumber |> should equal header2.blockNumber

[<Test>]
let ``wallet won't spend coinbase if not mature enough``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let account = DataAccess.Account.get dataAccess session

    {account with blockNumber = Chain.testParameters.coinbaseMaturity - 1ul}
    |> DataAccess.Account.put dataAccess session

    let origin =
        {
            version=Version0
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, accountPKHash); spend= {asset = Asset.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }
    let originHash = Transaction.hash origin

    let view = View.addMempoolTransaction dataAccess session originHash origin View.empty

    let expected: Result<Transaction,string>= Error "Not enough tokens"

    TransactionCreator.createTransaction testParameters dataAccess session view password 0ul [ { lock = (PK accountPKHash); spend = { asset = Asset.Zen; amount = 1UL } } ]
    |> should equal expected

[<Test>]
let ``wallet spend coinbase with coinbase mature enough``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let account = DataAccess.Account.get dataAccess session

    {account with blockNumber = 100ul}
    |> DataAccess.Account.put dataAccess session

    let origin =
        {
            version=Version0
            inputs=[]
            outputs=[{lock =  Coinbase (1ul, accountPKHash); spend= {asset = Asset.Zen;amount=100000000UL}}]
            witnesses=[]
            contract=None
        }

    let originHash = Transaction.hash origin

    let view = View.addMempoolTransaction dataAccess session originHash origin View.empty

    TransactionCreator.createTransaction testParameters dataAccess session view password 0ul [ { lock = (PK accountPKHash); spend = { asset = Asset.Zen; amount = 1UL } } ]
    |> should be ok

[<Test>]
let ``wallet spend coinbase when come from block``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext
    let origin =
            {
                version=Version0
                inputs=[]
                outputs=[{lock =  Coinbase (1ul, accountPKHash); spend= {asset = Asset.Zen;amount=100000000UL}}]
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

    let block = { header=header;
                  transactions=[Transaction.toExtended origin];
                  commitments=[];
                  txMerkleRoot=Hash.zero;
                  witnessMerkleRoot=Hash.zero;
                  activeContractSetMerkleRoot=Hash.zero; }

    Account.addBlock dataAccess session (Block.hash block.header) block

    TransactionCreator.createTransaction testParameters dataAccess session View.empty password 0ul [ { lock = (PK accountPKHash); spend = { asset = Asset.Zen; amount = 1UL } } ]
    |> should be ok

[<Test>]
let ``Should get expected history``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let output1 = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=10UL}}
    let tx1 = {version=Version0;inputs=[];outputs=[output1];witnesses=[];contract=None}
    let tx1Hash = Transaction.hash tx1
    let output2A = {lock = PK Hash.zero; spend={asset=Asset.Zen;amount=2UL}}
    let output2B = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=8UL}}
    let tx2 = {version=Version0;inputs=[Outpoint { txHash = tx1Hash; index = 0ul } ];outputs=[output2A;output2B];witnesses=[];contract=None}

    let header = {
        version = 0ul
        parent = localGenesisHash
        blockNumber = 2ul
        commitments = Hash.zero
        timestamp = 0UL
        difficulty = 0ul
        nonce = 0UL,0UL
    }

    let block = {
        header = header
        transactions = [Transaction.toExtended tx1;Transaction.toExtended tx2]
        txMerkleRoot = Hash.zero
        witnessMerkleRoot = Hash.zero
        activeContractSetMerkleRoot = Hash.zero
        commitments = []
    }

    let blockHash = Block.hash block.header

    let genesisBlock = Block.createGenesis chainParams [Transaction.toExtended rootTx] (0UL,0UL)
    let genesisHeader = genesisBlock.header

    let getHeader = function
    | h when h = localGenesisHash -> genesisHeader
    | h when h = blockHash -> header
    | h -> failwithf "unexpected block hash %A" h

    let getBlock = function
    | h when h = localGenesisHash -> genesisBlock
    | h when h = blockHash -> block
    | h -> failwithf "unexpected block hash %A" h

    Account.sync dataAccess session blockHash header getHeader getBlock

    let tx2Hash = Transaction.hash tx2
    let output3A = {lock = PK Hash.zero; spend={asset=Asset.Zen;amount=3UL}}
    let output3B = {lock = PK accountPKHash; spend={asset=Asset.Zen;amount=5UL}}
    let tx3 = {version=Version0;inputs=[Outpoint { txHash = tx2Hash; index = 1ul } ];outputs=[output3A;output3B];witnesses=[];contract=None}

    let expected = [
        (tx2Hash, TransactionDirection.Out, {asset=Asset.Zen;amount= 2UL}, 1u, PK accountPKHash);
        (tx1Hash, TransactionDirection.In,{asset=Asset.Zen;amount= 10UL}, 1u, PK accountPKHash) ]

    let result = Account.getHistory dataAccess session View.empty 0 10

    result |> should equal expected

    let tx3Hash = Transaction.hash tx3

    // add tx3 to mempool
    let view = View.addMempoolTransaction dataAccess session tx3Hash tx3 View.empty

    let expected = (tx3Hash, TransactionDirection.Out, {asset=Asset.Zen;amount= 3UL}, 0u, PK accountPKHash) :: expected
    let result = Account.getHistory dataAccess session view 0 10

    result |> should equal expected

[<Test>]
let ``sign contract wintess``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext
    let view = fundAccount session

    let contractId = ContractId (Version0,Hash.zero)
    let command = ""
    let messageBody = None

    let executeContract _ _ _ _ txSkeleton =
        let tx =
            txSkeleton
            |> TxSkeleton.addOutput {lock=Contract <| ContractId  (Version0,Hash.zero);spend={asset=Asset.Zen;amount=1UL}}
            |> Transaction.fromTxSkeleton

        let tx = {tx with witnesses=[
                                        ContractWitness {
                                            contractId = contractId
                                            command = command
                                            messageBody = messageBody
                                            stateCommitment = NotCommitted
                                            beginInputs=1ul
                                            beginOutputs = 0ul
                                            inputsLength = 0ul
                                            outputsLength = 1ul
                                            signature = None
                                            cost = 8UL
                                        }]
        }

        tx |> Ok

    let spends = Map.add Asset.Zen 1UL Map.empty

    let result = TransactionCreator.createExecuteContractTransaction testParameters dataAccess session view executeContract password (ContractId (Version0,Hash.zero)) "" None true (Some "m/0'") spends 0ul

    result |> should be ok

    let tx:Transaction = Result.get result

    let txHash = Transaction.hash tx
    let message =
        {
            recipient = contractId
            command = ""
            body = None
        }
        |> Serialization.Message.serialize

    let msg =
        [ Hash.bytes txHash; message ]
        |> Hash.computeMultiple

    let publicKey = ExtendedKey.derivePath "m/0'" privateKey |> Result.get |> ExtendedKey.getPublicKey |> Result.get

    let (Crypto.PublicKey pk) = publicKey

    match tx.witnesses.[1] with
    | ContractWitness cw ->
        match cw.signature with
        | Some (publicKey',signature) ->
            Crypto.verify publicKey signature msg |> should equal Crypto.Valid
            publicKey' |> should equal publicKey
        | None ->
            failwith "expected signature"
    | _ -> failwith "expected contract witness"

[<Test>]
let ``Typescript testvector``() =
    let mnemonic = "one one one one one one one one one one one one one one one one one one one one one one one one"
    let keyPair =
        ExtendedKey.fromMnemonicPhrase mnemonic
        >>= ExtendedKey.derivePath "m/44'/258'/0'/0/0"
        >>= ExtendedKey.getKeyPair
        |> get

    let input = {txHash = Hash.zero; index=0ul}
    let output = {lock = PK (Hash.zero);spend= {amount=100UL;asset=Asset.Zen}}

    let tx =
        {version=Version0;inputs=[Outpoint input];outputs=[output];contract=None;witnesses=[]}
        |> Transaction.sign [keyPair] TxHash

//    printfn "%A" (Transaction.hash tx)
//    printfn "%A" (Transaction.toHex tx)

    let expectedTxHash =
        (Hash.fromString "2df00d7cf448facbe9883a032dd435b08c74e40a18c9f91c1a43be583ea3ce4a")
        |> Infrastructure.Result.get

    Transaction.hash tx |> should equal expectedTxHash

[<Test>]
let ``creating and signing raw transaction``() =
    use session = DataAccess.DatabaseContext.createSession databaseContext

    let lockToBob = Account.getNewPKHash dataAccess session |> Result.get |> fst |> PK
    let lockToAlice = Hash.zero  |> PK

    // giving some money to bob
    let output = {lock = lockToBob; spend={asset=Asset.Zen;amount=10UL}}
    let tx = {version=Version0;inputs=[];outputs=[output];witnesses=[];contract=None}
    let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx View.empty

    // sending money to alice
    let raw =
        TransactionCreator.createRawTransaction testParameters dataAccess session bob None 0ul [ { lock = lockToAlice; spend = { asset = Asset.Zen; amount = 7UL } } ]
        |> Result.bind (TransactionCreator.signRawTransaction dataAccess session password)

    match raw with
    | Error x -> failwithf "expected transaction %s" x
    | Ok raw ->
        let tx = Transaction.fromRaw raw

        let bob = View.addMempoolTransaction dataAccess session (Transaction.hash tx) tx bob

        bob |> balanceShouldBe session Asset.Zen 3UL
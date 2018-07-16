module Blockchain.Tests.InitialBlockDownload

open NUnit.Framework
open FsUnit
open Blockchain
open Blockchain.InitialBlockDownload
open Consensus
open Consensus.Chain
open Consensus.Types
open Infrastructure
open Messaging.Services

let chain = localParameters
let peerId = Array.empty
let difficulty = Difficulty.compress localParameters.proofOfWorkLimit

let NetworkCommand = EffectsWriter.NetworkCommand

let tempDir () = System.IO.Path.Combine
                    [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]

let peerHeader = {
    version=0ul;
    parent=Hash.zero;
    blockNumber=3000ul;
    commitments=Hash.zero;
    timestamp=0UL;
    difficulty=difficulty;
    nonce=1UL,1UL;
}

let peerBlockHash = Block.hash peerHeader

let createHeaders from _to =
    [from.._to]
    |> List.fold (fun headers blockNumber ->
        let parent =
            match headers with
            | [] -> Hash.zero
            | parent :: _ -> parent |> Block.hash

        {
            version=0ul;
            parent=parent;
            blockNumber=blockNumber;
            commitments=Hash.zero;
            timestamp=0UL;
            difficulty=difficulty;
            nonce=1UL,1UL;
        } :: headers) []
    |> List.rev

[<Test>]
let ``from genesis``() =
    let idb = Inactive

    let effects, ibd =
        start 0UL Hash.zero Block.genesisParent peerId peerHeader
        |> Writer.unwrap

    effects |> should equal [
        NetworkCommand <| Network.GetHeaders (peerId, [Hash.zero], peerBlockHash)
    ]

    isActive ibd |> should equal true

[<Test>]
let ``process valid headers``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let headers = createHeaders 1ul 2000ul

    let effects,ibd =
        processHeaders chain session 1000UL peerId headers ibd
        |> Writer.unwrap

    isActive ibd |> should equal true

    let expected =
        headers
        |> List.take maxDownloading
        |> List.map (Block.hash)
        |> List.map (fun hash -> Network.GetBlockFrom (peerId, hash) |> NetworkCommand)

    effects |> should equal expected

[<Test>]
let ``process invalid headers``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let headers =
        [1ul..2000ul]
        |> List.map (fun blockNumber ->
            {
                version=0ul;
                parent=Hash.zero;
                blockNumber=blockNumber;
                commitments=Hash.zero;
                timestamp=0UL;
                difficulty=0ul;
                nonce=1UL,1UL;
            })

    let effects,ibd =
        processHeaders chain session 0UL peerId headers ibd
        |> Writer.unwrap

    isActive ibd |> should equal false

    effects |> should equal [
        NetworkCommand <| Network.DisconnectPeer peerId
        NetworkCommand <| Network.GetTipFromAllPeers
    ]

[<Test>]
let ``processing headers, all block in db``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let headers = createHeaders 1ul 2000ul

    List.iter (fun header ->
        {
            header=header;
            txMerkleRoot = Hash.zero;
            witnessMerkleRoot=Hash.zero;
            activeContractSetMerkleRoot=Hash.zero;
            commitments=[]
            transactions=[]
        }
        |> ExtendedBlockHeader.createOrphan (Block.hash header)
        |> BlockRepository.saveHeader session) headers

    let effects,ibd =
        processHeaders chain session 1000UL peerId headers ibd
        |> Writer.unwrap

    isActive ibd |> should equal true

    effects |> should equal [
           NetworkCommand <| Network.GetHeaders (peerId, [List.last headers |> Block.hash], peerBlockHash)
    ]

[<Test>]
let ``block received``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let headers = createHeaders 1ul 2000ul

    let _,ibd =
        processHeaders chain session 1000UL peerId headers ibd
        |> Writer.unwrap

    let effects,ibd =
        InitialBlockDownload.received 0UL (Block.hash headers.[0]) ibd
        |> Writer.unwrap

    isActive ibd |> should equal true

    effects |> should equal [
        NetworkCommand <| Network.GetBlockFrom (peerId, headers.[20] |> Block.hash)
    ]

[<Test>]
let ``last block in a batch received``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let headers = createHeaders 1ul 2000ul

    let _,ibd =
        processHeaders chain session 1000UL peerId headers ibd
        |> Writer.unwrap

    let effects,ibd =
        List.fold (fun (_,ibd) header ->
            InitialBlockDownload.received 0UL (Block.hash header) ibd
            |> Writer.unwrap) ([],ibd) headers

    isActive ibd |> should equal true

    effects |> should equal [
        NetworkCommand <| Network.GetHeaders (peerId, [List.last headers |> Block.hash], peerBlockHash)
    ]

[<Test>]
let ``last block in initial bload download received``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let headers = createHeaders 1001ul 3000ul

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let _,ibd =
        processHeaders chain session 1000UL peerId headers ibd
        |> Writer.unwrap

    let effects,ibd =
        List.fold (fun (_,ibd) header ->
            InitialBlockDownload.received 0UL (Block.hash header) ibd
            |> Writer.unwrap) ([],ibd) headers

    isActive ibd |> should equal false

    effects |> should equal [
        NetworkCommand <| Network.GetTipFromAllPeers
    ]

[<Test>]
let ``block invalid``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let headers = createHeaders 1ul 2000ul

    let _,ibd =
        processHeaders chain session 1000UL peerId headers ibd
        |> Writer.unwrap

    let effects,ibd =
        InitialBlockDownload.invalid 0UL (Block.hash headers.[0]) ibd
        |> Writer.unwrap

    isActive ibd |> should equal false

    effects |> should equal [
        NetworkCommand <| Network.DisconnectPeer peerId
        NetworkCommand <| Network.GetTipFromAllPeers
    ]

[<Test>]
let ``timeout on get headers request``() =
    let idb = Inactive

    let _, ibd =
        start 0UL Hash.zero Block.genesisParent peerId peerHeader
        |> Writer.unwrap

    let effects, ibd =
        tick 1000UL ibd
        |> Writer.unwrap

    isActive ibd |> should equal true
    effects |> should equal []
    
    let _, ibd =
        tick 9000UL ibd
        |> Writer.unwrap

    let effects, ibd =
        tick 10001UL ibd
        |> Writer.unwrap

    effects |> should equal [
        NetworkCommand <| Network.DisconnectPeer peerId
        NetworkCommand <| Network.GetTipFromAllPeers
    ]

    isActive ibd |> should equal false

[<Test>]
let ``timeout on downloading``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let ibd = GettingHeaders ({peerId=peerId;peerTipHeader=peerHeader;peerTipHash=peerBlockHash},0UL,0UL)

    let headers = createHeaders 1ul 2000ul

    let _,ibd =
        processHeaders chain session 1000UL peerId headers ibd
        |> Writer.unwrap

    let effects,ibd =
        InitialBlockDownload.received 2000UL (Block.hash headers.[0]) ibd
        |> Writer.unwrap

    let effects, ibd =
        tick 11000UL ibd
        |> Writer.unwrap

    isActive ibd |> should equal true
    effects |> should equal []

    let effects, ibd =
        tick 12001UL ibd
        |> Writer.unwrap

    effects |> should equal [
        NetworkCommand <| Network.DisconnectPeer peerId
        NetworkCommand <| Network.GetTipFromAllPeers
    ]

    isActive ibd |> should equal false


[<Test>]
let ``getting headers from genesis``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let headers = createHeaders 1ul 2000ul

    List.head headers |> Block.hash
    |> BlockRepository.saveGenesisHash session

    List.iter (fun header ->
        {
            header=header;
            txMerkleRoot = Hash.zero;
            witnessMerkleRoot=Hash.zero;
            activeContractSetMerkleRoot=Hash.zero;
            commitments=[]
            transactions=[]
        }
        |> ExtendedBlockHeader.createOrphan (Block.hash header)
        |> fun header -> {header with status = ExtendedBlockHeader.MainChain}
        |> BlockRepository.saveHeader session) headers

    let effect, _  =
        getHeaders chain session peerId [Hash.zero] (List.last headers |> Block.hash)
        |> Writer.unwrap

    effect |> should equal [
        NetworkCommand <| Network.SendHeaders (peerId,headers)
    ]

[<Test>]
let ``getting headers from unknown hash``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let headers = createHeaders 1ul 2000ul
    List.head headers |> Block.hash
    |> BlockRepository.saveGenesisHash session

    List.iter (fun header ->
        {
            header=header;
            txMerkleRoot = Hash.zero;
            witnessMerkleRoot=Hash.zero;
            activeContractSetMerkleRoot=Hash.zero;
            commitments=[]
            transactions=[]
        }
        |> ExtendedBlockHeader.createOrphan (Block.hash header)
        |> fun header -> {header with status = ExtendedBlockHeader.MainChain}
        |> BlockRepository.saveHeader session) headers

    let effect, _  =
        getHeaders chain session peerId [Hash.compute "HELLO"B] (List.last headers |> Block.hash)
        |> Writer.unwrap

    effect |> should equal [
        NetworkCommand <| Network.SendHeaders (peerId,headers)
    ]

[<Test>]
let ``getting headers from none main chain hash``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let headers = createHeaders 1ul 2000ul
    List.head headers |> Block.hash
    |> BlockRepository.saveGenesisHash session

    List.iter (fun header ->
        {
            header=header;
            txMerkleRoot = Hash.zero;
            witnessMerkleRoot=Hash.zero;
            activeContractSetMerkleRoot=Hash.zero;
            commitments=[]
            transactions=[]
        }
        |> ExtendedBlockHeader.createOrphan (Block.hash header)
        |> fun header -> {header with status = ExtendedBlockHeader.MainChain}
        |> BlockRepository.saveHeader session) headers

    let sideBlock = {
        header={peerHeader with blockNumber = 5ul;nonce=5UL,5UL}
        txMerkleRoot = Hash.zero;
        witnessMerkleRoot=Hash.zero;
        activeContractSetMerkleRoot=Hash.zero;
        commitments=[]
        transactions=[]
    }

    sideBlock
    |> ExtendedBlockHeader.createOrphan (Block.hash sideBlock.header)
    |> BlockRepository.saveHeader session

    let effect, _  =
        getHeaders chain session peerId [Block.hash sideBlock.header] (List.last headers |> Block.hash)
        |> Writer.unwrap

    effect |> should equal [
        NetworkCommand <| Network.SendHeaders (peerId,headers)
    ]

[<Test>]
let ``getting headers from middle``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let headers = createHeaders 1ul 2000ul
    List.head headers |> Block.hash
    |> BlockRepository.saveGenesisHash session

    List.iter (fun header ->
        {
            header=header;
            txMerkleRoot = Hash.zero;
            witnessMerkleRoot=Hash.zero;
            activeContractSetMerkleRoot=Hash.zero;
            commitments=[]
            transactions=[]
        }
        |> ExtendedBlockHeader.createOrphan (Block.hash header)
        |> fun header -> {header with status = ExtendedBlockHeader.MainChain}
        |> BlockRepository.saveHeader session) headers

    let effect, _  =
        getHeaders chain session peerId [headers.[1000] |> Block.hash] (List.last headers |> Block.hash)
        |> Writer.unwrap

    effect |> should equal [
        NetworkCommand <| Network.SendHeaders (peerId,headers.[1001..1999])
    ]

[<Test>]
let ``getting headers from multiple hashes``() =
    use databaseContext = DatabaseContext.createEmpty (tempDir())
    use session = DatabaseContext.createSession databaseContext

    let headers = createHeaders 1ul 2000ul
    List.head headers |> Block.hash
    |> BlockRepository.saveGenesisHash session

    List.iter (fun header ->
        {
            header=header;
            txMerkleRoot = Hash.zero;
            witnessMerkleRoot=Hash.zero;
            activeContractSetMerkleRoot=Hash.zero;
            commitments=[]
            transactions=[]
        }
        |> ExtendedBlockHeader.createOrphan (Block.hash header)
        |> fun header -> {header with status = ExtendedBlockHeader.MainChain}
        |> BlockRepository.saveHeader session) headers

    let sideBlock = {
        header={peerHeader with blockNumber = 5ul;nonce=5UL,5UL}
        txMerkleRoot = Hash.zero;
        witnessMerkleRoot=Hash.zero;
        activeContractSetMerkleRoot=Hash.zero;
        commitments=[]
        transactions=[]
    }

    sideBlock
    |> ExtendedBlockHeader.createOrphan (Block.hash sideBlock.header)
    |> BlockRepository.saveHeader session

    let effect, _  =
        getHeaders chain session peerId [Block.hash sideBlock.header;Block.hash headers.[3]; Block.hash headers.[2]] (List.last headers |> Block.hash)
        |> Writer.unwrap

    effect |> should equal [
        NetworkCommand <| Network.SendHeaders (peerId,headers.[4..1999])
    ]

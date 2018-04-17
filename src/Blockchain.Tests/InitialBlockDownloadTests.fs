module Blockchain.Tests.InitialBlockDownload

open NUnit.Framework
open FsUnit
open Blockchain
open Blockchain.InitialBlockDownload
open Blockchain.EffectsWriter
open Consensus
open Consensus.Chain
open Consensus.Types
open Infrastructure
open Messaging.Services

let chain = localParameters
let peerId = Array.empty
let difficulty = Difficulty.compress localParameters.proofOfWorkLimit

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
    |> List.map (fun blockNumber ->
        {
            version=0ul;
            parent=Hash.zero;
            blockNumber=blockNumber;
            commitments=Hash.zero;
            timestamp=0UL;
            difficulty=difficulty;
            nonce=1UL,1UL;
        })

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
    use databaseContext = DatabaseContext.createEmpty "test"
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
    use databaseContext = DatabaseContext.createEmpty "test"
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
    use databaseContext = DatabaseContext.createEmpty "test"
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
    use databaseContext = DatabaseContext.createEmpty "test"
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
    use databaseContext = DatabaseContext.createEmpty "test"
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
    use databaseContext = DatabaseContext.createEmpty "test"
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
    use databaseContext = DatabaseContext.createEmpty "test"
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
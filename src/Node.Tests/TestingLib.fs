module Node.Tests.TestingLib

open Config
open NBitcoin
open Consensus
open Consensus.Types
open Consensus.Serialization.Serialization
open Consensus.Tests.Helper
open FsNetMQ
open Infrastructure
open Api.Types
open Cli.Request.Request
open Cli.Request.Load
open Cli.Util

#if DEBUG

module Actor = FsNetMQ.Actor

module CryptoSig = Consensus.Crypto.Signature

type Request = {
    request : string
    args : string list option
 }

type NodeStatus =
    | Mine
    | Validate

type WipeStatus =
    | Wipe
    | WipeFull
    | Continue

type Local =
    | Miner of int
    | Receiver of int

let password = "1234"

let CGP_FILENAME = "CGP.fst"

let private wrap x = Some x
let private wrapList x = Some [ x ]

let private checkResponse request =
    if request = 0 then printfn "Ok"
    else printfn "Not ok"

let private TempPath = System.IO.Path.GetTempPath()

module private Wipe =

    let Blockchain =
        function
        | Wipe     -> true
        | WipeFull -> true
        | Continue -> false

    let Network =
        function
        | Wipe     -> true
        | WipeFull -> true
        | Continue -> false

    let Wallet =
        function
        | Wipe     -> Wallet.Main.Wipe.Reset
        | WipeFull -> Wallet.Main.Wipe.Full
        | Continue -> Wallet.Main.Wipe.NoWipe

    let AddressDB =
        function
        | Wipe     -> AddressDB.Main.Wipe.Reset
        | WipeFull -> AddressDB.Main.Wipe.Full
        | Continue -> AddressDB.Main.Wipe.NoWipe

let mineString : NodeStatus -> string =
    function
    | Mine -> "--miner"
    | Validate -> ""

let wipeString : WipeStatus -> string =
    function
    | Wipe     -> "--wipe"
    | WipeFull -> "--wipe full"
    | Continue -> ""

module Cli =

    let request (index : int) (request : Request) =
        let (arg : string []) =
            match request.args with
            | Some args -> Array.concat [ [| "--local"; (string index); request.request; |]; args |> List.toArray ]
            | None -> [| "--local"; (string index); request.request; |]

        Program.main arg

module Parser =

    type WalletKeys =
        {publicKey:string;path:string}

    let publicKey (string: string) : WalletKeys list =
        PublicKeyDataJson.Parse string
        |> Array.map (fun data -> {publicKey=data.PublicKey;path=data.Path})
        |> Array.toList

    let unwrapString string =
        let endString = (String.length string) - 2
        string.[1..endString]


module Node =

    let init (index : int) (mine : NodeStatus) (wipe : WipeStatus) (dataPath:string) : System.Diagnostics.Process =

        match mine, wipe with
        | Mine, Wipe -> failwith "Is not suggested to operate in this way"
        | _,Wipe
        | _,WipeFull -> failwith "CHECK DATA PATH"
        | _,_ -> ()

        let BUILD = "Debug"

        let zen_node_dir = System.IO.Path.Combine([| ".." ;"Node"; "bin"; BUILD |])

        let owd = System.IO.Directory.GetCurrentDirectory()

        System.IO.Directory.SetCurrentDirectory(zen_node_dir)

        let args = String.concat " " [| "zen-node.exe"; "--local"; string index; wipeString wipe; mineString mine; sprintf "--data-path \"%s\"" dataPath |]
        let res = Async.RunSynchronously(async { return System.Diagnostics.Process.Start("mono", args) })

        System.IO.Directory.SetCurrentDirectory(owd)

        res


    let setGenesisBlock() =
        Consensus.Block.createGenesis chainParams [ rootTxExtended ] (0UL, 0UL)
        //not sure if we using it

    let getBlockHeader index : uint32 =
        (blockchainInfo (getURL index)).headers

    let getBlockMedianTime index : uint64 =
        (blockchainInfo (getURL index)).medianTime

    let getBlockNumber index : uint32 =
        (blockchainInfo (getURL index)).blocks

    let isSynced index : bool =
        getBlockHeader index = getBlockNumber index

    /// getblock info and check we are in that correct block (might get stuck)
    let waitForBlock index (delay : int) (blockNumber : uint32) =
        while getBlockNumber index < blockNumber do
            System.Threading.Thread.Sleep( delay )

    let waitForBlockTimeout index (delay : int) (blockNumber : uint32) (timeout : uint64) =
        let startingTime = Timestamp.now()
        while getBlockNumber index < blockNumber do
            if Timestamp.now() - startingTime > timeout then
                failwithf "Timeout error: waited too long (more than %A milliseconds) for block %A" timeout blockNumber
            System.Threading.Thread.Sleep( delay )

    /// polling until correct block is recived
    let wait index (delay : int) (numOfBlocks : uint32) =
       waitForBlock index delay (getBlockNumber index + numOfBlocks)

    let onBlock index (delay : int) (blockNumber : uint32) (action : unit -> unit) : unit =
        waitForBlock index delay blockNumber
        action()

module Wallet_ =
    let getKeys index =
        walletKeys (getURL index) password

    let sign index message path =
        signMessage (getURL index) message path password

module Contract =

    module Address =

        let CGP =
            "ctzn1qqqqqqq82cmzchmv39le3phuld9sw3m2u9z4vswu2nztyyf96k8sxcaumjvdw5gac"

        let Voting =
            "ctzn1qqqqqqq8gjuu8rz5q9f7jz72p3qh0arjctcstyzgp8ydux7hjt7kz7gkg4v5lec88"

    module Code =

        let CGP = Blockchain.Tests.ContractCode.cgpContractCode

        let VotingContract = Blockchain.Tests.ContractCode.votingContractCode

        let getFullpath filename =
            System.IO.Path.Combine(TempPath, filename)

        let createFile filename code =
            System.IO.File.WriteAllText(filename, code)

        let readFile filename =
            System.IO.File.ReadAllText(filename)


    module MessageBody =

        module ZData = Zen.Types.Data
        module Cost = Zen.Cost.Realized

        let createVoteData index ballot blockNumber : byte array =
            let ballotString =
                match ballot with
                | Ballot.Payout     _ -> "Payout"
                | Ballot.Allocation _ -> "Allocation"
                |> ZFStar.fsToFstString

            let signatureString =
                "Signature"
                |> ZFStar.fsToFstString

            let ballotSer = Ballot.serialize ballot |> FsBech32.Base16.encode |> ZFStar.fsToFstString

            let message = Blockchain.Tally.VoteParser.hashBallot chainParams blockNumber ballotSer |> Hash.toString
            // get keys
            let keys = Parser.publicKey (Wallet_.getKeys index)

            //Create dictionary
            let signatureData =
                let map =
                    keys
                    |> List.map (fun walletKey ->
                        // sign message
                        let publicKey = walletKey.publicKey |> ZFStar.fsToFstString
                        let signature = Wallet_.sign index message walletKey.path |> unwrapString
                        let s = CryptoSig.fromString signature |> Option.defaultValue (Crypto.Signature ""B)

                        publicKey, s |> ZFStar.fsToFstSignature |> ZData.Signature
                        )
                    |> Map.ofList
                ZData.Collection (ZData.Dict (map, Map.count map |> uint32))
            let signatures =
                Zen.Dictionary.add signatureString signatureData Zen.Dictionary.empty
                |> Cost.__force

            Zen.Dictionary.add ballotString (ZData.String ballotSer) signatures
            |> Cost.__force
            |> ZData.Dict
            |> ZData.Collection
            |> Serialization.Data.serialize

        let createCGPData (winner : Consensus.Types.Recipient * Consensus.Types.Spend list) : byte array =
            let outputs = Consensus.CGP.internalizeRecipient winner
            match Consensus.CGP.Contract.createPayoutMsgBody outputs with
            | Some msgBody -> Consensus.Serialization.Data.serialize msgBody
            | None -> [||]


    let activate index code numOfBlocks =
        activateContract (getURL index) code numOfBlocks 0 password

    let execute index address command messageBody asset amount =
        execute (getURL index) address command messageBody asset amount password

    let activateVotingContract index numOfBlocks =
        activate index Code.VotingContract numOfBlocks

    let activateCGPContract index numOfBlocks =
        activate index Code.CGP numOfBlocks


module Wallet =

    let inline getKeys index =
        Wallet_.getKeys index

    let inline sign index message path =
        Wallet_.sign index message path

    let exist index : bool =
        walletExists (getURL index)
        |> function | "false" -> false | "true" -> true | _ -> failwith "walletExists failed"


    let resync index =
        resync (getURL index)

    let import index seed =
        let seed =
            match seed with
            | "" -> NBitcoin.Mnemonic(NBitcoin.Wordlist.English, NBitcoin.WordCount.TwentyFour).Words
            | _ -> split seed [|' '|]
        let error =
            import (getURL index) seed password

        if error = "" then
            (resync index)
        else
            error

    let create index =
         import index ""

    let remove index =
        removeWallet (getURL index) password

    let sendToken index address asset amount =
        send (getURL index) address asset amount password

    let publish index raw =
        publishRawTx (getURL index) raw

    let showBalances index : (string * int64) [] =
        balance (getURL index)

    let checkBalance index asset : int64 =
        showBalances index
        |> Array.tryFind (fun (asset', _) -> asset' = asset)
        |> Option.map (fun (_,amount) -> amount)
        |> Option.defaultValue 0L

    let showTransactions index =
        walletTransaction (getURL index) 0 65535

    let address index =
        address (getURL index)

    let vote index ballot =
        let address = Contract.Address.Voting
        let command =
            match ballot with
            | Ballot.Allocation _ -> "Allocation"
            | Ballot.Payout _     -> "Payout"
        let blockNumber = Node.getBlockNumber index
        let messageBody = Contract.MessageBody.createVoteData index ballot blockNumber
        let asset = ""
        let amount = 0L
        Contract.execute index address command (FsBech32.Base16.encode messageBody) asset amount

    let executeCGP index (winner : Consensus.Types.Recipient * Consensus.Types.Spend list) =
        let cgpAddress = Contract.Address.CGP
        let messageBody = Contract.MessageBody.createCGPData winner
        Contract.execute index cgpAddress "Payout" (FsBech32.Base16.encode messageBody) "" 0L

    let getCGP index : CGP.T =
        let allocation, payout = getCGP (getURL index)
        let recipient =
            payout.Recipient
            |> Wallet.Address.decodeAny Chain.Local
            |> Result.map
                begin function
                | Wallet.Address.PK       pk  -> PKRecipient pk
                | Wallet.Address.Contract cid -> ContractRecipient cid
                end
            |> Option.ofResult

        let spends =
            payout.Spendlist
            |> Array.map (fun d -> SpendJson.Root d.JsonValue)
            |> Array.map (fun v -> { asset = v.Asset |> Asset.fromString |> Option.get ; amount = uint64 v.Amount })
            |> Array.toList

        { allocation = byte allocation ; payout = recipient |> Option.map (fun r -> (r, spends)) }



module Reader =

    type Env = {
        index : int
        delay : int
    }

    module Node =

        let init mine wipe dataPath env =
            Node.init env.index mine wipe dataPath

        let setGenesisBlock () (_ : Env) =
            Node.setGenesisBlock ()

        let getBlockHeader env =
            Node.getBlockHeader env.index

        let getBlockMedianTime env =
            Node.getBlockMedianTime env.index

        let getBlockNumber env =
            Node.getBlockNumber env.index

        let isSynced env =
            Node.isSynced env.index

        let waitForBlock blockNumber env =
            Node.waitForBlock env.index env.delay blockNumber

        let waitForBlockTimeout blockNumber timeout env =
            Node.waitForBlockTimeout env.index env.delay blockNumber timeout

        let wait numOfBlocks env =
            Node.wait env.index env.delay numOfBlocks

        let onBlock blockNumber action env =
            Node.onBlock env.index env.delay blockNumber action

    module Wallet =

        let inline getKeys env =
            Wallet.getKeys env.index

        let inline sign message path env =
            Wallet.sign env.index message path

        let inline exist env =
            Wallet.exist env.index

        let inline resync env =
            Wallet.resync env.index

        let inline import seed env =
            Wallet.import env.index seed

        let inline create env =
            Wallet.create env.index

        let inline remove env =
            Wallet.remove env.index

        let inline sendToken address asset amount env =
            Wallet.sendToken env.index address asset amount

        let inline publish raw env =
            Wallet.publish env.index raw

        let inline showBalances env =
            Wallet.showBalances env.index

        let inline checkBalance asset env =
            Wallet.checkBalance env.index asset

        let inline showTransactions env =
            Wallet.showTransactions env.index

        let inline address env =
            Wallet.address env.index

        let inline vote ballot env =
            Wallet.vote env.index ballot

        let inline executeCGP winner env =
            Wallet.executeCGP env.index winner

        let inline getCGP env =
            Wallet.getCGP env.index

    module Contract =

        module MessageBody =

            let inline createVoteData ballot blockNumber env =
                Contract.MessageBody.createVoteData env.index ballot blockNumber

            let inline createCGPData winner (_ : Env) =
                Contract.MessageBody.createCGPData winner

        let inline activate code numOfBlocks env =
            Contract.activate env.index code numOfBlocks

        let inline execute address command messageBody asset amount env =
            Contract.execute env.index address command messageBody asset amount

        let inline activateVotingContract numOfBlocks env =
            Contract.activateVotingContract env.index numOfBlocks

        let inline activateCGPContract numOfBlocks env =
            Contract.activateCGPContract env.index numOfBlocks
#endif

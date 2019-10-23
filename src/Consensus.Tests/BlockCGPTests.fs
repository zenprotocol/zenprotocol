module Consensus.Tests.BlockCGPTests

#if DEBUG

open Consensus
open Consensus
open Consensus.Chain
open Consensus.Types
open NUnit.Framework
open TestsInfrastructure.Nunit
open Consensus.Tests.CGPContractCode

module CryptoPublicKey = Crypto.PublicKey

let timestamp = 1515594186383UL + 1UL
let difficulty = 0x20fffffful

let rlimit = 8000000u

let chainParams = Chain.testParameters
let cgpContractId = chainParams.cgpContractId 
let otherContractId = ContractId (0u, Hash.zero)

let context0 = { blockNumber=1u; timestamp = 1UL }

let contractPath =
    NUnit.Framework.TestContext.CurrentContext.TestDirectory

let env block : BlockConnection.Env = {
    chainParams      = chainParams
    timestamp        = timestamp
    getUTXO          = (fun _ -> failwith "shouldn't be used")
    getContractState = (fun _ -> failwith "shouldn't be used")
    contractsPath    = contractPath
    parent           = block.header
    block            = block
}

let state cgp ema : BlockConnection.State = {
    utxoSet        = Map.empty
    acs            = ActiveContractSet.empty
    cgp            = cgp
    ema            = ema
    contractCache  = Map.empty
    contractStates = Map.empty
}

let shouldFailWith expectedMsg =
    function
    | Ok _ -> failwithf "\nexpected error msg: %A\nbut was:  Ok" expectedMsg
    | Error wasMsg -> shouldEqual (wasMsg, expectedMsg)

let shouldFail =
    function
    | Ok _ -> failwithf "\nexpected an error\nbut was: Ok"
    | Error _ -> ()

let shouldSucceed =
    function
    | Ok _ -> ()
    | Error errMsg -> failwithf "\nexpected Ok\nbut got an error: %A" errMsg

let shouldBe x y =
    if x = y
        then ()
        else failwithf "\nexpected %A\nbut got: %A" x y

let result = new Infrastructure.Result.ResultBuilder<string>()

let (>>=) x f = Result.bind f x
let (|@>) x f = Result.map f x
let (|@!>) x f = Result.mapError f x
let ( *>) x y = x >>= fun _ -> y

let createWalletExact (outputs : List<Output>) : Contract.ContractWallet =
    let inputs =
        outputs
        |> List.map ( fun out -> { out with lock = Contract cgpContractId } )
    List.zip [ 0 .. List.length outputs - 1 ] inputs
    |> List.map ( fun (i, out) -> ({ txHash=Hash.zero; index=uint32 i }, out) )

let createWalletExactWith (outputs : List<Output>) (txHash : Hash.Hash) : Contract.ContractWallet =
    let inputs =
        outputs
        |> List.map ( fun out -> { out with lock = Contract cgpContractId } )
    List.zip [ 0 .. List.length outputs - 1 ] inputs
    |> List.map ( fun (i, out) -> ({ txHash=txHash; index=uint32 i }, out) )

let createWalletWithChange (outputs : List<Output>) : Contract.ContractWallet =
    createWalletExact outputs
    |> List.map ( fun (p, out) -> (p, { out with spend = { out.spend with amount = out.spend.amount * 2UL } }) )

let compile code = lazy (result {
    let! hints = Contract.recordHints code
    let! queries = Infrastructure.ZFStar.totalQueries hints

    let contract = {
        code = code
        hints = hints
        rlimit = rlimit
        queries = queries
    }

    return!
        Contract.compile contractPath contract
        >>= Contract.load contractPath 100ul code
})

let compiledCgpContract = lazy Contract.load contractPath 100ul cgpContractCode chainParams.cgpContractId

let executeCgpContract (txSkeleton : TxSkeleton.T) context messageBody wallet =
    result {
        let! contract = compiledCgpContract.Force()
        
        let command = "Payout"
        
        let! (txSkel,_,_) = Contract.run contract txSkeleton context command Zen.Types.Main.sender.Anonymous messageBody wallet None
        
        let cw = {
            contractId      = cgpContractId
            command         = command
            messageBody     = messageBody
            stateCommitment = NoState
            beginInputs     = txSkeleton.pInputs |> List.length |> uint32
            beginOutputs    = txSkeleton.outputs |> List.length |> uint32
            inputsLength    = List.length txSkel.pInputs - List.length txSkeleton.pInputs |> uint32
            outputsLength   = List.length txSkel.outputs - List.length txSkeleton.outputs |> uint32
            signature       = None
            cost            = 13634UL + 100UL * (uint64 (List.length wallet) * 128UL + 287UL)
        }
        
        //failwithf "inputsLength=%A; outputsLength=%A" cw.inputsLength cw.outputsLength
        
        let inputs =
            txSkel.pInputs
            |> List.map (function TxSkeleton.PointedOutput (pt,op) -> Outpoint pt | TxSkeleton.Mint s -> Mint s)
        
        let utxosInputs =
            txSkel.pInputs
            |> List.choose ((function TxSkeleton.PointedOutput (pt,op) -> Some (pt,op) | TxSkeleton.Mint s -> None))
            |> List.fold (fun m (pt,op) -> Map.add pt (UtxoSet.Unspent op) m) Map.empty
        
        let utxos =
            wallet
            |> List.fold (fun m (pt,op) -> Map.add pt (UtxoSet.Unspent op) m) utxosInputs
        
        let tx =
            {
                version   = 0u
                inputs    = inputs
                outputs   = txSkel.outputs
                witnesses = [ContractWitness cw]
                contract  = None
            }
        
        let ex = Transaction.toExtended tx
        
        return (ex, txSkel, utxos)
    }


let executeValidCgpContract (outputs : List<Output>) =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    let messageBody = CGP.Contract.createPayoutMsgBody outputs
    let wallet = createWalletExact outputs
    executeCgpContract TxSkeleton.empty context messageBody wallet

let executeCgpContractInContext (outputs : List<Output>) context =
    let messageBody = CGP.Contract.createPayoutMsgBody outputs
    let wallet = createWalletExact outputs
    executeCgpContract TxSkeleton.empty context messageBody wallet

let generateGetUTXO utxos pt =
    match Map.tryFind pt utxos with
    | Some op -> op
    | None -> UtxoSet.NoOutput

let ema : EMA.T =
    {
        difficulty = difficulty
        delayed    = []
    }

let cwCGP0 =
    {
        contractId      = cgpContractId
        command         = "Payout"
        messageBody     = None
        stateCommitment = NoState
        beginInputs     = 0u
        beginOutputs    = 0u
        inputsLength    = 0u
        outputsLength   = 0u
        signature       = None
        cost            = 0UL
    }

let cwOther1 =
    { cwCGP0 with contractId = otherContractId }

let tx1 =
    {
        tx =
            {
                version   = 0u
                inputs    = []
                outputs   = []
                witnesses = [ContractWitness cwOther1; ContractWitness cwCGP0; ContractWitness cwOther1]
                contract  = None
            }
        txHash      = Hash.zero
        witnessHash = Hash.zero
        raw         = [||]
    }

let header =
    {
        version     = Version0
        parent      = Hash.zero
        blockNumber = 15ul
        difficulty  = difficulty
        commitments = Hash.zero
        timestamp   = timestamp
        nonce       = (0UL, 0UL)
    }

let block1 =
    {
        header                      = header
        transactions                = [ tx1 ]
        commitments                 = []
        txMerkleRoot                = Hash.zero
        witnessMerkleRoot           = Hash.zero
        activeContractSetMerkleRoot = Hash.zero
    }

let cgp1 : CGP.T =
    {
        allocation = 32uy
        payout     = Some (ContractRecipient cgpContractId, [ {asset=Asset.Zen; amount=123UL} ])
    }

[<Test>]
let ``valid payout`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> ()
    | Error msg -> failwithf "Error: %s" msg

[<Test>]
let ``multiple payout Txs`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex; ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "Multiple payout Txs" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``different winner asset`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.Zen; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "Contract outputs are not the same as the payout winner" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``different winner amount`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=19UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "Contract outputs are not the same as the payout winner" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``different winner recipient`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (ContractRecipient cgpContractId, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "Contract outputs are not the same as the payout winner" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``different winner - extra spends`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } }; {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf chainParams.votingContractId ; amount=10UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "Contract outputs are not the same as the payout winner" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``same winner - different order`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } }; {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf chainParams.votingContractId ; amount=10UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf chainParams.votingContractId ; amount=10UL }; { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> ()
    | Error msg -> failwithf "Error: %s" msg

[<Test>]
let ``different winner - distributed spends`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=10UL } }; {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=10UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2  *> test3
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "Contract outputs are not the same as the payout winner" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``payout tx outside of payout block`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity + 3u }
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    test1 *> test2
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "Not a payout block" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg
     
[<Test>]
let ``no payout when there should be`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    let res = res |@> fun (ex,txSkel,utxos) -> ({ ex with tx = { ex.tx with witnesses = [PKWitness (TxHash, Crypto.PublicKey [| 0uy |], Crypto.Signature [| 0uy |])] }}, txSkel, utxos)
    
    let test1 =
        res >>= fun (ex, _, _) -> TransactionValidation.checkStructure ex.tx |@!> sprintf "%A"
    
    let test2 =
        res >>= fun (_, txSkel, _) -> TransactionValidation.checkAmounts txSkel |@!> sprintf "%A"
    
    let ex = match res with | Ok (ex, _, _) -> ex | Error msg -> failwithf "Error: %s" msg
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = Hash.zero
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = Hash.zero
            commitments                 = []
            transactions                = [ex]
        }
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (ContractRecipient cgpContractId, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let test3 =
        BlockConnection.PayoutTx.check (state cgp ema) (env block)
    
    test1 *> test2 *> test3
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "No payout Tx" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``valid connect`` ()  =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let ex, utxos = match res with | Ok (ex, _, utxos) -> ex, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let coinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> ()
    | Error msg -> failwithf "Error: %s" msg

[<Test>]
let ``connect with invalid coinbase - CGP overflow`` ()  =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let ex, utxos = match res with | Ok (ex, _, utxos) -> ex, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let validCoinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let coinbaseOutputs =
         [ {lock = Coinbase (context.blockNumber ,Hash.zero); spend = {asset = Asset.Zen; amount = 4400009026UL;};}
         ; {lock = Contract cgpContractId; spend = {asset = Asset.Zen; amount = 600000000UL + 1UL;};}
         ]
    
    let coinbaseTx =
        { validCoinbaseTx with tx = { validCoinbaseTx.tx with outputs = coinbaseOutputs } }
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "block reward is incorrect" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``connect with invalid coinbase - miner overflow`` ()  =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let ex, utxos = match res with | Ok (ex, _, utxos) -> ex, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let validCoinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let coinbaseOutputs =
         [ {lock = Coinbase (context.blockNumber ,Hash.zero); spend = {asset = Asset.Zen; amount = 4400009026UL + 1UL;};}
         ; {lock = Contract cgpContractId; spend = {asset = Asset.Zen; amount = 600000000UL;};}
         ]
    
    let coinbaseTx =
        { validCoinbaseTx with tx = { validCoinbaseTx.tx with outputs = coinbaseOutputs } }
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "block reward is incorrect" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``connect with invalid coinbase - cgp gets too much`` ()  =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let ex, utxos = match res with | Ok (ex, _, utxos) -> ex, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let validCoinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let coinbaseOutputs =
         [ {lock = Coinbase (context.blockNumber ,Hash.zero); spend = {asset = Asset.Zen; amount = 4400009026UL + 1UL;};}
         ; {lock = Contract cgpContractId; spend = {asset = Asset.Zen; amount = 600000000UL - 1UL;};}
         ]
    
    let coinbaseTx =
        { validCoinbaseTx with tx = { validCoinbaseTx.tx with outputs = coinbaseOutputs } }
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "reward is not divided correctly" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``connect with invalid coinbase - miner gets too much`` ()  =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let ex, utxos = match res with | Ok (ex, _, utxos) -> ex, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let validCoinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let coinbaseOutputs =
         [ {lock = Coinbase (context.blockNumber ,Hash.zero); spend = {asset = Asset.Zen; amount = 4400009026UL - 1UL;};}
         ; {lock = Contract cgpContractId; spend = {asset = Asset.Zen; amount = 600000000UL + 1UL;};}
         ]
    
    let coinbaseTx =
        { validCoinbaseTx with tx = { validCoinbaseTx.tx with outputs = coinbaseOutputs } }
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "reward is not divided correctly" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``coinbase creation - non-ZP asset`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let ex, utxos = match res with | Ok (ex, _, utxos) -> ex, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let validCoinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let coinbaseOutputs =
         [ {lock = Coinbase (context.blockNumber ,Hash.zero); spend = {asset = Asset.Zen; amount = 4400009026UL;};}
         ; {lock = Contract cgpContractId; spend = {asset = Asset.defaultOf cgpContractId; amount = 600000000UL;};}
         ]
    
    let coinbaseTx =
        { validCoinbaseTx with tx = { validCoinbaseTx.tx with outputs = coinbaseOutputs } }
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "block reward is incorrect" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``coinbase creation - non-CGP contract`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let res = executeCgpContractInContext [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } } ] context
    
    let ex, utxos = match res with | Ok (ex, _, utxos) -> ex, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=20UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let validCoinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let coinbaseOutputs =
         [ {lock = Coinbase (context.blockNumber ,Hash.zero); spend = {asset = Asset.Zen; amount = 4400009026UL;};}
         ; {lock = Contract chainParams.votingContractId ; spend = {asset = Asset.Zen; amount = 600000000UL;};}
         ]
    
    let coinbaseTx =
        { validCoinbaseTx with tx = { validCoinbaseTx.tx with outputs = coinbaseOutputs } }
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "reward to cgp contract in invalid" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

[<Test>]
let ``implementation execution of the CGP contract`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let mnemonicPhrase =
        NBitcoin.Mnemonic( NBitcoin.Wordlist.English, NBitcoin.WordCount.TwentyFour ).Words
        |> Array.toSeq
    
    let result = Infrastructure.Result.ResultBuilder<string>()
        
    result {
        let! (account,_) = TestWallet.import mnemonicPhrase "1234" Hash.zero 1ul
        
        let validCoinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [] Hash.zero CGP.empty

        let coinbaseOutputs =
             [ { lock = Coinbase (context.blockNumber - 501u, account.publicKey |> CryptoPublicKey.hash); spend = {asset = Asset.Zen; amount = 4400009026UL;}; }
             ; { lock = Contract cgpContractId; spend = { asset = Asset.Zen; amount = 600000000UL;}; }
             ; { lock = Contract cgpContractId; spend = { asset = Asset.defaultOf cgpContractId; amount=20UL } }
             ; { lock = Contract cgpContractId; spend = { asset = Asset.defaultOf chainParams.votingContractId ; amount=123UL } }
             ]
        
        let coinbaseTx =
            { validCoinbaseTx with tx = { validCoinbaseTx.tx with outputs = coinbaseOutputs } }
        
        let uns = UtxoSet.Unspent
        
        let utxos =
            Map.empty
            |> Map.add {txHash=Transaction.hash coinbaseTx.tx; index=0u} (uns { lock = Coinbase (context.blockNumber - 501u, account.publicKey |> CryptoPublicKey.hash); spend = {asset = Asset.Zen; amount = 4400009026UL;}; })
            |> Map.add {txHash=Transaction.hash coinbaseTx.tx; index=1u} (uns { lock = Contract cgpContractId; spend = { asset = Asset.Zen; amount = 600000000UL;}; })
            |> Map.add {txHash=Hash.zero; index=0u} (uns { lock=Contract cgpContractId; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } })
            |> Map.add {txHash=Hash.zero; index=1u} (uns { lock=Contract cgpContractId; spend = { asset=Asset.defaultOf chainParams.votingContractId ; amount=123UL } })
        
        let account =
            account
            |> TestWallet.addTransaction (Transaction.hash coinbaseTx.tx) coinbaseTx.tx
        
        let! extendedKey = Wallet.ExtendedKey.fromMnemonicPhrase (String.concat " " mnemonicPhrase)
        
        let outputs =
            [ { lock = account.publicKey |> CryptoPublicKey.hash |> PK ; spend = { asset=Asset.defaultOf cgpContractId; amount=20UL } }
            ; { lock = account.publicKey |> CryptoPublicKey.hash |> PK ; spend = { asset=Asset.defaultOf chainParams.votingContractId ; amount=123UL } }
            ]
        
        let msgBody = CGP.Contract.createPayoutMsgBody outputs
        
        let executeContract (contractId:ContractId) command sender (messageBody: Zen.Types.Data.data option) (txSkel : TxSkeleton.T) =
            let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
            let wallet = createWalletExact outputs
            
            let res = executeCgpContract txSkel context messageBody wallet
            res
            |> Result.map (fun (ex,_,_) -> ex.tx) 
        
        let! tx = TestWallet.createExecuteContractTransaction chainParams executeContract cgpContractId "Payout" msgBody false None Map.empty (account, extendedKey)
        
        return tx, utxos
    }
    |> Result.bind
           (fun (tx, utxos) ->
        TransactionValidation.validateInContext chainParams (generateGetUTXO utxos) contractPath (context.blockNumber) timestamp acs Map.empty utxos (fun _ -> None) Map.empty (Transaction.toExtended tx)
        |> Result.mapError (sprintf "%A"))
    |> function
    | Ok _ -> ()
    | Error msg -> failwith msg
    
    ()

[<Test>]
let ``no unreported payout vulnerability`` () =
    let context = { context0 with blockNumber = 10u * chainParams.intervalLength + chainParams.coinbaseMaturity }
    
    let (_, mPK) = Crypto.KeyPair.create()
    let mPKH =
        mPK
        |> CryptoPublicKey.serialize
        |> Hash.compute
    
    let txSkel =
        TxSkeleton.empty
        |> TxSkeleton.addOutput {lock=PK mPKH; spend = { asset=Asset.defaultOf cgpContractId; amount=1UL } }
    
    let outputs =
        [ {lock=PK Hash.zero ; spend = { asset=Asset.defaultOf cgpContractId; amount=1UL } } ]
    
    let wallet =
        [ {txHash=Hash.zero; index=0u}, {lock=Contract cgpContractId ; spend = { asset=Asset.defaultOf cgpContractId; amount=2UL } } ]
    
    let messageBody = CGP.Contract.createPayoutMsgBody outputs
    
    let res = executeCgpContract txSkel context messageBody wallet
    
    let ex, txSkel, utxos = match res with | Ok (ex, txSkel, utxos) -> ex, txSkel, utxos | Error msg -> failwithf "Error: %s" msg
    
    let cgp : CGP.T =
        {
            allocation = 12uy
            payout     = Some (PKRecipient Hash.zero, [ { asset=Asset.defaultOf cgpContractId; amount=1UL } ])
        }
    
    let acs =
        ActiveContractSet.empty
        |> ActiveContractSet.add cgpContractId (compiledCgpContract.Force() |> function | Ok x -> x | _ -> failwith "no")
    
    let getContractState _ = None
    
    let parent =
        {
            version     = Version0
            parent      = Hash.zero
            blockNumber = context.blockNumber - 1ul
            difficulty  = difficulty
            commitments = Hash.zero
            timestamp   = context.timestamp
            nonce       = (0UL, 0UL)
        }
    
    let coinbaseTx = Block.getBlockCoinbase chainParams acs context.blockNumber [ex] Hash.zero (cgp:CGP.T)
    
    let commitments =
            Block.createCommitments Hash.zero Hash.zero (ActiveContractSet.root acs) []
            |> MerkleTree.computeRoot
    
    let block =
        {
            header =
                {
                    version     = Version0
                    parent      = Hash.zero
                    blockNumber = context.blockNumber
                    difficulty  = difficulty
                    commitments = commitments
                    timestamp   = context.timestamp
                    nonce       = (0UL, 0UL)
                }
            txMerkleRoot                = Hash.zero
            witnessMerkleRoot           = Hash.zero
            activeContractSetMerkleRoot = ActiveContractSet.root acs
            commitments                 = []
            transactions                = [coinbaseTx; ex]
        }
    
    let state : BlockConnection.State = {
        utxoSet        = utxos
        acs            = acs
        cgp            = cgp
        ema            = ema
        contractCache  = ContractCache.empty
        contractStates = ContractStates.asDatabase
    }
    
    let env : BlockConnection.Env = {
        chainParams      = chainParams
        timestamp        = 1UL
        getUTXO          = generateGetUTXO utxos
        getContractState = getContractState
        contractsPath    = contractPath
        parent           = parent
        block            = block
    }
    
    BlockConnection.connect state env
    |> function
    | Ok _ -> failwithf "There should have been an error, but there wasn't one"
    | Error msg when msg = "transactions failed inputs validation due to General \"invalid amounts\"" -> ()
    | Error msg -> failwithf "Wrong error: %A" msg

#endif
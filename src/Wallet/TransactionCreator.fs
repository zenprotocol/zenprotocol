module Wallet.TransactionCreator

open Consensus
open Chain
open Crypto
open Types
open Hash
open Wallet
open Infrastructure
open Result
open Wallet
open Types
open Serialization
open Logary.Message

module ZData = Zen.Types.Data
module Cost = Zen.Cost.Realized

type ActiveContract = Messaging.Services.Blockchain.ActiveContract

[<Literal>]
let rlimit = 2723280u

let result = new ResultBuilder<string>()

// Collect inputs from the account unspent outputs
let private collectInputs chainParams dataAccess session view account assetAmounts =
    let spendableAddresses =
        DataAccess.Addresses.getAll dataAccess session
        |> List.filter (fun address -> address.addressType <> WatchOnly) // Both payment and change addresses
        |> List.map (fun address -> address.pkHash)
        |> Set.ofList

    let outputs =
        View.Outputs.getAll view dataAccess session
        |> List.filter (fun output -> output.status = Unspent && Set.contains output.pkHash spendableAddresses)
        |> List.fold (fun map output ->
            match Map.tryFind output.spend.asset map with
            | Some outputs -> Map.add output.spend.asset (output :: outputs) map
            | None -> Map.add output.spend.asset [output] map) Map.empty

    let rec collectAssetInputs outputs amount accInputs accAmount =
        if accAmount >= amount then
            Some accInputs
        else
        match outputs with
        | [] -> None
        | output :: outputs ->

            let isMature =
                match output.lock with
                | Coinbase (blockNumber,_) -> (account.blockNumber + 1ul) - blockNumber >= chainParams.coinbaseMaturity
                | _ -> true

            if isMature then
                let accInputs = (output.outpoint,{lock=output.lock;spend=output.spend},output.pkHash) :: accInputs
                let accAmount = accAmount + output.spend.amount

                collectAssetInputs outputs amount accInputs accAmount
            else
                collectAssetInputs outputs amount accInputs accAmount

    let inputs =
        Map.toSeq assetAmounts
        |> Seq.choose (fun (asset,amount) ->
            match Map.tryFind asset outputs with
            | None -> None
            | Some outputs ->
                collectAssetInputs outputs amount [] 0UL
                |> Option.map (fun xs -> asset,xs))
        |> Map.ofSeq

    if Map.count inputs <> Map.count assetAmounts then
        eventX "Not enough tokens {inputs} {assetAmounts}"
        >> setField "inputs" (sprintf "%A" inputs)
        >> setField "assetAmounts" (sprintf "%A" assetAmounts)
        |> Log.warning

        Error "Not enough tokens"
    else
        Ok inputs

let private getTxVersion =
    List.exists (function
        | ({ lock = Consensus.Types.Vote _; spend = _ }:Consensus.Types.Output) -> true
        | _ -> false
    )
    >> function | true -> Version1 | false -> Version0

let private existingVoteData outputs chainParams tip =
    outputs
    |> List.choose (function
        | ({lock = Vote (voteData,interval,_); spend = _ }:Consensus.Types.Output) when interval = CGP.getInterval chainParams tip ->
            Some voteData
        | _ -> 
            None)
    |> List.tryHead
    |> Option.defaultValue { allocation = None; payout = None }  
    
// Return the change outputs by subtract the required amount from the collected inputs
let private getChangeOutputs inputs amounts account chainParams tip =
    Map.fold (fun changes asset inputs ->
        let inputSum = List.sumBy (fun (_,(output:Consensus.Types.Output),_) -> output.spend.amount) inputs
        let outputAmount = Map.find asset amounts

        let change = inputSum - outputAmount

        let outputs = List.choose (fun (_,outputs,_) -> Some outputs) inputs
        let voteData = existingVoteData outputs chainParams tip
        let lock =
            match voteData with
            | { allocation = None; payout = None }  ->
                PK account.changePKHash
            | _ ->
                outputs
                |> List.choose (function
                    | { lock = Types.Vote (_,interval,pkHash); spend = _} when interval >= CGP.getInterval chainParams tip -> 
                        Some (Types.Vote (voteData, interval, pkHash))
                    | _ -> 
                        None)
                |> List.head
                
        if change > 0UL then
            let changeOutput = {lock=lock; spend={amount=change;asset=asset}}
            changeOutput :: changes
        else
            changes) List.empty inputs
            

     
let createVoteTransaction dataAccess session view chainParams tip password (allocation : byte option option) (payout : (Recipient * uint64) option option)  = result {
    let account = DataAccess.Account.get dataAccess session
        
    let! extendedPrivateKey =
            Secured.decrypt password account.secureMnemonicPhrase
            >>= ExtendedKey.fromMnemonicPhrase
            >>= (ExtendedKey.derivePath Account.zenKeyPath)
            
    let unspentOutputs = Account.getUnspentOutputs dataAccess session view 0ul
    
    let outputs = List.choose (fun (_,outputs) -> Some outputs) unspentOutputs
        
    let existingVoteData = existingVoteData outputs chainParams tip
        
    let voteData =  
        match allocation, payout with
        | Some allocation, Some payout -> { allocation = allocation; payout = payout}
        | Some allocation, None -> { allocation = allocation; payout = existingVoteData.payout }
        | None, Some payout -> { allocation = existingVoteData.allocation; payout = payout } 
        | None, None -> { allocation = existingVoteData.allocation; payout = existingVoteData.payout } 
        
    let inputs, keys, outputs =
        unspentOutputs
        |> List.choose (function
            | (outpoint, { lock = Coinbase (blockNumber,pkHash); spend = spend}) when (account.blockNumber + 1ul) - blockNumber >= chainParams.coinbaseMaturity ->
                Some (outpoint, (pkHash, spend))
            | (outpoint, { lock = PK pkHash; spend = spend }) ->
                Some (outpoint, (pkHash, spend)) 
            | (outpoint, { lock = Types.Vote (_,_,pkHash); spend = spend}) ->
                Some (outpoint, (pkHash, spend)) 
            | _ ->
                None
        )
        |> List.map (fun (outpoint, (pkHash, spend)) ->             
            let address = DataAccess.Addresses.get dataAccess session pkHash
            
            let secretkey =
                    match address.addressType with
                    | Change index -> Account.deriveChange index extendedPrivateKey >>= ExtendedKey.getPrivateKey |> get // The key must be valid as we already used it, safe to call get
                    | External index -> Account.deriveExternal index extendedPrivateKey >>= ExtendedKey.getPrivateKey |> get
                    | Payment index -> Account.deriveNewAddress index extendedPrivateKey  >>= ExtendedKey.getPrivateKey |> get
                    | WatchOnly -> failwith "watch only address cannot be spent" 
            
            let publicKey = SecretKey.getPublicKey secretkey |> Option.get
            
            let lock = 
                match voteData with
                | { allocation = None; payout = None } ->
                    PK pkHash
                | _ ->
                    Types.Vote (voteData, (CGP.getInterval chainParams tip), pkHash)
            
            Outpoint outpoint, (secretkey,publicKey),  { lock = lock; spend = spend }
        )
        |> List.unzip3
     
    let! inputs =   
         if List.length inputs = 0 then
           eventX "Not enough tokens"
           |> Log.warning
    
           Error "Not enough tokens"
         else
           Ok inputs
             
    let transaction = {
        version=getTxVersion outputs
        inputs=inputs
        outputs = outputs 
        witnesses = []
        contract = None 
    }
    
    return Transaction.sign keys TxHash transaction
}

let createTransactionFromOutputs chainParams dataAccess session view password contract tip outputs = result {
    let account = DataAccess.Account.get dataAccess session

    let! extendedPrivateKey =
        Secured.decrypt password account.secureMnemonicPhrase
        >>= ExtendedKey.fromMnemonicPhrase
        >>= (ExtendedKey.derivePath Account.zenKeyPath)

    // summarize the amount of inputs needed per asset
    let requiredAmounts = List.fold (fun amounts (output:Consensus.Types.Output) ->
        match Map.tryFind output.spend.asset amounts with
        | Some amount -> Map.add output.spend.asset (amount + output.spend.amount) amounts
        | None ->  Map.add output.spend.asset output.spend.amount amounts) Map.empty outputs

    let! inputs = collectInputs chainParams dataAccess session view account requiredAmounts
    let changeOutputs = getChangeOutputs inputs requiredAmounts account chainParams tip

    // Convert to outpoint list and get the key for every input
    let inputs,keys =
        Map.toSeq inputs
        |> Seq.collect (fun (_,inputs) -> inputs)
        |> Seq.map (fun (outpoint,_,pkHash) ->

            let address = DataAccess.Addresses.get dataAccess session pkHash

            let secretkey =
                match address.addressType with
                | Change index -> Account.deriveChange index extendedPrivateKey >>= ExtendedKey.getPrivateKey |> get // The key must be valid as we already used it, safe to call get
                | External index -> Account.deriveExternal index extendedPrivateKey >>= ExtendedKey.getPrivateKey |> get
                | Payment index -> Account.deriveNewAddress index extendedPrivateKey  >>= ExtendedKey.getPrivateKey |> get
                | WatchOnly -> failwith "watch only address cannot be spent"

            let publicKey = SecretKey.getPublicKey secretkey |> Option.get

            Outpoint outpoint, (secretkey,publicKey)
            )
        |> List.ofSeq
        |> List.unzip

    let transaction = {
        version=getTxVersion <| outputs @ changeOutputs
        inputs=inputs
        outputs = outputs @ changeOutputs
        witnesses = []
        contract = contract
    }

    return Transaction.sign keys TxHash transaction
}

let createTransaction chainParams dataAccess session view password tip outputs  =
    createTransactionFromOutputs chainParams dataAccess session view password None tip outputs 


let addReturnAddressToData pkHash data =
    let addReturnAddressToData' dict =
        let returnAddress = PK pkHash

        Zen.Dictionary.add "returnAddress"B (ZData.Lock (ZFStar.fsToFstLock returnAddress)) dict
        |> Cost.__force
        |> ZData.Dict
        |> ZData.Collection
        |> Some
        |> Ok

    match data with
    | Some (ZData.Collection (ZData.Dict dict)) -> addReturnAddressToData' dict
    | None -> addReturnAddressToData' Zen.Dictionary.empty
    | _ -> Error "data can only be empty or dict in order to add return address"

let private signFirstWitness signKey tx = result {
    match signKey with
    | Some signKey ->
        let! witnessIndex =
            List.tryFindIndex (fun witness ->
                match witness with
                | ContractWitness _ -> true
                | _ -> false) tx.witnesses
            |> ofOption "missing contract witness"
        let! witness =
            match tx.witnesses.[witnessIndex] with
            | ContractWitness cw -> Ok cw
            | _ -> Error "missing contract witness"

        let txHash = Transaction.hash tx
        let message =
            {
                recipient = witness.contractId
                command = witness.command
                body = witness.messageBody
            }
            |> Serialization.Message.serialize

        let msg =
            [ Hash.bytes txHash; message ]
            |> Hash.computeMultiple

        let! signature = ExtendedKey.sign msg signKey
        let! publicKey = ExtendedKey.getPublicKey signKey

        let witness = {witness with signature=Some (publicKey,signature)}
        let witnesses = List.update witnessIndex (ContractWitness witness) tx.witnesses

        return {tx with witnesses = witnesses}
    | None -> return tx
}

let createExecuteContractTransaction chainParams dataAccess session view executeContract password (contractId:ContractId) command data provideReturnAddress sign spends tip = result {
    let account = DataAccess.Account.get dataAccess session

    let! masterPrivateKey =
        Secured.decrypt password account.secureMnemonicPhrase
        >>= ExtendedKey.fromMnemonicPhrase

    let! accountPrivateKey = ExtendedKey.derivePath Account.zenKeyPath masterPrivateKey

    let! inputs, txSkeleton = result {
        if Map.isEmpty spends then
            // To avoid rejection of a valid contract transaction due to possible all-mint inputs
            // or same txhash, until we implement fees, we include a temp fee of one kalapa
            let tempFeeAmount = 1UL

            let spends = (Map.add Asset.Zen tempFeeAmount Map.empty)
            let! inputs = collectInputs chainParams dataAccess session view account spends

            let feeOutput = { lock = Fee; spend = { amount = tempFeeAmount; asset = Asset.Zen } }

            let changeOutputs = getChangeOutputs inputs spends account chainParams tip

            let txSkeleton =
                TxSkeleton.addOutputs changeOutputs TxSkeleton.empty
                |> TxSkeleton.addOutput feeOutput

            return inputs,txSkeleton
        else
            let! inputs = collectInputs chainParams dataAccess session view account spends

            let changeOutputs = getChangeOutputs inputs spends account chainParams tip

            let txSkeleton = TxSkeleton.addOutputs changeOutputs TxSkeleton.empty

            return inputs,txSkeleton
    }

    let inputs,keys =
        Map.toSeq inputs
        |> Seq.collect (fun (_,inputs) -> inputs)
        |> Seq.map (fun (outpoint,output,pkHash) ->

            let address = DataAccess.Addresses.get dataAccess session pkHash

            let secretkey =
                match address.addressType with
                | Change index -> Account.deriveChange index accountPrivateKey >>= ExtendedKey.getPrivateKey |> get // The key must be valid as we already used it, safe to call get
                | External index -> Account.deriveExternal index accountPrivateKey >>= ExtendedKey.getPrivateKey |> get
                | Payment index -> Account.deriveNewAddress index accountPrivateKey  >>= ExtendedKey.getPrivateKey |> get
                | WatchOnly -> failwith "watch only address cannot be spent"

            let publicKey = SecretKey.getPublicKey secretkey |> Option.get

            TxSkeleton.Input.PointedOutput (outpoint,output), (secretkey,publicKey)
            )
        |> List.ofSeq
        |> List.unzip

    // Adding the inputs
    let txSkeleton = TxSkeleton.addInputs inputs txSkeleton

    let! data =
        if provideReturnAddress then
            addReturnAddressToData account.externalPKHash data
        else
            Ok data

    let! signKey =
        match sign with
        | Some keyPath ->
            ExtendedKey.derivePath keyPath masterPrivateKey
            <@> Some
        | None -> Ok None

    let! sender =
        match signKey with
        | Some signKey ->
            ExtendedKey.getPublicKey signKey
            <@> Some
        | None -> Ok None

    let! unsignedTx = executeContract contractId command sender data txSkeleton

    let sign tx = signFirstWitness signKey tx <@> Transaction.sign keys FollowingWitnesses

    return! sign unsignedTx
}

let createActivateContractTransaction chainParams dataAccess session view chain password code (numberOfBlocks:uint32) tip =
    result {
        let contractId = Contract.makeContractId Version0 code

        let! hints = Measure.measure
                        (sprintf "recording hints for contract %A" contractId)
                        (lazy(Contract.recordHints code))
        let! queries = ZFStar.totalQueries hints

        let contract =
            {   code = code
                hints = hints
                rlimit = rlimit
                queries = queries }
            |> V0
            |> Some

        let codeLength = String.length code |> uint64

        let activationFee = queries * rlimit / 100ul |> uint64
        let activationSacrifice = chain.sacrificePerByteBlock * codeLength * (uint64 numberOfBlocks)

        let outputs =
            [
                { spend = { amount = activationSacrifice; asset = Asset.Zen }; lock = ActivationSacrifice }
                { spend = { amount = activationFee; asset = Asset.Zen }; lock = Fee }
            ]

        return! createTransactionFromOutputs chainParams dataAccess session view password contract tip outputs 
    }

let createExtendContractTransaction dataAccess session view (getContract:ContractId->ActiveContract option) chainParams password (contractId:ContractId) (numberOfBlocks:uint32) tip =
    result {
        let! code =
            match getContract contractId with
            | Some contract -> Ok contract.code
            | None -> Error "contract is not active"

        let codeLength = String.length code |> uint64
        let extensionSacrifice = chainParams.sacrificePerByteBlock * codeLength * (uint64 numberOfBlocks)
        let output = {lock=ExtensionSacrifice contractId; spend= { amount = extensionSacrifice; asset = Asset.Zen }}

        let outputs = [output]

        return! createTransactionFromOutputs chainParams dataAccess session view password None tip outputs 
    }

let createRawTransaction chainParams dataAccess session view contract tip outputs = result {
    let account = DataAccess.Account.get dataAccess session

    // summarize the amount of inputs needed per asset
    let requiredAmounts = List.fold (fun amounts (output:Consensus.Types.Output) ->
        match Map.tryFind output.spend.asset amounts with
        | Some amount -> Map.add output.spend.asset (amount + output.spend.amount) amounts
        | None ->  Map.add output.spend.asset output.spend.amount amounts) Map.empty outputs

    let! inputs = collectInputs chainParams dataAccess session view account requiredAmounts
    let changeOutputs = getChangeOutputs inputs requiredAmounts account chainParams tip

    // Convert to outpoint list and get the key for every input
    let inputs,witnesses =
        Map.toSeq inputs
        |> Seq.collect (fun (_,inputs) -> inputs)
        |> Seq.map (fun (outpoint,_,pkHash) ->

            let address = DataAccess.Addresses.get dataAccess session pkHash
            let keyPath = Account.getKeyPath address.addressType

            let publicKey =
                match address.addressType with
                | Change index -> Account.deriveChange index account.publicKey >>= ExtendedKey.getPublicKey |> get // The key must be valid as we already used it, safe to call get
                | External index -> Account.deriveExternal index account.publicKey >>= ExtendedKey.getPublicKey |> get
                | Payment index -> Account.deriveNewAddress index account.publicKey  >>= ExtendedKey.getPublicKey |> get
                | WatchOnly -> failwith "watch only address cannot be spent"

            Outpoint outpoint,  EmptyPKWitness (TxHash, publicKey, keyPath)
            )
        |> List.ofSeq
        |> List.unzip

    let raw:RawTransaction = {
        version=getTxVersion <| outputs @ changeOutputs
        inputs=inputs
        outputs = outputs @ changeOutputs
        witnesses = witnesses
        contract = contract
    }

    return raw
}

let signRawTransaction dataAccess session password (raw:RawTransaction) = result {
    let account = DataAccess.Account.get dataAccess session

    let! extendedPrivateKey =
        Secured.decrypt password account.secureMnemonicPhrase
        >>= ExtendedKey.fromMnemonicPhrase

    let txHash = Transaction.fromRaw raw |> Transaction.hash

    let folder witness witnesses =
        match witness with
        | Witness _
        | HighVRawWitness _ -> witness :: witnesses
        | EmptyPKWitness (sigHash,publicKey,keyPath) ->
            let extendedKey = ExtendedKey.derivePath keyPath extendedPrivateKey |> Result.get
            let publicKey' = ExtendedKey.getPublicKey extendedKey |> Result.get

            if publicKey' <> publicKey then
                EmptyPKWitness (sigHash,publicKey,keyPath) :: witnesses // It is not our key, skipping
            else
                let secretKey = ExtendedKey.getPrivateKey extendedKey |> Result.get

                match sigHash with
                | TxHash ->
                    Witness (PKWitness (sigHash, publicKey, Crypto.sign secretKey txHash)) :: witnesses
                | FollowingWitnesses ->
                    let signedWitnesses =
                        List.choose (fun w ->
                            match w with
                            | Witness w -> Some w
                            | _ -> None) witnesses

                    if List.length signedWitnesses <> List.length witnesses then
                        witness :: witnesses // Not all witnesses are signed, we are not ready to sign
                    else

                    let witnessesHash = Serialization.Witnesses.hash signedWitnesses
                    let msg = Hash.joinHashes [ txHash; witnessesHash ]

                    Witness (PKWitness (sigHash, publicKey, Crypto.sign secretKey msg)) :: witnesses

                | _ -> witness :: witnesses // unknown sighash, skipping

    let witnesses =
        List.foldBack folder raw.witnesses List.empty

    return {raw with witnesses = witnesses}
}
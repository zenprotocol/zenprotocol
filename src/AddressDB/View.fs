module AddressDB.View

open Consensus
open Wallet.Types
open Wallet.Address
open Consensus.Types
open DataAccess
open Types
open Infrastructure
open Messaging.Services.AddressDB
open Zen.Types.Data
open Messaging.Services.Wallet
module CryptoPublicKey = Crypto.PublicKey

let addOutput outputs outpoint output =
    Map.add outpoint output outputs

let addAddressOutpoints addressOutpoints address outpoints =
    let outpoints =
        outpoints @
        match Map.tryFind address addressOutpoints with
        | Some outpoints -> outpoints
        | None -> []
    Map.add address outpoints addressOutpoints

let addContractHistory contractData contractId data =
    let data =
        data @
        match Map.tryFind contractId contractData with
        | Some data -> data
        | None -> []
    Map.add contractId data contractData

let addContractAsset
    (contractAssetData:Map<Asset, uint32 * string option * string * data option>)
    (mintLists: List<Asset * uint32 * string option * string * data option>) =
        let mutable mintMap = contractAssetData
        for mint in mintLists do
            match mint with
            | asset,blockNumber, sender, command, msgBody ->
               mintMap <- Map.add asset (blockNumber, sender, command, msgBody) mintMap
        mintMap

let setContractData contractData witnessPoint data =
    Map.add witnessPoint data contractData

let setContractConfirmationStatus contractData witnessPoint data =
    Map.add witnessPoint data contractData

type T =
    {
        outpointOutputs: Map<Outpoint, DBOutput>
        addressOutpoints: Map<Address, List<Outpoint>>
        contractHistory: Map<ContractId, List<WitnessPoint>>
        contractData: Map<WitnessPoint, string * data option>
        contractAsset: Map<Asset, uint32 * string option * string * data option>
        contractConfirmations: Map<WitnessPoint, ConfirmationStatus>
    }
    with
        member this.getOutputs outputs = // add given (db) onto view (memory)
            Map.fold addOutput outputs this.outpointOutputs
        member this.getAddressOutpoints addressOutpoints = // add given (db) onto view (memory)
            Map.fold addAddressOutpoints addressOutpoints this.addressOutpoints

let empty = {
    outpointOutputs = Map.empty
    addressOutpoints = Map.empty
    contractHistory = Map.empty
    contractData = Map.empty
    contractAsset = Map.empty
    contractConfirmations = Map.empty
}

let witnessPoint txHash witnesses cw =
    {
        txHash = txHash
        index =
            witnesses
            |> List.findIndex ((=) (ContractWitness cw))
            |> uint32
    }

module OutpointOutputs =
    let get view dataAccess session outpoints =
        outpoints
        |> List.map (fun outpoint ->
            match Map.tryFind outpoint view.outpointOutputs with
            | Some dbOutput -> dbOutput
            | None -> OutpointOutputs.get dataAccess session outpoint)

module AddressOutpoints =
    let private get' view addresses =
        addresses
        |> List.map (fun address -> Map.tryFind address view.addressOutpoints)
        |> List.map (function
            | Some list -> list
            | None -> List.empty)
        |> List.concat
    let get view dataAcesss session addresses =
        AddressOutpoints.get dataAcesss session addresses @ get' view addresses

module ContractHistory =
    let private get' view contractId =
        Map.tryFind contractId view.contractHistory
        |> function
            | Some list -> list
            | None -> List.empty
    let get view dataAccess session contractId =
        ContractHistory.get dataAccess session contractId @ get' view contractId

module ContractAssets =
    let get (view:T) dataAccess session asset =
        match ContractAssets.tryGet dataAccess session asset with
        | Some a ->
            Some a
        | None -> Map.tryFind asset view.contractAsset


module ContractData =
    let get view dataAccess session witnessPoint =
        Map.tryFind witnessPoint view.contractData
        |> Option.defaultWith (fun _ ->
            ContractData.get dataAccess session witnessPoint)
        |> fun (command, data) -> (command, data, witnessPoint.txHash)

module ContractConfirmations =
    let get view dataAccess session witnessPoint =
        Map.tryFind witnessPoint view.contractConfirmations
        |> Option.defaultWith (fun _ ->
            ContractConfirmations.get dataAccess session witnessPoint)

let mapUnspentTxOutputs outputs txHash confirmationStatus =
    outputs
    |> List.mapi (fun index output -> uint32 index, output)
    |> List.choose (fun (index, output) ->
        match output.lock with
        | Coinbase (_,pkHash)
        | PK pkHash -> Some (Address.PK pkHash, index, output)
        | Contract contractId -> Some (Address.Contract contractId, index, output)
        | _ -> None)
    |> List.map (fun (address, index, output) ->
        {
            address = address
            outpoint = { txHash = txHash; index = index }
            spend = output.spend
            lock = output.lock
            status = Unspent
            confirmationStatus = confirmationStatus
        })
let private extractPK ( signature: (Crypto.PublicKey * 'a) option ) =
        match signature with
        | Some (pk , _) -> Some (CryptoPublicKey.toString pk)
        | None -> None

let addMempoolTransaction dataAccess session txHash tx view =
    let account = DataAccess.Tip.get dataAccess session
    let mintsList =
        tx.inputs
        |> List.choose (function | Mint spend-> Some spend.asset | _ -> None)
        |> List.map (fun asset ->
                    tx.witnesses
                    |> List.choose (fun x -> match x with | ContractWitness cw -> Some (asset,account.blockNumber, extractPK cw.signature, cw.command, cw.messageBody) | _ -> None)
                    |> List.head)

    tx.inputs
    |> List.choose (function | Outpoint outpoint -> Some outpoint | _ -> None)
    |> List.fold (fun view outpoint ->
        view
        |> Option.bind (fun view ->
            Map.tryFind outpoint view.outpointOutputs
            |> function
            | Some dbOutput -> Some dbOutput
            | None -> DataAccess.OutpointOutputs.tryGet dataAccess session outpoint
            |> Option.map (fun dbOutput -> { dbOutput with status = Spent (txHash, Unconfirmed) })
            |> Option.map (fun dbOutput -> { view with outpointOutputs = addOutput view.outpointOutputs outpoint dbOutput })
        )
    ) (Some view)
    |> Option.map (fun view ->
        mapUnspentTxOutputs tx.outputs txHash Unconfirmed
        |> List.fold (fun view dbOutput ->
        {
            outpointOutputs = addOutput view.outpointOutputs dbOutput.outpoint dbOutput
            addressOutpoints = addAddressOutpoints view.addressOutpoints dbOutput.address [ dbOutput.outpoint ]
            contractHistory = view.contractHistory
            contractData = view.contractData
            contractAsset = view.contractAsset
            contractConfirmations = view.contractConfirmations
        }) view)
    |> Option.map (fun view ->
        tx.witnesses
        |> List.fold (fun view -> function
            | ContractWitness cw ->
                let witnessPoint = witnessPoint txHash tx.witnesses cw

                {
                    outpointOutputs = view.outpointOutputs
                    addressOutpoints = view.addressOutpoints
                    contractHistory = addContractHistory view.contractHistory cw.contractId [ witnessPoint ]
                    contractData = setContractData view.contractData witnessPoint (cw.command, cw.messageBody)
                    contractAsset = view.contractAsset
                    contractConfirmations = setContractConfirmationStatus view.contractConfirmations witnessPoint Unconfirmed
                }
            | _ ->
                view
        ) view)
    |> function
    | Some view' ->
        {view' with contractAsset = addContractAsset view.contractAsset mintsList}
    | None -> view

let fromMempool dataAccess session =
    List.fold (fun view (txHash,tx) -> addMempoolTransaction dataAccess session txHash tx view) empty

let private filterDBOutput blockNumber dbOutput  =

    let compareToConfirmationStatus blockNumberUpperBound =
        match dbOutput.confirmationStatus with
        | Confirmed (_,databaseBlockNumber,_,_) ->
            databaseBlockNumber <= blockNumberUpperBound
        | _ -> false

    blockNumber
    |> Option.map compareToConfirmationStatus
    |> Option.defaultValue true

let getOutputs dataAccess session view mode addresses : PointedOutput list =
    AddressOutpoints.get view dataAccess session addresses
    |> OutpointOutputs.get view dataAccess session
    |> List.choose (fun dbOutput ->
        if mode <> UnspentOnly || dbOutput.status = Unspent then
            Some (dbOutput.outpoint, { spend = dbOutput.spend; lock = dbOutput.lock })
        else
            None)
let private addBalance asset amount balance =
    match Map.tryFind asset balance with
    | Some amount' -> Map.add asset (amount' + amount) balance
    | None -> Map.add asset amount balance

let getBalance dataAccess session view mode (blockNumber: uint32 option) addresses =
    match blockNumber with
    | None ->
        getOutputs dataAccess session view mode addresses
        |> List.fold (fun balance (_,output) ->
        match Map.tryFind output.spend.asset balance with
        | Some amount -> Map.add output.spend.asset (amount + output.spend.amount) balance
        | None -> Map.add output.spend.asset output.spend.amount balance
        ) Map.empty
    | Some blockNumberUpperBound ->
        addresses
        |> List.fold (fun balance address ->
            AddressOutpoints.get view dataAccess session [address]
            |> OutpointOutputs.get view dataAccess session
            |> List.fold (fun addressBalance dbOutput ->
                match dbOutput.confirmationStatus with
                | Confirmed (_,databaseBlockNumber,_,_) ->
                    if databaseBlockNumber <= blockNumberUpperBound then
                        match dbOutput.status with
                        | Unspent ->
                            addBalance dbOutput.spend.asset dbOutput.spend.amount addressBalance
                        | Spent (_,confirmation) ->
                            match confirmation with
                            | Confirmed (_,dbBlockNumber,_,_) ->
                                if (dbBlockNumber > blockNumberUpperBound) then
                                    addBalance dbOutput.spend.asset dbOutput.spend.amount addressBalance
                                else
                                    addressBalance
                            | Unconfirmed ->
                                addBalance dbOutput.spend.asset dbOutput.spend.amount addressBalance
                    else addressBalance
                | Unconfirmed ->
                    match dbOutput.status with
                    | Unspent ->
                        let account = Tip.get dataAccess session
                        if account.blockNumber + 1ul = blockNumberUpperBound then
                            addBalance dbOutput.spend.asset dbOutput.spend.amount addressBalance
                        else
                            addressBalance
                    | _ ->
                        addressBalance
                ) balance) Map.empty
let getConfirmations blockNumber =
    function
    | Confirmed (timestamp,blockNumber',_,blockIndex) ->
        blockNumber - blockNumber' + 1ul, blockIndex, timestamp
    | Unconfirmed ->
        0ul, 0,Timestamp.now()

let private filterDBOutputStartEnd startBlock endBlock currentBlockNumber outputsInfos =
    match outputsInfos with
    | _,_,_,confirmation,_,_,_ ->
        let databaseBlockNumber = currentBlockNumber - confirmation + 1ul
        startBlock <= databaseBlockNumber && databaseBlockNumber < endBlock


let private filterConfirmationStartEnd startBlock endBlock confirmation =
    match snd confirmation with
    | Confirmed (_,databaseBlockNumber,_,_) ->
        startBlock <= databaseBlockNumber && databaseBlockNumber < endBlock
    | Unconfirmed -> true

let getOutputsInfo blockNumber outputs =

    let incoming = List.map (fun (output:DBOutput) ->
        let txHash = output.outpoint.txHash
        let confirmations,blockIndex,timestamp = getConfirmations blockNumber output.confirmationStatus

        txHash, output.spend.asset, output.spend.amount |> bigint, timestamp, confirmations, blockIndex, output.lock) outputs

    let outgoing = List.choose (fun (output:DBOutput) ->
        match output.status with
        | Unspent -> None
        | Spent (txHash,confirmationStatus) ->
            let confirmations,blockIndex,timestamp = getConfirmations blockNumber confirmationStatus

            (txHash, output.spend.asset, output.spend.amount |> bigint |> (*) -1I, timestamp, confirmations,blockIndex,output.lock)
            |> Some) outputs

    incoming @ outgoing
    |> List.fold (fun txs (txHash, asset, amount,timestamp, confirmations, blockIndex, lock) ->
        match Map.tryFind (txHash, asset) txs with
        | None -> Map.add (txHash, asset) (amount, confirmations,timestamp, blockIndex, lock) txs
        | Some (amount',_,_,_,lock) -> Map.add (txHash, asset) (amount + amount', confirmations,timestamp, blockIndex, lock) txs) Map.empty
    |> Map.toSeq
    |> Seq.map (fun ((txHash, asset),(amount, confirmations,timestamp, blockIndex, lock)) ->
        if amount >= 0I then
            (txHash,TransactionDirection.In, {asset=asset;amount = uint64 amount}, confirmations, timestamp, blockIndex, lock)
        else
            (txHash,TransactionDirection.Out, {asset=asset;amount = amount * -1I |> uint64}, confirmations, timestamp, blockIndex, lock))
    |> List.ofSeq

let getTransactionCount dataAccess session view blockNumber addresses =
    let outputs =
        AddressOutpoints.get view dataAccess session addresses
        |> OutpointOutputs.get view dataAccess session
    List.length (outputs |> getOutputsInfo blockNumber)

let txComparer (_,_,_,confirmation,_,blockIndex,_) (_,_,_,confirmation',_,blockIndex',_) =
    if confirmation < confirmation' then
       -1
    elif confirmation > confirmation' then
       1
    else
        blockIndex' - blockIndex

let getHistory dataAccess session view skip take addresses =
    let account = DataAccess.Tip.get dataAccess session

    AddressOutpoints.get view dataAccess session addresses
    |> OutpointOutputs.get view dataAccess session
    |> getOutputsInfo account.blockNumber
    |> List.sortWith txComparer
    |> Wallet.Account.paginate skip take
    |> List.map (fun (txHash,direction,spend,confirmations,timestamp,_,lock) -> txHash,direction,spend,confirmations,timestamp,lock)

let getHistoryByBlockNumber dataAccess session view startBlock endBlock addresses =
    let account = DataAccess.Tip.get dataAccess session

    AddressOutpoints.get view dataAccess session addresses
    |> OutpointOutputs.get view dataAccess session
    |> getOutputsInfo account.blockNumber
    |> List.filter (filterDBOutputStartEnd startBlock endBlock account.blockNumber)
    |> List.sortWith txComparer
    |> List.map (fun (txHash,direction,spend,confirmations,timestamp,_,lock) -> txHash,direction,spend,confirmations,timestamp,lock)

let private contractHistoryComparer a1 a2 =
    let comparer (index1, block1, _) (index2, block2, _) =
            if index1 = index2 then
                block2 - block1
            else
                int (index2 - index1)
    comparer (snd a1) (snd a2)

let getContractHistory dataAccess session view skip take contractId =
    let account = DataAccess.Tip.get dataAccess session

    ContractHistory.get view dataAccess session contractId
    |> List.map (fun witnessPoint ->
        ContractData.get view dataAccess session witnessPoint,
        ContractConfirmations.get view dataAccess session witnessPoint
        |> getConfirmations account.blockNumber)
    |> List.sortWith contractHistoryComparer
    |> Wallet.Account.paginate skip take
    |> List.map (fun ((command, messageBody, txHash), (confirmations, _, _)) -> command, messageBody, txHash, confirmations)

let getContractHistoryByBlockNumber dataAccess session view startBlock endBlock contractId =
    let account = DataAccess.Tip.get dataAccess session

    ContractHistory.get view dataAccess session contractId
    |> List.map (fun witnessPoint ->
        ContractData.get view dataAccess session witnessPoint,
        ContractConfirmations.get view dataAccess session witnessPoint)
    |> List.filter (filterConfirmationStartEnd startBlock endBlock)
    |> List.map (fun (contractData, confirmationStatus) -> contractData, confirmationStatus |> getConfirmations account.blockNumber)
    |> List.sortWith contractHistoryComparer
    |> List.map (fun ((command, messageBody, txHash), (confirmations, _,_)) -> command, messageBody, txHash, confirmations)


let getContractAsset dataAccess session view asset =
    ContractAssets.get view dataAccess session asset
let result = Result.ResultBuilder<string>()

let getContractInfo limit code =
    result {
        let contractId = Contract.makeContractId Version0 code

        let rlimit = limit |> Option.defaultValue Wallet.TransactionCreator.Rlimit

        let! hints = Measure.measure
                        (sprintf "recording hints for contract %A" contractId)
                        (lazy(Contract.recordHints rlimit code))
        let! queries = ZFStar.totalQueries hints

        let contractV0 =
            {   code = code
                hints = hints
                rlimit = rlimit
                queries = queries }
        return (contractId,contractV0)
}

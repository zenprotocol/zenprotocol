module AddressDB.View

open Consensus
open Wallet.Types
open Wallet.Address
open Consensus.Types
open DataAccess
open Types
open Hash
open Infrastructure
open Messaging.Services.AddressDB
open Zen.Types.Data
open Result

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
    
let addContractData contractData contractId data =
    let data = 
        data @
        match Map.tryFind contractId contractData with 
        | Some data -> data
        | None -> []
    Map.add contractId data contractData
    
type T =
    {
        outpointOutputs: Map<Outpoint, DBOutput>
        addressOutpoints: Map<Address, List<Outpoint>>
        contractHistory: Map<ContractId, List<Hash * uint32>>
        contractData: Map<Hash * uint32, List<string * data option>>
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
}

let private witnessIndex tx =
    List.tryFindIndex (fun witness ->
        match witness with
        | ContractWitness _ -> true
        | _ -> false) tx.witnesses
    |> ofOption "missing contract witness"
    |> get
    |> uint32


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
    open AddressDB.Serialization

    let private get' view contractId =
        Map.tryFind contractId view.contractHistory
        |> function 
            | Some list -> list
            | None -> List.empty
    let get view dataAccess session contractId =
        ContractHistory.get dataAccess session contractId @ get' view contractId
        
module ContractData =
    open AddressDB.Serialization
    
    let get view dataAccess session txHash witnessIndex =
        match Map.tryFind (txHash, witnessIndex) view.contractData with 
        | Some [command,data] -> (command, data, txHash)
        | _ -> 
            ContractData.get dataAccess session (txHash,witnessIndex)
            |> fun (command, data) -> (command, data, txHash)   

        
        
let mapTxOutputs tx txHash confirmationStatus =
    tx.outputs
    |> List.mapi (fun index output -> uint32 index, output)
    |> List.choose (fun (index, output) ->
        match output.lock with
        | Coinbase (_,pkHash) -> Some (Address.PK pkHash, index, output)
        | PK pkHash -> Some (Address.PK pkHash, index, output)
        | Contract contractId -> Some (Address.Contract contractId, index, output)
        | _ -> None)
    |> List.map (fun (address, index, output) -> address, { txHash = txHash; index = index }, output)
    |> List.map (fun (address, outpoint, output) -> address, outpoint, {
        address = address
        outpoint = outpoint
        spend = output.spend
        lock = output.lock
        status = Unspent
        confirmationStatus = confirmationStatus
    })

let addMempoolTransaction dataAccess session txHash tx view =
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
        mapTxOutputs tx txHash Unconfirmed
        |> List.fold (fun view (address, outpoint, output) ->  
        {
            outpointOutputs = addOutput view.outpointOutputs outpoint output
            addressOutpoints = addAddressOutpoints view.addressOutpoints address [ outpoint ]
            contractHistory = view.contractHistory
            contractData = view.contractData
        }) view)
    |> Option.map (fun view ->
        tx.witnesses
        |> List.fold (fun view -> function 
            | ContractWitness cw ->
                {
                    outpointOutputs = view.outpointOutputs
                    addressOutpoints = view.addressOutpoints
                    contractHistory = addContractHistory view.contractHistory cw.contractId [ txHash, witnessIndex tx ]
                    contractData = addContractData view.contractData (txHash, witnessIndex tx)  [cw.command, cw.messageBody]
                }                
            | _ ->
                view
        ) view)
    |> function
    | Some view' -> view'
    | None -> view
        
let fromMempool dataAccess session =
    List.fold (fun view (txHash,tx) -> addMempoolTransaction dataAccess session txHash tx view) empty
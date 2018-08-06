module AddressDB.View

open Consensus
open Wallet.Types
open Wallet.Address
open Consensus.Types
open DataAccess
open Types
open Hash
open Messaging.Services.AddressDB
open Zen.Types.Data

let addOutput outputs outpoint output =
    Map.add outpoint output outputs

let addAddressOutpoints addressOutpoints address outpoints =
    let outpoints = 
        outpoints @
        match Map.tryFind address addressOutpoints with 
        | Some outpoints -> outpoints
        | None -> []
    Map.add address outpoints addressOutpoints

let addContractData contractData contractId data =
    let data = 
        data @
        match Map.tryFind contractId contractData with 
        | Some outpoints -> outpoints
        | None -> []
    Map.add contractId data contractData
    
type T =
    {
        outpointOutputs: Map<Outpoint, DBOutput>
        addressOutpoints: Map<Address, List<Outpoint>>
        contractData: Map<ContractId, List<string * data option>>
    }
    with
        member this.getOutputs outputs = // add given (db) onto view (memory)
            Map.fold addOutput outputs this.outpointOutputs
        member this.getAddressOutpoints addressOutpoints = // add given (db) onto view (memory)
            Map.fold addAddressOutpoints addressOutpoints this.addressOutpoints

let empty = {
    outpointOutputs = Map.empty
    addressOutpoints = Map.empty
    contractData = Map.empty
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

module ContractData =
    let private get' view contractId =
        Map.tryFind contractId view.contractData
        |> function 
            | Some list -> list
            | None -> List.empty
    let get view dataAcesss session contractId =
        ContractData.get dataAcesss session contractId @ get' view contractId
        
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
            contractData = view.contractData
        }) view)
    |> Option.map (fun view ->
        tx.witnesses
        |> List.fold (fun view -> function 
            | ContractWitness cw ->
                {
                    outpointOutputs = view.outpointOutputs
                    addressOutpoints = view.addressOutpoints
                    contractData = addContractData view.contractData cw.contractId [ cw.command, cw.messageBody ]
                }                
            | _ ->
                view
        ) view)
    |> function
    | Some view' -> view'
    | None -> view
        
let fromMempool dataAccess session =
    List.fold (fun view (txHash,tx) -> addMempoolTransaction dataAccess session txHash tx view) empty
module AddressDB.DataAccess

open System.Text
open Infrastructure
open DataAccess
open Consensus
open Types
open Wallet.Serialization
open AddressDB.Serialization
open Consensus.Serialization
open Wallet.Address
open Zen.Types.Data

let private getBytes str = Encoding.UTF8.GetBytes (str : string)

[<Literal>]
let DbVersion = 3

type T = {
    outpointOutputs: Collection<Outpoint, DBOutput>
    addressOutpoints: MultiCollection<Address, Outpoint>
    contractHistory: MultiCollection<ContractId, WitnessPoint>
    contractData: Collection<WitnessPoint, string * data option>
    contractConfirmations: Collection<WitnessPoint, Wallet.Types.ConfirmationStatus>
    contractAssets: Collection<Asset,uint32 * string option* string * data option>
    tip: SingleValue<Tip>
    dbVersion: SingleValue<int>
}

let createContext dataPath =
    Platform.combine dataPath "addressDB"
    |> DatabaseContext.create DatabaseContext.Medium

module WitnessPoint = Serialization.Outpoint

let init databaseContext =

    use session = DatabaseContext.createSession databaseContext
    let outpointOutputs = Collection.create session "outpointOutputs" Outpoint.serialize Output.serialize Output.deserialize
    let addressOutpoints = MultiCollection.create session "addressOutpoints" Address.serialize Outpoint.serialize Outpoint.deserialize
    let contractData = Collection.create session "contracData" WitnessPoint.serialize ContractData.serialize ContractData.deserialize
    let contractHistory = MultiCollection.create session "contractHistory" Serialization.ContractId.serialize WitnessPoint.serialize WitnessPoint.deserialize
    let contractConfirmations = Collection.create session "contractConfirmations" WitnessPoint.serialize ConfirmationStatus.serialize ConfirmationStatus.deserialize
    let contractAssets = Collection.create session "contractAssets" Asset.serialize ContractAsset.serialize ContractAsset.deserialize
    let tip = SingleValue.create databaseContext "blockchain" Tip.serialize Tip.deserialize
    let dbVersion = SingleValue.create databaseContext "dbVersion" Version.serialize Version.deserialize

    match SingleValue.tryGet dbVersion session with
    | None ->
        SingleValue.put dbVersion session DbVersion
    | Some version when version < DbVersion->
        Platform.cleanDirectory "addressDB"
        SingleValue.put dbVersion session DbVersion
    | Some DbVersion ->
        ()
    | Some version ->
        failwithf "AddressDB: wrong db version, expected %d but got %d" DbVersion version

    let t = {
        outpointOutputs = outpointOutputs
        addressOutpoints = addressOutpoints
        contractHistory = contractHistory
        contractData = contractData
        contractConfirmations = contractConfirmations
        contractAssets = contractAssets
        tip = tip
        dbVersion = dbVersion
    }

    Session.commit session
    t

let dispose t =
    Disposables.dispose t.outpointOutputs

module Tip =
    let put t = SingleValue.put t.tip
    let tryGet t = SingleValue.tryGet t.tip
    let get t session = tryGet t session |> Option.get

module OutpointOutputs =
    let get t = Collection.get t.outpointOutputs
    let tryGet t = Collection.tryGet t.outpointOutputs
    let put t = Collection.put t.outpointOutputs
    let delete t = Collection.delete t.outpointOutputs
    let truncate t = Collection.truncate t.outpointOutputs
    let contains t = Collection.containsKey t.outpointOutputs

module AddressOutpoints =
    let put t = MultiCollection.put t.addressOutpoints
    let delete t = MultiCollection.delete t.addressOutpoints
    let get t session addresses =
        addresses
        |> List.map (MultiCollection.get t.addressOutpoints session)
        |> List.concat
    let truncate t = MultiCollection.truncate t.addressOutpoints

module ContractData =
    let get t = Collection.get t.contractData
    let tryGet t = Collection.tryGet t.contractData
    let put t = Collection.put t.contractData
    let delete t = Collection.delete t.contractData
    let truncate t = Collection.truncate t.contractData

module ContractAssets =
    let get t = Collection.get t.contractAssets
    let tryGet t = Collection.tryGet t.contractAssets
    let put t = Collection.put t.contractAssets
    let delete t = Collection.delete t.contractAssets
    let truncate t = Collection.truncate t.contractAssets


module ContractHistory =
    let get t = MultiCollection.get t.contractHistory
    let put t = MultiCollection.put t.contractHistory
    let delete t = MultiCollection.delete t.contractHistory
    let truncate t = MultiCollection.truncate t.contractHistory

module ContractConfirmations =
    let get t = Collection.get t.contractConfirmations
    let put t = Collection.put t.contractConfirmations
    let delete t = Collection.delete t.contractConfirmations
    let truncate t = Collection.truncate t.contractConfirmations

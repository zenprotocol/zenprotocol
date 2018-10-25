module AddressDB.DataAccess

open System.Text
open Infrastructure
open DataAccess
open Consensus
open Types
open Hash
open Wallet.Serialization
open AddressDB.Serialization
open Consensus.Serialization
open Wallet.Address
open Zen.Types.Data

let private getBytes str = Encoding.UTF8.GetBytes (str : string)

[<Literal>]
let DbVersion = 1

type T = {
    outpointOutputs: Collection<Outpoint, DBOutput>
    addressOutpoints: MultiCollection<Address, Outpoint>
    contractData: MultiCollection<ContractId, string * data option * Hash>
    tip: SingleValue<Tip>
    dbVersion: SingleValue<int>
}

let createContext dataPath =
    Platform.combine dataPath "addressDB"
    |> DatabaseContext.create DatabaseContext.Medium

let init databaseContext =
    use session = DatabaseContext.createSession databaseContext
    let outpointOutputs = Collection.create session "outpointOutputs" Outpoint.serialize Output.serialize Output.deserialize
    let addressOutpoints = MultiCollection.create session "addressOutpoints" Address.serialize Outpoint.serialize Outpoint.deserialize
    let contractData = MultiCollection.create session "contractData" Serialization.ContractId.serialize ContractData.serialize ContractData.deserialize
    let tip = SingleValue.create databaseContext "blockchain" Tip.serialize Tip.deserialize
    let dbVersion = SingleValue.create databaseContext "dbVersion" Version.serialize Version.deserialize

    match SingleValue.tryGet dbVersion session with
    | Some version when version <> DbVersion ->
        failwithf "AddressDB: wrong db version, expected %d but got %d" DbVersion version
    | None ->
        SingleValue.put dbVersion session DbVersion
    | _ -> () // TODO: in the future we should have here db upgrade script

    let t = {
        outpointOutputs = outpointOutputs
        addressOutpoints = addressOutpoints
        contractData = contractData
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
    let get t = MultiCollection.get t.contractData
    let put t = MultiCollection.put t.contractData
    let delete t = MultiCollection.delete t.contractData
    let truncate t = MultiCollection.truncate t.contractData
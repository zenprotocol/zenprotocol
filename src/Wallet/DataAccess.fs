module Wallet.DataAccess

open System.Text
open Infrastructure
open DataAccess
open Consensus
open Consensus.Types
open Consensus.Hash
open Wallet.Types
open Wallet.Serialization
open Wallet.Serialization

let private getBytes str = Encoding.UTF8.GetBytes (str : string)

[<Literal>]
let DbVersion = 1

type T = {
    outputs: Collection<Outpoint, Output>
    addresses: Collection<Hash,Address>
    addressOutputs: MultiCollection<Hash,Outpoint>
    account: SingleValue<Account>
    dbVersion: SingleValue<int>
}

let createContext dataPath =
    Platform.combine dataPath "payment"
    |> DatabaseContext.create

let init databaseContext =
    use session = DatabaseContext.createSession databaseContext
    let outputs = Collection.create session "outputs" Outpoint.serialize Output.serialize Output.deserialize
    let addresses = Collection.create session "addresses" Hash.bytes Address.serialize Address.deserialize
    let addressOutputs = MultiCollection.create session "addressOutputs" Hash.bytes Outpoint.serialize Outpoint.deserialize
    let account = SingleValue.create databaseContext "account" Account.serialize Account.deserialize

    let dbVersion = SingleValue.create databaseContext "dbVersion" Version.serialize Version.deserialize

    match SingleValue.tryGet dbVersion session with
    | Some version when version <> DbVersion ->
        failwithf "Wallet: wrong db version, expected %d but got %d" DbVersion version
    | None ->
        SingleValue.put dbVersion session DbVersion
    | _ -> () // TODO: in the future we should have here db upgrade script

    let t = {
        outputs = outputs
        addresses = addresses
        addressOutputs = addressOutputs
        account = account
        dbVersion = dbVersion
    }

    Session.commit session
    t

let dispose t =
    Disposables.dispose t.outputs
    Disposables.dispose t.addresses

module Account =
    let put t = SingleValue.put t.account
    let tryGet t = SingleValue.tryGet t.account
    let get t session = tryGet t session |> Option.get

module Addresses =
    let contains t = Collection.containsKey t.addresses
    let getAll t = Collection.getAll t.addresses
    let get t = Collection.get t.addresses
    let put t  = Collection.put t.addresses

module Outputs =
    let getAll t = Collection.getAll t.outputs

    let tryGet t = Collection.tryGet t.outputs
    let put t = Collection.put t.outputs

    let delete t = Collection.delete t.outputs
    let truncate t = Collection.truncate t.outputs

    let contains t = Collection.containsKey t.outputs

module AddressOutputs =
    let put t = MultiCollection.put t.addressOutputs
    let delete t = MultiCollection.delete t.addressOutputs
    let get t session key =
        MultiCollection.get t.addressOutputs session key
        |> List.map (Collection.get t.outputs session)
    let truncate t = MultiCollection.truncate t.addressOutputs

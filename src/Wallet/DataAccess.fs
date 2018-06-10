module Wallet.DataAccess

open Account
open Infrastructure
open DataAccess
open System.Text

open Serialization

let private getBytes str = Encoding.UTF8.GetBytes (str : string)

[<Literal>]
let private MainAccountName = "MAIN"

[<Literal>]
let private SecuredName = "SECURED"

[<Literal>]
let DbVersion = 0

type Collection<'a> = Collection<string, 'a>

type T = {
    accountCollection: Collection<Account.T>
    securedCollection: Collection<Secured.T>
    dbVersion: SingleValue<int>
}

let private createCollection session serializer deserializer name =
    Collection.create session name
        getBytes
        serializer
        deserializer

let createContext dataPath =
    Platform.combine dataPath "wallet"
    |> DatabaseContext.create

let init databaseContext =
    use session = DatabaseContext.createSession databaseContext
    let accountCollection = createCollection session Wallet.serialize Wallet.deserialize "accounts"
    let securedCollection = createCollection session Secured.serialize Secured.deserialize "secured"

    let dbVersion =
        SingleValue.create databaseContext "dbVersion"
            Version.serialize
            Version.deserialize

    match SingleValue.tryGet dbVersion session with
    | Some version when version <> DbVersion ->
        failwithf "Wallet: wrong db version, expected %d but got %d" DbVersion version
    | None ->
        SingleValue.put dbVersion session DbVersion
    | _ -> () // TODO: in the future we should have here db upgrade script

    let t = {
        accountCollection = accountCollection
        securedCollection = securedCollection
        dbVersion = dbVersion
    }

    Session.commit session
    t

let dispose t =
    Disposables.dispose t.accountCollection
    Disposables.dispose t.securedCollection

module Account =
    open Collection

    let put t session =
        put t.accountCollection session MainAccountName

    let tryGet t session =
        tryGet t.accountCollection session MainAccountName

module Secured =
    open Collection

    let put t session =
        put t.securedCollection session SecuredName

    let tryGet t session =
        tryGet t.securedCollection session SecuredName

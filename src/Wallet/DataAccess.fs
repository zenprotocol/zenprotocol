module Wallet.DataAccess

open MBrace.FsPickler
open Account
open Infrastructure
open DataAccess
open System.Text

let private binarySerializer = FsPickler.CreateBinarySerializer()
let private getBytes str = Encoding.UTF8.GetBytes (str : string)

[<Literal>]
let private MainAccount = "MAIN"

[<Literal>]
let private Secured = "SECURED"

[<Literal>]
let DbVersion = 0

type Collection<'a> = Collection<string, 'a>

type T = {
    accountCollection: Collection<Account.T>
    securedCollection: Collection<Secured.T>
    dbVersion: SingleValue<int>
}

let private createCollection<'a> session name =
    Collection.create session name
        getBytes
        binarySerializer.Pickle<'a>
        binarySerializer.UnPickle<'a>

let createContext dataPath =
    Platform.combine dataPath "wallet"
    |> DatabaseContext.create

let init databaseContext =
    use session = DatabaseContext.createSession databaseContext
    let accountCollection = createCollection<Account.T> session "accounts"
    let securedCollection = createCollection<Secured.T> session "secured"

    let dbVersion =
        SingleValue.create databaseContext "dbVersion"
            binarySerializer.Pickle<int>
            binarySerializer.UnPickle<int>

    match SingleValue.tryGet dbVersion session with
    | Some version when version <> DbVersion ->
        failwithf "Wallet: wrong db version, expected %d but got %d" DbVersion version
    | None ->
        SingleValue.put dbVersion session DbVersion
    | _ -> () // TODO: in the future we should have here db upgrade script

    let t = {
        accountCollection = accountCollection
        securedCollection = securedCollection
        dbVersion =dbVersion
    }

    Session.commit session
    t

let dispose t =
    Disposables.dispose t.accountCollection
    Disposables.dispose t.securedCollection

module Account =
    open Collection

    let put t session =
        put t.accountCollection session MainAccount

    let tryGet t session =
        tryGet t.accountCollection session MainAccount

module Secured =
    open Collection

    let put t session =
        put t.securedCollection session Secured

    let tryGet t session =
        tryGet t.securedCollection session Secured

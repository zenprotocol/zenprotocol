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

type Collection<'a> = Collection<string, 'a>

type T = {
    accountCollection: Collection<Account.T>
    securedCollection: Collection<Secured.T>
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

    let t = {
        accountCollection = accountCollection
        securedCollection = securedCollection
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

    let delete t session =
        delete t.accountCollection session MainAccount

module Secured =
    open Collection

    let put t session =
        put t.securedCollection session Secured

    let tryGet t session =
        tryGet t.securedCollection session Secured

    let delete t session =
        delete t.securedCollection session Secured
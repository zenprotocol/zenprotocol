module Network.AddressBook

open DataAccess
open MBrace.FsPickler
open Infrastructure
open Infrastructure.Timestamp

let binarySerializer = FsPickler.CreateBinarySerializer()

[<Literal>]
let DbVersion = 1ul

type T = {
    databaseContext: DatabaseContext
    dbVersion: SingleValue<uint32>
    addresses:Collection<string, string*Timestamp>
}

let create dataPath =
    let databaseContext =
        Platform.combine dataPath "addressbook"
        |> DatabaseContext.create DatabaseContext.Small

    use session = DatabaseContext.createSession databaseContext

    let addresses = Collection.create session "addresses"
                        (fun (address:string) ->System.Text.Encoding.ASCII.GetBytes address)
                        binarySerializer.Pickle<string*Timestamp>
                        (fun bytes -> try binarySerializer.UnPickle<string*Timestamp> bytes |> Some with _ -> None)

    let dbVersion =
        SingleValue.create databaseContext "dbVersion"
            BigEndianBitConverter.uint32ToBytes
            (BigEndianBitConverter.toUint32 >> Some)

    match SingleValue.tryGet dbVersion session with
    | Some version when version <> DbVersion ->
        failwithf "Wallet: wrong db version, expected %d but got %d" DbVersion version
    | None ->
        SingleValue.put dbVersion session DbVersion
    | _ -> () // TODO: in the future we should have here db upgrade script

    Session.commit session

    {
        databaseContext=databaseContext
        dbVersion=dbVersion
        addresses=addresses
    }

let ValidAddressTimespan = Second * 60UL * 60UL * 3UL // 3 hours
let MaxSeenTimeTimestamp = Second * 60UL * 60UL * 24UL * 3UL // Three days

// TODO: only return addresses from the last 3 hours?
let getValidAddresses (now:Timestamp) (book:T) =
    use session = DatabaseContext.createSession book.databaseContext

    Collection.getAll book.addresses session
    |> List.filter (fun (_,timestamp) -> timestamp > now || now - timestamp <= ValidAddressTimespan)
    |> List.shuffle
    |> List.truncate 1000

let private addInternal now address timestamp session (book:T) =
    let timestamp = if timestamp > now then now else timestamp

    match Collection.tryGet book.addresses session address with
    | None ->
        Collection.put book.addresses session address (address,timestamp)
    | Some (_,timestamp') when timestamp > timestamp' ->
        Collection.put book.addresses session address (address,timestamp)
    | _ -> ()

let add now address timestamp book =
    use session = DatabaseContext.createSession book.databaseContext

    addInternal now address timestamp session book

    Session.commit session

let addList now list book =
    use session = DatabaseContext.createSession book.databaseContext

    List.iter (fun (address,timestamp) -> addInternal now address timestamp session book) list

    Session.commit session

let contains address book =
    use session = DatabaseContext.createSession book.databaseContext

    Collection.containsKey book.addresses session address

let haveEnoughAddresses (_:T) =
    // TODO: check if we need more addresses
    false

let take now exclude length seeds book =
    use session = DatabaseContext.createSession book.databaseContext

    Collection.getAll book.addresses session
    |> List.filter (fun (_,timestamp) -> now - timestamp <= MaxSeenTimeTimestamp) 
    |> List.map fst
    |> List.append seeds
    |> List.distinct
    |> List.shuffle
    |> List.filter (fun address -> not <| Set.contains address exclude)
    |> List.truncate length


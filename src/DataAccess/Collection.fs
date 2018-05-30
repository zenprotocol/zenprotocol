[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.Collection

open System
open Lmdb
open System.Runtime.InteropServices

let create (session:Session) name keySerializer valueSerializer valueDeseralizer =
    let mutable db = 0ul

    mdb_dbi_open(session.tx, name, MDB_CREATE, &db)
    |> checkErrorCode

    {
        name=name
        environment=session.env
        database=db;
        keySerializer=keySerializer
        valueSerializer=valueSerializer
        valueDeseralizer=valueDeseralizer
        indices=[]
    }

let addIndex index collection =
    let indexFunc (session:Session) key value =
        let key,value = index.getIndexKeys session key value
        let key = index.indexKeySerializer key
        let value = collection.keySerializer value

        use pinnedKey = pin key
        use pinnedValue = pin value

        let mutable keyData = byteArrayToData pinnedKey
        let mutable valueData = byteArrayToData pinnedValue

        let result =
            mdb_put(session.tx, index.database, &keyData, &valueData, MDB_NODUPDATA)

        if result <> 0 && result <> MDB_KEYEXIST then
            errorToString result
            |> failwith

    let indices = indexFunc :: collection.indices
    {collection with indices=indices}

let tryGet collection (session:Session) key =
    let keyBytes = collection.keySerializer key
    use pinnedKey = pin keyBytes

    let mutable keyData = byteArrayToData pinnedKey
    let mutable valueData = Data.empty

    let result = mdb_get (session.tx,collection.database,&keyData,&valueData)

    if result = 0 then
        dataToByteArray valueData
        |> collection.valueDeseralizer
    elif result = MDB_NOTFOUND then
        None
    else
        errorToString result |> failwith

let getAll (collection:Collection<'a,'b>) (session:Session) =
    let mutable cursor = IntPtr.Zero

    mdb_cursor_open (session.tx, collection.database, &cursor) |> checkErrorCode

    let mutable keyData = Data.empty
    let mutable valueData = Data.empty

    let moveTo = mdb_cursor_get (cursor, &keyData, &valueData,CursorOperation.First) = 0

    // TODO: change to sequence
    if moveTo then
        let rec getNext values =
            mdb_cursor_get (cursor, &keyData, &valueData, CursorOperation.GetCurrent) |> checkErrorCode

            let bytes = dataToByteArray valueData
            let value = collection.valueDeseralizer bytes
                        |> Option.get

            let moveNext = mdb_cursor_get (cursor, &keyData, &valueData, CursorOperation.Next) = 0

            let values = value :: values

            if moveNext then
                getNext values
            else
                values

        getNext [] |> List.rev
    else
        []

let get collection (session:Session) key =
    match tryGet collection session key with
    | Some value -> value
    | None -> failwithf "key was not found in the collection %s %A" collection.name key

let put collection session key value =
    let keyBytes = collection.keySerializer key
    let valueBytes = collection.valueSerializer value

    use pinnedKey = pin keyBytes
    use pinnedValue = pin valueBytes

    let mutable keyData = byteArrayToData pinnedKey
    let mutable valueData = byteArrayToData pinnedValue
    mdb_put(session.tx, collection.database, &keyData, &valueData, 0ul)
    |> checkErrorCode

    List.iter (fun indexFunc -> indexFunc session key value) collection.indices

let delete collection session key =
    let keyBytes = collection.keySerializer key
    use pinnedKey = pin keyBytes
    let mutable keyData = byteArrayToData pinnedKey
    let mutable valueData = Data.empty

    let result = mdb_del(session.tx, collection.database, &keyData, &valueData)
    
    if result = 0 || result = MDB_NOTFOUND then
        //TODO: at the moment delete is not supported with indices, so
        // we throw when collection has indices
        if not <| List.isEmpty collection.indices then
            failwith "delete doesn't work with indices"
    else
        errorToString result |> failwith

let truncate (collection:Collection<'a,'b>) session =
    mdb_drop(session.tx, collection.database,0)
    |> checkErrorCode

let containsKey collection (session:Session) key =
    Option.isSome <| tryGet collection session key
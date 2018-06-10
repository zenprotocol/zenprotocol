module DataAccess.MultiCollection

open System
open Lmdb
open System.Runtime.InteropServices

let create (session:Session) name keySerializer valueSerializer valueDeseralizer =
    let mutable db = 0ul
        
    mdb_dbi_open(session.tx, name, MDB_CREATE ||| MDB_DUPSORT, &db)
    |> checkErrorCode
        
    {
        environment=session.env
        database=db;        
        keySerializer=keySerializer
        valueSerializer=valueSerializer
        valueDeseralizer=valueDeseralizer
        indices=[]
    }
    |> MultiCollection
    
let put (MultiCollection collection) session key value =
    let keyBytes = collection.keySerializer key
    let valueBytes = collection.valueSerializer value
    
    use pinnedKey = pin keyBytes
    use pinnedValue = pin valueBytes      
    
    let mutable keyData = byteArrayToData pinnedKey
    let mutable valueData = byteArrayToData pinnedValue
    let result = mdb_put(session.tx, collection.database, &keyData, &valueData, MDB_NODUPDATA)
    
    if result <> 0 && result <> MDB_KEYEXIST then
        errorToString result 
        |> failwith             
    
let delete (MultiCollection collection) session key value =
    let keyBytes = collection.keySerializer key
    let valueBytes = collection.valueSerializer value
    
    use pinnedKey = pin keyBytes
    use pinnedValue = pin valueBytes      
    
    let mutable keyData = byteArrayToData pinnedKey
    let mutable valueData = byteArrayToData pinnedValue
    mdb_del(session.tx, collection.database, &keyData, &valueData)
    |> checkErrorCode    
    
let get (MultiCollection collection) session key = 
    let mutable cursor = IntPtr.Zero      
        
    mdb_cursor_open (session.tx, collection.database, &cursor) |> checkErrorCode
    
    let keyBytes = collection.keySerializer key
    use pinnedKey = pin keyBytes
    
    let mutable keyData = byteArrayToData pinnedKey   
    let mutable valueData = Data.empty
    
    let moveTo = mdb_cursor_get (cursor, &keyData, &valueData,CursorOperation.Set) = 0

    if moveTo then        
        let rec getNext values =                    
            mdb_cursor_get (cursor, &keyData, &valueData, CursorOperation.GetCurrent) |> checkErrorCode
            
            let bytes = dataToByteArray valueData            
            let value = collection.valueDeseralizer bytes
                        |> Option.get
                        
            let moveNext = mdb_cursor_get (cursor, &keyData, &valueData, CursorOperation.NextDuplicate) = 0
            
            let values = value :: values
            
            if moveNext then                 
                getNext values
            else
                values                          
        
        getNext [] |> List.rev                              
    else
        []

            
            
            

            
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.Index 
open DataAccess

open Lmdb
open System

let create (session:Session) collection name indexKeySize indexKeySerializer getIndexKeys =
    let mutable db = 0ul
            
    mdb_dbi_open(session.tx, name, MDB_CREATE ||| MDB_DUPSORT ||| MDB_DUPFIXED, &db)
    |> checkErrorCode 

    {
        collection = collection
        database = db
        environment = session.env
        getIndexKeys = getIndexKeys
        indexKeySerializer = indexKeySerializer
        indexKeySize = indexKeySize
    }
    
let getAll index (session:Session) key =
    let parseKeys bytes = 
        seq {
            let keys = 
                bytes           
                |> Array.chunkBySize index.indexKeySize
                
            for key in keys do
                use pinnedKey = pin key
                
                let mutable keyData = byteArrayToData pinnedKey 
                let mutable valueData = Data.empty
                           
                let result = mdb_get (session.tx,index.collection.database, &keyData,&valueData)
                            
                if result = 0 then
                    let value = 
                        dataToByteArray valueData
                        |> index.collection.valueDeseralizer
                                       
                    yield value                
        }
        
    let mutable cursor = IntPtr.Zero      
    
    mdb_cursor_open (session.tx, index.database, &cursor) |> checkErrorCode
    
    let key = index.indexKeySerializer key
    use pinnedKey = pin key
    
    let mutable keyData = byteArrayToData pinnedKey   
    let mutable valueData = Data.empty
    
    let moveTo = mdb_cursor_get (cursor, &keyData, &valueData,CursorOperation.Set) = 0
                    
    if moveTo then                
        seq {
            let mutable moveNext = mdb_cursor_get (cursor, &keyData, &valueData, CursorOperation.GetMultiple) = 0
            
            while moveNext do
                let bytes = dataToByteArray valueData
                yield! parseKeys bytes
                    
                moveNext <- mdb_cursor_get (cursor, &keyData, &valueData, CursorOperation.NextMultiple) = 0
                                                                                                        
            mdb_cursor_close cursor
        }         
    else 
        Seq.empty
    
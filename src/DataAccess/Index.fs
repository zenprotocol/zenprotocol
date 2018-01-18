[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.Index 

open LightningDB

let create (session:Session) collection name indexKeySize indexKeySerializer getIndexKeys =
    let databaseConfiguration = new DatabaseConfiguration()
    databaseConfiguration.Flags <- 
        DatabaseOpenFlags.Create ||| DatabaseOpenFlags.DuplicatesFixed ||| DatabaseOpenFlags.DuplicatesSort
 
    let db = session.OpenDatabase (name, databaseConfiguration)
     
    {
        collection = collection
        database = db
        getIndexKeys = getIndexKeys
        indexKeySerializer = indexKeySerializer
        indexKeySize = indexKeySize
    }
    
let getAll index (session:Session) key =
    let parseKeys (cursor:LightningCursor) = 
        seq {
            let keys = 
                cursor.Current.Value            
                |> Array.chunkBySize index.indexKeySize
                
            for key in keys do
                let isExist,value = session.TryGet (index.collection.database, key)
                if isExist then 
                    yield index.collection.valueDeseralizer value                
        }
      
    let key = index.indexKeySerializer key
    
    let cursor = session.CreateCursor index.database
        
    if cursor.MoveTo key && cursor.GetMultiple () then
        seq {                            
            yield! parseKeys cursor
                
            while cursor.MoveNextMultiple () do 
                yield! parseKeys cursor  
                
            cursor.Dispose()                                                                     
        }         
    else 
        Seq.empty
    
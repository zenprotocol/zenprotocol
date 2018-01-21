[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.Collection

open LightningDB

let create (session:Session) name keySerializer valueSerializer valueDeseralizer =
    let databaseConfiguration = new DatabaseConfiguration()
    databaseConfiguration.Flags <- DatabaseOpenFlags.Create
 
    let db = session.OpenDatabase (name, databaseConfiguration)  
      
    {
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
               
        // code thrown exception if key/value already exist
        // so we catch and ignore
        // the other option is to open a cursor and check if the key/value already exist
        // which will be more expensive.
        // We should send a PR to the Lightning project without an exception, maybe TryPut
        try 
            session.Put(index.database, key, value, PutOptions.NoDuplicateData)
        with 
        | :? LightningException as ex -> 
            if ex.StatusCode <> -30799 then
                raise ex
 
    let indices = indexFunc :: collection.indices
    {collection with indices=indices}
            
let tryGet collection (session:Session) key =
    let key = collection.keySerializer key 
    let isExist,value = session.TryGet (collection.database, key)
    
    if isExist then 
        Some <| collection.valueDeseralizer value
    else 
        None 
        
let get collection (session:Session) key = 
    match tryGet collection session key with
    | Some value -> value
    | None -> failwith "key was not found in the collection"  
        
let put collection (session:Session) key value =
    let keyBytes = collection.keySerializer key
    let valueBytes = collection.valueSerializer value
    
    session.Put(collection.database, keyBytes, valueBytes)
    
    List.iter (fun indexFunc -> indexFunc session key value) collection.indices         
    
let containsKey collection (session:Session) key = 
    let keyBytes = collection.keySerializer key

    session.ContainsKey (collection.database,keyBytes)        
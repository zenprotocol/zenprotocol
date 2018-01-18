namespace DataAccess

open LightningDB

type DatabaseContext = 
    {
        environment: LightningEnvironment
        values: LightningDatabase
    }
    interface System.IDisposable with   
        member x.Dispose () =
            x.values.Dispose()
            x.environment.Dispose()

type Session = LightningTransaction

type Collection<'key,'value> = 
    {
        database: LightningDatabase
        keySerializer: 'key->byte[]
        valueSerializer: 'value->byte[]
        valueDeseralizer: byte[]->'value
        indices: (Session->'key->'value->unit) list
    }
    interface System.IDisposable with   
        member x.Dispose () =
            x.database.Dispose()
                
type Index<'key,'value,'indexKey> = 
    {
        collection: Collection<'key,'value>
        database: LightningDatabase
        getIndexKeys: Session->'key->'value-> ('indexKey*'key) 
        indexKeySerializer: 'indexKey -> byte[]
        indexKeySize: int
    }
    interface System.IDisposable with   
        member x.Dispose () =
            x.database.Dispose()  
            
type SingleValue<'value> = 
    {
        collection: LightningDatabase
        name:byte[]
        serializer: 'value->byte[]
        deserializer: byte[]->'value
    }  
                        
            



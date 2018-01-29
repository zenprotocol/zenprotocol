namespace DataAccess

open System
open System.Runtime.InteropServices

open Lmdb

type Session = 
    {
        mutable tx:IntPtr
        env:IntPtr
    }
    interface System.IDisposable with   
        member x.Dispose () =  
            if x.tx <> IntPtr.Zero then          
                mdb_txn_abort(x.tx)
                x.tx <- IntPtr.Zero      
      
type Collection<'key,'value> = 
    {
        environment: IntPtr
        database: uint32        
        keySerializer: 'key->byte[]
        valueSerializer: 'value->byte[]
        valueDeseralizer: byte[]->'value
        indices: (Session->'key->'value->unit) list
    }
    interface System.IDisposable with   
        member x.Dispose () =            
            mdb_dbi_close(x.environment,x.database)
            
type MultiCollection<'key,'value> = MultiCollection of Collection<'key,'value>            
            
type DatabaseContext = 
    {
        environment: IntPtr
        values: Collection<byte[],byte[]>
    }
    interface System.IDisposable with   
        member x.Dispose () =
            (x.values :> IDisposable).Dispose()
            mdb_env_close(x.environment)
                
type Index<'key,'value,'indexKey> = 
    {
        collection: Collection<'key,'value>
        database: uint32
        environment: IntPtr
        getIndexKeys: Session->'key->'value-> ('indexKey*'key) 
        indexKeySerializer: 'indexKey -> byte[]
        indexKeySize: int
    }
    interface System.IDisposable with   
        member x.Dispose () =
            mdb_dbi_close(x.environment,x.database)
           
type SingleValue<'value> = 
    {
        collection: Collection<byte[],byte[]>
        name:byte[]
        serializer: 'value->byte[]
        deserializer: byte[]->'value
    }  
                        
            



[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.DatabaseContext

open System
open System.IO
open System.Text
open Lmdb

let createSession (context:DatabaseContext) : Session =
    let mutable tx = IntPtr.Zero  
 
    mdb_txn_begin(context.environment,IntPtr.Zero,0ul,&tx)
    |> checkErrorCode
    
    {tx=tx;env=context.environment}

// Create child session
let createChildSession (parent:Session) : Session =
    let mutable tx = IntPtr.Zero

    mdb_txn_begin(parent.env, parent.tx, 0ul, &tx)
    |> checkErrorCode

    {tx=tx; env=parent.env}

let create pathToFolder : DatabaseContext =             
    let mutable environment = IntPtr.Zero       
            
    let fileInfo = FileInfo(pathToFolder)             
            
    if not fileInfo.Directory.Exists then
        fileInfo.Directory.Create ()
            
    mdb_env_create(&environment)
    |> checkErrorCode        
    
    mdb_env_set_maxdbs(environment, 20ul)
    |> checkErrorCode
    
    mdb_env_open(environment,pathToFolder,MDB_NOSUBDIR,MDB_DEFAULT_MODE)
    |> checkErrorCode
   
    let mutable tx = IntPtr.Zero  
    
    mdb_txn_begin(environment,IntPtr.Zero,0ul,&tx)
    |> checkErrorCode
    
    let db = Collection.create {tx=tx;env=environment} "values" id id id                    
    
    mdb_txn_commit(tx)
    |> checkErrorCode
    
    {
        environment = environment
        values=db
    }
        
// Delete data folder before creating the database
let createEmpty pathToFolder : DatabaseContext =
    if Directory.Exists pathToFolder then 
        Directory.Delete (pathToFolder,true)
    create pathToFolder
        

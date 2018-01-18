[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.DatabaseContext

open System.IO
open LightningDB

let create pathToFolder : DatabaseContext =             
    let context = new LightningEnvironment(pathToFolder)
    context.MaxDatabases <- 100
    context.Open ()
    
    use session = context.BeginTransaction()
    
    let databaseConfiguration = new DatabaseConfiguration()
    databaseConfiguration.Flags <- DatabaseOpenFlags.Create
    
    let values = session.OpenDatabase("values", databaseConfiguration)
    
    session.Commit ()
    
    {
        environment = context
        values=values
    }
        

// Delete data folder before creating the database
let createEmpty pathToFolder : DatabaseContext =
    if Directory.Exists pathToFolder then 
        Directory.Delete (pathToFolder,true)
    create pathToFolder
        
let createSession (context:DatabaseContext) : Session = 
    context.environment.BeginTransaction()    
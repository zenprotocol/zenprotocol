[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.Session

open LightningDB

let commit (session:Session) = 
    session.Commit ()
    

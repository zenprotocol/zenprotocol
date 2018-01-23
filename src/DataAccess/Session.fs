[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module DataAccess.Session

open DataAccess
open System

open Lmdb

let commit (session:Session) =
    let tx = session.tx
    session.tx <- IntPtr.Zero
    
    mdb_txn_commit tx
    |> checkErrorCode

module DataAccess.Lmdb

open System
open System.Runtime.InteropServices

let MDB_DEFAULT_MODE = 438ul
let MDB_NOSUBDIR = 0x4000ul

let MDB_CREATE  = 0x40000ul
let MDB_DUPSORT = 0x04ul
let MDB_DUPFIXED =  0x10ul

let MDB_NODUPDATA = 0x20ul


let MDB_KEYEXIST = -30799  

let MDB_NOTFOUND = -30798

type CursorOperation = 
    | First = 0
    | FirstDuplicate = 1
    | GetBoth = 2
    | GetBothRange = 3
    | GetCurrent=4
    | GetMultiple=5
    | Last=6
    | LastDuplicate=7
    | Next=8
    | NextDuplicate=9
    | NextMultiple=10
    | NextNoDuplicate=11
    | Previous=12
    | PreviousDuplicate=13
    | PreviousNoDuplicate=14
    | Set=15
    | SetKey=16
    | SetRange=17    

[<Struct;StructLayout(LayoutKind.Sequential)>]
type Data =
    {
        size:uint64
        data:IntPtr
    }

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_env_create(IntPtr& env)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern void mdb_env_close(IntPtr env)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_env_open(IntPtr env, string path, uint32 flags, uint32 mode)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_env_set_maxdbs(IntPtr env, uint32 dbs)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_env_set_mapsize(IntPtr env, IntPtr size)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_dbi_open(IntPtr txn, string name, uint32 flags, uint32& db)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern void mdb_dbi_close(IntPtr env, uint32 dbi)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_txn_begin(IntPtr env, IntPtr parent, uint32 flags, IntPtr& txn)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_txn_commit(IntPtr txn)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern void mdb_txn_abort(IntPtr txn)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_get(IntPtr txn, uint32 dbi, Data& key, Data& data)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_put(IntPtr txn, uint32 dbi, Data& key, Data& data, uint32 flags)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_del(IntPtr txn, uint32 dbi, Data& key, Data& value);

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_cursor_open(IntPtr txn, uint32 dbi, IntPtr& cursor)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern void mdb_cursor_close(IntPtr cursor)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern int mdb_cursor_get(IntPtr cursor, Data& key, Data& data, CursorOperation op)

[<DllImport("lmdb", CallingConvention = CallingConvention.Cdecl)>]
extern IntPtr mdb_strerror(int err);

let errorToString err = 
    let handle = mdb_strerror err
    
    Marshal.PtrToStringAnsi (handle)

let checkErrorCode result = 
    if result <> 0 then
        failwith (errorToString result)     
        
type PinnedByteArray =
    | PinnedByteArray of GCHandle*byte[]
    interface System.IDisposable with   
        member x.Dispose () =            
            let (PinnedByteArray (handle,_)) = x            
            handle.Free()           
        
let dataToByteArray (value:Data) =
    let count = int32 value.size 
    let bytes = Array.zeroCreate<byte> count
    
    Marshal.Copy (value.data,bytes,0,count)
    
    bytes
    
let byteArrayToData (PinnedByteArray (_, bytes)) =
    let address = Marshal.UnsafeAddrOfPinnedArrayElement (bytes,0)
 
    {size=Array.length bytes |> uint64; data=address}  

let pin data = 
    let handle = GCHandle.Alloc (data,GCHandleType.Pinned)
    
    PinnedByteArray (handle,data)                  

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]   
module Data =
    let empty = {size=0UL;data=IntPtr.Zero}        
            

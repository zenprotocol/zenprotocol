module Infrastructure.Disposables

let empty = 
    { new System.IDisposable with
        member x.Dispose() = () }
        
let toDisposable (x:System.IDisposable) = x 

let dispose (x:System.IDisposable) = x.Dispose ()    

let fromFunction f = 
     { new System.IDisposable with
            member x.Dispose() = f() }
            
let fromList list = 
    fromFunction (fun () -> List.iter dispose list)                         
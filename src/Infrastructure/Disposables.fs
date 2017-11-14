module Infrastructure.Disposables

let empty = 
    { new System.IDisposable with
        member x.Dispose() = () }
        
let toDisposable (x:System.IDisposable) = x 
            
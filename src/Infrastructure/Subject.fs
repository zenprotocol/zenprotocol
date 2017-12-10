module Infrastructure.Subject

open Infrastructure

type T<'a>() = 
    let observers = new System.Collections.Generic.List<System.IObserver<'a>>() 
    
    member x.Next (value: 'a) =
        observers.ForEach (fun observer -> observer.OnNext(value))
    
    interface System.IObservable<'a> with
        member x.Subscribe (observer : System.IObserver<'a>) = 
            observers.Add (observer)
            
            Disposables.fromFunction (fun () -> observers.Remove (observer) |> ignore)
            
let create<'a> () = new T<'a>()

let next<'a> (subject:T<'a>) (value:'a) = 
    subject.Next (value)
    
let observable (subject:T<'a>) = subject :> System.IObservable<'a> 
module Network.DiscoverIP

open System.Net
open FsNetMQ
open Infrastructure
open FSharp.Data

type T = 
    {
        inproc: Socket.T;
        actor: Actor.T;
    }
    interface System.IDisposable with
        member x.Dispose() = 
           Disposables.dispose x.actor
           Disposables.dispose x.inproc 
 
let create () : T =     
    let user,inproc = Pair.createPairs ()

    let actor = FsNetMQ.Actor.create (fun shim ->        
        let rec loop () = 
            match SingleFrame.tryRecv shim 5000<milliseconds> with
            | Some _ -> 
                // exit loop
                ()
            | None ->
                try 
                    let ip = Http.RequestString ("https://api.ipify.org/",timeout = 1000)
                    let valid,_ = IPAddress.TryParse ip
                    
                    if valid then
                        Log.info "External IP address %s found" ip
                        System.Text.Encoding.ASCII.GetBytes(ip) 
                        |> Frame.send inproc 
                with 
                | _ -> loop ()
                
        Actor.signal shim                
        loop ()
        Disposables.dispose inproc   
    )
        
    {actor=actor;inproc = user}  
        
let addToPoller poller {inproc=inproc} = 
    Poller.addSocket poller inproc
    |> Observable.map (fun _ ->         
        SingleFrame.recv inproc
        |>System.Text.Encoding.ASCII.GetString)
        
        
        
    
        
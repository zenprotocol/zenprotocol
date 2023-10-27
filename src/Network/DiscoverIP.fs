module Network.DiscoverIP

open System.Net
open FsNetMQ
open Infrastructure
open FSharp.Data
open Logary.Message

let maxRetries = 5

let rec getExternalIP retries =
    if retries > maxRetries then None
    else
        try 
            let ip = Http.RequestString ("https://api.ipify.org/",timeout = 1000)
            let valid,_ = IPAddress.TryParse ip
            if valid then Some ip else None
        with
        | _ -> 
            System.Threading.Thread.Sleep(retries * 1000)
            getExternalIP (retries + 1)

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
                match getExternalIP 0 with
                | Some ip ->
                    eventX "External IP address {ip} found"
                    >> setField "ip" ip
                    |> Log.info
                    System.Text.Encoding.ASCII.GetBytes(ip) 
                    |> Frame.send inproc 
                | None -> loop ()
                
        Actor.signal shim                
        loop ()
        Disposables.dispose inproc   
    )
        
    {actor=actor;inproc = user}  
        
let addToPoller poller {inproc=inproc} = 
    Poller.addSocket poller inproc
    |> Observable.map (fun _ ->         
        try
            SingleFrame.recv inproc
            |>System.Text.Encoding.ASCII.GetString
        with
        | ex -> 
            eventX "Exception occurred while converting bytes to string"
            >> setField "exception" ex.Message
            |> Log.error
            "")
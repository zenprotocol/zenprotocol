module Network.Tests.PeersManagerHelper

open Network
open FsNetMQ

[<Literal>]
let ActivePeersCommand = 1uy

let startPeer listener = 
    Actor.create (fun shim -> 
        use poller = Poller.create ()
        
        let shimObservable = 
            Poller.addSocket poller shim
            |> Observable.map ( fun _ ->
                let message = SingleFrame.recv shim
                match message.[0] with
                | ActivePeersCommand -> fun m ->
                    let activePeers = PeersManager.countActivePeers m
                    Frame.send shim (System.BitConverter.GetBytes (activePeers)) 
                    m
                | _ -> 
                    Poller.stop poller
                    id)        
    
        use manager = 
            if listener then 
                PeersManager.create poller true "127.0.0.1:5555" []
            else 
                PeersManager.create poller false "" ["127.0.0.1:5555"]
                
        use observer = 
            PeersManager.observable manager
            |> Observable.merge shimObservable
            |> Observable.scan (fun state f -> f state) manager 
            |> Observable.subscribe (ignore)                 
    
        Actor.signal shim
        Poller.run poller        
    )
    
let stopPeer peer = 
    (peer :> System.IDisposable).Dispose()
    
let getActivePeers peer = 
    Frame.send (Actor.asSocket peer) (Array.create 1 ActivePeersCommand)
    let bytes = SingleFrame.recv (Actor.asSocket peer)
    
    System.BitConverter.ToInt32 (bytes, 0)   
module PeersManagerTests

open Xunit
open FsUnit.Xunit
open FsNetMQ
open Network

[<Fact>]
let ``Two peers connect to each other``() =
    use poller = Poller.create ()        

    use manager1 = PeersManager.create poller true "127.0.0.1:5555" []
    use manager2 = PeersManager.create poller false "" ["127.0.0.1:5555"]
    
    let timer = Timer.create 100<milliseconds>
    
    let timerObservable = 
        Poller.addTimer poller timer
        |> Observable.map (fun _ ->
            fun (m1,m2) ->  
                Poller.stop poller
                
                PeersManager.countActivePeers m1 |> should equal 1
                PeersManager.countActivePeers m2 |> should equal 1
                
                printf "%d %d" (PeersManager.countActivePeers m1) (PeersManager.countActivePeers m2)
                
                
                m1,m2
            )
    
    let observable1 =
        PeersManager.observable manager1 
        |> Observable.map (fun f -> fun (m1,m2) -> (f m1), m2)
            
    let observable2 =
        PeersManager.observable manager2
        |> Observable.map (fun f -> fun (m1,m2) -> m1,(f m2))
        
    use observer = 
        Observable.merge observable1 observable2
        |> Observable.merge timerObservable
        |> Observable.scan (fun state f -> f state) (manager1,manager2)
        |> FSharp.Control.Reactive.Observable.subscribe (ignore)
                
    Poller.run poller
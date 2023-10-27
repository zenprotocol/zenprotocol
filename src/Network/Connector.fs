module Network.Connector

open Network
open Network.Transport
open System
open Infrastructure
open Logary.Message

type ConnectionStatus =
    | Connected
    | Connecting of attempts:int
    | Failed of attempts:int
    | Suspended of DateTime

type T = {
    seeds: List<string>
    connections: Map<string,ConnectionStatus>
    maxConnections: int
}

[<LiteralAttribute>]
let MaxAttempts = 5

let create seeds maxConnection =
    {maxConnections=maxConnection; connections = Map.empty;seeds=Seq.toList seeds}

let connected connector address =
    let connections = Map.add address Connected connector.connections

    {connector with connections = connections}

let disconnected connector address =

    let connections =
        match Map.tryFind address connector.connections with
        | Some Connected ->
            eventX "Disconnected from {address}"
            >> setField "address" address
            |> Log.info
            Map.remove address connector.connections
        | Some (Connecting attempts) ->
            if attempts >= MaxAttempts then
                eventX "Address {address} was suspended due to too many failed attempts"
                >> setField "address" address
                |> Log.debug
                Map.add address (Suspended DateTime.UtcNow) connector.connections
            else
                if List.contains address connector.seeds then
                    Map.add address (Failed 0) connector.connections // We never suspend seeds
                else
                    Map.add address (Failed attempts) connector.connections
        | _ -> failwith "Expecting connected of connecting address"

    {connector with connections = connections;}

let private countConnectingOrConnected connector =
    Map.filter (fun _ value ->
        match value with
        | Connecting _
        | Connected -> true
        | _ -> false) connector.connections
    |> Map.count

let countConnected connector =
    Map.filter (fun _ value ->
            match value with
            | Connected -> true
            | _ -> false) connector.connections
    |> Map.count

let connect transport addressBook now connector =

    let connectInternal transport addressBook connector exclude =
        let connectionsToOpen = connector.maxConnections - (countConnectingOrConnected connector)

        let addressesToExclude = exclude |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        let addresses = AddressBook.take now addressesToExclude connectionsToOpen connector.seeds addressBook

        // Connect to new addresses
        Seq.iter (Transport.connect transport) addresses

        // Add the new addresses to the connections set
        let connections = Seq.fold (fun connections address ->
            match Map.tryFind address connections with
            | Some (Failed attempts) ->
                Map.add address (Connecting <| attempts + 1) connections
            | _ ->
                Map.add address (Connecting 0) connections) connector.connections addresses

        {connector with connections = connections}

    // Try first without failed one
    let connector = connectInternal transport addressBook connector connector.connections

    // Filter the excluding list to not include failed, therefore we will try to connect failed as well
    let includeFailing connector =
        Map.filter (fun _ status ->
            match status with
            | Failed _ -> false
            | _ -> true) connector.connections

    connectInternal transport addressBook connector (includeFailing connector)
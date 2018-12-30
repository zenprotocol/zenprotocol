module Wallet.View

open Consensus
open Consensus.Types
open Wallet.DataAccess
open Wallet.Types

type T = Set<Hash.Hash> * Map<Outpoint, Output>

// TODO: add transaction to mempool

let empty = Set.empty, Map.empty

module Outputs =
    let getAll (_,view) dataAccess session =
        let outputs =
            Outputs.getAll dataAccess session
            |> List.map (fun output -> output.outpoint, output)
            |> Map.ofSeq

        Map.fold (fun outputs outpoint output -> Map.add outpoint output outputs) outputs view
        |> Map.toSeq
        |> Seq.map snd
        |> List.ofSeq

    let tryGet view dataAccess session outpoint =
        match Map.tryFind outpoint view with
        | Some output -> Some output
        | None -> Outputs.tryGet dataAccess session outpoint

    let contains view dataAccess session outpoint =
        if Map.containsKey outpoint view then
            true
        else
            Outputs.contains dataAccess session outpoint

module AddressOutputs =
    let get (_,view) dataAcesss session key =
        let outputs =
            AddressOutputs.get dataAcesss session key
            |> List.map (fun output -> output.outpoint, output)
            |> Map.ofSeq

        Map.filter (fun _ output -> output.pkHash = key) view
        |> Map.fold (fun outputs outpoint output -> Map.add outpoint output outputs) outputs
        |> Map.toSeq
        |> Seq.map snd
        |> List.ofSeq

let addMempoolTransaction dataAccess session txHash tx (set, outputs) =
    if Set.contains txHash set then
        (set, outputs)
    else

    let outputs = List.fold (fun view input ->
        match input with
        | Outpoint outpoint ->
            match Outputs.tryGet view dataAccess session outpoint with
            | Some output -> Map.add outpoint {output with status = Spent (txHash,Unconfirmed)} view
            | None -> view
        | _ -> view) outputs tx.inputs

    let outputs =
        tx.outputs
        |> List.mapi (fun index output -> uint32 index,output)
        |> List.choose (fun (index,output) ->
            match output.lock with
            | Coinbase (_,pkHash)
            | Vote (_,_,pkHash)
            | PK pkHash when (DataAccess.Addresses.contains dataAccess session pkHash) ->
                Some (pkHash,index,output)
            | _ -> None)
        |> List.fold (fun view (pkHash,index,output) ->
            let outpoint = {txHash=txHash; index=index}

            let output = {
                pkHash = pkHash
                spend = output.spend
                lock = output.lock
                outpoint = outpoint
                status = Unspent
                confirmationStatus = Unconfirmed
            }

            Map.add outpoint output view) outputs

    Set.add txHash set, outputs

let fromMempool dataAccess session transactions : T =
    List.fold (fun view (txHash,tx) -> addMempoolTransaction dataAccess session txHash tx view) empty transactions
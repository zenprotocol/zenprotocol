module Blockchain.ActiveContractSetRepository

open DataAccess
open DatabaseContext
open Consensus
open ActiveContractSet

let get session =
    let contracts =
        Collection.getAll session.context.activeContractSet session.session
        |> List.map (fun contractKey ->
            match Contract.load session.context.contractPath contractKey.expiry contractKey.code contractKey.contractId with
            | Ok contract -> contract
            | Error error -> failwithf "fail to load contract on startup %A" error)

    ActiveContractSet.load contracts

let save session ((_,changes):ActiveContractSet.T) =
    Map.iter (fun contractId change ->
        let cHash = ContractId.contractHash contractId

        match change with
        | Updated contract -> Collection.put session.context.activeContractSet session.session cHash contract
        | Expired -> Collection.delete session.context.activeContractSet session.session cHash) changes
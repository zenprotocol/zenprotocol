module Consensus.Contract

open Microsoft.FSharp.Core
open System.Reflection
open System.Text
open FsBech32
open Hash
open Types
open Infrastructure
open Zen.Types.Extracted
open FStar.Pervasives
open Zen.Cost.Realized
open Zen.Types.Realized
open Zen.Types.Data
open Zen.Types.Main
open Zen.Types.Main

type ContractWallet = PointedOutput list
type ContractFn = Hash -> string -> sender -> data option -> ContractWallet -> TxSkeleton.T -> Result<(TxSkeleton.T * Message Option),string>
type ContractCostFn = string -> sender -> data option -> ContractWallet -> TxSkeleton.T -> int64

type T = {
    hash: Hash
    fn:   ContractFn
    costFn: ContractCostFn
    expiry: uint32
    code:string
}

let private findMethods (assembly:Assembly) =
    try
        let t = assembly.GetModules().[0].GetTypes().[0]

        let (MainFunc ((CostFunc (_,cost)), main)) = downcast (t.GetProperty("mainFunction").GetValue null)

        Ok (main, cost)
    with _ as ex ->
        Exception.toError "get contract methods" ex

let private wrap (main, cost) =
    (
    fun (Hash.Hash cHash) command (sender:sender) data contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data

        main txSkeleton' cHash command' sender data' contractWallet'
        |> ZFStar.unCost
        |> ZFStar.toResult
        |> Result.bind ZFStar.convertResult
    ,
    fun command (sender:sender) data contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data

        cost txSkeleton' command' sender data' contractWallet'
        |> ZFStar.unCost
    )

let hash contract = contract.hash

let private getModuleName =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let computeHash : string -> Hash = Hash.compute << Encoding.UTF8.GetBytes

let load contractsPath expiry code hash =
    getModuleName hash
    |> ZFStar.load contractsPath
    |> Result.bind findMethods
    |> Result.map wrap
    |> Result.map (fun (mainFn, costFn) ->
        {
            hash = hash
            fn = mainFn
            costFn = costFn
            expiry = expiry
            code = code
        })

let compile contractsPath (contract:Consensus.Types.Contract) expiry =
    let hash = computeHash contract.code

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath contract.code contract.hints contract.rlimit
    |> Result.bind (fun _ -> load contractsPath expiry contract.code hash)

let recordHints code =
    code
    |> computeHash
    |> getModuleName
    |> ZFStar.recordHints code

let getCost contract = contract.costFn

let run contract = contract.fn contract.hash

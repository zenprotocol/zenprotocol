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

type private zfstarMainFn =
    txSkeleton
        -> contractHash
        -> Prims.string
        -> sender
        -> Native.option<data>
        -> wallet
        -> cost<contractResult, Prims.unit>

type private zfstarCostFn =
    txSkeleton
        -> Prims.string
        -> sender
        -> Native.option<data>
        -> wallet
        -> cost<Prims.nat, Prims.unit>

type ContractWallet = PointedOutput list

type ContractMainFn =
    TxSkeleton.T
        -> Hash
        -> string
        -> sender
        -> data option
        -> ContractWallet
        -> Result<(TxSkeleton.T * Message Option),string>

type ContractCostFn =
    TxSkeleton.T
        -> string
        -> sender
        -> data option
        -> ContractWallet
        -> int64

type Contract = {
    hash: Hash
    mainFn: ContractMainFn
    costFn: ContractCostFn
    expiry: uint32
    code:string
}
type T = Contract


let private getMainFunction assembly =
    try
        let getProperty name =
            (assembly:Assembly)
                .GetModules().[0]  // Should get ModuleName.dll
                .GetTypes().[0]    // Should get ModuleName
                .GetProperty(name) // ModuleName.name
        (getProperty "mainFunction").GetValue null
        :?> mainFunction
        |> Ok
    with _ as ex ->
        Exception.toError "get contract mainFunc" ex

let private wrapMainFn (mainFn : zfstarMainFn) : ContractMainFn =
    fun txSkeleton (Hash.Hash cHash) command sender data contractWallet ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption data
        let contractWallet' = ZFStar.convertWallet contractWallet
        mainFn txSkeleton' cHash command' sender data' contractWallet'
        |> ZFStar.unCost
        |> ZFStar.toResult
        |> Result.map ZFStar.convertResult

let private wrapCostFn (costFn: zfstarCostFn) : ContractCostFn =
    fun txSkeleton command sender data contractWallet ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption data
        let contractWallet' = ZFStar.convertWallet contractWallet
        costFn txSkeleton' command' sender data' contractWallet'
        |> ZFStar.unCost

let getHash contract : Hash = contract.hash

let private getModuleName : Hash -> string =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let computeHash : string -> Hash = Hash.compute << Encoding.UTF8.GetBytes

let load contractsPath expiry code hash =
    getModuleName hash
    |> ZFStar.load contractsPath
    |> Result.bind getMainFunction
    |> Result.map (fun (MainFunc (CostFunc (_, costFn), mainFn) : mainFunction) ->
        {
            hash = hash
            mainFn = wrapMainFn mainFn
            costFn = wrapCostFn costFn
            expiry = expiry
            code = code
        })

let compile (contractsPath:string)
            (contract:Consensus.Types.Contract) =
    let hash = computeHash contract.code

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath contract.code contract.hints contract.rlimit
    |> Result.map (fun _ -> hash)

let recordHints (code:string) : Result<string, string> =
    code
    |> computeHash
    |> getModuleName
    |> ZFStar.recordHints code

let getCost contract = contract.costFn

let run contract txSkeleton command sender data wallet =
    contract.mainFn txSkeleton contract.hash command sender data wallet

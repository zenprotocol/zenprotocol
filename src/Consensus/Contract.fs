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
open System
open Zen.Cost.Realized
open Zen.Types.Realized
open Zen.Types.Data
open Zen.Types.Main
open Zen.Types.Main

type private zfstarMainFn =
    txSkeleton
        -> contractId
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
        -> ContractId
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
    contractId:ContractId
    mainFn: ContractMainFn
    costFn: ContractCostFn
    expiry: uint32
    code:string
}
with
    member x.version =
        let (ContractId (version,_)) = x.contractId
        version

    member x.hash =
        let (ContractId (_,cHash)) = x.contractId
        cHash

type T = Contract


let private getMainFunction assembly =
    try
        let getProperty name =
            (assembly:Assembly)
                .GetModules().[0]
                .GetTypes().[0]
                .GetProperty(name)
        (getProperty "mainFunction").GetValue null
        :?> mainFunction
        |> Ok
    with _ as ex ->
        Exception.toError "get contract mainFunc" ex

let private wrapMainFn (mainFn : zfstarMainFn) : ContractMainFn =
    fun txSkeleton (ContractId (version, Hash.Hash cHash)) command sender data contractWallet ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data
        let contractWallet' = ZFStar.convertWallet contractWallet
        mainFn txSkeleton' (version,cHash) command' sender data' contractWallet'
        |> ZFStar.unCost
        |> ZFStar.toResult
        |> Result.bind ZFStar.convertResult

let private wrapCostFn (costFn: zfstarCostFn) : ContractCostFn =
    fun txSkeleton command sender data contractWallet ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data
        let contractWallet' = ZFStar.convertWallet contractWallet
        costFn txSkeleton' command' sender data' contractWallet'
        |> ZFStar.unCost

let private getModuleName : Hash -> string =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let private computeHash version (code:string) =
    let versionBytes = BigEndianBitConverter.uint32ToBytes version
    let codeBytes = Encoding.UTF8.GetBytes code

    Hash.computeMultiple [versionBytes; codeBytes]

let makeContractId version code =
    ContractId (version, computeHash version code)

let load contractsPath expiry code (ContractId (version,hash)) =
    getModuleName hash
    |> ZFStar.load contractsPath
    |> Result.bind getMainFunction
    |> Result.map (fun (MainFunc (CostFunc (_, costFn), mainFn) : mainFunction) ->
        {
            contractId=ContractId (version,hash)
            mainFn = wrapMainFn mainFn
            costFn = wrapCostFn costFn
            expiry = expiry
            code = code
        })

let compile (contractsPath:string)
            (contract:ContractV0) =
    let hash = computeHash Version0 contract.code

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath contract.code contract.hints contract.rlimit
    |> Result.map (fun _ -> ContractId (Version0,hash))

let recordHints (code:string) : Result<string, string> =
    code
    |> computeHash Version0
    |> getModuleName
    |> ZFStar.recordHints code

let getCost contract = contract.costFn

let run contract txSkeleton command sender data wallet =
    contract.mainFn txSkeleton contract.contractId command sender data wallet

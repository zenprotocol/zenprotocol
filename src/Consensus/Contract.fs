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

type ContractWallet = PointedOutput list
type ContractMainFn = TxSkeleton.T -> Hash -> string -> data option -> ContractWallet -> Result<(TxSkeleton.T * Message Option),string>
type ContractCostFn = TxSkeleton.T -> string -> data option -> ContractWallet -> int64

type T = {
    hash: Hash
    mainFn: ContractMainFn
    costFn: ContractCostFn
    expiry: uint32
    size: uint32
    code:string
}

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

let private getCostFn (MainFunc(CostFunc(_, cf), _)) = cf
let private getMainFn (MainFunc (_, mf)) = mf
    
let private wrapMainFn
                (mainFn: 
                    txSkeleton 
                    -> contractHash 
                    -> Prims.string 
                    -> Native.option<data> 
                    -> wallet
                    -> cost<contractResult, Prims.unit>)
                : ContractMainFn =
    fun txSkeleton (Hash.Hash cHash) command data contractWallet ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data
        let contractWallet' = ZFStar.convertWallet contractWallet
        mainFn txSkeleton' cHash command' data' contractWallet'
        |> ZFStar.unCost
        |> ZFStar.toResult
        |> Result.bind ZFStar.convertResult

let private wrapCostFn
                (costFn: 
                    txSkeleton 
                    -> Prims.string 
                    -> Native.option<data> 
                    -> wallet
                    -> cost<Prims.nat, Prims.unit>)
                : ContractCostFn =
    fun txSkeleton command data contractWallet ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data
        let contractWallet' = ZFStar.convertWallet contractWallet
        costFn txSkeleton' command' data' contractWallet'
        |> ZFStar.unCost

let hash contract = contract.hash

let private getModuleName =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let computeHash : string -> Hash = Hash.compute << Encoding.UTF8.GetBytes

let load contractsPath expiry size code hash =
    let mainFunc = getModuleName hash
                   |> ZFStar.load contractsPath
                   |> Result.bind getMainFunction
    let mainFn = mainFunc 
                 |> Result.map getMainFn
                 |> Result.map wrapMainFn
    let costFn = mainFunc 
                 |> Result.map getCostFn
                 |> Result.map wrapCostFn
    
    mainFn |> Result.bind (fun mainFn ->
    costFn |> Result.map (fun costFn ->
        {
            hash = hash
            mainFn = mainFn
            costFn = costFn
            expiry = expiry
            size = size //TODO: remove, use: String.length code |> uint32
            code = code
        }))

let compile contractsPath (contract:Consensus.Types.Contract) expiry =
    let hash = computeHash contract.code

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath contract.code contract.hints contract.rlimit
    |> Result.bind (fun _ -> load contractsPath expiry (String.length contract.code |> uint32) contract.code hash) //TODO: remove size

let recordHints code =
    code
    |> computeHash
    |> getModuleName
    |> ZFStar.recordHints code

let getCost contract = contract.costFn

let run contract = fun txSkeleton -> contract.mainFn txSkeleton contract.hash

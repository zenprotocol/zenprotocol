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
type ContractFn = Hash -> string -> data option -> ContractWallet -> TxSkeleton.T -> Result<(TxSkeleton.T * Message Option),string>
type ContractCostFn = string -> data option -> ContractWallet -> TxSkeleton.T -> int64

type T = {
    hash: Hash
    fn:   ContractFn
    costFn: ContractCostFn
    expiry: uint32
    size: uint32
    code:string
}

let private findMethods assembly =
    try
        let getMethod name =
            (assembly:Assembly)
                .GetModules().[0]
                .GetTypes().[0].GetMethod(name)
        Ok (getMethod "main", getMethod "cf")
    with _ as ex ->
        Exception.toError "get contract methods" ex


let private invokeMainFn
                methodInfo
                (cHash : byte[])
                (command : byte[])
                (data : Native.option<Zen.Types.Data.data>)
                (contractWallet : Prims.list<pointedOutput>)
                (input : txSkeleton)
                : obj =
    (methodInfo:MethodInfo).Invoke (null, [| input; cHash; command; data; contractWallet |])

let private invokeCostFn
                methodInfo
                (command : byte[])
                (data : Native.option<Zen.Types.Data.data>)
                (contractWallet : Prims.list<pointedOutput>)
                (input : txSkeleton)
                : obj =
    (methodInfo:MethodInfo).Invoke (null, [| input; command; data; contractWallet |])

let private castMainFnOutput output =
    (output:System.Object) :?> cost<result<(txSkeleton * message Native.option)>, unit>

let private castCostFnOutput output =
    (output:System.Object) :?> cost<int64, unit>

let private wrap (mainMethodInfo, costMethodInfo) =
    (
    fun (Hash.Hash cHash) command data contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data

        invokeMainFn mainMethodInfo cHash command' data' contractWallet' txSkeleton'
        |> castMainFnOutput
        |> ZFStar.unCost
        |> ZFStar.toResult
        |> Result.bind ZFStar.convertResult
    ,
    fun command data contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let command' = ZFStar.fsToFstString command
        let data' = ZFStar.fsToFstOption id data

        invokeCostFn costMethodInfo command' data' contractWallet' txSkeleton'
        |> castCostFnOutput
        |> ZFStar.unCost
    )

let hash contract = contract.hash

let private getModuleName =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let computeHash : string -> Hash = Hash.compute << Encoding.UTF8.GetBytes

let load contractsPath expiry size code hash =
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
            size = size //TODO: remove, use: String.length code |> uint32
            code = code
        })

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

let run contract = contract.fn contract.hash

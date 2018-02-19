module Consensus.Contract

open System.Reflection
open System.Text
open FsBech32
open Hash
open Consensus.TxSkeleton
open Infrastructure
open Zen.Types.Extracted
open FStar.Pervasives
open Microsoft.FSharp.Core
open Zen.Cost.Realized
open Zen.Types.Extracted
open Zen.Types.Realized
open Exception
open Consensus.Types
open Zen.Types.Main

type ContractWallet = PointedOutput list
type ContractFn = Hash -> string -> Data -> Lock option -> ContractWallet -> TxSkeleton.T -> Result<(TxSkeleton.T * Message Option),string>
type ContractCostFn = string -> Data -> Lock option -> ContractWallet -> TxSkeleton.T -> Result<bigint,string>

type T = {
    hash: Hash
    fn:   ContractFn
    costFn: ContractCostFn
    expiry: uint32
    size: uint32
}

let EmptyData = Data [||]

let private findMethods assembly =
    try
        let getMethod name =
            (assembly:Assembly)
                .GetModules().[0]
                .GetTypes().[0].GetMethod(name)
        Ok (getMethod "main", getMethod "cf")
    with _ as ex ->
        Exception.toError "get contract methods" ex


let private invokeMainFn methodInfo cHash command data returnAddress contractWallet input =
    try
        (methodInfo:MethodInfo).Invoke (null, [| input; cHash; command; data; returnAddress; ZFStar.vectorLength contractWallet; contractWallet |]) |> Ok
    with _ as ex ->
        Exception.toError "invoke contract main fn" ex

let private invokeCostFn methodInfo command data returnAddress contractWallet input =
    try
        (methodInfo:MethodInfo).Invoke (null, [| input; command; data; returnAddress; ZFStar.vectorLength contractWallet;  contractWallet |]) |> Ok
    with _ as ex ->
        Exception.toError "invoke contract cost fn" ex

let private castMainFnOutput output =
    try
        (output:System.Object) :?> cost<result<(txSkeleton * message Native.option)>, unit> |> Ok
    with _ as ex ->
        Exception.toError "cast contract main fn output" ex

let private castCostFnOutput output =
    try
        (output:System.Object) :?> cost<bigint, unit> |> Ok
    with _ as ex ->
        Exception.toError "cast contract cost fn output" ex

let private wrap (mainMethodInfo, costMethodInfo) =
    (
    fun (Hash.Hash cHash) command data returnAddress contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let returnAddress' = ZFStar.fsToFstOption ZFStar.fsToFstLock returnAddress
        let data' = ZFStar.fsToFstData data

        invokeMainFn mainMethodInfo cHash command data' returnAddress' contractWallet' txSkeleton'
        |> Result.bind castMainFnOutput
        |> Result.map ZFStar.unCost
        |> Result.bind ZFStar.toResult
        |> Result.bind ZFStar.convertResult
    ,
    fun command data returnAddress contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let returnAddress' = ZFStar.fsToFstOption ZFStar.fsToFstLock returnAddress
        let data' = ZFStar.fsToFstData data

        invokeCostFn costMethodInfo command data' returnAddress' contractWallet' txSkeleton'
        |> Result.bind castCostFnOutput
        |> Result.map ZFStar.unCost
    )

let hash contract = contract.hash

let private getModuleName =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let computeHash code =
    (code : string)
    |> Encoding.UTF8.GetBytes
    |> Hash.compute

let load contractsPath expiry size hash =
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
            size = size
        })

let compile contractsPath (code, hints) expiry =
    let hash = computeHash code

    let size = String.length code |> uint32

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath (code, hints)
    |> Result.map (fun _ -> hash)
    |> Result.bind (load contractsPath expiry size)

let recordHints code =
    code
    |> computeHash
    |> getModuleName
    |> ZFStar.recordHints code

let getCost contract command =
    contract.costFn command

let run contract command data returnAddress wallet inputTx =
    contract.fn contract.hash command data returnAddress wallet inputTx

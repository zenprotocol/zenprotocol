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
type ContractCostFn = string -> Data -> Lock option -> ContractWallet -> TxSkeleton.T -> bigint

type T = {
    hash: Hash
    fn:   ContractFn
    costFn: ContractCostFn
    expiry: uint32
    size: uint32
    code:string
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
    (methodInfo:MethodInfo).Invoke (null, [| input; cHash; command; data; returnAddress; ZFStar.vectorLength contractWallet; contractWallet |])

let private invokeCostFn methodInfo command data returnAddress contractWallet input =
    (methodInfo:MethodInfo).Invoke (null, [| input; command; data; returnAddress; ZFStar.vectorLength contractWallet;  contractWallet |])

let private castMainFnOutput output =
    (output:System.Object) :?> cost<result<(txSkeleton * message Native.option)>, unit>

let private castCostFnOutput output =
    (output:System.Object) :?> cost<bigint, unit>

let private wrap (mainMethodInfo, costMethodInfo) =
    (
    fun (Hash.Hash cHash) command data returnAddress contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let returnAddress' = ZFStar.fsToFstOption ZFStar.fsToFstLock returnAddress
        let data' = ZFStar.fsToFstData data

        invokeMainFn mainMethodInfo cHash command data' returnAddress' contractWallet' txSkeleton'
        |> castMainFnOutput
        |> ZFStar.unCost
        |> ZFStar.toResult
        |> Result.bind ZFStar.convertResult
    ,
    fun command data returnAddress contractWallet txSkeleton ->
        let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
        let contractWallet' = ZFStar.convertWallet contractWallet
        let returnAddress' = ZFStar.fsToFstOption ZFStar.fsToFstLock returnAddress
        let data' = ZFStar.fsToFstData data

        invokeCostFn costMethodInfo command data' returnAddress' contractWallet' txSkeleton'
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
            size = size
            code = code
        })

let compile contractsPath (code, hints) expiry =
    let hash = computeHash code

    let size = String.length code |> uint32

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath (code, hints)
    |> Result.map (fun _ -> hash)
    |> Result.bind (load contractsPath expiry size code)

let recordHints code =
    code
    |> computeHash
    |> getModuleName
    |> ZFStar.recordHints code

let getCost contract = contract.costFn

let run contract = contract.fn contract.hash

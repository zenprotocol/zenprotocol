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
open Zen.Types.TxSkeleton
open Exception

type ContractFn = Hash -> string -> TxSkeleton -> Result<TxSkeleton,string>

type T = {
    hash: Hash
    fn:   ContractFn
}

let private findMethod assembly =
    try
        (assembly:Assembly)
            .GetModules().[0]
            .GetTypes().[0]
            .GetMethods().[1]
            |> Ok
    with _ as ex ->
        Exception.toError "get contract method" ex

let private invoke methodInfo cHash command input =
    try
        (methodInfo:MethodInfo).Invoke (null, [| input; cHash; command |]) |> Ok
    with _ as ex ->
        Exception.toError "invoke contract" ex

let private castOutput output =
    try
        (output:System.Object) :?> cost<result<txSkeleton>, unit> |> Ok
    with _ as ex ->
        Exception.toError "cast contract output" ex

let private wrap methodInfo =
    fun (Hash.Hash cHash) command txSkeleton ->
        ZFStar.convertInput txSkeleton
        |> invoke methodInfo cHash command
        |> Result.bind castOutput
        |> Result.bind ZFStar.convertResult

let hash contract = contract.hash


let private getModuleName =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let computeHash code =
    (code : string)
    |> Encoding.UTF8.GetBytes
    |> Hash.compute

let load contractsPath hash =
    getModuleName hash
    |> ZFStar.load contractsPath
    |> Result.bind findMethod
    |> Result.map wrap
    |> Result.map (fun fn ->
        {
            hash = hash
            fn = fn
        })

let compile contractsPath (code, hints) =
    let hash = computeHash code

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath (code, hints)
    |> Result.map (fun _ -> hash)
    |> Result.bind (load contractsPath)

let recordHints code =
    code
    |> computeHash
    |> getModuleName
    |> ZFStar.recordHints code

let run contract command =
    contract.fn contract.hash command

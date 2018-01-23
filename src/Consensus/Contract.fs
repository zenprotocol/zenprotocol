module Consensus.Contract

open System.Reflection
open System.Text
open FsBech32
open Hash
open Consensus.TxSkeleton
open Infrastructure
open ZFStar
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

let private findMethod (assembly:Assembly) =
    try
        assembly
            .GetModules().[0]
            .GetTypes().[0]
            .GetMethods().[1]
            |> Ok
    with _ as ex ->
        Exception.toError "get contract method" ex

let private invoke (methodInfo:MethodInfo) cHash command input =
    try
        methodInfo.Invoke (null, [| input; cHash; command |]) |> Ok
    with _ as ex ->
        Exception.toError "invoke contract" ex

let private castOutput (output:System.Object) =
    try
        output :?> cost<result<txSkeleton>, unit> |> Ok
    with _ as ex ->
        Exception.toError "cast contract output" ex

let private wrap methodInfo =
    fun (Hash.Hash cHash) command txSkeleton ->
        convertInput txSkeleton
        |> invoke methodInfo cHash command
        |> Result.bind castOutput
        |> Result.bind convertResult

let hash contract = contract.hash

let computeHash (code:string) =
    code
    |> Encoding.UTF8.GetBytes
    |> Hash.compute

let compile contractsPath code =
    let hash = computeHash code

    hash
    |> Hash.bytes
    |> Base16.encode
    |> ZFStar.compile contractsPath code
    |> Result.bind findMethod
    |> Result.map wrap
    |> Result.map (fun fn ->
        {
            hash = hash
            fn = fn
        })

let run contract command =
    contract.fn contract.hash command

let load contractsPath (hash:Hash.Hash) =

    ZFStar.load contractsPath (Hash.toString hash)
    |> Result.bind findMethod
    |> Result.map wrap
    |> Result.map (fun fn ->
        {
            hash = hash
            fn = fn
        })

module Consensus.Contract

open System.Reflection
open System.Text
open FsBech32
open Hash
open Consensus.TxSkeleton
open Consensus.ZFStar
open Infrastructure
open Zen.Types.Extracted
open Exception

type ContractFn = Hash -> TxSkeleton -> Result<TxSkeleton,string>

type T = {
    hash: Hash
    fn:   ContractFn
}

let private findMethod (assembly : Assembly) =
    try
        assembly
            .GetModules().[0]
            .GetTypes().[0]
            .GetMethods().[0]
            |> Ok
    with _ as ex ->
        Exception.toError "get method" ex

let private wrap (methodInfo : MethodInfo) : ContractFn =
    fun (Hash.Hash cHash) txSkeleton -> 
        try
            methodInfo.Invoke (null, [| convertInput txSkeleton; cHash |])
            :?> transactionSkeleton
            |> convertResult
            |> Ok
        with _ as ex ->
            Exception.toError "run contract" ex

let hash contract = contract.hash

let compile (code : string) : Result<T, string> =
    let hash =
        code
        |> Encoding.UTF8.GetBytes
        |> Hash.compute

    hash 
    |> Hash.bytes
    |> Base16.encode
    |> ZFStar.compile code
    |> Result.bind findMethod 
    |> Result.map wrap
    |> Result.map (fun fn ->
        {
            hash = hash
            fn = fn
        })

let run (contract : T) input = 
    contract.fn contract.hash input
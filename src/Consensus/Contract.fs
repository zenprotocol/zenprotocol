module Consensus.Contract

open System.Reflection
open System.Text
open FsBech32
open Hash
open Infrastructure.ZFStar

type T = System.Numerics.BigInteger -> Result<System.Numerics.BigInteger,string>

let private getMethodInfo (assembly : Assembly) =
    try
        assembly
            .GetModules().[0]
            .GetTypes().[0]
            .GetMethods().[0]
            |> Ok
    with _ as ex ->
        "get method ex: " + ex.Message
        |> Error

let private wrap (methodInfo : MethodInfo) : T =
    fun (input : System.Numerics.BigInteger) -> 
        try
            methodInfo.Invoke (null, [| input |])
            :?> System.Numerics.BigInteger
            |> Ok
        with _ as ex ->
            "run contract ex: " + ex.Message
            |> Error

let hash (code:string) = 
    code
    |> Encoding.ASCII.GetBytes
    |> Hash.compute

let compile code : Result<Hash * T, string> =
    let hash' = hash code
    hash' 
    |> Hash.bytes
    |> Base16.encode
    |> compile code
    |> Result.bind getMethodInfo 
    |> Result.map wrap
    |> Result.map (fun f -> (hash', f))

let run contract = contract
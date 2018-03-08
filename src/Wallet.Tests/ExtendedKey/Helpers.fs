module Wallet.Tests.ExtendedKey.Helpers

open Infrastructure.Result
open Wallet.ExtendedKey
open FsUnit

let result = new ResultBuilder<string>()

let decode bytes =
    Base58Check.Base58CheckEncoding.Decode bytes

let toPrint =
    decode >> Array.fold (fun state t ->
        let s = sprintf "%A" t
        let s = s.Replace("uy", "")
        sprintf "%s %s" state s
    ) ("")

let private (>>=) a b = Result.bind b a

let getEncodedPair chainArgs extendedKey = result {
    let! encodedPublic = neuter extendedKey >>= encode chainArgs 
    let! encodedPrivate = encode chainArgs extendedKey
        
    return (encodedPublic, encodedPrivate)
}
        
let shouldEqual expected was message = result {
    let! was = was
    let! expected = Ok expected : Result<string*string,string>

    if snd expected <> snd was then
        printf "%s private:" message
        expected
        |> snd
        |> toPrint
        |> printfn "\n  expected:%s"
        was
        |> snd
        |> toPrint
        |> printfn "       got:%s\n"

    if fst expected <> fst was then 
        printf "%s public:" message
        expected
        |> fst
        |> toPrint
        |> printfn "\n  expected:%s"
        was
        |> fst
        |> toPrint
        |> printfn "       got:%s\n"
        
    should equal expected was
}

let shouldOk : (Result<T,_> -> _) = 
    Result.mapError failwith
    >> ignore

let shouldError expected was = 
    let expected = (Error expected: Result<T,string>)
    
    if was <> expected then
        printfn "\nexpected: %A\nbut was:  %A" expected was

    should equal expected was

let create seed expected = result {
    let! master = create seed
    do! shouldEqual expected (getEncodedPair Constants.main master) "m"
    return master
}

let derive index expected message extendedKey = result {
    let! derivedExtendedKey = derive index extendedKey
    do! shouldEqual expected (getEncodedPair Constants.main derivedExtendedKey) message
    return derivedExtendedKey
}
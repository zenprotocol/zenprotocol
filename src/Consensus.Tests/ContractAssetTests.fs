module Consensus.Tests.ContractAssetTests

open Consensus
open NUnit.Framework
open Infrastructure

let result = new Infrastructure.Result.ResultBuilder<string>()

let contractPath = "./test"

let compile code = result {
    let! hints = Contract.recordHints code
    return! Contract.compile contractPath (code, hints) 1000ul
}

let dataPath = ".data"

let clean() =
    Platform.cleanDirectory dataPath

[<OneTimeSetUp>]
let setUp = fun () ->
    clean()

[<TearDown>]
let tearDown = fun () ->
    clean()

let compileAndRun code =
    compile code
    |> Result.bind (fun contract ->
        Contract.run contract "" Contract.EmptyData None List.empty TxSkeleton.empty
        |> Result.map (fun (tx, _) -> tx)
    )

let shouldBeOk result =
    result
    |> Result.mapError failwith
    |> ignore

[<Test>]
let ``Should generate assets from a string and from an int``() =
    let contractCode = """
        open Zen.Types
        open Zen.Vector
        open Zen.Base
        open Zen.Cost
        open Zen.Asset

        module ET = Zen.ErrorT
        module Tx = Zen.TxSkeleton
        module S = FStar.String

        val main: txSkeleton -> hash -> string -> data -> option lock -> #l:nat -> wallet l 
            -> result (txSkeleton ** option message) `cost` (256 + 26)
        let main txSkeleton contractHash command data returnAddress #l wallet =
            let str = "Test" in

            if S.length str < 29 then
            begin
                let! assetString = Zen.Asset.fromString contractHash str in
                let! assetInt = Zen.Asset.fromInt contractHash 9999999ul in
                let! txSkeleton =
                    Tx.mint 10UL assetInt txSkeleton
                    >>= Tx.mint 20UL assetString
                in
                ET.ret (txSkeleton, None)
            end
            else
                ET.incFailw 272 "unexpected"
                
        val cf: txSkeleton -> string -> data -> option lock -> #l:nat -> wallet l -> cost nat 1
                let cf _ _ _ _ #l _ = ret 282
        """
    compileAndRun contractCode
    |> shouldBeOk

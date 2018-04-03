module Consensus.Tests.ContractAssetTests

open Consensus
open NUnit.Framework
open Infrastructure

let result = new Infrastructure.Result.ResultBuilder<string>()

let contractPath = "./test"

[<Literal>]
let rlimit = 2723280u

let compile code = result {
    let! hints = Contract.recordHints code
    let! queries = Infrastructure.ZFStar.totalQueries hints

    let contract : Consensus.Types.Contract = {
        code = code
        hints = hints
        rlimit = rlimit
        queries = queries
    }
    
    return! Contract.compile contractPath contract 1000ul
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
        Contract.run contract "" None List.empty TxSkeleton.empty
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

        module RT = Zen.ResultT
        module Tx = Zen.TxSkeleton
        module S = FStar.String

        val main: txSkeleton -> hash -> string -> option data -> wallet
            -> result (txSkeleton ** option message) `cost` (64 + (64 + (64 + 64 + 0)) + 26)
        let main txSkeleton contractHash command data wallet =
            let str = "Test" in

            if S.length str < 29 then
            begin
                let! assetString = Zen.Asset.fromString contractHash str in
                let! assetInt = Zen.Asset.fromInt contractHash 9999999ul in
                let! txSkeleton =
                    Tx.mint 10UL assetInt txSkeleton
                    >>= Tx.mint 20UL assetString
                in
                RT.ok (txSkeleton, None)
            end
            else
                RT.autoFailw "unexpected"

        val cf: txSkeleton -> string -> option data -> wallet -> cost nat 11
                let cf _ _ _ _ = ret (64 + (64 + (64 + 64 + 0)) + 26)
        """
    compileAndRun contractCode
    |> shouldBeOk

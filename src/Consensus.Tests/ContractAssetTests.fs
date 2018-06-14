module Consensus.Tests.ContractAssetTests

open Consensus
open Consensus.Types
open NUnit.Framework
open Infrastructure

let result = new Infrastructure.Result.ResultBuilder<string>()

let tempDir () =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]
let contractPath = tempDir()


[<Literal>]
let rlimit = 2723280u

let compile code = result {
    let! hints = Contract.recordHints code
    let! queries = Infrastructure.ZFStar.totalQueries hints

    let contract : Consensus.Types.ContractV0 = {
        code = code
        hints = hints
        rlimit = rlimit
        queries = queries
    }

    return!
        Contract.compile contractPath contract
        |> Result.bind (Contract.load contractPath 100ul code)
}

let dataPath = tempDir()

let clean() =
    Platform.cleanDirectory dataPath

[<OneTimeSetUp>]
let setUp = fun () ->
    clean()

[<TearDown>]
let tearDown = fun () ->
    clean()

let compileAndRun code =
    let stubContext:Types.ContractContext = {blockNumber=1000u;timestamp=1000000UL}
    compile code
    |> Result.bind (fun contract ->
        Contract.run contract TxSkeleton.empty stubContext "" Types.Anonymous None List.empty None
        |> Result.map (fun (tx, _, _) -> tx)
    )

let shouldBeOk result =
    result
    |> Result.mapError failwith
    |> ignore

[<Test>]
[<Parallelizable>]
let ``Should generate assets from a string and from an int``() =
    let contractCode = """
        open Zen.Types
        open Zen.Base
        open Zen.Cost
        open Zen.Asset

        module RT = Zen.ResultT
        module Tx = Zen.TxSkeleton
        module S = FStar.String
        module C = Zen.Cost

        let main txSkeleton _ contractId command sender messageBody wallet state =
            let str = "Test" in

            if S.length str < 29 then
            begin
                let! assetString = Zen.Asset.fromSubtypeString contractId str in
                let! assetInt = Zen.Asset.fromSubtypeInt contractId 9999999ul in
                let! txSkeleton =
                    Tx.mint 10UL assetInt txSkeleton
                    >>= Tx.mint 20UL assetString
                in
                RT.ok @ { tx = txSkeleton; message = None; state = NoChange}
            end
            else
                RT.autoFailw "unexpected"

        let cf _ _ _ _ _ _ _ =
            64 + (64 + (64 + 64 + 0)) + 28
            |> cast nat
            |> C.ret
        """
    compileAndRun contractCode
    |> shouldBeOk

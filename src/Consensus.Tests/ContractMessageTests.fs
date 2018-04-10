module Consensus.Tests.ContractMessageTests

open Consensus
open Types
open Infrastructure
open NUnit.Framework
open Hash
open System.Text
open TxSkeleton
open Crypto
open SampleContract
open FsUnit

open Consensus
open Consensus.Tests
open FSharp.Compatibility.OCaml
open FStar.Pervasives
open FsCheck.Experimental

let result = new Infrastructure.Result.ResultBuilder<string>()

let contractPath = "./test"

[<Literal>]
let rlimit = 2723280u

let clean() =
    Platform.cleanDirectory contractPath

let compile code = result {
    let! hints = Contract.recordHints code
    let! queries = Infrastructure.ZFStar.totalQueries hints

    let contract = {
        code = code
        hints = hints
        rlimit = rlimit
        queries = queries
    }

    return! Contract.compile contractPath contract 1000ul
}

// Message passing 'artificial' test:
// Contract1 mints 25 tokens, and passes a message to contract2 which expects it and mints 50 tokens

let mutable contracts : Result<(Contract.T * Contract.T), string> = Error "uninitialized"

[<OneTimeSetUp>]
let setup = fun () ->
    clean()
    let contract2Code = """
    open Zen.Types
    open Zen.Base
    open Zen.Cost

    module RT = Zen.ResultT
    module Tx = Zen.TxSkeleton

    let main txSkeleton contractHash command sender data wallet =
        if command = "contract2_test" then
        begin
            let! contractToken = Zen.Asset.getDefault contractHash in
            let! txSkeleton =
                Tx.mint 50UL contractToken txSkeleton
                >>= Tx.lockToContract contractToken 50UL contractHash in
            RT.ok (txSkeleton, None)
        end
        else
            RT.autoFailw "unsupported command"

    val cf: txSkeleton -> string -> sender -> option data -> wallet -> cost nat 9
        let cf _ _ _ _ _ = ret (64 + (64 + 64 + 0) + 21)
    """
    let contract2Hash = Contract.computeHash contract2Code

    let contract1Code =
        contract2Hash
        |> Hash.bytes
        |> System.Convert.ToBase64String
        |> sprintf """
            open Zen.Types
            open Zen.Util
            open Zen.Base
            open Zen.Cost

            module RT = Zen.ResultT
            module Tx = Zen.TxSkeleton

            let main txSkeleton contractHash command sender data wallet =
                if command = "contract1_test" then
                begin
                    let! asset = Zen.Asset.getDefault contractHash in
                    let! txSkeleton =
                        Tx.mint 25UL asset txSkeleton
                        >>= Tx.lockToContract asset 25UL contractHash in
                    let message = {
                        cHash = hashFromBase64 "%s";
                        command = "contract2_test";
                        data = data
                    } in
                    RT.ok (txSkeleton, Some message)
                end
                else
                    RT.autoFailw "unsupported command"

            val cf: txSkeleton -> string -> sender -> option data -> wallet -> cost nat 9
            let cf _ _ _ _ _ = ret (64 + (64 + 64 + 0) + 26)
        """

    contracts <- result {
        let! contract1 = compile contract1Code
        let! contract2 = compile contract2Code

        return (contract1, contract2)
    }

[<OneTimeSetUp>]
let tearDown = fun () ->
    clean ()

[<Test>]
[<ParallelizableAttribute>]
let ``Should produce execute contracts with message passed between them``() =
    result {
        let! (contract1, contract2) = contracts

        let spend1 = {asset = contract1.hash, Hash.zero; amount = 25UL}
        let spend2 = {asset = contract2.hash, Hash.zero; amount = 50UL}

        let expectedTx =
            {
                pInputs =
                    [
                        Mint spend1
                        Mint spend2
                    ]
                outputs =
                    [
                        {lock = Contract contract1.hash; spend = {asset = contract1.hash, Hash.zero; amount = 25UL}}
                        {lock = Contract contract2.hash; spend = {asset = contract2.hash, Hash.zero; amount = 50UL}}
                    ]
            }

        let stringData = Zen.Types.Data.data.String "Some string data"B |> Some

        let! (tx, message) = Contract.run contract1 "contract1_test" Anonymous stringData List.empty TxSkeleton.empty

        let command =
            match message with
            | Some {cHash=cHash;command=command;data=data} when cHash = contract2.hash ->
                data
                |> should equal stringData

                command
            | _ ->
                failwithf "should be some message"

        let! (tx, message) = Contract.run contract2 command Anonymous None List.empty tx

        match message with
        | Some _ ->
            failwithf "should be no message"
        | _ ->
            ()

        should equal tx expectedTx
    }
    |> Result.mapError failwith
    |> ignore

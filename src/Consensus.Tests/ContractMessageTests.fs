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

let contractPath =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]


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

    return!
        Contract.compile contractPath contract
        |> Result.bind (Contract.load contractPath 100ul code)
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
    module C = Zen.Cost

    let main txSkeleton _ contractId command sender messageBody wallet state =
        if command = "contract2_test" then
        begin
            let! contractToken = Zen.Asset.getDefault contractId in
            let! txSkeleton =
                Tx.mint 50UL contractToken txSkeleton
                >>= Tx.lockToContract contractToken 50UL contractId in
            RT.ok @ { tx = txSkeleton; message = None; state = NoChange}
        end
        else
            RT.autoFailw "unsupported command"

    let cf _ _ _ _ _ _ _ = 
        64 + (64 + 64 + 0) + 23
        |> cast nat
        |> C.ret
    """
    let contract2Id = Contract.makeContractId Version0 contract2Code

    let contract1Code =
        (contract2Id.ToString())
        |> sprintf """
            open Zen.Types
            open Zen.Base
            open Zen.Cost

            module RT = Zen.ResultT
            module Tx = Zen.TxSkeleton
            module ContractId = Zen.ContractId
            module C = Zen.Cost
            
            let main txSkeleton _ contractId command sender messageBody wallet state =
                if command = "contract1_test" then
                begin
                    let! asset = Zen.Asset.getDefault contractId in
                    let! txSkeleton =
                        Tx.mint 25UL asset txSkeleton
                        >>= Tx.lockToContract asset 25UL contractId in
                    let! contractId = ContractId.fromString "%s" in
                    match contractId with 
                    | Some contractId -> 
                        let message = {
                            recipient = contractId;
                            command = "contract2_test";
                            body = messageBody
                        } in
                        RT.ok @ { tx = txSkeleton; message = Some message; state = NoChange}
                    | None ->
                        RT.autoFailw "could not parse contractId from string" 
                end
                else
                    RT.autoFailw "unsupported command"

            let cf _ _ _ _ _ _ _ = 
                64 + (64 + 64 + (64 + 0)) + 33
                |> cast nat
                |> C.ret
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
[<Parallelizable>]
let ``Should produce execute contracts with message passed between them``() =
    result {
        let! (contract1, contract2) = contracts

        let spend1 = {asset = Asset (ContractId (Version0, contract1.hash), Hash.zero); amount = 25UL}
        let spend2 = {asset = Asset (ContractId (Version0, contract2.hash), Hash.zero); amount = 50UL}

        let expectedTx =
            {
                pInputs =
                    [
                        Mint spend1
                        Mint spend2
                    ]
                outputs =
                    [
                        {lock = Contract <| ContractId (Version0,contract1.hash); spend = {asset = Asset (ContractId (Version0, contract1.hash), Hash.zero); amount = 25UL}}
                        {lock = Contract <| ContractId (Version0,contract2.hash); spend = {asset = Asset (ContractId (Version0, contract2.hash), Hash.zero); amount = 50UL}}
                    ]
            }

        let stringData = Zen.Types.Data.data.String "Some string data"B |> Some
        let context = {blockNumber=100u;timestamp=1_000_000UL}

        let! (tx, message, _) = Contract.run contract1 TxSkeleton.empty context "contract1_test" Anonymous stringData List.empty None

        let command =
            match message with
            | Some {recipient=recipient;command=command;body=messageBody} when recipient = ContractId (contract2.version,contract2.hash) ->
                messageBody
                |> should equal stringData

                command
            | _ ->
                failwithf "should be some message"

        let! (tx, message, _) = Contract.run contract2 tx context command Anonymous None List.empty None

        match message with
        | Some _ ->
            failwithf "should be no message"
        | _ ->
            ()

        should equal tx expectedTx
    }
    |> Result.mapError failwith
    |> ignore

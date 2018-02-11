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

let clean() =
    Platform.cleanDirectory contractPath

let compile code = result {
    let! hints = Contract.recordHints code
    return! Contract.compile contractPath (code, hints)
}

let emptyOutpoint = {txHash = Hash.zero; index = 0u}

// Message passing 'artificial' test:
// Contract1 mints 25 tokens, and passes a message to contract2 which expects it and mints 50 tokens

let mutable contracts : Result<(Contract.T * Contract.T), string> = Error "uninitialized"

[<OneTimeSetUp>]
let setup = fun () ->
    clean()
    let contract2Code = """
    open Zen.Types
    open Zen.Vector
    open Zen.Util
    open Zen.Base
    open Zen.Cost
    
    module ET = Zen.ErrorT
    module Tx = Zen.TxSkeleton
    
    val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 7
    let cf _ _ _ #l _ = ret (64 + 64 + 0 + 17)
    
    val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + 64 + 0 + 17)
    let main txSkeleton contractHash command returnAddress #l wallet =
        if command = "contract2_test" then
        begin
            let! txSkeleton =
                Tx.mint 50UL contractHash txSkeleton
                >>= Tx.lockToContract contractHash 50UL contractHash in
            ET.ret (txSkeleton, None)
        end
        else 
            ET.autoFailw "unsupported command"
    """
    let contract2Hash = Contract.computeHash contract2Code
        
    let contract1Code = 
        contract2Hash
        |> Hash.bytes
        |> System.Convert.ToBase64String
        |> sprintf """
            open Zen.Types
            open Zen.Vector
            open Zen.Util
            open Zen.Base
            open Zen.Cost
            
            module ET = Zen.ErrorT
            module Tx = Zen.TxSkeleton
            
            val cf: txSkeleton -> string -> option lock -> #l:nat -> wallet l -> cost nat 7
            let cf _ _ _ #l _ = ret (64 + 64 + 0 + 21)
            
            val main: txSkeleton -> hash -> string -> option lock -> #l:nat -> wallet l -> cost (result (txSkeleton ** option message)) (64 + 64 + 0 + 21)
            let main txSkeleton contractHash command returnAddress #l wallet =
                if command = "contract1_test" then
                begin
                    let! txSkeleton =
                        Tx.mint 25UL contractHash txSkeleton
                        >>= Tx.lockToContract contractHash 25UL contractHash in
                    let message = { 
                        cHash = hashFromBase64 "%s";
                        command = "contract2_test"
                    } in
                    ET.ret (txSkeleton, Some message)
                end
                else 
                    ET.autoFailw "unsupported command"
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
let ``Should produce execute contracts with message passed between them``() =
    result {
        let! (contract1, contract2) = contracts

        let expectedTx = 
            {
                pInputs = 
                    [
                        (emptyOutpoint, {lock = Contract contract1.hash; spend = {asset = contract1.hash; amount = 25UL}})
                        (emptyOutpoint, {lock = Contract contract2.hash; spend = {asset = contract2.hash; amount = 50UL}})
                    ]
                outputs = 
                    [
                        {lock = Contract contract1.hash; spend = {asset = contract1.hash; amount = 25UL}}
                        {lock = Contract contract2.hash; spend = {asset = contract2.hash; amount = 50UL}}
                    ]
            }
        
        let! (tx, message) = Contract.run contract1 "contract1_test" None List.empty TxSkeleton.empty

        let command = 
            match message with 
            | Some {cHash=cHash;command=command} when cHash = 
                contract2.hash -> command
            | _ -> 
                failwithf "should be some message"
            
        let! (tx, message) = Contract.run contract2 command None List.empty tx

        match message with 
        | Some _ ->
            failwithf "should be no message"
        | _ -> 
            ()

        should equal tx expectedTx
    }    
    |> Result.mapError failwith
    |> ignore
module Node.Tests.Config

open Consensus
open Types

let busName = "test"
let chain = Chain.Local
let chainParams = Chain.getChainParameters chain
let tempDir () =
    System.IO.Path.Combine
        [| System.IO.Path.GetTempPath(); System.IO.Path.GetRandomFileName() |]
let dataPath = tempDir()
let apiUri = "127.0.0.1:29555"


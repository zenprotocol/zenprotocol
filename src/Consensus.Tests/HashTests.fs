module Consensus.Tests.HashTests

open Consensus
open Hash
open NUnit.Framework
open FsCheck.NUnit


[<Property(EndSize=10000)>]
let ``Hash size should be 32``(bs:byte[]) =
    let (Hash h) = Hash.compute bs
    Array.length h = 32
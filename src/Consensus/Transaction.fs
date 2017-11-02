module Consensus.Transaction

open Consensus.Types
open MBrace.FsPickler.Combinators

let pickler = Pickler.auto<Transaction>

type SerializationMode = 
    | Full
    | WithoutWitness

let serialize mode tx =
    let tx = 
        match mode with
        | Full -> tx
        | WithoutWitness -> {tx with witnesses=[]}

    Binary.pickle pickler tx

let deserialize =
    Binary.unpickle pickler 

let hash =
    serialize WithoutWitness >> Hash.compute 
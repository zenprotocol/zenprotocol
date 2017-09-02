#light "off"
module Consensus.Realized

type lockCore = {version: uint32; lockData: byte[] list}


type contract = {code: byte[]; bounds: byte[]; hint: byte[]}


type extraData = byte[] list

type extendedContract =                                         
    | Contract of contract
    | HighVContract of version : uint32 * data : byte[]
module Zen.Bitcoin

open System.Security.Cryptography
open FsBech32
open Zen.Types.Extracted

module Cost = Zen.Cost.Realized

type bitcoinHeader = byte array
type target = byte array

let difficultyAdjustmentInterval = 2016ul

let parent (header:bitcoinHeader) : hash = header.[4..35]

let nbits (header:bitcoinHeader) : target = header.[72..75]

let parseHeader (b16:byte array) : Cost.t<FStar.Pervasives.Native.option<bitcoinHeader>, unit> =
    lazy(
        if Array.length b16 <> 160 then
            FStar.Pervasives.Native.None
        else
            let b16 = System.Text.Encoding.ASCII.GetString b16
            match Base16.decode b16 with
            | None -> FStar.Pervasives.Native.None
            | Some h -> FStar.Pervasives.Native.Some h
    ) |> Cost.C

let computeHeaderHash (header:bitcoinHeader) : Cost.t<hash,unit> =
    lazy (
        use sha = new SHA256Managed()
        let h = sha.ComputeHash(header,0,80)
        sha.ComputeHash(h,0,32)
    ) |> Cost.C

let checkProofOfWork
    (h:hash)
    (target:target) : Cost.t<bool,unit> =
    lazy(
        // TODO: implemet actual PoW check
        true
    ) |> Cost.C

let calculateNextWorkRequired
    (first:bitcoinHeader)
    (last:bitcoinHeader) : Cost.t<target,unit> =
        lazy (
            // TODO: actually calculate next proof of work
            nbits first
        ) |> Cost.C

let checkInclusion (_:int)
                   (auditPath : hash array)
                   (txHash: hash)
                   (header:bitcoinHeader) : Cost.t<bool,unit> =
    lazy (
        // TODO: implement check of inclusion
        true
    ) |> Cost.C



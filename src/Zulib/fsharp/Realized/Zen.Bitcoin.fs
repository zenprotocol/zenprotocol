module Zen.Bitcoin

open System.Security.Cryptography
open FsBech32
open Zen.Types.Extracted

module Cost = Zen.Cost.Realized

type bitcoinHeader = byte array
type target = byte array

let difficultyAdjustmentInterval = 2016ul

let private targetTimespan = 14L * 24L * 60L * 60L
let private powLimit = 2I**224 - 1I

let parent (header:bitcoinHeader) : Cost.t<hash,unit> =
    lazy(header.[4..35]) |> Cost.C

let private getTarget (header:bitcoinHeader) : target = header.[72..75]

let nbits (header:bitcoinHeader) : Cost.t<target,unit> =
    Cost.C <| lazy (getTarget header)

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

let private uncompressNbits (target:target) : bigint =
    let exponent = (int32 target.[3]) - 3
    let significand =
        if target.[2] &&& 0x80uy <> 0uy then 0 else
        65536 * int32 target.[2]
        + 256 * int32 target.[1]
        + int32 target.[0]
    if exponent <= 3 then
        bigint (significand >>> (3-exponent))
    else
        let r = bigint significand * pown 256I exponent
        if r >= pown 2I 256 then 0I else r

let private compressNbits (bTarget:bigint) : target =
    if bTarget <= 0I then Array.zeroCreate 4 else
    let bs = bTarget.ToByteArray()
    let n = Array.length bs
    let toCopy = min n 3
    let res = Array.zeroCreate<byte> 4
    Array.blit bs (n-toCopy) res 0 toCopy
    res.[3] <- (Microsoft.FSharp.Core.Operators.byte n)
    res

let private hashToBigint (hash:hash) =
    hash
    |> Array.mapi (fun i (b:byte) -> (bigint (int32 b)) * pown 256I i)
    |> Array.sum


let checkProofOfWork
    (h:hash)
    (target:target) : Cost.t<bool,unit> =
    lazy(
        (hashToBigint h) <= (uncompressNbits target)
    ) |> Cost.C

let private timestamp (header:bitcoinHeader) : int64 =
    let bs = header.[68..71]
    16777216L * int64 bs.[3]
    + 65536L * int64 bs.[2]
    + 256L * int64 bs.[1]
    + int64 bs.[0]

let calculateNextWorkRequired
    (first:bitcoinHeader)
    (last:bitcoinHeader) : Cost.t<target,unit> =
        lazy (
            let timespan =
                max
                    (min
                        ((timestamp last) - (timestamp first))
                        (targetTimespan * 4L))
                    (targetTimespan / 4L)
            let bTimespan = bigint timespan
            let bCurrent = uncompressNbits <| getTarget first
            let bNext = min
                            ((bCurrent * bTimespan) / (bigint targetTimespan))
                            powLimit
            compressNbits bNext
        ) |> Cost.C

let private doubleHash (bs: byte array) =
    use sha = new SHA256Managed()
    let h = sha.ComputeHash(bs,0,Array.length bs)
    sha.ComputeHash(h,0,32)

let private hashPath auditPath index txHash =
    let concatAndHash a (i, b) =
        let conc =
            if ((index >>> i) &&& 1u) <> 0u
            then Array.append b a
            else Array.append a b
        doubleHash conc
    Array.fold concatAndHash txHash (Array.indexed auditPath)

let checkInclusion (_:int)
                   (auditPath : hash array)
                   (index : uint32)
                   (txHash: hash)
                   (header:bitcoinHeader) : Cost.t<bool,unit> =
    lazy (
        let r = hashPath auditPath index txHash
        let merkleRoot = header.[36..67]
        r = merkleRoot
    ) |> Cost.C

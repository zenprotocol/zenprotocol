module Zulib.Tests.Helper

module Cost = Zen.Cost.Realized

open System.Security.Cryptography
open FSharp.Data
open FsBech32

let unCost (Cost.C inj:Zen.Cost.Realized.cost<'Aa, 'An>) : 'Aa = inj.Force()

let private doubleHash (bs: byte array) =
    use sha = new SHA256Managed()
    let h = sha.ComputeHash(bs,0,Array.length bs)
    sha.ComputeHash(h,0,32)

// Takes txids in internal byte order (little-endian)
let bitcoinMerkleTree : byte[] [] -> byte[] [] [] =
    let next (hashes:byte[] []) =
        if Array.length hashes <= 2 then (hashes, [||])
        else
            let toHash =
                if Array.length hashes % 2 = 0 then hashes
                else Array.append hashes [| hashes.[Array.length hashes - 1] |]
            (
                toHash,
                Array.chunkBySize 2 toHash
                |> Array.map (doubleHash << Array.concat)
            )
    Array.unfold
        (fun (hashes:byte[] []) ->
            if Array.length hashes = 0 then None
            else
                Some (
                    next hashes
                )
            )

//https://blockchain.info/block/0000000000000000001eef561471b035e019ca7b5ea180f97fc78c78ee86f1fa?format=json
type BitcoinBlockJson = JsonProvider<"./BitcoinBlock.json">

let blockDocument = BitcoinBlockJson.GetSample()
let txidStrings = blockDocument.Tx |> Array.map (fun txJ -> txJ.Hash)
let txidBytes =
    Array.map (Array.rev << Option.get << Base16.decode) txidStrings

let exampleMerkleTree = bitcoinMerkleTree txidBytes

let createAuditPath (index:uint32) : byte[] [] [] -> byte[] [] =
    Array.mapi
        (fun i hashes ->
            hashes.[int((index >>> i) ^^^ 1u)]
        )

let exampleBitcoinHeader =
    "0000002028b9e5f8a150284316d3c9dca4bdea1fdf3e7da632c01e0000000000000000008db636d94167456e2e716d62145031e2e7114701993e493099b551b4e9417e9bd774fb5aa9ec4317922ec350"
    |> Base16.decode |> Option.get
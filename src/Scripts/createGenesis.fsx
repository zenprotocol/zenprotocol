#r "../../packages/Base58Check/lib/Net40/Base58Check.dll"
#r "../Infrastructure/bin/Debug/Infrastructure.dll"
#r "../Consensus/bin/Debug/Consensus.dll"
#r "../Wallet/bin/Debug/Wallet.dll"

open Consensus
open Consensus.Types
open Consensus.Chain
open Infrastructure
open System.IO
open Wallet

let random = new System.Random()

let getRandomNonce () =
    let array =
        Array.zeroCreate 64

    random.NextBytes (array)

    System.BitConverter.ToUInt64 (array,0)

let faucetAddress = "tp1qlvm8ey4m5mj6ak3an39qdfzf7yx4uz59flzlant6jp0gzld48egsrwtv7j"
let faucetPKHash = Address.decodePK Chain.Test faucetAddress |> function | Ok hash -> hash | Error error -> failwith error

let decodeCrowdsale pk =
    let bytes = Base58Check.Base58CheckEncoding.Decode pk
    let bytes = bytes.[1..]

    PK <| Hash.compute bytes

let OneZen = 100_000_000UL

let tx = {
    version=0ul;
    inputs=[];
    outputs = [
                {lock = decodeCrowdsale "RrVG2pE61KGUbpbjpyKtriZ3kqgJtPUgmMpPbL8aXJrUPPxfSk4q";spend = {asset=Asset.Zen;amount=54_321UL * OneZen}}
                {lock = decodeCrowdsale "RrXKYsvHb94WQKDJ9sf8v6e9egzFQ8Kos94r7kMtPa3S2MNGQKGX";spend = {asset=Asset.Zen;amount=999UL * OneZen}}
                {lock = decodeCrowdsale "RrUqzc34y5ZYnV6Ap5ZFdseZYojMvPUH3d65dB28Esg2LKUsbkLy";spend = {asset=Asset.Zen;amount=55UL * OneZen}}
                {lock = decodeCrowdsale "RrXWGBpqkpJDy44asv5AuWVXFD8fiayuC2vuBTUnehD1Gk8rLdBf";spend = {asset=Asset.Zen;amount=1_523UL * OneZen}}
                {lock = decodeCrowdsale "RrVRgvPZVJKqhyYo5kQouoc7U6X1VHkFPP8MM5dqZRim3veq76KR";spend = {asset=Asset.Zen;amount=23_000UL * OneZen}}
                {lock = decodeCrowdsale "RrWAZXMDqeAY8RTqP5QDbPTCx5H7ZKhtsFHiVGzn8TDtgUXE8xiS";spend = {asset=Asset.Zen;amount=15UL * OneZen}}

                {lock = PK faucetPKHash; spend = {asset = Asset.Zen;amount=100_000_000UL * 20_000_000UL}}
                //{lock = Contract faucetHash; spend = {asset = Constants.Zen;amount=100000000UL * 100000000UL - 1UL}}
              ]
    contract = None
    witnesses = []
}

let genesisTime = Timestamp.now()

let testParameters = { testParameters with genesisTime = genesisTime}

let block = Block.createGenesis testParameters [tx] (getRandomNonce (),getRandomNonce())



printfn "Block timestamp is: %A" <| genesisTime
printf "Block hash is \n%A\n\n" <| Block.hash block.header

printfn "---------Block------------"
printfn "%s" <| Block.toHex block
printfn "--------------------------"

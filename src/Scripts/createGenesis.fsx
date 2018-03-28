#r "../Infrastructure/bin/Debug/Infrastructure.dll"
#r "../Consensus/bin/Debug/Consensus.dll"
#r "../Wallet/bin/Debug/Wallet.dll"

open Consensus
open Consensus.Types
open Consensus.Chain
open Infrastructure
open System.IO
open Wallet

let faucetContract = File.ReadAllText "../ContractExamples/Faucet.fst"
let faucetHash = Contract.computeHash faucetContract

let random = new System.Random()

let getRandomNonce () =
    let array =
        Array.zeroCreate 64

    random.NextBytes (array)

    System.BitConverter.ToUInt64 (array,0)

let faucetAddress = "tp1qlvm8ey4m5mj6ak3an39qdfzf7yx4uz59flzlant6jp0gzld48egsrwtv7j"
let faucetPKHash = Address.decodePK Chain.Test faucetAddress |> function | Ok hash -> hash | Error error -> failwith error

let tx = {
    inputs=[];
    outputs = [
                {lock = PK faucetPKHash; spend = {asset = Constants.Zen;amount=100_000_000UL * 20_000_000UL}}
                //{lock = Contract faucetHash; spend = {asset = Constants.Zen;amount=100000000UL * 100000000UL - 1UL}}
              ]
    contract = None
    witnesses = []
}

let block = Block.createGenesis (getChainParameters Chain.Test) [tx] (getRandomNonce (),getRandomNonce())

//printfn "Current timestamp %A" <| Timestamp.now()

printf "Block hash is \n%A\n\n" <| Block.hash block.header

printfn "---------Block------------"
printfn "%s" <| Block.toHex block
printfn "--------------------------"

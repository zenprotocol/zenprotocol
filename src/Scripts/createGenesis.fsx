#r "../Infrastructure/bin/Debug/Infrastructure.dll"
#r "../Consensus/bin/Debug/Consensus.dll"


open Consensus
open Consensus.Types
open Consensus.Chain
open Infrastructure
open System.IO

let faucetContract = File.ReadAllText "../ContractExamples/Faucet.fst"
let faucetHash = Contract.computeHash faucetContract

let random = new System.Random()

let getRandomNonce () =
    let array =
        Array.zeroCreate 64

    random.NextBytes (array)

    System.BitConverter.ToUInt64 (array,0)

let tx = {
    inputs=[];
    outputs = [
                {lock = PK Transaction.rootPKHash; spend = {asset = Constants.Zen;amount=1UL}}
                {lock = Contract faucetHash; spend = {asset = Constants.Zen;amount=100000000UL * 100000000UL - 1UL}}
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

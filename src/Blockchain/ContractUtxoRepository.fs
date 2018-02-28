module Blockchain.ContractUtxoRepository

open DataAccess
open DatabaseContext
open Consensus
open Consensus.Types
open Consensus.UtxoSet
open Infrastructure

let getContractUtxo (session:Session) cHash (utxoSet:UtxoSet.T) = 
    let contractUtxo = 
        MultiCollection.get session.context.contractUtxo session.session cHash
        |> Map.ofList
        
    // updating the contract's utxo according to memory utxoset
    // we end up with recent block utxo and memory utxo
    let memory,block = 
        Map.fold (fun (memory,block) outpoint outputStatus -> 
            if Map.containsKey outpoint block then
                match outputStatus with
                | NoOutput
                | Spent _ -> memory,Map.remove outpoint block
                | _ -> failwith "output cannot be both in memory and disk"
            else
                match outputStatus with
                | Unspent output ->
                    if output.lock = Contract cHash then
                        Map.add outpoint output memory,block
                    else
                        memory,block
                | _ -> memory,block
            
                ) (Map.empty,contractUtxo) utxoSet
                
    // we randomize the lists in order to avoid using the same input as other contract run 
    let block = Map.toList block |> List.shuffle
    let memory = Map.toList memory |> List.shuffle
    
    // combining th lists, we prefer to use block utxos in order to not be dependent on anoter transaction
    block @ memory
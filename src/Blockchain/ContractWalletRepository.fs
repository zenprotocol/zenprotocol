module Blockchain.ContractWalletRepository

open DataAccess
open Consensus
open DatabaseContext

let get (session:Session) cHash = 
    match Collection.tryGet session.context.contractWallets session.session cHash with
    | Some wallet -> wallet
    | None -> Map.empty
    
let save (session:Session) wallets = 
    let isWalletEmpty = 
        Map.fold (fun isEmpty _ assetOutputs -> isEmpty && List.isEmpty assetOutputs) true 

    Map.iter (fun cHash wallet ->    
        if isWalletEmpty wallet then
            Collection.delete session.context.contractWallets session.session cHash
        else
            Collection.put session.context.contractWallets session.session cHash wallet
        ) wallets
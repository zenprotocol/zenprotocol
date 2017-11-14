module Messaging.Services

open Consensus
open Consensus.Types
open Infrastructure.ServiceBus.Client

module Blockchain = 
    let serviceName = "blockchain"

    type Command = 
        | ValidateTransaction of Consensus.Types.Transaction
        
    type Request = 
        | GetMemPool
        
    type Response = 
        | MemPool
        
    let validateTransaction client tx = 
        Command.send client serviceName (ValidateTransaction tx)
    
    let getMemPool client = 
        Request.send client serviceName GetMemPool        

module Network =
    type Command = unit 
                   
    type Request = unit
                
    type Response = unit        

    let serviceName = "network"       
    
module Wallet = 
    type Command = unit
    
    type Request = 
        | GetAddress
        | GetBalance
        | Send of address:string * asset:Hash.Hash * amount:uint64
                                         
    let serviceName = "wallet"
    
    let getBalance client =
        Request.send<Request, Map<Hash.Hash,uint64>> client serviceName GetBalance
        
    let getAddress client =
        Request.send<Request, string> client serviceName GetAddress
        
    let send client address asset amount =     
        Request.send client serviceName (Send (address,asset,amount))
    
            
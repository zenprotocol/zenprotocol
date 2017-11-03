module Messaging.Services

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
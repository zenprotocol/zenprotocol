module Network.AddressBook 

type T = Set<string> 

let empty = Set.empty
    
// TODO: only return addresses from the last 3 hours?    
let getValidAddresses = Set.toList
    
let addList book list = 
    Set.union book (Set.ofList list)    
    
let add book address = Set.add address book      
    
let contains book address = Set.contains address book     
    
let haveEnoughAddresses book = 
    // TODO: check if we need more addresses 
    false    
    
let take book exclude length =  
    let diff = Set.difference book exclude         
    
    if Set.isEmpty diff then
        Seq.empty
    elif Set.count diff >=length then
        Seq.take length diff
    else
        Set.toSeq diff         

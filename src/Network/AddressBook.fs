module Network.AddressBook 

type T = Set<string> 

let create seeds = 
    Seq.fold (fun set seed -> Set.add seed set) Set.empty seeds
    
let add book address = Set.add address book      
    
let contains book address = Set.contains address book     
    
let take book exclude length =  
    let diff = Set.difference book exclude         
    
    match Set.isEmpty diff with 
    | true -> Seq.empty
    | false ->
        match Set.count diff >=length with
        | true -> Seq.take length diff
        | false -> Set.toSeq diff         

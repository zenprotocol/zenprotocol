module Infrastructure.Endpoint

open System
open System.Net

let getPort address =     
    let uri = new Uri(sprintf "tcp://%s" address)
    
    uri.Port
    
let isValid (address:string) = 
    let index = address.LastIndexOf(':')
    
    if index = -1 then 
        false
    else          
        let host = address.Substring(0, index)
        let port = address.Substring(1 + index)
        
        let isInteger, port = System.Int32.TryParse port
        if isInteger && port >= 1 && port <= 65535 then                     
            match Uri.CheckHostName (host) with
            | UriHostNameType.IPv4 
            | UriHostNameType.IPv6 -> true
            | _ -> false
        else false
            
            
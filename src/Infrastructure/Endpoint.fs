module Infrastructure.Endpoint

open System
open System.Net

let getPort (address:string)  =     
    let index = address.LastIndexOf(':')
    
    if index = -1 then 
        failwith "invalid address"
    else 
        let port = address.Substring(1 + index)
        let isInteger, port = System.Int32.TryParse port
        
        if isInteger && port >= 1 && port <= 65535 then
            port
        else                      
            failwith "invalid address"     
    
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
            
            
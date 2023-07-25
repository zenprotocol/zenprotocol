module Infrastructure.Endpoint

open System
open System.Net

let getPort (address:string)  =     
    let index = address.LastIndexOf(':')
    
    if index = -1 then 
        failwith "invalid address"
    else 
        let port = address.Substring(index + 1)  // Correct index offset
        let isInteger, portValue = System.Int32.TryParse(port)  // Rename output of TryParse
        
        if isInteger && portValue >= 1 && portValue <= 65535 then
            portValue
        else                      
            failwith "invalid address"     

let isValid (address:string) = 
    let index = address.LastIndexOf(':')
    
    if index = -1 then 
        false
    else          
        let host = address.Substring(0, index)
        let port = address.Substring(index + 1) 
        
        let isInteger, portValue = System.Int32.TryParse port
        if isInteger && portValue >= 1 && portValue <= 65535 then                     
            match Uri.CheckHostName (host) with
            | UriHostNameType.IPv4 
            | UriHostNameType.IPv6 -> true
            | _ -> false
        else false

let tryGetFirstIpFromHost (address:string) (bindPort:string) : Option<string>  =
    try
        Some (sprintf "%s:%d" (Dns.GetHostAddresses(address).[0].ToString()) (getPort bindPort))
    with _ ->
        None 
            
let parseIp bindPort (address:string) =
    if isValid address then
        Some address
    else
        tryGetFirstIpFromHost address bindPort
            
let getEndpoint url port = sprintf "%s:%i" url port

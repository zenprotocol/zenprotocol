module Infrastructure.Endpoint

open System

let getPort address =     
    let uri = new Uri(sprintf "tcp://%s" address)
    
    uri.Port
module Infrastructure.EndpointTests

open NUnit.Framework
open FsUnit

open Infrastructure

[<Test>]
let ``valid ipv4 address``() =
    Endpoint.isValid "127.0.0.1:5555" |> should equal true
    
[<Test>]
let ``port out of range``() =
    Endpoint.isValid "127.0.0.1:555555" |> should equal false
    
[<Test>]
let ``port not a number``() =
    Endpoint.isValid "127.0.0.1:gf" |> should equal false
    
[<Test>]
let ``ip is a number``() =
    Endpoint.isValid "127:gf" |> should equal false            
    
[<Test>]
let ``valid ipv4 address without port``() =
    Endpoint.isValid "127.0.0.1" |> should equal false    
    
[<Test>]
let ``hostname should fail``() =
    Endpoint.isValid "google.com:5555" |> should equal false

[<Test>]
let ``uri``() = 
    Endpoint.isValid "http://127.0.0.1:5555" |> should equal false

[<Test>]
let ``with resource``() =        
    Endpoint.isValid "127.0.0.1:5555/hello" |> should equal false
 
[<Test>]
let ``valid ipv6 address``() =
    Endpoint.isValid "2001:0db8:85a3:0000:0000:8a2e:0370:7334:5555" |> should equal true

[<Test>]
let ``ipv6 without port``() =
    Endpoint.isValid "2001:0db8:85a3:0000:0000:8a2e:0370:7334" |> should equal false

[<Test>]
let ``ipv6 with brackets``() =
    Endpoint.isValid "[2001:0db8:85a3:0000:0000:8a2e:0370:7334]:5555" |> should equal true        

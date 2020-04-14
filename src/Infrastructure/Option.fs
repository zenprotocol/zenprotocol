module Infrastructure.Option

open FSharpx.Option
open FSharpx.Operators

let traverseM f xs =
    let retn x = returnM maybe x
    let initState = retn []
    let folder h t =
        f h >>= (fun h ->
        t >>= (fun t ->
        retn (h::t)))
    List.foldBack folder xs initState

let traverseA f xs =
    let retn x = returnM maybe x
    let cons h t = h::t
    let initState = retn []
    let folder head tail =
        retn cons <*> (f head) <*> tail
    List.foldBack folder xs initState

let ofResult =
    function
    | Ok res -> Some res
    | Error _ -> None

let ofBool b =
    if b then Some () else None

let toBool o =
    match o with | Some _ -> true | None -> false

let validateWith (predicate : 'b -> bool) (x : 'b) : Option<'b> =
    predicate x
    |> ofBool
    |> Option.map (fun _ -> x)


module OptionBool =
    
    type OptionBool = Option<unit>
    
    let force (x : Lazy<'a>) : 'a = x.Force()
    
    let ifThenElse (if' : Option<'b>) (then' : Lazy<'a>) (else' : Lazy<'a>) : Lazy<'a> =
        FSharpx.Option.option else' (fun _ -> then') if'
    
    let (&&->) (x : Lazy<Option<'a>>) (y : Lazy<Option<'a>>) : Lazy<Option<'a>> =
        ifThenElse (x.Force()) y x
    
    let (<-&&) (x : Lazy<Option<'a>>) (y : Lazy<Option<'a>>) : Lazy<Option<'a>> =
        ifThenElse (y.Force()) x y
    
    let (||->) (x : Lazy<Option<'a>>) (y : Lazy<Option<'a>>) : Lazy<Option<'a>> =
        ifThenElse (x.Force()) x y
    
    let (<-||) (x : Lazy<Option<'a>>) (y : Lazy<Option<'a>>) : Lazy<Option<'a>> =
        ifThenElse (y.Force()) y x
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

let validateWith (predicate : 'b -> bool) (x : 'b) : Option<'b> =
    predicate x
    |> ofBool
    |> Option.map (fun _ -> x)

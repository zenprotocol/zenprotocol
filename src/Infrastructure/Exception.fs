module Infrastructure.Exception

let toError s (e : System.Exception) =
    sprintf "%s %A" s e
    |> Error
    
let resultWrap<'T> f error : Result<'T, string> = 
    try
        f()
        |> Ok
    with _ as ex ->
        sprintf "%s %s" error ex.Message
        |> Error

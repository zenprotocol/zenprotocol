module Infrastructure.Exception

let toError s (e : System.Exception) =
    sprintf "%s %A" s e
    |> Error
    
let resultWrap<'T,'TError> f error : Result<'T,'TError> = 
    try
        f
        |> Ok
    with _ ->
        Error error

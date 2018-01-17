module Infrastructure.Exception

let toError s (e : System.Exception) =
    sprintf "%s %A" s e
    |> Error
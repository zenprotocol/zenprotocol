module Infrastructure.Exception

let toError s (e : System.Exception) =
    sprintf "%s %s %s" s (e.GetBaseException().GetType().Name) e.Message
    |> Error
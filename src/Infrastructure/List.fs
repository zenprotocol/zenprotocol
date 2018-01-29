module Infrastructure.List

let random = System.Random()

let remove item = List.filter (fun x -> x <> item)


let shuffle list =  
    List.map (fun item -> random.Next(),item) list
    |> List.sortBy (fun (r,_) -> r)
    |> List.map snd
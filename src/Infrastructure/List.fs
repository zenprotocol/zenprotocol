module Infrastructure.List

let private random = System.Random()

let remove item = List.filter (fun x -> x <> item)

let reject fn = List.filter (fn >> not)

let add item list = List.append list [item] 

let shuffle list =  
    List.map (fun item -> random.Next(),item) list
    |> List.sortBy (fun (r,_) -> r)
    |> List.map snd
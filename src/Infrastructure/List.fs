module Infrastructure.List

let private random = System.Random()

let remove item = List.filter (fun x -> x <> item)

let reject fn = List.filter (fn >> not)

let add item list = List.append list [item]

let update index newValue =
    List.mapi (fun i value -> if i = index then newValue else value)

// Fisher-Yates shuffle
let shuffle list : 'a list =
    let arr = List.toArray list
    for i = Array.length arr - 1 downto 1 do
        let j = random.Next(i+1)    // 0<=j<i+1
        let v = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- v
    Array.toList arr

let concatMap (f : 'a -> List<'b>) : List<'a> -> List<'b> =
    List.map f >> List.concat

let sequencePair ((s,xs) : 's * List<'a>) : List<'s * 'a> =
    List.map (fun x -> (s,x)) xs 

let pairWith (s : 's) (xs : List<'a>) : List<'s * 'a> =
    sequencePair (s, xs)

let traversePair (f : 'a -> List<'b>) ((s,x) : 's * 'a) : List<'s * 'b> =
    sequencePair (s, f x)

let isSingleton =
    function
    | [_] -> true
    | _   -> false

module Sorted =
    
    let rec private isSortedBy_ (f : 'a -> 'key) (ls : 'a list) : bool =
        match ls with
        | [] ->
            true
        | [_] ->
            true
        | x :: y :: xs ->
            // this is tail recursive assuming the `&&` operator is lazy.
            f x <= f y && isSortedBy_ f (y :: xs)
    
    type Sorted<'a> =
        
        val private internalList : 'a list
        
        private new(ls) = { internalList = ls }
        
        member this.list =
            this.internalList
        
        static member headTail (s : Sorted<'a>) : Option<'a * Sorted<'a>> =
            match s.list with
            | [] ->
                None
            | x :: xs ->
                Some (x , Sorted xs)
        
        static member isSortedBy (f : 'a -> 'key) (ls : 'a list) : Option<Sorted<'a>> =
            isSortedBy_ f ls
            |> Option.ofBool
            |> Option.map (fun () -> Sorted ls)
        
        static member sortBy (f : 'a -> 'key) (ls : 'a list) : Sorted<'a> =
            Sorted( List.sortBy f ls )
        
        static member sortWith (f : 'a -> 'a -> int) (ls : 'a list) : Sorted<'a> =
            Sorted( List.sortWith f ls )
        
        static member sortByDescending (f : 'a -> 'key) (ls : 'a list) : Sorted<'a> =
            Sorted( List.sortByDescending f ls )
        
        static member distinctBy (f : 'a -> 'key) (s : Sorted<'a>) : Sorted<'a> =
            Sorted( List.distinctBy f s.list )
    
    let list (s : Sorted<'a>) : 'a list =
        s.list
    
    let headTail =
        Sorted.headTail
    
    let isSortedBy =
        Sorted.isSortedBy
    
    let isSorted (ls : 'a list) : Option<Sorted<'a>> =
        Sorted.isSortedBy id ls
    
    let sortBy =
        Sorted.sortBy
    
    let sort (ls : 'a list) : Sorted<'a> =
        sortBy id ls
    
    let sortWith =
        Sorted.sortWith
    
    let sortByDescending =
        Sorted.sortByDescending
    
    let sortDescending (ls : 'a list) : Sorted<'a> =
        sortByDescending id ls
    
    let rec private isDistinctBy_ (f : 'a -> 'key) (s : Sorted<'a>) : bool =
        match headTail s with
        | None ->
            true
        | Some (x , tl) ->
            match headTail tl with
            | None ->
                true
            | Some (y , _) ->
                // this is tail recursive assuming the `&&` operator is lazy.
                f x <> f y && isDistinctBy_ f tl
    
    module Distinct =
        
        type Distinct< 'a > =
            
            val private internalList : Sorted<'a>
            
            private new(ls) = { internalList = ls }
            
            member this.sorted =
                this.internalList
            
            member this.list =
                this.internalList.list
            
            static member headTail (s : Distinct<'a>) : Option<'a * Distinct<'a>> =
                match headTail s.internalList with
                | None ->
                    None
                | Some ( x , xs ) ->
                    Some ( x , Distinct xs )
            
            static member isDistinctBy (f : 'a -> 'key) (s : Sorted<'a>) : Option<Distinct<'a>> =
                isDistinctBy_ f s
                |> Option.ofBool
                |> Option.map (fun () -> Distinct s)
            
            static member distinctBy (f : 'a -> 'key) (s : Sorted<'a>) : Distinct<'a> =
                Distinct( Sorted.distinctBy f s )
        
        let headTail =
            Distinct.headTail
        
        let isDistinctBy =
            Distinct.isDistinctBy
        
        let isDistinct (s : Sorted<'a>) : Option<Distinct<'a>> =
            isDistinctBy id s
        
        let distinctBy =
            Distinct.distinctBy
        
        let distinct (s : Sorted<'a>) : Distinct<'a> =
            distinctBy id s


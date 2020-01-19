module Infrastructure.Map

open Functional

module Option =
     
     let traverse (f : 'a -> 'b option) (m : Map<'k, 'a>) : Option<Map<'k, 'b>> =
          m
          |> Map.toList
          |> FSharpx.Option.mapM (pairBimap pair f >> uncurry Option.map)
          |> Option.map Map.ofList
     
     let sequence (m : Map<'k, Option<'a>>) : Option<Map<'k, 'a>> =
          traverse id m
     
     let foldM (f : 'state -> 'k -> 'a -> 'state option) (init : 'state) (m : Map<'k,'a>) : Option<'state> =
          m
          |> Map.toList
          |> FSharpx.Option.foldM (f >> uncurry) init

/// For each item in each value of the 1st map - replace it with its value in the 2nd map.
/// If an item doesn't exist in the 2nd map - omit it.
let aggregate (m1 : Map<'a, 'b list>) (m2 : Map<'b, 'c> ) : Map<'a, 'c list> =
     Map.map (konst <| List.choose (flip Map.tryFind m2)) m1

/// For each item in each value of the 1st map - replace it with its value in the 2nd map.
/// If an item doesn't exist in the 2nd map - return None.
let forceAggregate (m1 : Map<'a, 'b list>) (m2 : Map<'b, 'c> ) : Option<Map<'a, 'c list>> =
     Option.traverse (FSharpx.Option.mapM (flip Map.tryFind m2)) m1

/// Compute the product of 2 maps, which is a map where for each key which is common for both maps the value is
/// the pair of values of that key from both maps.
/// If a one of the maps have a key which the other map doesn't - it will be omitted. 
let product (m1 : Map<'a, 'b>) (m2 : Map<'a, 'c> ) : Map<'a, 'b * 'c> =
     let folder m key value1 =
          m2
          |> Map.tryFind key
          |> Option.map (fun value2 -> Map.add key (value1, value2) m)
          |> Option.defaultValue m
     Map.fold folder Map.empty m1

/// Compute the product of 2 maps, which is a map where for each key which is common for both maps the value is
/// the pair of values of that key from both maps.
/// If a one of the maps have a key which the other map doesn't - return None. 
let forceProduct (m1 : Map<'a, 'b>) (m2 : Map<'a, 'c> ) : Option<Map<'a, 'b * 'c>> =
     let folderM m key value1 =
          m2
          |> Map.tryFind key
          |> Option.map (fun value2 -> Map.add key (value1, value2) m)
     Option.foldM folderM Map.empty m1

let addIfSome (key : 'k) (value : Option<'a>) (m : Map<'k,'a>) : Map<'k,'a> =
     value
     |> Option.map (fun value -> Map.add key value m)
     |> Option.defaultValue m
module Zen.Float
//open Prims
open FSharp.Compatibility.OCaml.Pervasives


type float = Prims.float
type double = float
type t = float

let nan         : float = nan
let infinity    : float = infinity
let neg_infinity: float = neg_infinity
let max_float   : float = max_float
let min_float   : float = min_float
let epsion_float: float = epsilon_float

let ( ~-. ) : float -> float = ( ~-. )
let ( ~+. ) : float -> float = ( ~+. )
let ( +. )  : float -> float -> float = ( +. )
let ( -. )  : float -> float -> float = ( -. )
let ( *. )  : float -> float -> float = ( *. )
let ( /. )  : float -> float -> float = ( /. )
let ( ** )  : float -> float -> float = ( ** )

let sqrt      : float -> float = sqrt
let exp       : float -> float = exp
let log       : float -> float = log
let log10     : float -> float = log10
let ceil      : float -> float = ceil
let floor     : float -> float = floor
let abs_float : float -> float = abs
let mod_float : float -> float -> float = mod_float
//let frexp     : float -> float * int = FSharp.Compatibility.OCaml.Pervasives.frexp
let modf      : float -> float * float = modf
//let float_of_int : Prims.int -> float = FSharp.Compatibility.OCaml.Pervasives.float_of_int
//let truncate     : float -> int
//let int_of_float : float -> int = FSharp.Compatibility.OCaml.Pervasives.int_of_float

type fpclass = FSharp.Compatibility.OCaml.Pervasives.fpclass
let classify_float: float -> fpclass = classify_float

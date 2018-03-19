#light "off"
module Prims
open FSharp.Core.Operators.Checked
open FSharp.Compatibility.OCaml.Pervasives
module Obj = FSharp.Compatibility.OCaml.Obj

type int      = int64
type nonzero  = int
let ( + ) : int -> int -> int = ( + )
let ( - ) : int -> int -> int = ( - )
let ( * ) : int -> int -> int = ( * )
let op_Star = (*)
let ( / ) : int -> int -> int = ( / )
let ( <= ): int -> int -> bool = ( <= )
let ( >= ): int -> int -> bool = ( >= )
let ( < ) : int -> int -> bool = ( < )
let ( > ) : int -> int -> bool = ( > )
let ( mod ) : int -> int -> int = ( % )
let ( ~- )  : int -> int = ( ~- )
let abs     : int -> int = abs
let parse_int : string -> int = System.Int64.Parse
let to_string (x:int) : string = x.ToString()

(** Some misc. types defined in Prims *)
type unit      = Microsoft.FSharp.Core.unit
type bool      = Microsoft.FSharp.Core.bool
type 'a array  = 'a Microsoft.FSharp.Core.array
type string    = byte array
type exn       = Microsoft.FSharp.Core.exn

type 'a list   = 'a Microsoft.FSharp.Collections.list
let length (ls:'a list) : int =
    Microsoft.FSharp.Collections.List.length ls
    |> int64
type 'a option = 'a Microsoft.FSharp.Core.option

type range     = unit
type nat       = int
type pos       = int
type 'd b2t    = unit

type 'a squash = unit

type (' p, ' q) c_or =
  | Left of ' p
  | Right of ' q

type (' p, ' q) l_or = ('p, 'q) c_or squash

let uu___is_Left = function Left _ -> true | Right _ -> false

let uu___is_Right = function Left _ -> false | Right _ -> true

type (' p, ' q) c_and =
| And of ' p * ' q

type (' p, ' q) l_and = ('p, 'q) c_and squash

let uu___is_And _ = true


type c_True =
  | T

type l_True = c_True squash

let uu___is_T _ = true

type c_False = unit
(*This is how Coq extracts Inductive void := . Our extraction needs to be fixed to recognize when there
       are no constructors and generate this type abbreviation*)
type l_False = c_False squash

type (' p, ' q) l_imp = ('p -> 'q) squash

type (' p, ' q) l_iff = ((' p, ' q) l_imp, (' q, ' p) l_imp) l_and

type ' p l_not = (' p, l_False) l_imp

type (' a, ' p) l_Forall = unit

type (' a, ' p) l_Exists = unit


type (' p, ' q, 'dummyP) eq2 =  unit
type (' p, ' q, 'dummyP, 'dummyQ) eq3 =  unit

type prop     = Obj.t

type lex_t =
  | LexTop
  | LexCons of unit * Obj.t * lex_t
let (uu___is_LexTop : lex_t -> bool) =
  fun projectee  ->
    match projectee with | LexTop  -> true | uu____18 -> false

let (uu___is_LexCons : lex_t -> bool) =
  fun projectee  ->
    match projectee with | LexCons (a,_1,_2) -> true | uu____30 -> false

type 'Aprojectee __proj__LexCons__item__a = Obj.t
let (__proj__LexCons__item___1 :
  lex_t -> unit __proj__LexCons__item__a) =
  fun projectee  -> match projectee with | LexCons (a,_1,_2) -> _1
let (__proj__LexCons__item___2 : lex_t -> lex_t) =
  fun projectee  -> match projectee with | LexCons (a,_1,_2) -> _2

let cut = ()
let admit () = failwith "no admits"
let _assume () = ()
let _assert x = ()
let magic () = failwith "no magic"
let unsafe_coerce x = Obj.magic x
let op_Negation x = not x

let range_0 = ()
let range_of _ = ()
let mk_range _ _ _ _ _ = ()
let set_range_of x = x


(* for partially variants of the operators *)
let op_Equality x y = x = y
let op_disEquality x y = x<>y
let op_AmpAmp x y = x && y
let op_BarBar x y  = x || y
let uu___is_Nil l = l = [] (*consider redefining List.isEmpty as this function*)
let uu___is_Cons l = not (uu___is_Nil l)
let strcat x y = x ^ y

let string_of_bool = string_of_bool
let string_of_int = to_string

type ('a, 'b) dtuple2 =
  | Mkdtuple2 of 'a * 'b

let __proj__Mkdtuple2__item___1 x = match x with
  | Mkdtuple2 (x, _) -> x
let __proj__Mkdtuple2__item___2 x = match x with
  | Mkdtuple2 (_, x) -> x

let rec pow2 n =
  if n = 0L then
    1L
  else
    2L * pow2 (n - 1L)

let __proj__Cons__item__hd = List.head

let __proj__Cons__item__tl = List.tail

let min = min

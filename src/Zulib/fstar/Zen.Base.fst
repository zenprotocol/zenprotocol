module Zen.Base

val id(#a:Type): a -> a
let id(#_) x = x

val const_(#a #b:Type): a -> b -> a
let const_ #_ #_ x _ = x

(** [|>] is equivalent to F#'s [|>]. *)
val ( |> ) (#a #b:Type): a -> (a->b) -> b
let ( |> ) #_ #_ x f = f x

(** [<|] is equivalent to F#'s [<|].
    Note that [<|] is left associative!
    Use [@] for right associativity. *)
val ( <| ) (#a #b:Type): (a->b) -> a -> b
let ( <| ) #_ #_ f x = f x

(** [@] is equivalent to Haskell's [$].
    Note that [@] is right associative!
    Use [<|] for left associativity. *)
val ( @ ) (#a #b:Type): (a->b) -> a -> b
let ( @ ) #_ #_ f x = f x

(** [>>] is equivalent to F#'s [>>]. *)
val ( >> ) (#a #b #c:Type): (a->b)->(b->c)->(a->c)
let ( >> ) #_ #_ #_ f g = fun x -> g (f x)

(** [<<] is equivalent to F#'s [<<] or Haskell's [.].
    Note that [<<] is left associative!
    Use [@] for right associativity. *)
val ( << ) (#a #b #c:Type): (b->c)->(a->b)->(a->c)
let ( << ) #_ #_ #_ f g = fun x -> f (g x)

(** [@<<] is equivalent to F#'s [<<] or Haskell's [.].
    Note that [@<<] is right associative!
    Use [<<] for left associativity.*)
val ( @<< ) (#a #b #c:Type): (b->c)->(a->b)->(a->c)
let ( @<< ) #_ #_ #_ f g = fun x -> f (g x)

(** [flip] x y reverses the order of arguments. Named as in Haskell.
    [flip] is useful for writing 'pointfree' code.
    However, pointfree code is rarely useful.
    Use with caution. With great power comes great responsibility. *)
val flip(#a #b #c:Type): (a -> b -> c) -> b -> a -> c
let flip #_ #_ #_ f x y = f y x

(*) Robocow

        (___)
        |===|
 ________\_/
| |        |
# ||______||
  ||      ||
  ~~      ~~

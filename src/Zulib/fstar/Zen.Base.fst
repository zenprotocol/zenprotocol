module Zen.Base

(** [|>] is equivalent to F#'s [|>]. *)
unfold val ( |> ) (#a #b:Type): a -> (a->b) -> b
unfold let ( |> ) #_ #_ x f = f x

(** [<|] is equivalent to F#'s [<|].
    Note that [<|] is left associative!
    Use [@] for right associativity. *)
unfold val ( <| ) (#a #b:Type): (a->b) -> a -> b
unfold let ( <| ) #_ #_ f x = f x

(** [@] is equivalent to Haskell's [$].
    Note that [@] is right associative!
    Use [<|] for left associativity. *)
unfold val ( @ ) (#a #b:Type): (a->b) -> a -> b
unfold let ( @ ) #_ #_ f x = f x

(** [>>] is equivalent to F#'s [>>]. *)
unfold val ( >> ) (#a #b #c:Type): (a->b)->(b->c)->(a->c)
unfold let ( >> ) #_ #_ #_ f g = fun x -> g (f x)

(** [<<] is equivalent to F#'s [<<] or Haskell's [.].
    Note that [<<] is left associative!
    Use [@] for right associativity. *)
unfold val ( << ) (#a #b #c:Type): (b->c)->(a->b)->(a->c)
unfold let ( << ) #_ #_ #_ f g = fun x -> f (g x)

(** [@<<] is equivalent to F#'s [<<] or Haskell's [.].
    Note that [@<<] is right associative!
    Use [<<] for left associativity.*)
unfold val ( @<< ) (#a #b #c:Type): (b->c)->(a->b)->(a->c)
unfold let ( @<< ) #_ #_ #_ f g = fun x -> f (g x)

(** [flip] x y reverses the order of arguments. Named as in Haskell.
    [flip] is useful for writing 'pointfree' code.
    However, pointfree code is rarely useful.
    Use with caution. With great power comes great responsibility. *)
unfold val flip(#a #b #c:Type): (a -> b -> c) -> b -> a -> c
unfold let flip #_ #_ #_ f x y = f y x

(*) Robocow

        (___)
        |===|
 ________\_/
| |        |
# ||______||
  ||      ||
  ~~      ~~

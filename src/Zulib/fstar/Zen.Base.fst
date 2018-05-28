module Zen.Base

val id(#a:Type): a -> a
let id(#_) x = x

val const_(#a #b:Type): a -> b -> a
let const_ #_ #_ x _ = x

val cast: a:Type -> a -> a
let cast a x = x

val cast_id: a:Type -> x:a
    -> Lemma (cast a x == x)
       [SMTPat (cast a x)]
let cast_id _ _ = ()

val retype(#a:Type): b:Type{b==a} -> a -> b
let retype #_ _ x = x

val retype_id(#a:Type): b:Type{b==a} -> x:a
    -> Lemma (retype b x == x)
       [SMTPat (retype b x)]
let retype_id #_ _ _ = ()

val refine(#a:Type):
    p:(a -> prop)
    -> x:a
    -> Pure (result:a{p result})
            (requires (p x))
            (ensures (fun _ -> True))
let refine #_ _ x = x

val refineType: a:Type u#t -> p:(a -> prop) -> Type u#t
let refineType a p = x:a{p x}

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

val swap(#a #b:Type): a**b -> b**a
let swap #_ #_ (x,y) = y,x

val curry(#a #b #c:Type): (a**b -> c) -> a -> b -> c
let curry #_ #_ #_ f x y = f (x,y)

val uncurry(#a #b #c:Type): (a -> b -> c) -> a**b -> c
let uncurry #_ #_ #_ f = fun (x,y) -> f x y

val curry3(#a #b #c #d:Type): (a**b**c -> d) -> a -> b -> c -> d
let curry3 #_ #_ #_ #_ f x y z = f (x,y,z)

val uncurry3(#a #b #c #d:Type): (a->b->c->d) -> a**b**c -> d
let uncurry3 #_ #_ #_ #_ f = fun (x,y,z) -> f x y z

val curry4(#a #b #c #d #e:Type): (a**b**c**d -> e) -> a -> b -> c -> d -> e
let curry4 #_ #_ #_ #_ #_ f w x y z = f (w,x,y,z)

val uncurry4(#a #b #c #d #e:Type): (a->b->c->d->e) -> a**b**c**d -> e
let uncurry4 #_ #_ #_ #_ #_ f = fun (w,x,y,z) -> f w x y z

(*) Robocow

        (___)
        |===|
 ________\_/
| |        |
# ||______||
  ||      ||
  ~~      ~~

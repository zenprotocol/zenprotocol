module Zen.Optics

open Zen.Base
open Zen.Option

// A `lens a b` focuses on the `b` component of `a`
// It provides a `get` to access the component
// And a `put` to update and `a` by updating its `b` component
noeq type lens (a:Type) (b:Type) = {
  lget: a -> b;
  lset: b -> a -> a
}

val lget(#a #b: Type): lens a b -> a -> b
let lget #_ #_ l = l.lget

val lset(#a #b: Type): lens a b -> b -> a -> a
let lset #_ #_ l = l.lset

// A `prism a b` focuses on the `b` component of `a`
// It provides a `get` to access the component
// And a `put` to update and `a` by updating its `b` component
noeq type prism (a:Type) (b:Type) = {
    pget: a -> option b;
    pset: b -> a -> a
}

val pget(#a #b: Type): prism a b -> a -> option b
let pget #_ #_ p = p.pget

val pset(#a #b: Type): prism a b -> b -> a -> a
let pset #_ #_ p = p.pset

(** lens-lens composition *)
val ( |-- ) (#a #b #c:Type): lens a b -> lens b c -> lens a c
let ( |-- ) #_ #_ #_ l1 l2 = {
  lget = l1.lget >> l2.lget;
  lset = (fun y x -> x |> l1.lset (l1.lget x |> l2.lset y))
}
unfold let llcompose = ( |-- )

(** lens-prism composition *)
val ( |-? ) (#a #b #c:Type): lens a b -> prism b c -> prism a c
let ( |-? ) #_ #_ #_ l p = {
  pget = l.lget >> p.pget;
  pset = (fun y x -> x |> l.lset (l.lget x |> p.pset y))
}
unfold let lpcompose = ( |-? )

(** prism-lens composition *)
val ( |?- ) (#a #b #c:Type): prism a b -> lens b c -> prism a c
let ( |?- ) #_ #_ #_ p l = {
  pget = (fun x -> l.lget `map` p.pget x);
  pset = (fun y x -> match p.pget x with
                     | None -> x
                     | Some v -> x |> p.pset (l.lset y v))
}
unfold let plcompose = ( |?- )

(** prism-prism composition *)
val ( |?? ) (#a #b #c:Type): prism a b -> prism b c -> prism a c
let ( |?? ) #_ #_ #_ p1 p2 = {
  pget = p1.pget >=> p2.pget;
  pset = (fun y x -> match p1.pget x with
                     | None -> x
                     | Some v -> x |> p1.pset (p2.pset y v))
}
unfold let ppcompose = ( |?? )

module Zen.ErrorT

open Zen.Cost
open Zen.Base

module E = Zen.Error

val fail: exn -> cost (result 'a) 0
let fail e = ret (E.fail e)

val failw: string -> cost (result 'a) 0
let failw msg = ret (E.failw msg)

val ret(#a:Type): a -> cost (result a) 0
let ret(#_) x = ret (OK x)

val retT(#a:Type)(#n:nat): cost a n -> cost (result a) n
let retT #_ #_ mx =
   mx >>= ret

val incFail(#a:Type): n:nat -> exn -> cost (result a) n
let incFail #_ n e = inc (fail e) n

val incFailw(#a:Type): n:nat -> string -> cost (result a) n
let incFailw #_ n msg = inc (failw msg) n

val incRet(#a:Type): n:nat -> a -> cost (result a) n
let incRet(#_) n x = inc (ret x) n

val autoFailw(#a:Type)(#n:nat): string -> cost (result a) n
let autoFailw #_ #_ = failw >> autoInc

val autoFail(#a:Type)(#n:nat): exn -> cost (result a) n
let autoFail #_ #_ = fail >> autoInc

val autoRet(#a:Type)(#n:nat): a -> cost (result a) n
let autoRet #_ #_ = ret >> autoInc

val bind(#a #b:Type)(#m #n:nat):
  cost (result a) m
  -> (a -> cost (result b) n)
  -> cost (result b) (m+n)
let bind #_ #_ #_ #n mx f =
  mx >>= (function
  | OK x -> f x
  | EX e -> fail e `inc` n
  | ERR msg -> failw msg `inc` n)

val map(#a #b:Type)(#n:nat): (a -> b) -> cost (result a) n
  -> cost (result b) n
let map #_ #_ #_ f mx =
  bind mx (ret << f)

val ap(#a #b:Type)(#m #n:nat): cost (result (a->b)) m -> cost (result a) n
  -> cost (result b) (n+m)
let ap #_ #_ #_ #_ mf mx =
  bind mf (fun f -> f `map` mx)

val of_option(#a:Type): string -> option a -> cost (result a) 0
let of_option(#_) msg = function
  | Some v -> ret v
  | None -> failw msg

val of_optionT(#a:Type)(#n:nat): string -> cost (option a) n -> cost (result a) n
let of_optionT #_ #_ msg mx = Zen.Cost.bind mx (of_option msg)

module Zen.String

open FStar.String

type t = string

val str_access: s:string -> n:nat
    -> Lemma (n < length s ==> length (s `at` n) == 1)
let str_access _ _ = ()

val memType0(#a:Type): a -> l:list a{Prims.length l > 0} -> GTot Type0
let rec memType0 #_ x = function
    | [hd] -> x == hd
    | hd::tl -> x == hd \/ memType0 x tl


val is_base_16_char: s:string{length s == 1} -> GTot Type0
let is_base_16_char s =
    memType0 s ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9";
                "a";"b";"c";"d";"e";"f";
                "A";"B";"C";"D";"E";"F"]

type base16 = s:string
              { forall (n:nat{n < length s}).
                  is_base_16_char (s `at` n) }

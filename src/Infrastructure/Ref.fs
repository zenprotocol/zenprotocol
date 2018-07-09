module Infrastructure.Ref

type 'a ref = { mutable contents : 'a }
let ref v = { contents = v }      (* val ref  : 'a -> 'a ref *)
let (!) r = r.contents            (* val (!)  : 'a ref -> 'a *)
let (:=) r v = r.contents <- v    (* val (:=) : 'a ref -> 'a -> unit *)
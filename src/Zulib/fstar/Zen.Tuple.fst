module Zen.Tuple

val curry(#a #b #c:Type): (a**b -> c) -> a -> b -> c
let curry #_ #_ #_ f x y = f (x,y)

val uncurry(#a #b #c:Type): (a -> b -> c) -> a**b -> c
let uncurry #_ #_ #_ f = fun (x,y) -> f x y

val swap(#a #b:Type): a**b -> b**a
let swap #_ #_ (x,y) = y,x

val curry3(#a #b #c #d:Type): (a**b**c -> d) -> a -> b -> c -> d
let curry3 #_ #_ #_ #_ f x y z = f (x,y,z)

val uncurry3(#a #b #c #d:Type): (a->b->c->d) -> a**b**c -> d
let uncurry3 #_ #_ #_ #_ f = fun (x,y,z) -> f x y z

val curry4(#a #b #c #d #e:Type): (a**b**c**d -> e) -> a -> b -> c -> d -> e
let curry4 #_ #_ #_ #_ #_ f w x y z = f (w,x,y,z)

val uncurry4(#a #b #c #d #e:Type): (a->b->c->d->e) -> a**b**c**d -> e
let uncurry4 #_ #_ #_ #_ #_ f = fun (w,x,y,z) -> f w x y z

// tuple map
val map(#a #b:Type): (a->b) -> a**a -> b**b
let map #_ #_ f (x,y) = (f x, f y)

val map3(#a #b:Type): (a->b) -> a**a**a -> b**b**b
let map3 #_ #_ f (x,y,z) = (f x, f y, f z)

val map4(#a #b:Type): (a->b) -> a**a**a**a -> b**b**b**b
let map4 #_ #_ f (w,x,y,z) = (f w, f x, f y, f z)

(** [x |*> (f,g)] is equivalent to [(f x, g x)] *)
val ( |*> ) (#a #b #c:Type): a -> (a->b)**(a->c) -> b**c
let ( |*> ) #_ #_ #_ x (f,g) = f x, g x
(** [x |**> (f,g,h)] is equivalent to [(f x, g x, h x)] *)
val ( |**> ) (#a #b #c #d:Type): a -> (a->b)**(a->c)**(a->d) -> b**c**d
let ( |**> ) #_ #_ #_ #_ x (f,g,h) = f x, g x, h x
(** [x |***> (f1,f2,f3,f4)] is equivalent to [(f1 x, f2 x, f3 x, f4 x)] *)
val ( |***> ) (#a #b #c #d #e:Type): a -> (a->b)**(a->c)**(a->d)**(a->e) -> b**c**d**e
let ( |***> ) #_ #_ #_ #_ #_ x (f1,f2,f3,f4) = f1 x, f2 x, f3 x, f4 x

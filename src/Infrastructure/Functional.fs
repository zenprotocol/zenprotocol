module Infrastructure.Functional

let flip f x y = f y x

let diag x = (x,x)

let pairBimap f g (x,y) = (f x, g y)

let firstMap f (x,y) = (f x, y)

let secondMap g (x,y) = (x, g y)

let (<|*|>) f g x = (f x, g x)

let swap (x,y) = (y,x)

let konst x _ = x

let curry f x y = f (x,y)

let uncurry f (x,y) = f x y

let pair x y = (x,y)
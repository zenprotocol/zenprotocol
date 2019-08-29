module Infrastructure.Functional

let flip f x y = f y x

let diag x = (x,x)

let pairBimap f g (x,y) = (f x, g y)

let (<|*|>) f g x = (f x, g x)

let swap (x,y) = (y,x)

let konst x _ = x

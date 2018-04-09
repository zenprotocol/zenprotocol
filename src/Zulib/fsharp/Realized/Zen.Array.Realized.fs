#light "off"
module Zen.Array.Realized

open Microsoft.FSharp.Core.Operators.Checked // For checked casts

module Arr = Microsoft.FSharp.Collections.Array
module Cost = Zen.Cost.Realized

type array<'Aa, 'An> = array<'Aa>

let init ( _: Prims.nat)
         ( l: Prims.nat)
         ( f: Prims.nat -> Cost.t<'Aa, Prims.unit>)
         : Cost.t< array<'Aa, Prims.unit>, Prims.unit> =
    let init_index : int -> 'Aa =
      int64 >> f >> Cost.__force in
    lazy (Arr.init (int l) init_index)
    |> Cost.C

let empty ( _: Prims.unit)
          : array<'Aa, Prims.unit> = [||]

let item ( _: Prims.nat)
         ( i : Prims.nat)
         ( arr : array<'Aa, Prims.unit> )
         : Cost.t<'Aa, Prims.unit> =
    lazy ( arr.[int i] )
    |> Cost.C

let init_unique (_:array<'a, unit>) : unit = ()
let init_item (_:Prims.nat) (_:Prims.nat)
              (_:(Prims.nat -> Cost.t<'a, unit>))
              (_:Prims.nat)
              : unit = ()

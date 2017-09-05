#light "off"
module Zen.Crypto

module Cost = Zen.Cost.Realized
module ZArr = Zen.Array.Extracted

type hash      = ZArr.t<FStar.UInt8.t, Prims.unit>
type signature = ZArr.t<FStar.UInt8.t, Prims.unit>
type key       = ZArr.t<FStar.UInt8.t, Prims.unit>

let sha2_256 ( _: Prims.nat)
             ( a : ZArr.t<FStar.UInt8.byte, Prims.unit> )
             : Cost.t< ZArr.t<FStar.UInt8.byte, Prims.unit>, Prims.unit> =
    lazy (
      let sha = System.Security.Cryptography.SHA256.Create() in
      sha.ComputeHash a)
    |> Zen.Cost.Realized.C


let sha2_512 ( _: Prims.nat)
             ( a : ZArr.t<FStar.UInt8.byte, Prims.unit> )
             : Cost.t< ZArr.t<FStar.UInt8.byte, Prims.unit>, Prims.unit> =
    lazy (
      let sha = System.Security.Cryptography.SHA256.Create() in
      sha.ComputeHash a)
    |> Zen.Cost.Realized.C

let sign ( _: Prims.nat)
         ( msg : ZArr.t<FStar.UInt8.byte, Prims.unit> )
         ( key : ZArr.t<FStar.UInt8.byte, Prims.unit> )
         : Cost.t< ZArr.t<FStar.UInt8.byte, Prims.unit>, Prims.unit> =
    lazy ( Sodium.PublicKeyAuth.SignDetached(msg, key) )
    |> Zen.Cost.Realized.C

let verify ( _: Prims.nat)
           ( msg  : ZArr.t<FStar.UInt8.byte, Prims.unit> )
           ( sign : ZArr.t<FStar.UInt8.byte, Prims.unit> )
           ( key  : ZArr.t<FStar.UInt8.byte, Prims.unit> )
           : Prims.bool =
    Sodium.PublicKeyAuth.VerifyDetached(sign, msg, key)

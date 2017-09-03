#light "off"
module Zen.Crypto

type hash = Zen.Array.t<FStar.UInt8.t, Prims.unit>
type signature = Zen.Array.t<FStar.UInt8.t, Prims.unit>
type key = Zen.Array.t<FStar.UInt8.t, Prims.unit>

let sha2_256 (_:Prims.nat)
  (Zen.ArrayRealized.A a:Zen.Array.t<FStar.UInt8.byte, Prims.unit>)
  : Zen.Cost.cost<Zen.Array.t<FStar.UInt8.byte, Prims.unit>,Prims.unit> =
  let sha = System.Security.Cryptography.SHA256.Create() in
  sha.ComputeHash a |> Zen.ArrayRealized.A |> Zen.Cost.ret


let sha2_512 (_:Prims.nat)
  (Zen.ArrayRealized.A a:Zen.Array.t<FStar.UInt8.byte, Prims.unit>)
  : Zen.Cost.cost<Zen.Array.t<FStar.UInt8.byte, Prims.unit>, Prims.unit> =
  let sha = System.Security.Cryptography.SHA512.Create() in
  sha.ComputeHash a |> Zen.ArrayRealized.A |> Zen.Cost.ret

let sign (_:Prims.nat)
  (Zen.ArrayRealized.A msg:Zen.Array.t<FStar.UInt8.byte, Prims.unit>)
  (Zen.ArrayRealized.A key:Zen.Array.t<FStar.UInt8.byte, Prims.unit>)
  : Zen.Cost.cost<Zen.Array.t<FStar.UInt8.byte, Prims.unit>,Prims.unit> =
  Sodium.PublicKeyAuth.SignDetached(msg, key)
  |> Zen.ArrayRealized.A |> Zen.Cost.ret

let verify (_:Prims.nat)
  (Zen.ArrayRealized.A  msg:Zen.Array.t<FStar.UInt8.byte, Prims.unit>)
  (Zen.ArrayRealized.A sign:Zen.Array.t<FStar.UInt8.byte, Prims.unit>)
  (Zen.ArrayRealized.A  key:Zen.Array.t<FStar.UInt8.byte, Prims.unit>)
  : Prims.bool =
  Sodium.PublicKeyAuth.VerifyDetached(sign, msg, key)
#light "off"
module Zen.Crypto

module Cost = Zen.Cost.Realized
module ZArr = Zen.Array.Extracted

module ZenTypes = Zen.Types.Extracted

type hash      = ZenTypes.hash
type signature = ZenTypes.signature
type key       = ZenTypes.key

let sha2_256 ( _: Prims.nat )
             (   msg: ZArr.t<ZenTypes.byte, 'l> )
             : Cost.t<ZArr.t<ZenTypes.byte, 'l>, 'n> =
    lazy ( let sha = System.Security.Cryptography.SHA256.Create() in
           sha.ComputeHash msg )
    |> Zen.Cost.Realized.C


let sha2_512 ( _: Prims.nat )
             (   msg: ZArr.t<ZenTypes.byte, 'l> )
             : Cost.t<ZArr.t<ZenTypes.byte, 'l>, 'n> =
    lazy ( let sha = System.Security.Cryptography.SHA256.Create() in
           sha.ComputeHash msg )
    |> Zen.Cost.Realized.C

let sign ( _: Prims.nat )
         ( msg: ZArr.t<ZenTypes.byte, 'l1> )
         ( key: key )
              : signature =
    Sodium.PublicKeyAuth.SignDetached(msg, key)

let verify ( _: Prims.nat )
           ( msg: ZArr.t<ZenTypes.byte, 'l> )
           ( signature: signature )
           ( pubkey: key )
           : Prims.bool =
    Sodium.PublicKeyAuth.VerifyDetached(signature, msg, pubkey)

module Zen.Sha3.Realized

let hash256 : byte[] -> byte[] =
    fun bs ->
    let res = Array.zeroCreate 32 in
    let sha3 = new Org.BouncyCastle.Crypto.Digests.Sha3Digest(256) in
    sha3.BlockUpdate(bs,0,Array.length bs);
    sha3.DoFinal(res, 0) |> ignore;
    res

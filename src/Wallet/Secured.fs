module Wallet.Secured

open Consensus.Crypto
open Infrastructure.Result
open Infrastructure.Crypto.SecretBox

type T = {
    cipher: byte[]
    iv: byte[]
}

let create secret key =
    let iv = generateIV
    create secret key iv
    <@> fun cipher ->
    {
        cipher = cipher
        iv = iv
    }
    
let decrypt key secured =
    openBox secured.cipher key secured.iv
    <@> SecretKey
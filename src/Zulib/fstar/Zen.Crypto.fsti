module Zen.Crypto

open Zen.Types
open Zen.Cost

val verify:
  publicKey ->
  signature ->
  msg:hash ->
  bool `cost` 800

val parsePublicKey:
  string ->

  option publicKey `cost` 120

val parseHash: string -> option hash `cost` 32

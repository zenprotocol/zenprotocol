module Zen.Types.Extracted

module      A = Zen.Array
module      V = Zen.Vector
module     U8 = FStar.UInt8
module    U32 = FStar.UInt32
module    U64 = FStar.UInt64

type byte = U8.byte
type opcode = U8.t
type hash      = A.t U8.byte 32
type signature = A.t U8.byte 64
type key       = A.t U8.byte 64
type asset = hash
type contractHash = hash
type witness (n:nat) = A.t byte n
type nonce = U64.t * U64.t

type outpoint =
    { txHash: hash;
      index: U32.t }

type spend =
    { asset: asset;
      amount: U64.t }

type lock =
    | PKLock of hash
    | ContractLock of contractHash
    | DestroyLock

type output =
    { lock: lock;
      spend: spend }


unopteq type blockHeader = {
    version: U32.t;
    parent: hash;
    blockNumber: U32.t;
    commitments: hash;
    timestamp: U64.t;
    difficulty: U32.t;
    nonce: nonce;
    }

noeq type data : nat -> Type =
  | Bool: v:bool -> data 1
  | Byte: v:U8.t -> data 1
  | Empty: data 0
  | Hash: v:hash -> data 1
  | Key: v:key -> data 1
  | Outpoint: v:outpoint -> data 1
  | Output: v:output -> data 1
  | Sig: v:signature -> data 1
  | UInt8 : v:U8.t -> data 1
  | UInt32: v:U32.t -> data 1
  | UInt64: v:U64.t -> data 1
  | BoolVector: l:nat -> v: V.t bool l -> data l
  | BoolArray:  l:nat -> a: A.t bool l -> data l
  | ByteVector: l:nat -> v: V.t U8.t l -> data l
  | ByteArray:  l:nat -> a: A.t U8.t l -> data l
  | HashVector: l:nat -> v: V.t hash l -> data l
  | HashArray:  l:nat -> a: A.t hash l -> data l
  | KeyVector: l:nat -> v: V.t key l -> data l
  | KeyArray:  l:nat -> a: A.t key l -> data l
  | SigVector: l:nat -> v: V.t signature l -> data l
  | SigArray:  l:nat -> a: A.t signature l -> data l
  | OutpointVector: l:nat -> v: V.t outpoint l -> data l
  | OutpointArray:  l:nat -> a: A.t outpoint l -> data l
  | OutputVector: l:nat -> v: V.t output l -> data l
  | OutputArray:  l:nat -> a: A.t output l -> data l
  | UInt8Vector: l:nat -> v: V.t U8.t l -> data l
  | UInt8Array:  l:nat -> a: A.t U8.t l -> data l
  | UInt32Vector: l:nat -> v: V.t U32.t l -> data l
  | UInt32Array:  l:nat -> a: A.t U32.t l -> data l
  | UInt64Vector: l:nat -> v: V.t U64.t l -> data l
  | UInt64Array:  l:nat -> a: A.t U64.t l -> data l
  | Optional: l:nat -> option (data l) -> data (l+1)
  | Data2: n1:nat -> n2:nat -> _1:data n1 -> _2:data n2 -> data (n1+n2)
  | Data3: n1:nat -> n2:nat -> n3:nat -> _1:data n1 -> _2:data n2 -> _3:data n3 -> data (n1+n2+n3)
  | Data4: n1:nat -> n2:nat -> n3:nat -> n4:nat -> _1:data n1 -> _2:data n2 -> _3:data n3 -> _4:data n4 -> data (n1+n2+n3+n4)

type inputData (n:nat) = data n

type utxo = outpoint -> option output

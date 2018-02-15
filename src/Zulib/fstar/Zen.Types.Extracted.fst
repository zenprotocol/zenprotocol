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
type contractHash = hash
type asset = contractHash ** hash
type witness (n:nat) = A.t byte n
type nonce = U64.t ** U64.t

type outpoint =
    { txHash: hash;
      index: U32.t }

type spend =
    { asset: asset;
      amount: U64.t }

type lock =
    | PKLock of hash
    | ContractLock of contractHash
    | FeeLock
    | DestroyLock

type output =
    { lock: lock;
      spend: spend }

type pointedOutput = outpoint ** output

type input =
    | PointedOutput of pointedOutput
    | Mint of spend

unopteq type blockHeader = {
    version: U32.t;
    parent: hash;
    blockNumber: U32.t;
    commitments: hash;
    timestamp: U64.t;
    difficulty: U32.t;
    nonce: nonce }

type wallet (n:nat) = pointedOutput `V.t` n

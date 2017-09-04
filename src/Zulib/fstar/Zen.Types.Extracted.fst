module Zen.Types.Extracted

module      A = Zen.Array
module      V = Zen.Vector
module     U8 = FStar.UInt8
module    U32 = FStar.UInt32
module    U64 = FStar.UInt64
module Realized = Zen.Types.Realized
module Crypto = Zen.Crypto

type lockCore = Realized.lockCore
type contract = Realized.contract
type extendedContract = Realized.extendedContract
type extraData = Realized.extraData

type byte = U8.byte
type opcode = U8.t
type hash : eqtype = Zen.Crypto.hash
type spend = {asset: hash; amount: U64.t}
type outpoint = {txHash: hash; index: U32.t}
type witness (n:nat) = A.t byte n
type nonce = A.t byte 64

unopteq type blockHeader = {
    version: U32.t;
    parent: hash;
    blockNumber: U32.t;
    txMerkleRoot: hash;
    witnessMerkleRoot: hash;
    contractMerkleRoot: hash;
    extraData: extraData;
    timestamp: FStar.Int64.t;
    pdiff: U32.t;
    nonce: nonce
    }

noeq type data : nat -> Type =
  | Bool: v:bool -> data 1
  | Byte: v:U8.t -> data 1
  | Empty: data 0
  | Hash: v:hash -> data 1
  | Key: v:Crypto.key -> data 1
  | Outpoint: v:outpoint -> data 1
  | Output: v:output -> data 1
  | OutputLock: v:outputLock -> data 1
  | Sig: v:Crypto.signature -> data 1
  | UInt8 : v:U8.t -> data 1
  | UInt32: v:U32.t -> data 1
  | UInt64: v:U64.t -> data 1
  | BoolVector: l:nat -> v: V.t bool l -> data l
  | BoolArray:  l:nat -> a: A.t bool l -> data l
  | ByteVector: l:nat -> v: V.t U8.t l -> data l
  | ByteArray:  l:nat -> a: A.t U8.t l -> data l
  | HashVector: l:nat -> v: V.t hash l -> data l
  | HashArray:  l:nat -> a: A.t hash l -> data l
  | KeyVector: l:nat -> v: V.t Crypto.key l -> data l
  | KeyArray:  l:nat -> a: A.t Crypto.key l -> data l
  | SigVector: l:nat -> v: V.t Crypto.signature l -> data l
  | SigArray:  l:nat -> a: A.t Crypto.signature l -> data l
  | OutpointVector: l:nat -> v: V.t outpoint l -> data l
  | OutpointArray:  l:nat -> a: A.t outpoint l -> data l
  | OutputVector: l:nat -> v: V.t output l -> data l
  | OutputArray:  l:nat -> a: A.t output l -> data l
  | OutputLockVector: l:nat -> v: V.t outputLock l -> data l
  | OutputLockArray:  l:nat -> a: A.t outputLock l -> data l
  | UInt8Vector: l:nat -> v: V.t U8.t l -> data l
  | UInt8Array:  l:nat -> a: A.t U8.t l -> data l
  | UInt32Vector: l:nat -> v: V.t U32.t l -> data l
  | UInt32Array:  l:nat -> a: A.t U32.t l -> data l
  | UInt64Vector: l:nat -> v: V.t U64.t l -> data l
  | UInt64Array:  l:nat -> a: A.t U64.t l -> data l
  | Data2: n1:nat -> n2:nat -> _1:data n1 -> _2:data n2 -> data (n1+n2)
  | Data3: n1:nat -> n2:nat -> n3:nat -> _1:data n1 -> _2:data n2 -> _3:data n3 -> data (n1+n2+n3)
  | Data4: n1:nat -> n2:nat -> n3:nat -> n4:nat -> _1:data n1 -> _2:data n2 -> _3:data n3 -> _4:data n4 -> data (n1+n2+n3+n4)

(*
and dataContainer : nat -> Type =
  | Single: n:nat -> v:data n -> dataContainer n
  | Data2: n1:nat -> n2:nat -> _1:data n1 -> _2:data n2 -> dataContainer (n1+n2)
  | Data3: n1:nat -> n2:nat -> n3:nat -> _1:data n1 -> _2:data n2 -> _3:data n3 -> dataContainer (n1+n2+n3)
  | Data4: n1:nat -> n2:nat -> n3:nat -> n4:nat _1:data n1 -> _2:data n2 -> _3:data n3 -> _4:data n4 -> dataContainer (n1+n2+n3+n4)
  | Cont2: _1:dataContainer -> _2:dataContainer -> dataContainer
  | Cont3: _1:dataContainer -> _2:dataContainer -> _3:dataContainer -> dataContainer
  | Cont4: _1:dataContainer -> _2:dataContainer -> _3:dataContainer -> _4:dataContainer -> dataContainer
//noeq type lockCore (n:nat) = {version: U32.t; lockData: list (A.t  byte n)}
*)

and outputLock =
  | CoinbaseLock of lockCore
  | FeeLock of lockCore
  | ContractSacrificeLock of lockCore
  | PKLock of pkHash: hash
  | ContractLock: contractHash:hash -> n:nat -> data n -> outputLock
  | HighVLock: lockcore:lockCore -> typeCode:nat -> outputLock
and output = {lock: outputLock; spend: spend}
(*)
type inputData (n:nat) = data n

unopteq type transactionSkeleton = | Tx: l1:nat -> outpoints:V.t outpoint l1
                                      -> l2:nat -> outputs:  V.t output l2
                                      -> l3:nat -> data:data l3
                                      -> transactionSkeleton

type utxo = outpoint -> option output

unopteq type inputMsg = {
  cmd: opcode;
  data: n:nat & inputData n;
  contractHash: hash;
  utxo: utxo;
  lastTx: option outpoint
  }


//val contractVersion : extendedContract -> U32.t

//let contractHashBytes = 32ul
//let pubKeyHashBytes = 32ul
//let txHashBytes = 32ul

//type Transaction = {version: uint32; inputs: Outpoint list; witnesses: Witness list; outputs: Output list; contract: Contract option}
(*
type transaction = {version: U32.t;
  inputs: list outpoint;
  witnesses: list witness;
  outputs: list output;
  contract: option extendedContract}
*)

//type contractContext = {contractId: byte[]; utxo: Map<outpoint, output>; tip: blockHeader; }

//TODO: Review merkleData
//val merkleData : blockHeader -> list

(*
type block = {
    header: blockHeader;
    transactions: transaction list
    }
*)

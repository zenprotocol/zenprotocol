namespace Consensus.Tests

open FsCheck
open Consensus
open Types
open Crypto
open Zen.Types.Data

type UniqueHashes = UniqueHashes of list<Hash.Hash>

type ThreeBytesHash = ThreeBytesHash of Hash.Hash

type LeadingZerosHash = LeadingZerosHash of Hash.Hash

type NonEmptyTransactions = NonEmptyTransactions of list<Transaction> with
    static member op_Explicit(NonEmptyTransactions txs) = txs


type ConsensusGenerator =
    static member BytesGenerator() =
        Gen.arrayOf Arb.generate<byte>
        |> Gen.filter ((<>) null)
        |> Gen.filter ((<>) [||])
        |> Arb.fromGen

    static member Block() =
        gen {
            let! transactions =
                Arb.generate<Transaction list>
                |> Gen.map (fun txs -> List.distinct txs)

            let! parentHash = Arb.generate<Hash.Hash>
            let! timestamp = Arb.generate<uint64>
            let! coinbasePkHash = Arb.generate<Hash.Hash>
            let! blockNumber = Arb.generate<uint32> |> Gen.filter(fun i -> i > 1ul)
            let reward = Block.blockReward blockNumber

            let coinbase = {
                version = Version0
                inputs=[]
                outputs=
                    [
                        {
                            lock=Coinbase (blockNumber, coinbasePkHash)
                            spend={asset=Asset.Zen;amount=reward}
                        }
                    ]
                contract = None
                witnesses=[]
            }

            let transactions = coinbase :: transactions

            let txMerkleRoot =
                transactions
                |> List.map Transaction.hash
                |> MerkleTree.computeRoot

            let witnessMerkleRoot =
                transactions
                |> List.map Transaction.witnessHash
                |> MerkleTree.computeRoot

            let acsMerkleRoot = SparseMerkleTree.root ActiveContractSet.empty

            let commitments =
                [ txMerkleRoot; witnessMerkleRoot; acsMerkleRoot; ]
                |> MerkleTree.computeRoot

            let header =
                {
                    version=Version0;
                    parent=parentHash;
                    blockNumber=blockNumber;
                    commitments=commitments;
                    timestamp=timestamp;
                    difficulty=0x20fffffful;
                    nonce=0UL,0UL;
                }

            return {header=header;transactions=transactions;commitments=[];txMerkleRoot=txMerkleRoot;witnessMerkleRoot=witnessMerkleRoot;activeContractSetMerkleRoot=acsMerkleRoot}
        }

    static member Transactions() =
        Arb.from<Transaction list>
        |> Arb.mapFilter List.distinct (fun txs -> List.length txs > 0)
        |> Arb.convert NonEmptyTransactions NonEmptyTransactions.op_Explicit

    static member Hashes() =
        Arb.from<Hash.Hash list>
            |> Arb.mapFilter List.distinct (fun xs -> List.length xs > 0)
            |> Arb.convert UniqueHashes (fun (UniqueHashes xs) -> xs)

    static member HashGenerator() =
        Arb.fromGen (gen {
             let! hash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
             return (Hash.Hash hash)
        })

    static member ThreeBytesHashGenerator() =
         Arb.fromGen (gen {
                     let! size = Gen.choose (1,3)
                     let! hash =
                        Gen.arrayOfLength size Arb.generate<byte>
                        |> Gen.filter ((<>) [|0uy;0uy;0uy;|])
                        |> Gen.filter ((<>) [|0uy;0uy;|])
                        |> Gen.filter ((<>) [|0uy;|])

                     return ThreeBytesHash (Hash.Hash (Array.append (Array.zeroCreate (Hash.Length - size)) hash))
                })

    static member LeadingZerosHashGenerator() =
        Arb.fromGen (gen {
            let! leadingZerosLength = Gen.choose (0,28)
            let leadingZeros = Array.zeroCreate leadingZerosLength

            let! rest = Gen.arrayOfLength (Hash.Length - leadingZerosLength) Arb.generate<byte>

            return LeadingZerosHash (Hash.Hash (Array.append leadingZeros rest))
        })

    static member DataGenerator() =
        let makeDtuple arr =
            Prims.Mkdtuple2 (int64 (Array.length arr), arr)
        let array gen1 =
            gen {
                let! len = Arb.generate<int32>
                let! arr = Gen.arrayOfLength len gen1

                return arr
            }

        let i64 = Arb.generate<int64>
        let i64Gen =
            gen {
                let! a = i64
                return I64 a
            }
        let i64ArrayGen =
            gen {
                let! arr = array i64
                return I64Array (makeDtuple arr)
            }
        let byte = Arb.generate<byte>
        let byteGen =
            gen {
                let! a = byte
                return Byte a
            }
        let byteArrayGen =
            gen {
                let! arr = array byte
                return ByteArray (makeDtuple arr)
            }
        let u32 = Arb.generate<uint32>
        let u32Gen =
            gen {
                let! a = u32
                return U32 a
            }
        let u32ArrayGen =
            gen {
                let! arr = array u32
                return U32Array (makeDtuple arr)
            }
        let u64 = Arb.generate<uint64>
        let u64Gen =
            gen {
                let! a = u64
                return U64 a
            }
        let u64ArrayGen =
            gen {
                let! arr = array u64
                return U64Array (makeDtuple arr)
            }
        let string = gen {
            let! s = Arb.generate<string> |> Gen.filter ((<>) null)
            return Consensus.ZFStar.fsToFstString s
        }
        let stringGen =
            gen {
                let! a = string
                return String a
            }
        let stringArrayGen =
            gen {
                let! arr = array string
                return StringArray (makeDtuple arr)
            }
        let hash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
        let hashGen =
            gen {
                let! a = hash
                return Hash a
            }
        let hashArrayGen =
            gen {
                let! arr = array hash
                return HashArray (makeDtuple arr)
            }
        let lock = gen {
            let! lock =
                Arb.generate<Lock>
                |> Gen.filter (function
                | HighVLock (identifier, _) -> identifier > 7u // last reserved identifier
                | _ -> true)
            return Consensus.ZFStar.fsToFstLock lock
        }
        let lockGen =
            gen {
                let! a = lock
                return Zen.Types.Data.data.Lock a
            }
        let lockArrayGen =
            gen {
                let! arr = array lock
                return LockArray (makeDtuple arr)
            }
        let tupleGen =
            gen {
                let! a = Arb.generate<data>
                let! b = Arb.generate<data>
                return Tuple (a,b)
            }

        Arb.fromGen (Gen.oneof
        [
            i64Gen; i64ArrayGen;
            byteGen; byteArrayGen;
            u32Gen; u32ArrayGen;
            u64Gen; u64ArrayGen;
            stringGen; stringArrayGen;
            hashGen; hashArrayGen;
            lockGen; lockArrayGen;
            tupleGen
        ])

    static member Transaction() =
        let outpointGenerator =
            gen {
                let! bytes = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let txHash = Hash.Hash bytes
                let! index = Gen.choose (0,10)
                let index = uint32 index

                return Outpoint {txHash = txHash;index=index;}
            }

        let mintGenerator =
            gen {
                let! bytes1 = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let! bytes2 = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let asset = Asset (ContractId (Version0, Hash.Hash bytes1), Hash.Hash bytes2)
                let! amount = Arb.generate<uint32> |> Gen.filter ((<>) 0ul)
                let amount = uint64 amount

                return Mint { asset=asset; amount=amount }
            }

        let inputGenerator =
            Gen.oneof [ outpointGenerator; mintGenerator ]

        let outputGenerator =
            gen {
                let notCoinbaseLockOrAcivationSacrifice = function
                | ActivationSacrifice _
                | Coinbase _ -> false
                | _ -> true

                let! lock =
                    Arb.generate<Lock>
                    |> Gen.filter notCoinbaseLockOrAcivationSacrifice
                    |> Gen.filter (function
                    | HighVLock (identifier, _) -> identifier > 7u // last reserved identifier
                    | _ -> true)
                let! asset = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let asset = Asset (ContractId (Version0, Hash.Hash asset), Hash.zero)
                let! amount = Arb.generate<uint64> |> Gen.filter ((<>) 0UL)

                return {lock=lock;spend={asset=asset;amount=amount}}
            }

        let contractGenerator =
            gen {
                let! shouldHaveContract = Arb.generate<bool>
                let! isHighVContract = Arb.generate<bool>
                let! NonEmptyString code = Arb.generate<NonEmptyString>
                let! NonEmptyString hints = Arb.generate<NonEmptyString>
                let! rlimit = Arb.generate<uint32> |> Gen.filter ((<>) 0u)
                let! queries = Arb.generate<uint32> |> Gen.filter ((<>) 0u)

                if shouldHaveContract then
                    if isHighVContract then
                        let! version = Arb.generate<uint32> |> Gen.filter ((<>) Version0)
                        let! bytes = Arb.generate<byte[]>
                        return Some (HighV (version, bytes))
                    else
                        return Some (V0 { code = code; hints = hints; rlimit = rlimit; queries = queries })
                else
                    return None
            }

        let pkWitnessGenerator =
            gen {
                let secretKey, publicKey = KeyPair.create()
                let! hash = Arb.generate<Hash.Hash>
                return PKWitness (publicKey, sign secretKey hash)
            }

        let contractWitnessGenerator nInputs nOutputs =
            gen {
                let! cHash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let! command = Arb.generate<string> |> Gen.filter ((<>) null)

                let! beginInputs = Arb.generate<uint32> |> Gen.filter (fun i -> i < nInputs)
                let! beginOutputs = Arb.generate<uint32> |> Gen.filter (fun i -> i < nOutputs)
                let! inputsLength = Arb.generate<uint32> |> Gen.filter ((<>) 0ul) |> Gen.filter (fun i -> i <= nInputs - beginInputs)
                let! outputsLength = Arb.generate<uint32> |> Gen.filter (fun i -> i <= nOutputs - beginOutputs)
                let! cost = Arb.generate<uint32> |> Gen.filter ((<>) 0ul)
                let! data = Arb.generate<Option<data>>

                let! hasSignature = Arb.generate<bool>

                let secretKey, publicKey = KeyPair.create()
                let! hash = Arb.generate<Hash.Hash>
                let signature = sign secretKey hash

                let signature =
                    if hasSignature then
                        Some (publicKey, signature)
                    else
                        None

                return ContractWitness {
                    contractId = ContractId (Version0, Hash.Hash cHash)
                    command = command
                    data = data
                    beginInputs = beginInputs
                    beginOutputs = beginOutputs
                    inputsLength = inputsLength
                    outputsLength = outputsLength
                    signature = signature
                    cost = cost
                }
            }

        let highVWitnessGenerator =
            gen {
                let! identifier =
                    Arb.generate<uint32>
                    |> Gen.filter (fun i -> i > 2u) // last reserved identifier
                let! bytes = Arb.generate<byte[]>
                return HighVWitness (identifier, bytes)
            }

        let activationSacrificeOutputGenerator =
            gen {
                let! amount = Arb.generate<uint64> |> Gen.filter ((<>) 0UL)
                return [{lock=ActivationSacrifice; spend={asset=Asset (ContractId (Version0,Hash.zero),Hash.zero);amount=amount}}]
            }

        Arb.fromGen (gen {
            let checkMintsOnly = 
                List.forall (function | Mint _ -> true | _ -> false) 
                >> not

            let! inputs = Gen.nonEmptyListOf inputGenerator |> Gen.filter checkMintsOnly
            let! outputs = Gen.nonEmptyListOf outputGenerator
            let! witnesses =
                [ highVWitnessGenerator
                  contractWitnessGenerator (List.length inputs |> uint32) (List.length outputs |> uint32)
                  pkWitnessGenerator ]
                |> Gen.oneof
                |> Gen.nonEmptyListOf
            let! contract = contractGenerator

            let! activationSacrificeOutputs =
                if Option.isSome contract then
                    activationSacrificeOutputGenerator
                else
                    Gen.constant []


            return {version = Version0;inputs=inputs;outputs=List.append outputs activationSacrificeOutputs;contract=contract;witnesses=witnesses}
        })

        static member TxSkeleton() =
            let pointedOutputGenerator =
                gen {
                    let! bytes = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                    let txHash = Hash.Hash bytes
                    let! index = Gen.choose (0,10)
                    let index = uint32 index

                    let outpoint = {txHash = txHash;index=index;}

                    let notCoinbaseLock =
                        function
                        | Coinbase _ -> false
                        | _ -> true

                    let! lock =
                        Arb.generate<Lock>
                        |> Gen.filter notCoinbaseLock
                        |> Gen.filter (function
                        | HighVLock (identifier, _) -> identifier > 7u // last reserved identifier
                        | _ -> true)
                    let! asset = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                    let asset = Asset (ContractId (Version0,Hash.Hash asset), Hash.zero)
                    let! amount = Arb.generate<uint64> |> Gen.filter ((<>) 0UL)

                    let output = {lock=lock;spend={asset=asset;amount=amount}}

                    return TxSkeleton.Input.PointedOutput (outpoint, output)
                }

            let mintGenerator =
                gen {
                    let! bytes1 = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                    let! bytes2 = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                    let asset = Asset (ContractId (Version0,Hash.Hash bytes1), Hash.Hash bytes2)
                    let! amount = Arb.generate<uint32> |> Gen.filter ((<>) 0ul)
                    let amount = uint64 amount

                    return TxSkeleton.Input.Mint { asset=asset; amount=amount }
                }

            let outputGenerator =
                gen {
                    let! lock =
                        Arb.generate<Lock>
                        |> Gen.filter (function
                        | HighVLock (identifier, _) -> identifier > 7u // last reserved identifier
                        | _ -> true)
                    let! asset = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                    let asset = Asset (ContractId (Version0, Hash.Hash asset), Hash.zero)
                    let! amount = Arb.generate<uint64> |> Gen.filter ((<>) 0UL)

                    return {lock=lock;spend={asset=asset;amount=amount}}
                }

            let inputGenerator =
                Gen.oneof [ pointedOutputGenerator; mintGenerator ]

            Arb.fromGen (gen {
                let! inputs = Gen.nonEmptyListOf inputGenerator
                let! outputs = Gen.nonEmptyListOf outputGenerator

                return ({pInputs=inputs;outputs=outputs} : TxSkeleton.T)
            })

    static member Output() =
        Arb.fromGen (
            gen {
                let! lock =
                    Arb.generate<Lock>
                    |> Gen.filter (function
                    | HighVLock (identifier, _) -> identifier > 7u // last reserved identifier
                    | _ -> true)

                let! asset = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let asset = Asset (ContractId (Version0, Hash.Hash asset), Hash.zero)
                let! amount = Arb.generate<uint64> |> Gen.filter ((<>) 0UL)

                let output = {lock=lock;spend={asset=asset;amount=amount}}

                return output
            }
        )

    //TODO: refactor, avoid repeating code
    static member PointedOutput() =
        Arb.fromGen (
            gen {
                let! bytes = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let txHash = Hash.Hash bytes
                let! index = Arb.generate<uint32>

                let outpoint = { txHash = txHash; index = index }
                let! output = Arb.generate<Output>

                return (outpoint, output)
            }
        )
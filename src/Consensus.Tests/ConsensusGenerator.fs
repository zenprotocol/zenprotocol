namespace Consensus.Tests

open FsCheck
open Consensus
open Consensus.Types

type UniqueHashes = UniqueHashes of list<Hash.Hash>

type ThreeBytesHash = ThreeBytesHash of Hash.Hash

type LeadingZerosHash = LeadingZerosHash of Hash.Hash

type NonEmptyTransactions = NonEmptyTransactions of list<Transaction> with
    static member op_Explicit(NonEmptyTransactions txs) = txs

type ConsensusGenerator =
    static member Block() =
        gen {
            let! transactions =
                Arb.generate<Transaction list>
                |> Gen.map (fun txs -> List.distinct txs)

            let! parentHash = Arb.generate<Hash.Hash>
            let! timestamp = Arb.generate<uint64>
            let! coinbasePkHash = Arb.generate<Hash.Hash>
            let! blockNumber = Arb.generate<uint32> |> Gen.filter(fun n -> n > 1ul)
            let reward = Block.getBlockReward blockNumber

            let coinbase = {
                inputs=[]
                outputs=
                    [
                        {
                            lock=Coinbase (blockNumber, coinbasePkHash)
                            spend={asset=Constants.Zen;amount=reward}
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
                    version=Block.Version;
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
        |> Arb.mapFilter (fun txs -> List.distinct txs) (fun txs -> List.length txs > 0)
        |> Arb.convert NonEmptyTransactions NonEmptyTransactions.op_Explicit

    static member Hashes() =
        Arb.from<Hash.Hash list>
            |> Arb.mapFilter (fun xs -> List.distinct xs) (fun xs -> List.length xs > 0)
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
                        |> Gen.filter (fun h -> h <> [|0uy;0uy;0uy;|])
                        |> Gen.filter (fun h -> h <> [|0uy;0uy;|])
                        |> Gen.filter (fun h -> h <> [|0uy;|])

                     return ThreeBytesHash (Hash.Hash (Array.append (Array.zeroCreate (Hash.Length - size)) hash))
                })

    static member LeadingZerosHashGenerator() =
        Arb.fromGen (gen {
            let! leadingZerosLength = Gen.choose (0,28)
            let leadingZeros = Array.zeroCreate leadingZerosLength

            let! rest = Gen.arrayOfLength (Hash.Length - leadingZerosLength) Arb.generate<byte>

            return LeadingZerosHash (Hash.Hash (Array.append leadingZeros rest))
        })

    static member Transaction() =
        let inputGenerator =
            gen {
                let! txHash = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let txHash = Hash.Hash txHash
                let! index = Gen.choose (0,10)
                let index = uint32 index

                return {txHash = txHash;index=index;}
            }

        let outputGenerator =
            gen {
                let notCoinbaseLock lock =
                    match lock with
                    | Coinbase _ -> false
                    | _ -> true

                let! lock = Arb.generate<Lock> |> Gen.filter notCoinbaseLock
                let! asset = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let asset = Hash.Hash asset, Hash.zero
                let! amount = Arb.generate<uint64> |> Gen.filter ((<>) 0UL)

                return {lock=lock;spend={asset=asset;amount=amount}}
            }

//        let contractGenerator =
//            gen {
//                let! shouldHaveContract = Gen.choose (1,10)
//                let! NonEmptyString contract = Arb.generate<NonEmptyString>
//
//                let contract =
//                    if shouldHaveContract = 1 then
//                        Some contract
//                    else
//                        None
//
//                return contract
//            }

        Arb.fromGen (gen {
                let! inputs = Gen.nonEmptyListOf inputGenerator
                let! outputs = Gen.nonEmptyListOf outputGenerator
//                let! contract = contractGenerator

                return {inputs=inputs;outputs=outputs;contract=None;witnesses=[]}
            })

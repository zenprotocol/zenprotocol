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
                     let! hash = Gen.arrayOfLength size Arb.generate<byte>
                                                              
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
                let! lock = Arb.generate<Lock>
                let! asset = Gen.arrayOfLength Hash.Length Arb.generate<byte>
                let asset = Hash.Hash asset
                let! amount = Arb.generate<uint64>
                
                return {lock=lock;spend={asset=asset;amount=amount}}
            }
            
        let contractGenerator =
            gen {
                let! shouldHaveContract = Gen.choose (1,10)
                let! NonEmptyString contract = Arb.generate<NonEmptyString>
                                
                let contract =
                    if shouldHaveContract = 1 then                                 
                        Some contract
                    else 
                        None
                        
                return contract                                    
            }
                                                                     
        Arb.fromGen (gen {                                
                let! inputs = Gen.nonEmptyListOf inputGenerator
                let! outputs = Gen.nonEmptyListOf outputGenerator
                let! contract = contractGenerator
                
                return {inputs=inputs;outputs=outputs;contract=contract;witnesses=[]}                                                                            
            })            
                    
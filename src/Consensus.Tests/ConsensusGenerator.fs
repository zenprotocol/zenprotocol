namespace Consensus.Tests


open FsCheck
open Consensus.Types

type NonEmptyTransactions = NonEmptyTransactions of list<Transaction> with
    static member op_Explicit(NonEmptyTransactions txs) = txs

type ConsensusGenerator = 
    static member Transactions() = 
        Arb.from<Transaction list>
        |> Arb.mapFilter (fun txs -> List.distinct txs) (fun txs -> List.length txs > 0)
        |> Arb.convert NonEmptyTransactions NonEmptyTransactions.op_Explicit
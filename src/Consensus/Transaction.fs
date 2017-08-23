module Consensus.Transaction

open Consensus.Types

let serialize tx =         
    System.BitConverter.GetBytes (tx.amount)
    
let deserialize bytes =
    let amount = System.BitConverter.ToInt32 (bytes, 0)
    {amount=amount}
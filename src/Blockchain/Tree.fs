module Blockchain.Tree

// Helper function to traverse the blockchain tree

let iter session f root =
    let rec iter' root (continuation:unit->unit) =
        f root

        let children = BlockRepository.getBlockChildren session root

        if Seq.isEmpty children then
            continuation ()
        else
            let head = Seq.head children
            let tail = Seq.tail children

            let rec con continuation children () =
                if Seq.isEmpty children then
                    continuation ()
                else
                    let head = Seq.head children
                    //let tail = Seq.tail children

                    iter' head continuation

            iter' head (con continuation tail)

    iter' root id

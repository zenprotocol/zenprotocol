module Infrastructure.BigInteger

let fromBytes32 bytes =
    bytes
    |> Array.append [|0uy|] // adding zero as the MSB to avoid rare case of negative number
    |> Array.rev            // We have to reverse, as we use big-endian and bigint using little
    |> bigint

let toBytes32 (b:bigint) =
    let bs = b.ToByteArray()
    let h =
        match bs.Length with
        | n when n <= 32 ->
            let ar = Array.zeroCreate (32 - n)            
            Array.append ar <| Array.rev bs
        | 33 ->
            if bs.[32] <> 0uy then failwith "Negative difficulty target"
            else Array.rev bs.[..31]
        | _ -> failwith "Difficulty target out of range"
    h
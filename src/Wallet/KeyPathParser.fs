module Wallet.KeyPathParser

type Syntax =
    | EndOfPath
    | Invalid
    | Derive of int

let rec private readNumber chars num =
    let addDigit tail digit =
        if (10 * num) + digit < 0 then
            Invalid, chars
        else
            (10 * num) + digit |> readNumber tail

    match chars with
    | '0' :: tail -> addDigit tail 0
    | '1' :: tail -> addDigit tail 1
    | '2' :: tail -> addDigit tail 2
    | '3' :: tail -> addDigit tail 3
    | '4' :: tail -> addDigit tail 4
    | '5' :: tail -> addDigit tail 5
    | '6' :: tail -> addDigit tail 6
    | '7' :: tail -> addDigit tail 7
    | '8' :: tail -> addDigit tail 8
    | '9' :: tail -> addDigit tail 9
    | '\'' :: '/' :: tail -> Derive (num + 0x80000000), tail
    | '\'' :: [] -> Derive (num + 0x80000000), []
    | '/' :: tail -> Derive num, tail
    | [] -> Derive num, []
    | _ -> Invalid, chars

let parse (path:string) =
    let parsePart chars =
        if List.isEmpty chars then
            EndOfPath,[]
        else
            match List.head chars with
            | '0'
            | '1'
            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7'
            | '8'
            | '9' -> readNumber chars 0
            | _ -> Invalid, chars

    let rec parseAll chars parts =
        match parsePart chars with
        | EndOfPath,_ -> List.rev parts |> Ok
        | Derive index, chars -> parseAll chars (index :: parts)
        | Invalid,_ -> Error "invalid key path"

    // first two characters must be m/
    if path.StartsWith "m/" then
        let chars = [for c in path -> c].[2..]

        parseAll chars []
    else
        Error "invalid key path"
module Consensus.Serialization

open System
open Crypto
open Hash
open Types

module ZData = Zen.Types.Data

exception SerializationException

type Stream(buffer:byte array) =
    let mutable offset = 0

    member this.Offset = offset
    member this.Buffer = buffer
    member this.readNumber1 () =
        if offset + 1 <= buffer.Length then
            let x = buffer.[offset]
            offset <- offset + 1
            x
        else
            raise SerializationException

    member this.readNumber2 () =
        if offset + 2 <= buffer.Length then
            let number =
                ((uint16 (buffer.[offset])) <<< 8) +
                (uint16 buffer.[offset+1])

            offset <- offset + 2

            number
        else
            raise SerializationException

    member this.readNumber4 () =
        if offset + 4 <= buffer.Length then
            let number =
                ((uint32 (buffer.[offset])) <<< 24) +
                ((uint32 (buffer.[offset+1])) <<< 16) +
                ((uint32 (buffer.[offset+2])) <<< 8) +
                (uint32 buffer.[offset+3])

            offset <- offset + 4

            number
        else
            raise SerializationException

    member this.readNumber8 () =
        if offset + 8 <= buffer.Length then
            let number =
                ((uint64 (buffer.[offset]) <<< 56)) +
                ((uint64 (buffer.[offset+1]) <<< 48)) +
                ((uint64 (buffer.[offset+2]) <<< 40)) +
                ((uint64 (buffer.[offset+3]) <<< 32)) +
                ((uint64 (buffer.[offset+4]) <<< 24)) +
                ((uint64 (buffer.[offset+5]) <<< 16)) +
                ((uint64 (buffer.[offset+6]) <<< 8)) +
                (uint64 buffer.[offset+7])

            offset <- offset + 8

            number
        else
            raise SerializationException

    member this.readBytes size =
        if offset + size <= buffer.Length then
            let bytes = Array.zeroCreate<byte> size
            System.Buffer.BlockCopy(buffer, offset, bytes, 0, size)
            offset <- offset + size

            bytes
        else
            raise SerializationException

    member this.writeBytes bytes size =
        System.Buffer.BlockCopy (bytes,0,buffer,offset,size)
        offset <- offset + size

    member this.writeNumber1 (n:byte) =
        buffer.[offset] <- n
        offset <- offset + 1
    member this.writeNumber2 (n:UInt16)  =
        buffer.[offset] <- byte ((n >>> 8) &&& 255us)
        buffer.[offset+1] <- byte (n &&& 255us)
        offset <- offset + 2
    member this.writeNumber4 (n:UInt32) =
        buffer.[offset] <- byte ((n >>> 24) &&& 255u)
        buffer.[offset+1] <- byte ((n >>> 16) &&& 255u)
        buffer.[offset+2] <- byte ((n >>> 8) &&& 255u)
        buffer.[offset+3] <- byte ((n) &&& 255u)
        offset <- offset + 4
    member this.writeNumber8 (n:UInt64) =
        buffer.[offset] <- (byte) ((n >>> 56) &&& 255UL)
        buffer.[offset+1] <- (byte) ((n >>> 48) &&& 255UL)
        buffer.[offset+2] <- (byte) ((n >>> 40) &&& 255UL)
        buffer.[offset+3] <- (byte) ((n >>> 32) &&& 255UL)
        buffer.[offset+4] <- (byte) ((n >>> 24) &&& 255UL)
        buffer.[offset+5] <- (byte) ((n >>> 16) &&& 255UL)
        buffer.[offset+6] <- (byte) ((n >>> 8)  &&& 255UL)
        buffer.[offset+7] <- (byte) ((n)       &&& 255UL)
        offset <- offset + 8

let serialize size write x =
    let stream =
        size x
        |> Array.zeroCreate
        |> Stream

    write stream x

    stream.Buffer

let deserialize fn bytes =
    try
        Stream(bytes)
        |> fn
        |> Option.Some
    with
    | SerializationException as ex-> None

[<Literal>]
let SerializedHeaderSize = 100

type TransactionSerializationMode =
    | Full
    | WithoutWitness

module Serialization =

    let ofOption opt  =
        match opt with
        | None -> raise SerializationException
        | Some opt -> opt

    module Byte =
        let size = 1
        let write (stream:Stream) byte = stream.writeNumber1 byte
        let read (stream:Stream) = stream.readNumber1()

    module Option =
        [<Literal>]
        let private None = 0uy
        [<Literal>]
        let private Some = 1uy

        let size sizeFn = function
            | Option.Some value ->
                Byte.size + sizeFn value
            | Option.None ->
                Byte.size

        let write (stream:Stream) writerFn = function
            | Option.Some value ->
                Byte.write stream Some

                writerFn stream value
            | Option.None ->
                Byte.write stream None

        let read streamFn (stream:Stream) =
            let discriminator = Byte.read stream
            match discriminator with
            | Some ->
                let item = streamFn stream
                Option.Some item
            | None ->
                Option.None
            | _ ->
                raise SerializationException

    module VarInt =
        let size x =
            let rec loop n len =
                if n <= 0x7Ful then
                    len + 1
                else
                    loop ((n >>> 7) - 1ul) (len + 1)

            loop x 0

        let write (stream:Stream) (x:uint32) =
            // https://github.com/bitcoin/bitcoin/blob/v0.13.2/src/serialize.h#L307L372
            let tmp = Array.zeroCreate 5

            let rec loop n len =
                tmp.[len] <- (byte (n &&& 0x7Ful)) ||| (if len <> 0 then 0x80uy else 0x00uy)

                if n <= 0x7Ful then
                    len
                else
                    loop ((n >>> 7) - 1ul) (len + 1)

            let len = loop x 0
            let bytes = Array.rev tmp.[0..len]

            stream.writeBytes bytes (len + 1)

        let read (stream:Stream) =
            let rec loop n  =
                let data = stream.readNumber1 ()

                let n = ((uint32 n) <<< 7) ||| ((uint32 data) &&& 0x7Ful)

                if data &&& 0x80uy <> 0uy then
                    (loop (n + 1ul))
                else
                    n

            loop 0ul

    module Bytes =
        let size bytes =
            let length = Array.length bytes
            VarInt.size (length |> uint32) + length

        let write (stream:Stream) bytes =
            let len = Array.length bytes
            VarInt.write stream (uint32 len)

            stream.writeBytes bytes len
        let read (stream:Stream) =
            let len = VarInt.read stream
            stream.readBytes (int len)

    module FixedSizeBytes =
        let size bytes = Array.length bytes
        let write (stream:Stream) bytes =
            stream.writeBytes bytes (Array.length bytes)
        let read len (stream:Stream) = stream.readBytes len

    module Hash =
        let size = Hash.Length
        let write (stream:Stream) hash = FixedSizeBytes.write stream (Hash.bytes hash)
        let read stream =
            let bytes = FixedSizeBytes.read Hash.Length stream
            Hash bytes

    module String =
        let size (s:string) =
            let size = System.Text.Encoding.ASCII.GetByteCount s

            VarInt.size (uint32 size) + size

        let write (stream:Stream) (s:string) =
            System.Text.Encoding.ASCII.GetBytes s
            |> Bytes.write stream
        let read stream =
            let bytes = Bytes.read stream
            System.Text.Encoding.ASCII.GetString bytes

    module Seq =
        let size<'a> (sizeFn:'a->int) seq =
            let length = (Seq.length seq)
            VarInt.size (length |> uint32) + Seq.sumBy sizeFn seq

        let sizeBody (sizeFn:'a->int) seq = Seq.sumBy sizeFn seq

        let writeBody writerFn (stream:Stream) seq =
            Seq.iter (fun item -> writerFn stream item) seq

        let write writerFn (stream:Stream) seq =
            VarInt.write stream (Seq.length seq |> uint32)
            writeBody writerFn stream seq

    module List =
        let size = Seq.size

        let write writerFn stream (ls: list<'B>) =
            Seq.write writerFn stream ls

        let readBody length streamFn stream =
            [1..int length]
            |> List.map (fun _ -> streamFn stream)

        let read streamFn stream =
            let length = VarInt.read stream
            readBody length streamFn stream

    module Array =
        let size = Seq.size

        let write writerFn stream (arr: array<'B>) =
            Seq.write writerFn stream arr

        let readBody length streamFn stream =
            let length = int length
            let array = Array.zeroCreate (int length)

            for i in 0 .. (length-1) do
                array.[i] <- streamFn stream

            array

        let read streamFn stream =
            let length = VarInt.read stream
            readBody length streamFn stream

    module Map =
        let size sizeFn =
            Map.toList
            >> Seq.size sizeFn

        let write stream writerFn =
            Map.toList
            >> Seq.write stream writerFn
        let read streamFn stream =
            let l = List.read streamFn stream

            let rec isSorted l =
                match l with
                | [] | [_] -> true
                | h1::(h2::_ as tail) -> h1 <= h2 && isSorted tail

            if not <| isSorted l then
                raise SerializationException
            else
                Map.ofList l

    module ContractId =
        let size (ContractId (version,_)) =
            VarInt.size version + Hash.size

        let write stream (ContractId (version,cHash)) =
            VarInt.write stream version
            Hash.write stream cHash
        let read stream =
            let version = VarInt.read stream
            let cHash = Hash.read stream

            ContractId (version, cHash)

        let serialize = serialize size write
        let deserialize = deserialize read

    module Asset =
        let private writeSubtype stream l = fun sb ->
            Byte.write stream (byte l)
            FixedSizeBytes.write stream sb
        let private versionBytes (v:uint32) =
            if v &&& 0xFFFFFFE0u = 0u
            then [| byte v |]
            elif v &&& 0xFFFFF000u = 0u
            then [| 0x20uy ||| byte (v >>> 7);
                    0x7Fuy &&& byte v |]
            elif v &&& 0xFFF80000u = 0u
            then [| 0x20uy ||| byte (v >>> 14);
                    0x80uy ||| byte (v >>> 7);
                    0x7Fuy &&& byte v |]
            elif v &&& 0xFC000000u = 0u
            then [| 0x20uy ||| byte (v >>> 21);
                    0x80uy ||| byte (v >>> 14);
                    0x80uy ||| byte (v >>> 7);
                    0x7Fuy &&& byte v |]
            else [| 0x20uy ||| byte (v >>> 28);
                    0x80uy ||| byte (v >>> 21);
                    0x80uy ||| byte (v >>> 14);
                    0x80uy ||| byte (v >>> 7);
                    0x7Fuy &&& byte v |]

        let size (Asset (ContractId (version, cHash),(Hash sb as subtype))) =
            let versionBytes = versionBytes version
            let versionBytesLength = Array.length versionBytes

            if Hash.zero = cHash && Hash.zero = subtype then
                versionBytesLength
            else
                match Array.tryFindIndexBack (fun b -> b <> 0uy) sb with
                | None ->  versionBytesLength + 32
                | Some 30
                | Some 31 ->
                    versionBytesLength + 32 + 32
                | Some index ->
                    versionBytesLength + 32 + 1 + index + 1

        let write stream = fun (Asset (ContractId (version, cHash),(Hash sb as subtype))) ->
            let vbs = versionBytes version
            if cHash = Hash.zero && subtype = Hash.zero
            then
                FixedSizeBytes.write stream vbs
            else
                match Array.tryFindIndexBack (fun b -> b <> 0uy) sb with
                | None ->                               // subtype = Hash.zero
                    vbs.[0] <- 0x80uy ||| vbs.[0]
                    FixedSizeBytes.write stream vbs
                    Hash.write stream cHash
                | Some n when n < 30 ->
                    vbs.[0] <- 0x40uy ||| vbs.[0]
                    FixedSizeBytes.write stream vbs
                    Hash.write stream cHash
                    Byte.write stream (byte (n + 1))
                    FixedSizeBytes.write stream sb.[..n]
                | Some _ ->
                    vbs.[0] <- 0xC0uy ||| vbs.[0]
                    FixedSizeBytes.write stream vbs
                    Hash.write stream cHash
                    Hash.write stream subtype

        let rec private readVersion v counter stream =
            if counter > 3 || (counter <> 0 && v = 0u) then raise SerializationException else
            let b = Byte.read stream
            let nextV = v + (uint32 (b &&& 0x7Fuy))
            if b &&& 0x80uy = 0uy
            then nextV
            elif nextV >= pown 2u 25 then raise SerializationException     //will be > 2^32
            else
                let res = readVersion (nextV * 128u) (counter + 1) stream
                res

        let private readSubtype stream =
            let len = Byte.read stream
            if len = 0x00uy then Hash.zero else
            let subtype = FixedSizeBytes.read (int len) stream
            let res = Array.zeroCreate<byte> Hash.Length
            Array.blit subtype 0 res 0 (int len)
            Hash res

        let read stream =
            let first = Byte.read stream

            // Fast path for zen asset, in case first byte is zero
            if first = 0uy then
                Asset.Zen
            else

            let version =
                let v = (uint32 (first &&& 0x1Fuy))
                if (first &&& 0x20uy) = 0uy
                then
                    v
                else
                    readVersion (v*128u) 0 stream
            let cHash, isAbsentCHash =
                if (first &&& 0xC0uy) = 0uy
                then
                    Hash.zero, true
                else
                    let h = Hash.read stream
                    (h,false)

            let subtype, isAbsentSubtype, isCompressedSubtype =
                match (first &&& 0xC0uy) with
                | 0x0uy | 0x80uy ->
                    Hash.zero, true, false
                | 0xC0uy ->
                    let h = Hash.read stream
                    h,false,false
                | _ ->
                    let h = readSubtype stream
                    h,false,true

            match cHash, subtype with
            | cHash, subtype when
                cHash = Hash.zero && subtype = Hash.zero
                && ((not isAbsentCHash) || (not isAbsentSubtype)) ->
                raise SerializationException
            | _, subtype when
                subtype = Hash.zero
                && (not isAbsentSubtype) ->
                raise SerializationException
            | cHash, (Hash sb as subtype) when
                cHash <> Hash.zero && subtype <> Hash.zero &&
                sb.[Hash.Length-2] = 0uy && sb.[Hash.Length-1] = 0uy
                && (not isCompressedSubtype) ->
                raise SerializationException
            | _ ->
                Asset (ContractId (version, cHash), subtype)

    module Amount =
        let private factor (x:uint64) =
            let rec inner (s,e) =
                if s % 10UL <> 0UL
                then (s,e)
                else
                    inner (s/10UL,e+1)
            inner (x,0)
        let private figures s =
            let rec inner (s,f) =
                if s = 0UL then f
                else
                    inner (s/10UL,f+1)
            inner (s,0)
        let private parse x =
            if x = 0UL then (0UL, 0, 0)
            else
                let s,e = factor x
                let f = figures s
                (s,e,f)
        let private write16 (stream:Stream) = fun (s,e) ->
            let res =
                s ||| (e <<< 10)
            stream.writeNumber2 res
        [<Literal>]
        let cutoff = 0x04000000u
        let private write32 (stream:Stream) = fun (s,e) ->
            if s < cutoff
            then
                let shifted = (0x20u ||| e) <<< 26
                stream.writeNumber4 (s ||| shifted)
            else
                let shifted = (0x60u ||| e) <<< 25
                stream.writeNumber4 ((s - cutoff) ||| shifted)
        let private write64 (stream:Stream) = fun s ->
            let res = ((0x7EUL <<< 56) ||| s)
            stream.writeNumber8 res
        let private write72 (stream:Stream) = fun s ->
            stream.writeNumber1 0xFEuy
            stream.writeNumber8 s


        let size amount =
            let _,_,f = parse(amount)

            if f <= 3 then
                2
            elif f<= 8 then
                4
            elif amount < pown 2UL 56 then
                8
            else
                9

        let write stream = fun amount ->
            let s,e,f = parse amount
            if f <= 3
            then
                write16 stream (uint16 s, uint16 e)
            elif f <= 8
            then
                if e <= 12
                then
                    write32 stream (uint32 s, uint32 e)
                else
                    write32 stream (uint32 s * pown 10u (e-12),12u)
            elif amount < pown 2UL 56
            then
                write64 stream amount
            else
                write72 stream amount
        let read (stream:Stream) =
            let first = stream.readNumber1 ()
            if
                (first &&& 0x7Euy) = 0x7Cuy                //qNaN
                || (first &&& 0x7Cuy) = 0x78uy             //Infinity
            then
                raise SerializationException
            elif (first &&& 0x7Euy) = 0x7Euy
            then
                if first >= 0x80uy                          //72 bit
                then
                    let amount = stream.readNumber8 ()
                    amount
                else                                        //64 bit
                    let second = stream.readNumber1 ()
                    let third = stream.readNumber2 ()
                    let fourth = stream.readNumber4 ()

                    (uint64 fourth)
                    + ((uint64 third) <<< 32)
                    + ((uint64 second) <<< 48)
            elif first < 0x80uy                             //16 bit
            then
                let second = stream.readNumber1 ()
                if (first &&& 0x60uy) = 0x60uy              //non-canonical
                then
                    let e = int (first &&& 0x1Fuy)
                    let s = (uint64 second) + 0x400UL
                    s * pown 10UL e
                else                                        //canonical
                    let e = int ((first &&& 0x7Cuy) >>> 2)
                    let s = (uint64 second) + ((uint64 (first &&& 0x03uy)) <<< 8)
                    s * pown 10UL e
            else                                            //32 bit
                let second = stream.readNumber1 ()
                let lower = stream.readNumber2 ()
                if (first &&& 0x40uy) = 0uy
                then
                    let e = int ((first &&& 0x3Cuy) >>> 2)
                    let s =
                        (uint64 lower)
                        + ((uint64 second) <<< 16)
                        + ((uint64 (first &&& 0x03uy)) <<< 24)
                    s * pown 10UL e
                else
                    let e = int ((first &&& 0x1Euy) >>> 1)
                    let s =
                        (uint64 lower)
                        + ((uint64 second) <<< 16)
                        + ((uint64 (first &&& 0x01uy)) <<< 24)
                        + 0x4000000UL
                    s * pown 10UL e

    module Spend =
        let size ({ asset = asset; amount = amount }) =
            Asset.size asset + Amount.size amount

        let write stream = fun { asset = asset; amount = amount } ->
            Asset.write stream asset
            Amount.write stream amount
        let read stream =
            let asset = Asset.read stream
            let amount = Amount.read stream
            { asset = asset; amount = amount }

    module Outpoint =
        let size ({ index = index })= Hash.size + VarInt.size index

        let write stream = fun { txHash = txHash; index = index } ->
            Hash.write stream txHash
            VarInt.write stream index
        let read stream =
            let txHash = Hash.read stream
            let index = VarInt.read stream
            { txHash = txHash; index = index }

    module Input =
        [<Literal>]
        let private SerializedOutpoint = 1uy
        [<Literal>]
        let private SerializedMint = 2uy

        let size = function
           | Outpoint outpoint ->
               Byte.size + Outpoint.size outpoint
           | Mint spend ->
               Byte.size + Spend.size spend

        let write stream = function
            | Outpoint outpoint ->
                Byte.write stream SerializedOutpoint
                Outpoint.write stream outpoint
            | Mint spend ->
                Byte.write stream SerializedMint
                Spend.write stream spend
        let read stream =
            let discriminator = Byte.read stream
            match discriminator with
            | SerializedOutpoint ->
                let outpoint = Outpoint.read stream
                Outpoint outpoint
            | SerializedMint ->
                let spend = Spend.read stream
                Mint spend
            | _ ->
                raise SerializationException

    module Signature =
        let size = SerializedSignatureLength

        let write stream signature =
            FixedSizeBytes.write stream (Signature.serialize signature)

        let read stream =
            let bytes = FixedSizeBytes.read SerializedSignatureLength stream

            Signature.deserialize bytes
            |> ofOption

    module PublicKey =
        let size = SerializedPublicKeyLength

        let write stream publicKey =
            FixedSizeBytes.write stream (PublicKey.serialize publicKey)

        let read stream =
            let bytes = FixedSizeBytes.read SerializedPublicKeyLength stream

            PublicKey.deserialize bytes
            |> ofOption

    module VoteData =
        module Allocation =
            let size = Option.size (fun _ -> Byte.size)
            let write stream = Option.write stream Byte.write
            let read = Option.read Byte.read

        module Payout =
            module Recipient =
                [<Literal>]
                let private PKIdentifier = 1uy
                [<Literal>]
                let private ContractIdentifier = 2uy

                let size =
                    function
                    | PKRecipient _ -> Hash.size
                    | ContractRecipient cid -> ContractId.size cid
                    >> (+) Byte.size

                let private discriminator =
                    function
                    | PKRecipient _ -> PKIdentifier
                    | ContractRecipient _ -> ContractIdentifier

                let write stream = fun recipient ->
                    Byte.write stream (discriminator recipient)
                    match recipient with
                    | PKRecipient pkHash -> 
                        Hash.write stream pkHash
                    | ContractRecipient contractId ->
                        ContractId.write stream contractId

                let read stream =
                    let discriminator = Byte.read stream
                    match discriminator with
                    | PKIdentifier ->
                        Hash.read stream
                        |> PKRecipient
                    | ContractIdentifier ->
                        ContractId.read stream
                        |> ContractRecipient
                    | _ ->
                        raise SerializationException

            let size = fun (recipient, amount) ->
                Recipient.size recipient + 
                Amount.size amount

            let write stream = fun (recipient, amount) ->
                Recipient.write stream recipient
                Amount.write stream amount

            let read stream =
                Recipient.read stream,
                Amount.read stream

        let size = fun { allocation = allocation; payout = payout } ->
            Allocation.size allocation +
            Option.size Payout.size payout

        let write stream = fun { allocation = allocation; payout = payout } ->
            Allocation.write stream allocation
            Option.write stream (fun stream payout -> Payout.write stream payout) payout

        let read stream =
            {
                allocation = Allocation.read stream
                payout = Option.read Payout.read stream
            }

    module Lock =
        [<Literal>]
        let LastReservedIdentifier = 9u
        
        [<Literal>]
        let private PKIdentifier = 2u
        [<Literal>]
        let private ContractIdentifier = 4u
        [<Literal>]
        let private CoinbaseIdentifier = 6u
        [<Literal>]
        let private FeeIdentifier = 1u
        [<Literal>]
        let private ActivationSacrificeIdentifier = 3u
        [<Literal>]
        let private ExtensionSacrificeIdentifier = 5u
        [<Literal>]
        let private DestroyIdentifier = 7u
        [<Literal>]
        let private VoteIdentifier = LastReservedIdentifier // odd identifier - not spendable as HighV for older versions

        let private identifier = function
            | PK _ -> PKIdentifier
            | Contract _ -> ContractIdentifier
            | Coinbase _ -> CoinbaseIdentifier
            | Fee -> FeeIdentifier
            | ActivationSacrifice -> ActivationSacrificeIdentifier
            | ExtensionSacrifice _ -> ExtensionSacrificeIdentifier
            | Destroy -> DestroyIdentifier
            | Vote _ -> VoteIdentifier
            | HighVLock (identifier, _) -> identifier
        
        let private sizeFn = function
            | PK _ -> Hash.size
            | Contract contractId -> ContractId.size contractId
            | ExtensionSacrifice contractId -> ContractId.size contractId
            | Fee
            | ActivationSacrifice
            | Destroy -> 0
            | Coinbase _ -> 4 + Hash.size
            | Vote (data, blockNumber, _) ->
                VoteData.size data +
                VarInt.size blockNumber +
                Hash.size
            | HighVLock (_, bytes) -> FixedSizeBytes.size bytes

        let size lock =
            let payloadSize = sizeFn lock

            VarInt.size (identifier lock) + VarInt.size (payloadSize |> uint32) + payloadSize

        let private writerFn stream = function
            | PK hash -> Hash.write stream hash
            | Contract contractId -> ContractId.write stream contractId
            | ExtensionSacrifice contractId ->
                ContractId.write stream contractId
            | Fee
            | ActivationSacrifice
            | Destroy -> ()
            | Coinbase (blockNumber, pkHash) ->
                stream.writeNumber4 blockNumber
                Hash.write stream pkHash
            | Vote (data, blockNumber, pkHash) ->
                VoteData.write stream data
                VarInt.write stream blockNumber
                Hash.write stream pkHash
            | HighVLock (_, bytes) ->
                FixedSizeBytes.write stream bytes

        let write stream = fun lock ->
            VarInt.write stream (identifier lock)
            VarInt.write stream (sizeFn lock |> uint32)
            writerFn stream lock

        let read stream =
            let identifier = VarInt.read stream
            let count = VarInt.read stream
            let value =
                match identifier with
                | PKIdentifier ->
                    let hash = Hash.read stream
                    Lock.PK hash
                | ContractIdentifier ->
                    let contractId = ContractId.read stream
                    Lock.Contract contractId
                | CoinbaseIdentifier ->
                    let blockNumber = stream.readNumber4 ()
                    let pkHash = Hash.read stream
                    Lock.Coinbase (blockNumber, pkHash)
                | FeeIdentifier ->
                    Lock.Fee
                | ActivationSacrificeIdentifier ->
                    Lock.ActivationSacrifice
                | DestroyIdentifier ->
                    Lock.Destroy
                | ExtensionSacrificeIdentifier ->
                    let contractId = ContractId.read stream
                    Lock.ExtensionSacrifice contractId
                | VoteIdentifier ->
                    let data = VoteData.read stream
                    let blockNumber = VarInt.read stream
                    let pkHash = Hash.read stream
                    Lock.Vote (data, blockNumber, pkHash)
                | _ ->
                    let bytes = FixedSizeBytes.read (int count) stream
                    HighVLock (identifier, bytes)

            if (int count = sizeFn value) then
                value
            else
                raise SerializationException

    module Data =
        [<Literal>]
        let private I64Data = 1uy
        [<Literal>]
        let private ByteData = 2uy
        [<Literal>]
        let private ByteArrayData = 3uy
        [<Literal>]
        let private U32Data = 4uy
        [<Literal>]
        let private U64Data = 5uy
        [<Literal>]
        let private StringData = 6uy
        [<Literal>]
        let private HashData = 7uy
        [<Literal>]
        let private LockData = 8uy
        [<Literal>]
        let private SignatureData = 9uy
        [<Literal>]
        let private PublicKeyData = 10uy
        [<Literal>]
        let private CollectionArrayData = 11uy
        [<Literal>]
        let private CollectionDictData = 12uy
        [<Literal>]
        let private CollectionListData = 13uy

        module Int64 =
            let size = 8
            let write (stream:Stream) value = uint64 value |> stream.writeNumber8
            let read (stream:Stream) =
                let i = stream.readNumber8 ()
                int64 i

        module Hash =
            let size = Hash.size
            let write stream = Hash.Hash >> Hash.write stream
            let read (stream:Stream) =
                let hash = Hash.read stream
                hash |> Hash.bytes

        module Lock =
            let size = ZFStar.fstToFsLock >> Lock.size
            let write stream = ZFStar.fstToFsLock >> Lock.write stream
            let read (stream:Stream) =
                let lock = Lock.read stream
                ZFStar.fsToFstLock lock

        module String =
            let size = ZFStar.fstToFsString >> String.size
            let write stream = ZFStar.fstToFsString >> String.write stream
            let read (stream:Stream) =
                let s = String.read stream
                ZFStar.fsToFstString s

        module Signature =
            let size = Signature.size
            let write stream = Signature >> Signature.write stream
            let read (stream:Stream) =
                let (Signature signature) = Signature.read stream
                signature

        module PublicKey =
            let size = PublicKey.size
            let write stream = PublicKey >> PublicKey.write stream
            let read (stream:Stream) =
                 let (PublicKey publicKey) = PublicKey.read stream
                 publicKey

        let rec size data =
            let payloadSize =
                match data with
                | ZData.I64 _ -> Int64.size
                | ZData.Byte _ -> Byte.size
                | ZData.ByteArray arr -> Bytes.size arr
                | ZData.U32 _ -> 4
                | ZData.U64 _ -> 8
                | ZData.String s -> String.size s
                | ZData.Hash _ -> Hash.size
                | ZData.Lock lock -> Lock.size lock
                | ZData.Signature _ -> Signature.size
                | ZData.PublicKey _ -> PublicKey.size
                | ZData.Collection (ZData.Array arr) -> Seq.size size arr
                | ZData.Collection (ZData.Dict (map, _)) ->
                    Map.size (fun (key, data)-> String.size key + size data) map
                | ZData.Collection (ZData.List l) ->
                    Byte.size + Seq.size size (ZFStar.fstToFsList l)

            Byte.size + payloadSize

        let rec write (stream:Stream) = function
            | ZData.I64 i ->
                Byte.write stream I64Data
                Int64.write stream i
            | ZData.Byte b ->
                Byte.write stream ByteData
                Byte.write stream b
            | ZData.ByteArray arr ->
                Byte.write stream ByteArrayData
                Bytes.write stream arr
            | ZData.U32 i ->
                Byte.write stream U32Data
                stream.writeNumber4 i
            | ZData.U64 i ->
                Byte.write stream U64Data
                stream.writeNumber8 i
            | ZData.String s ->
                Byte.write stream StringData
                String.write stream s
            | ZData.Hash hash ->
                Byte.write stream HashData
                Hash.write stream hash
            | ZData.Lock l ->
                Byte.write stream LockData
                Lock.write stream l
            | ZData.Signature signature ->
                Byte.write stream SignatureData
                Signature.write stream signature
            | ZData.PublicKey publicKey ->
                Byte.write stream PublicKeyData
                PublicKey.write stream publicKey
            | ZData.Collection (ZData.Array arr) ->
                Byte.write stream CollectionArrayData
                Seq.write write stream arr
            | ZData.Collection (ZData.Dict (map, _)) ->
                Byte.write stream CollectionDictData
                Map.write (fun stream (key, data)->
                    String.write stream key
                    write stream data) stream map
            | ZData.Collection (ZData.List l) ->
                Byte.write stream CollectionListData
                Seq.write write stream (ZFStar.fstToFsList l)

        let rec read (stream:Stream) =
            let discriminator = Byte.read stream
            match discriminator with
            | I64Data ->
                let i = Int64.read stream
                ZData.I64 i
            | ByteData ->
                let byte = Byte.read stream
                ZData.Byte byte
            | ByteArrayData ->
                let arr = Array.read Byte.read stream
                ZData.ByteArray arr
            | U32Data ->
                let i = stream.readNumber4 ()
                ZData.U32 i
            | U64Data ->
                let i = stream.readNumber8 ()
                ZData.U64 i
            | StringData ->
                let s = String.read stream
                ZData.String s
            | HashData ->
                let hash = Hash.read stream
                ZData.Hash hash
            | LockData ->
                let lock = Lock.read stream
                ZData.Lock lock
            | SignatureData ->
                let signature = Signature.read stream
                ZData.Signature signature
            | PublicKeyData ->
                let publicKey = PublicKey.read stream
                ZData.PublicKey publicKey
            | CollectionArrayData ->
                let arr = Array.read read stream
                ZData.Collection (ZData.Array arr)
            | CollectionDictData ->
                let map = Map.read (fun stream ->
                                let key = String.read stream
                                let value = read stream
                                key, value
                            ) stream
                ZData.Collection (ZData.Dict (map, Map.count map |> uint32))
            | CollectionListData ->
                let l = List.read read stream
                let l = ZFStar.fsToFstList (List.ofSeq l)
                ZData.Collection (ZData.List l)
            | _ ->
                raise SerializationException

    module StateCommitment =
        [<Literal>]
        let private SerializedNoState = 1uy
        [<Literal>]
        let private SerializedState = 2uy
        [<Literal>]
        let private SerializedNotCommitted = 3uy

        let size = function
        | NoState
        | NotCommitted ->
            Byte.size
        | State _ ->
            Byte.size + Hash.size

        let write stream = function
        | NoState ->
            Byte.write stream SerializedNoState
        | State hash ->
            Byte.write stream SerializedState
            Hash.write stream hash
        | NotCommitted ->
            Byte.write stream SerializedNotCommitted

        let read stream =
            let discriminator = Byte.read stream
            match discriminator with
            | SerializedNoState ->
                NoState
            | SerializedState ->
                let hash = Hash.read stream
                State hash
            | SerializedNotCommitted ->
                NotCommitted
            | _ ->
                raise SerializationException

    module SigHash =
        [<Literal>]
        let private SigHashTxHash = 1uy

        [<Literal>]
        let private SigHashFollowingWitnesses = 3uy

        let size = Byte.size
        let write stream sigHash =

            let sigHashByte =
                match sigHash with
                | TxHash -> SigHashTxHash
                | FollowingWitnesses -> SigHashFollowingWitnesses
                | UnknownSigHash x -> x

            Byte.write stream sigHashByte

        let read stream =
            let sigHashByte = Byte.read stream

            match sigHashByte with
            | SigHashTxHash -> TxHash
            | SigHashFollowingWitnesses -> FollowingWitnesses
            | x -> UnknownSigHash x

    module Witness =
        [<Literal>]
        let private PKIdentifier = 1u
        [<Literal>]
        let private ContractIdentifier = 2u

        let private sizeFn = function
            | PKWitness _ ->
                Byte.size + PublicKey.size + Signature.size
            | ContractWitness cw ->
                ContractId.size cw.contractId
                + String.size cw.command
                + Option.size Data.size cw.messageBody
                + StateCommitment.size cw.stateCommitment
                + VarInt.size cw.beginInputs
                + VarInt.size cw.beginOutputs
                + VarInt.size cw.inputsLength
                + VarInt.size cw.outputsLength
                + Option.size (fun _ -> PublicKey.size + Signature.size) cw.signature +
                + 8 // cost
            | HighVWitness (_, bytes) -> FixedSizeBytes.size bytes

        let size witness =
            let identifier =
                match witness with
                | PKWitness _ -> PKIdentifier
                | ContractWitness _ -> ContractIdentifier
                | HighVWitness (identifier, _) -> identifier

            let payloadSize = sizeFn witness

            VarInt.size identifier + VarInt.size (payloadSize |> uint32) + payloadSize

        let private writerFn stream = function
            | PKWitness (sigHash, publicKey, signature) ->

                SigHash.write stream sigHash
                PublicKey.write stream publicKey
                Signature.write stream signature
            | ContractWitness cw ->
                ContractId.write stream cw.contractId
                String.write stream cw.command
                Option.write stream Data.write cw.messageBody
                StateCommitment.write stream cw.stateCommitment
                VarInt.write stream cw.beginInputs
                VarInt.write stream cw.beginOutputs
                VarInt.write stream cw.inputsLength
                VarInt.write stream cw.outputsLength
                Option.write stream (fun stream (publicKey, signature) ->
                    PublicKey.write stream publicKey
                    Signature.write stream signature
                  ) cw.signature
                stream.writeNumber8 cw.cost //TODO: optimize
            | HighVWitness (_, bytes) ->
                FixedSizeBytes.write stream bytes

        let write stream = fun witness ->
            let identifier =
                match witness with
                | PKWitness _ -> PKIdentifier
                | ContractWitness _ -> ContractIdentifier
                | HighVWitness (identifier, _) -> identifier

            VarInt.write stream identifier
            VarInt.write stream (sizeFn witness |> uint32)
            writerFn stream witness

        let read stream =
            let identifier = VarInt.read stream
            let count = VarInt.read stream
            let value =
                match identifier with
                | PKIdentifier ->
                    let sigHash = SigHash.read stream
                    let publicKey = PublicKey.read stream
                    let signature = Signature.read stream
                    PKWitness (sigHash, publicKey, signature)
                | ContractIdentifier ->
                    let contractId = ContractId.read stream
                    let command = String.read stream
                    let messageBody = Option.read Data.read stream
                    let stateCommitment = StateCommitment.read stream
                    let beginInputs = VarInt.read stream
                    let beginOutputs = VarInt.read stream
                    let inputsLength = VarInt.read stream
                    let outputsLength = VarInt.read stream
                    let signature = Option.read (fun stream ->
                                                    let publicKey = PublicKey.read stream
                                                    let signature = Signature.read stream
                                                    publicKey, signature
                                                ) stream
                    let cost = stream.readNumber8 () //TODO: optimize
                    ContractWitness {
                        contractId = contractId
                        command = command
                        messageBody = messageBody
                        stateCommitment = stateCommitment
                        beginInputs = beginInputs
                        beginOutputs = beginOutputs
                        inputsLength = inputsLength
                        outputsLength = outputsLength
                        signature = signature
                        cost = cost
                    }
                | _ ->
                    let bytes = FixedSizeBytes.read (int count) stream
                    HighVWitness (identifier, bytes)

            if int count = sizeFn value then
                value
            else
                raise SerializationException
    module Output =
        let size ({ lock = lock; spend = spend })=
            Lock.size lock + Spend.size spend

        let write stream = fun { lock = lock; spend = spend } ->
            Lock.write stream lock
            Spend.write stream spend
        let read stream =
            let lock = Lock.read stream
            let spend = Spend.read stream
            { lock = lock; spend = spend }

    module PointedOutput =
        let size (outpoint, output) =
            Outpoint.size outpoint + Output.size output

        let write stream = fun (outpoint, output) ->
            Outpoint.write stream outpoint
            Output.write stream output
        let read stream =
            let outpoint = Outpoint.read stream
            let output = Output.read stream
            outpoint, output

    module Contract =
        let sizeFn = function
            | V0 contract ->
                String.size contract.code +
                    String.size contract.hints +
                    VarInt.size contract.rlimit +
                    VarInt.size contract.queries
            | HighV (_, bytes) ->
                FixedSizeBytes.size bytes

        let size contract =
            let identifier =
                match contract with
                | V0 _ -> Version0
                | HighV (version, _) -> version

            let payloadSize = sizeFn contract

            VarInt.size identifier + VarInt.size (uint32 payloadSize) + payloadSize

        let private writerFn stream = function
            | V0 contract ->
                String.write stream contract.code
                String.write stream contract.hints
                VarInt.write stream contract.rlimit
                VarInt.write stream contract.queries
            | HighV (_, bytes) ->
                FixedSizeBytes.write stream bytes
        let write stream = fun contract ->
            let identifier =
                match contract with
                | V0 _ -> Version0
                | HighV (version, _) -> version

            VarInt.write stream identifier
            VarInt.write stream (sizeFn contract |> uint32)
            writerFn stream contract
        let read stream =
            let version = VarInt.read stream
            let count = VarInt.read stream
            let value =
                match version with
                | Version0 ->
                    let code = String.read stream
                    let hints = String.read stream
                    let rlimit = VarInt.read stream
                    let queries = VarInt.read stream
                    V0 {
                        code = code
                        hints = hints
                        rlimit = rlimit
                        queries = queries
                    }
                | _ ->
                    let bytes = FixedSizeBytes.read (int count) stream
                    Contract.HighV (version, bytes)


            if int count = sizeFn value then
                value
            else
                raise SerializationException

    module Nonce =
        let size = 16

        let write (stream:Stream) = fun (nonce1, nonce2) ->
            stream.writeNumber8 nonce1
            stream.writeNumber8 nonce2
        let read (stream:Stream) =
            let nonce1 = stream.readNumber8 ()
            let nonce2 = stream.readNumber8 ()
            nonce1,nonce2

    module RawTransactionWitness =
        [<Literal>]
        let private WitnessIdentifier = 0u
        [<Literal>]
        let private EmptyPKWitnessIdentifier = 1u

        let sizeFn = function
            | EmptyPKWitness (_, _, keyPath) -> SigHash.size + PublicKey.size +  (String.size keyPath)
            | Witness witness -> Witness.size witness
            | HighVRawWitness (_, bytes) -> FixedSizeBytes.size bytes

        let identifier = function
            | Witness _ -> WitnessIdentifier
            | EmptyPKWitness _ -> EmptyPKWitnessIdentifier
            | HighVRawWitness (identifier,_) -> identifier

        let size witness =
            let payloadSize = sizeFn witness

            VarInt.size (identifier witness) + VarInt.size (uint32 payloadSize) + payloadSize

        let writeFn stream = function
            | Witness witness ->
                Witness.write stream witness
            | EmptyPKWitness (sigHash, publicKey, keyPath) ->
                SigHash.write stream sigHash
                PublicKey.write stream publicKey
                String.write stream keyPath
            | HighVRawWitness (_, bytes) ->
                FixedSizeBytes.write stream bytes

        let write stream witness =
            VarInt.write stream (identifier witness)
            VarInt.write stream (sizeFn witness |> uint32)
            writeFn stream witness

        let read stream =
            let identifier = VarInt.read stream
            let size = VarInt.read stream

            match identifier with
            | WitnessIdentifier ->
                Witness.read stream |> Witness
            | EmptyPKWitnessIdentifier ->
                let sigHash = SigHash.read stream
                let publicKey = PublicKey.read stream
                let keyPath = String.read stream

                EmptyPKWitness (sigHash, publicKey, keyPath)
            | _ ->
                let bytes = FixedSizeBytes.read (int size) stream
                HighVRawWitness (identifier, bytes)

    module RawTransaction =

        let size (tx:RawTransaction) =
            4 + // version
                List.size Input.size tx.inputs +
                List.size Output.size tx.outputs +
                Option.size Contract.size tx.contract +
                List.size RawTransactionWitness.size tx.witnesses

        let write (stream:Stream) (tx:RawTransaction) =
            stream.writeNumber4 tx.version
            List.write Input.write stream tx.inputs
            List.write Output.write stream tx.outputs
            Option.write stream Contract.write tx.contract
            List.write RawTransactionWitness.write stream tx.witnesses

        let read (stream:Stream) =
            let version = stream.readNumber4 ()
            let inputs = List.read Input.read stream
            let outputs = List.read Output.read stream
            let contract = Option.read Contract.read stream
            let witnesses = List.read RawTransactionWitness.read stream

            { version = version; inputs = inputs; witnesses = witnesses; outputs = outputs; contract = contract }:RawTransaction

    module Transaction =
        let size mode tx =
            4 + // version
                List.size Input.size tx.inputs +
                List.size Output.size tx.outputs +
                Option.size Contract.size tx.contract +
                if mode = Full then
                    List.size Witness.size tx.witnesses
                else
                    0

        let write mode (stream:Stream) = fun (tx:Transaction) ->
            stream.writeNumber4 tx.version
            List.write Input.write stream tx.inputs
            List.write Output.write stream tx.outputs
            Option.write stream Contract.write tx.contract
            match mode with
            | Full -> List.write Witness.write stream tx.witnesses
            | WithoutWitness -> ()

        let read mode (stream:Stream) =
            let version = stream.readNumber4 ()
            let inputs = List.read Input.read stream
            let outputs = List.read Output.read stream
            let contract = Option.read Contract.read stream
            let witnesses =
                match mode with
                | Full -> List.read Witness.read stream
                | WithoutWitness -> []

            { version = version; inputs = inputs; witnesses = witnesses; outputs = outputs; contract = contract }


    module TransactionExtended =
        let size (ex:TransactionExtended) = Array.length ex.raw
        let write (stream:Stream) (ex:TransactionExtended) = stream.writeBytes ex.raw (Array.length ex.raw)

        let read (stream:Stream) =
            let beginOffset = stream.Offset

            let tx = Transaction.read Full stream

            let withoutWitnessesSize = Transaction.size WithoutWitness tx
            let size = stream.Offset - beginOffset

            let raw = Array.zeroCreate size
            System.Buffer.BlockCopy(stream.Buffer, beginOffset, raw, 0, size)

            let txHash = Array.zeroCreate Length
            let witnessHash = Array.zeroCreate Length
            let txHasher = new Org.BouncyCastle.Crypto.Digests.Sha3Digest(256)
            txHasher.BlockUpdate(raw,0, withoutWitnessesSize)

            let witnessHasher = new Org.BouncyCastle.Crypto.Digests.Sha3Digest(txHasher)
            witnessHasher.BlockUpdate(raw,withoutWitnessesSize,size-withoutWitnessesSize)

            txHasher.DoFinal(txHash, 0) |> ignore
            witnessHasher.DoFinal(witnessHash, 0) |> ignore

            let txHash = Hash txHash
            let witnessHash = Hash witnessHash

            {
                tx=tx
                txHash=txHash
                witnessHash=witnessHash
                raw=raw
            }

    module Header =
        let size = SerializedHeaderSize

        let write (stream:Stream) = fun header ->
            stream.writeNumber4 header.version
            Hash.write stream header.parent
            stream.writeNumber4 header.blockNumber
            Hash.write stream header.commitments
            stream.writeNumber8 header.timestamp
            stream.writeNumber4 header.difficulty
            Nonce.write stream header.nonce
        let read (stream:Stream) =
            let version = stream.readNumber4 ()
            let parent = Hash.read stream
            let blockNumber = stream.readNumber4 ()
            let commitments = Hash.read stream
            let timestamp = stream.readNumber8 ()
            let difficulty = stream.readNumber4 ()
            let nonce = Nonce.read stream

            {
                version = version
                parent = parent
                blockNumber = blockNumber
                commitments = commitments
                timestamp = timestamp
                difficulty = difficulty
                nonce = nonce
            }

    module Block =
        let size block =
            Header.size +
            List.size (fun _ -> Hash.size) (Block.createCommitments block.txMerkleRoot block.witnessMerkleRoot block.activeContractSetMerkleRoot block.commitments) +
            Seq.size TransactionExtended.size block.transactions

        let write stream bk =
            Header.write stream bk.header
            
            Block.createCommitments bk.txMerkleRoot bk.witnessMerkleRoot bk.activeContractSetMerkleRoot bk.commitments
            |> Seq.write Hash.write stream
            
            Seq.write TransactionExtended.write stream bk.transactions
        let read (stream:Stream) =
            let header = Header.read stream
            let commitments = List.read Hash.read stream
            
            let pop = 
                function
                | next :: rest -> next, rest
                | _ -> raise SerializationException
                
            let transactions = List.read TransactionExtended.read stream

            let txMerkleRoot, commitments = pop commitments
            let witnessMerkleRoot, commitments = pop commitments
            let activeContractSetMerkleRoot, commitments = pop commitments

            {
                header = header;
                txMerkleRoot = txMerkleRoot;
                witnessMerkleRoot = witnessMerkleRoot;
                activeContractSetMerkleRoot = activeContractSetMerkleRoot;
                commitments = commitments;
                transactions = transactions;
            }

    module CGP =
        open VoteData
        open Payout

        module Tally =
            let size = fun (tally:Tally.T) ->
                Map.size (fun (_, votes) -> 
                    Byte.size + 
                    Amount.size votes) tally.allocation +
                Map.size (fun ((recipient, amount), votes) -> 
                    Recipient.size recipient + 
                    Amount.size amount + 
                    Amount.size votes) tally.payout
        
            let write (stream:Stream) = fun (tally:Tally.T) ->
                Map.write (fun stream (allocation, votes)->
                    Byte.write stream allocation
                    Amount.write stream votes) stream tally.allocation
                Map.write (fun stream ((recipient, amount), votes)->
                    Recipient.write stream recipient
                    Amount.write stream amount
                    Amount.write stream votes) stream tally.payout
        
            let read (stream:Stream) =
                {
                    allocation = Map.read (fun stream -> Byte.read stream, Amount.read stream) stream
                    payout = Map.read (fun stream -> VoteData.Payout.read stream, Amount.read stream) stream
                } : Tally.T
                
        let size = fun (cgp:CGP.T) ->
            Byte.size +
            Amount.size cgp.amount +
            Map.size (fun (interval, tally) -> 
                VarInt.size interval + 
                Tally.size tally) cgp.tallies +
            Option.size Payout.size cgp.payout
        let write (stream:Stream) = fun (cgp:CGP.T) ->
            Byte.write stream cgp.allocation
            Amount.write stream cgp.amount
            Map.write (fun stream (interval, tally)->
                VarInt.write stream interval
                Tally.write stream tally) stream cgp.tallies
            Option.write stream Payout.write cgp.payout
        let read (stream:Stream) =
            {
                allocation = Byte.read stream
                amount = Amount.read stream
                tallies = Map.read (fun stream -> VarInt.read stream, Tally.read stream) stream
                payout = Option.read Payout.read stream
            } : CGP.T

open Serialization

module RawTransaction =
    let serialize = serialize RawTransaction.size RawTransaction.write
    let deserialize = deserialize RawTransaction.read
    let toHex = serialize >> FsBech32.Base16.encode

    let fromHex hex =
        FsBech32.Base16.decode hex
        |> Option.bind deserialize

module Transaction =
    let serialize mode = serialize (Transaction.size mode) (Transaction.write mode)
    let deserialize mode = deserialize (Transaction.read mode)

module TransactionExtended =
    let serialize = serialize TransactionExtended.size TransactionExtended.write
    let deserialize = deserialize TransactionExtended.read

module TransactionsRaw =
    let serialize txs = Array.concat txs

module TransactionsExtended =
    let deserialize count = deserialize (List.readBody count TransactionExtended.read)

module Header =
    let serialize = serialize (fun _ -> SerializedHeaderSize) Header.write

    let deserialize bytes =
        if Array.length bytes <> SerializedHeaderSize then
            None
        else
            deserialize Header.read bytes

module Block =
    let serialize = serialize Block.size Block.write
    let deserialize = deserialize Block.read

module Data =
    let serialize = serialize Data.size Data.write
    let deserialize = deserialize Data.read

module Witnesses  =
    let hash witnesses =
        let bytes = serialize (List.size Witness.size) (List.write Witness.write) witnesses

        Hash.compute bytes

module Message =
    let private size c =
        ContractId.size c.recipient + String.size c.command + Option.size Data.size c.body

    let private write stream = fun c ->
        ContractId.write stream c.recipient
        String.write stream c.command
        Option.write stream Data.write c.body

    let serialize = serialize size write

module Outpoint =
    let serialize = serialize Outpoint.size Outpoint.write
    let deserialize = deserialize Outpoint.read

module Input =
    open TxSkeleton

    [<Literal>]
    let private SerializedPointedOutput = 1uy
    [<Literal>]
    let private SerializedMint = 2uy

    let size = function
        | PointedOutput pointedOutput ->
            Byte.size + PointedOutput.size pointedOutput
        | Mint spend ->
            Byte.size + Spend.size spend

    let write stream = function
        | PointedOutput pointedOutput ->
            Byte.write stream SerializedPointedOutput
            PointedOutput.write stream pointedOutput
        | Mint spend ->
            Byte.write stream SerializedMint
            Spend.write stream spend
    let read reader =
        let discriminator = Byte.read reader
        match discriminator with
        | SerializedPointedOutput ->
            let pointedOutput = PointedOutput.read reader
            TxSkeleton.PointedOutput pointedOutput
        | SerializedMint ->
            let spend = Spend.read reader
            TxSkeleton.Mint spend
        | _ ->
            raise SerializationException

module TxSkeleton =
    open TxSkeleton

    let size txSkeleton =
        List.size Input.size txSkeleton.pInputs
        + List.size Output.size txSkeleton.outputs

    let private write stream txSkeleton =
        List.write Input.write stream txSkeleton.pInputs
        List.write Output.write stream txSkeleton.outputs

    let private read stream =
        let pInputs = List.read Input.read stream
        let outputs = List.read Output.read stream
        {
            pInputs = pInputs
            outputs = outputs
        }

    let serialize = serialize size write
    let deserialize = deserialize read


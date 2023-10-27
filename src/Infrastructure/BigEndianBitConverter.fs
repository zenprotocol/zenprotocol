module Infrastructure.BigEndianBitConverter

open System

let private adjustEndiannessIfNeeded bytes =
    if BitConverter.IsLittleEndian then Array.rev bytes else bytes

let uint32ToBytes (value:uint32) =
    value
    |> BitConverter.GetBytes
    |> adjustEndiannessIfNeeded

let toUint32 (bytes:byte array) =
    bytes
    |> adjustEndiannessIfNeeded
    |> fun arr -> BitConverter.ToUInt32 (arr, 0)

let putUInt64 (n:uint64) (buffer:byte[]) (offset:int) =
    let byteForShift shift = (byte)((n >>> shift) &&& 255UL)
    [56; 48; 40; 32; 24; 16; 8; 0]
    |> List.map byteForShift
    |> List.iteri (fun i byte -> buffer.[offset + i] <- byte)
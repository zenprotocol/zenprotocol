module Infrastructure.BigEndianBitConverter

open System

let uint32ToBytes (value:uint32) =
    let bytes = BitConverter.GetBytes value

    if BitConverter.IsLittleEndian then
        Array.rev bytes
    else
        bytes

let toUint32 (bytes:byte array) =
    let bytes = if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    BitConverter.ToUInt32 (bytes,0)
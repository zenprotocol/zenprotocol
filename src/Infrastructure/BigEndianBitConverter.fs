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


let putUInt64 (n:uint64) (buffer:byte[]) (offset:int) =
    buffer.[offset] <- (byte) ((n >>> 56) &&& 255UL)
    buffer.[offset+1] <- (byte) ((n >>> 48) &&& 255UL)
    buffer.[offset+2] <- (byte) ((n >>> 40) &&& 255UL)
    buffer.[offset+3] <- (byte) ((n >>> 32) &&& 255UL)
    buffer.[offset+4] <- (byte) ((n >>> 24) &&& 255UL)
    buffer.[offset+5] <- (byte) ((n >>> 16) &&& 255UL)
    buffer.[offset+6] <- (byte) ((n >>> 8)  &&& 255UL)
    buffer.[offset+7] <- (byte) ((n)       &&& 255UL)
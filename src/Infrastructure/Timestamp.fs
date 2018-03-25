module Infrastructure.Timestamp

open System

type Timestamp = UInt64

let private epoch = (new DateTime(1970, 1, 1, 0, 0, 0, 0))

let now () =
    (DateTime.UtcNow -  epoch).TotalMilliseconds
    |> uint64

let toString (timestamp:Timestamp) =
    (epoch.AddMilliseconds (timestamp |> float)).ToString("yy-MM-dd HH:mm:ss")

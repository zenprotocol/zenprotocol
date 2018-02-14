module Zen.String

open System
open System.Text

let empty = String.Empty

let length = String.length >> Prims.nat

let private getByteCount value = Encoding.UTF8.GetByteCount (value : String)

let byteCount = getByteCount >> Prims.nat

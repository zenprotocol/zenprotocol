module Measure

open Infrastructure
open Logary.Message

let measure<'T> (text:string) (f:Lazy<'T>) : 'T =
    let sw = System.Diagnostics.Stopwatch.StartNew()

    eventX "Begin {text}"
    >> setField "text" text
    |> Log.info
    
    let result = f.Force ()
    sw.Stop ()

    eventX "End {text}. Duration: {duration}ms"
    >> setField "text" text
    >> setField "duration" sw.ElapsedMilliseconds
    |> Log.info

    result
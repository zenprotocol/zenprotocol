module Measure

open Infrastructure

let measure<'T> text (f:Lazy<'T>) : 'T =
    let sw = System.Diagnostics.Stopwatch.StartNew()

    Log.info "Begin %s" text
    let result = f.Force ()
    sw.Stop ()

    Log.info "End %s. Took %Ams" text sw.ElapsedMilliseconds

    result
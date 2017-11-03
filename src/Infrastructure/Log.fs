module Infrastructure.Log

open System.IO

let info (format:FSharp.Core.Printf.TextWriterFormat<'T>) = 
    printf "Info - "
    printfn format
     
let warning (format:FSharp.Core.Printf.TextWriterFormat<'T>) = 
    printf "Warning - "
    printfn format

let error (format:FSharp.Core.Printf.TextWriterFormat<'T>) = 
    printf "Error - "
    printfn format     
    
    
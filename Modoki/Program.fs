module Main

open System.IO

// Completada la implementacion en F# <- igual a 
[<EntryPoint>]
let main argv =
    printfn "Modoki Lang en F# v:\n\n"
    let res = AnalisisLexico.obtenerTokens "sea mut excelente txt"
    match res with 
    | Some tokens -> printf "Hay tokens v: y son:\n%A\n" tokens
    | None -> printf "alv':"
    
    0

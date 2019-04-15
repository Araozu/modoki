module Main
open System.IO

let tokensATxt tokens =
    List.fold (
        fun acc (x: AnalisisLexico.Token) ->
            let datos = 
                sprintf "{\n  valor: \"%s\"\n  posInicio: (%i, %i)\n  posFinal: (%i, %i)\n  tipo: %A\n}\n" x.valor x.posInicio.col 
                <| x.posInicio.fil <| x.posFinal.col <| x.posFinal.fil <| x.tipo
            acc + datos
    ) "" tokens

// Completada la implementacion en F# <- igual a 
[<EntryPoint>]
let main argv =
    printfn "Modoki Lang en F# v:\n\n"
    let res = AnalisisLexico.obtenerTokens "20 + 30 ++ \"olol\" ++ 'v'"
    match res with 
    | Some tokens -> tokensATxt tokens |> printf "%s"
    | None -> printf "alv':"
    
    0

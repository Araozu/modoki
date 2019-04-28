module Main
open System.IO

let tokensATxt tokens =
    List.fold (
        fun acc (x: AnalisisLexico.Token) ->
            let datos = 
                sprintf "{\n  valor: \"%s\"\n  posInicio: (%i, %i)\n  posFinal: (%i, %i)\n  tipo: %A\n}\n" x.valor x.posInicio.fil 
                <| x.posInicio.col <| x.posFinal.fil <| x.posFinal.col <| x.tipo
            acc + datos
    ) "" tokens

// Completada la implementacion en F# <- igual a 
[<EntryPoint>]
let main argv =
    printfn "Modoki Lang en F#\n\n"
    
    let res = AnalisisLexico.obtenerTokens "sea num1 = 20\nsea num2 = 40"
    match res with 
    | Some tokens -> 
        tokensATxt tokens |> printf "%s"
    | None -> printf "alv':"
    
    
    0

module Main
open System

let tokensATxt tokens =
    List.fold (
        fun acc (x: AnalisisLexico.Token) ->
            let datos = 
                sprintf "{\n  valor: \"%s\"\n  posInicio: (%i, %i)\n  posFinal: (%i, %i)\n  tipo: %A\n}\n" x.valor x.posInicio.fil 
                <| x.posInicio.col <| x.posFinal.fil <| x.posFinal.col <| x.tipo
            acc + datos
    ) "" tokens

let rec iniciarREPL () =
    printf "> "
    let entrada = Console.ReadLine()
    if entrada = ":s" then
        ()
    else
        let tokens = AnalisisLexico.obtenerTokens entrada
        match tokens with 
        | Some tokens ->
            let tokens' = TokenMap.tokenMap tokens
            let ast = Arbol.construirAst tokens'
            printfn " Inorden  :> %s\n Preorden :> %s" ( Arbol.inorden ast ) (Arbol.preorden ast)
        | None -> printf "alv':"
        iniciarREPL ()

// Completada la implementacion en F# <- igual a 
[<EntryPoint>]
let main argv =
    printfn "Modoki REPL"
    printfn "Ingresa una expresión para evaluarla. Ingresa :s para salir."
    
    iniciarREPL()
    
    printfn "See ya!"
    0

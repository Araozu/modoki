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

let imprimirTokenAnotado (token: TokenMap.Token) =
    sprintf " %s : %s" token.valor (Tipos.obtSignature token.signature)

let rec imprAstAnotado (ast: AST.Ast) =
    match ast with
    | AST.Hoja -> ""
    | AST.Nodo (token, AST.Hoja, _) ->
        imprimirTokenAnotado token
    | AST.Nodo (token, izq, der) ->
        imprimirTokenAnotado token + "\n" + imprAstAnotado izq + "\n" + imprAstAnotado der 

let rec validarAst (ast: AST.Ast) =
    " " +
    try
        Tipos.obtSignature <| AnalisisSemantico.validarAst ast
    with
    | Failure msg -> msg

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
            let ast = AST.construirAst tokens' |> AnalisisSemantico.anotarAst
            // printfn " Inorden  :> %s\n Preorden :> %s\n" (AST.inorden ast) (AST.preorden ast)
            printfn " Anotaciones:\n%s\n" (imprAstAnotado ast)
            printfn " Validez:\n%s\n" (validarAst ast)
            printfn " AST:\n%s" (AST.imprimirAst ast)
        | None -> printf "alv':"
        iniciarREPL ()

[<EntryPoint>]
let main _ =
    printfn "Modoki REPL"
    printfn "Ingresa una expresión para evaluarla. Ingresa :s para salir."
    
    iniciarREPL ()
    
    printfn "See ya!"
    0

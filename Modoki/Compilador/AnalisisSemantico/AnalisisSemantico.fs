module AnalisisSemantico
open System
open AST
open TablaSimbolos

let identificarTipo token =
    ()

let anotarToken (token: TokenMap.Token) signature = { token with signature = signature }

let rec anotarAst ast =
    match ast with
    | Hoja -> Hoja
    | Nodo (token, izq, der) ->
        let nuevoToken =
            match token.tipo with
            | AnalisisLexico.Operador ->
                let tipoAnotadoOption = TablaSimbolos.buscarEnTablaSimbolos token.valor
                match tipoAnotadoOption with
                | Some (TipoAnotado (_, tipo)) -> anotarToken token tipo
                | None ->
                    sprintf "Error. El operador %s no está definido." token.valor
                    |> Exception |> raise
            | AnalisisLexico.NumeroLiteral ->
                anotarToken token TablaSimbolos.entero
            | _ -> raise ( Exception <| sprintf "Error. %A no está soportado :c" token.tipo )
        Nodo (nuevoToken, anotarAst izq, anotarAst der)


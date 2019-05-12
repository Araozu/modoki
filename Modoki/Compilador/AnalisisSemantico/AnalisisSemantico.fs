module AnalisisSemantico
open AST
open TablaSimbolos

let anotarToken (token: AnalisisLexico.Token) signature = { token with signature = signature }

let rec anotarAst ast =
    match ast with
    | Hoja -> Hoja
    | Nodo (token, izq, der) ->
        let ( <-< ) = anotarToken
        let nuevoToken =
            match token.tipo with
            | AnalisisLexico.Operador ->
                let tipoAnotadoOption = TablaSimbolos.buscarEnTablaSimbolos token.valor
                match tipoAnotadoOption with
                | Some (TipoAnotado (_, tipo)) -> anotarToken token tipo
                | None ->
                    failwithf "Error. El operador %s no está definido." token.valor
            | AnalisisLexico.NumeroLiteral -> token <-< TablaSimbolos.entero
            | AnalisisLexico.TextoLiteral -> token <-< TablaSimbolos.string
            | AnalisisLexico.CaracterLiteral -> token <-< TablaSimbolos.caracter
            | _ -> failwithf "Error. %A no está soportado :c" token.tipo
        Nodo (nuevoToken, anotarAst izq, anotarAst der)

let rec validarAst ast =
    match ast with
    | Hoja -> Tipos.sinTipo
    | Nodo (token, izq, der) ->
        match token.tipo with
        | AnalisisLexico.Operador ->
            match token.signature with
            | Tipos.Funcion (tipoIzq, Tipos.Funcion (tipoDer, tipoRes)) ->
                let izqValido = tipoIzq = validarAst izq
                let derValido = tipoDer = validarAst der
                if izqValido && derValido then
                    tipoRes
                else if not izqValido then
                    failwithf "Error. Se esperaba un %s a la izq del operador %s, pero se encontró un %s."
                    <| Tipos.obtSignature tipoIzq <| token.valor <| Tipos.obtSignature (validarAst izq)
                else
                    failwithf "Error. Se esperaba un %s a la der del operador %s, pero se encontró un %s."
                    <| Tipos.obtSignature tipoDer <| token.valor <| Tipos.obtSignature (validarAst der)
            | _ -> failwith "Error. Los operadores deben ser funciones de tipo 'a -> 'b -> 'c"
        | AnalisisLexico.NumeroLiteral -> TablaSimbolos.entero
        | AnalisisLexico.TextoLiteral -> TablaSimbolos.string
        | AnalisisLexico.CaracterLiteral -> TablaSimbolos.caracter
        | _ -> Tipos.sinTipo




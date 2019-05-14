module AnalisisSemantico
open AST
open TablaSimbolos
open Tipos

let anotarToken (token: AnalisisLexico.Token) signature = { token with signature = signature }

let rec anotarAst ast =
    match ast with
    | Hoja -> Hoja
    | Nodo (token, izq, der) ->
        let ( <-< ) = anotarToken
        let nuevoToken =
            match token.tipo with
            | AnalisisLexico.Operador ->
                let tipoAnotadoOption = buscarEnTablaSimbolos token.valor
                match tipoAnotadoOption with
                | Some (TipoAnotado (_, tipo)) -> anotarToken token tipo
                | None ->
                    failwithf "Error. El operador %s no está definido." token.valor
            | AnalisisLexico.NumeroLiteral -> token <-< entero
            | AnalisisLexico.TextoLiteral -> token <-< string
            | AnalisisLexico.CaracterLiteral -> token <-< caracter
            | AnalisisLexico.Identificador ->
                match buscarEnTablaSimbolos token.valor with
                | Some (TipoAnotado (_, sign)) -> token <-< sign
                | None -> failwithf "Error. El valor %s no existe." token.valor
            | AnalisisLexico.FunAppl -> token <-< funAppl
            | _ -> failwithf "Error. %A no está soportado :c" token.tipo
        Nodo (nuevoToken, anotarAst izq, anotarAst der)

let rec validarAst ast =
    match ast with
    | Hoja -> Tipos.sinTipo
    | Nodo (token, izq, der) ->
        match token.tipo with
        | AnalisisLexico.Operador ->
            match token.signature with
            | Funcion (tipoIzq, Funcion (tipoDer, tipoRes)) ->
                let izqValido = tipoIzq = validarAst izq
                let derValido = tipoDer = validarAst der
                if izqValido && derValido then
                    tipoRes
                else if not izqValido then
                    failwithf "Error. Se esperaba un %s a la izq del operador %s, pero se encontró un %s."
                    <| obtSignature tipoIzq <| token.valor <| obtSignature (validarAst izq)
                else
                    failwithf "Error. Se esperaba un %s a la der del operador %s, pero se encontró un %s."
                    <| obtSignature tipoDer <| token.valor <| obtSignature (validarAst der)
            | _ -> failwith "Error. Los operadores deben ser funciones de tipo 'a -> 'b -> 'c"
        | AnalisisLexico.NumeroLiteral -> entero
        | AnalisisLexico.TextoLiteral -> string
        | AnalisisLexico.CaracterLiteral -> caracter
        | AnalisisLexico.Identificador ->
            match buscarEnTablaSimbolos token.valor with
            | Some (TipoAnotado (_, sign)) -> sign
            | None -> failwithf "Error. %s no es una funcíon." token.valor
        | AnalisisLexico.FunAppl ->
            let tipoIzq = validarAst izq
            let tipoDer = validarAst der
            match tipoIzq with
            // a -> b
            | Funcion (izqFun, derFun) ->
                if izqFun = tipoDer then derFun
                else failwithf "Error. Se esperaba un %s, pero se obtuvo un %s"
                         (obtSignature tipoDer) (obtSignature izqFun) 
            | _ ->
                match izq with
                | Nodo (token', _, _) ->
                    failwithf "Error. %s no es una función." token'.valor
                | Hoja -> failwith "Error. No existe identificador a la izq de funAppl"
        | _ -> sinTipo




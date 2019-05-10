module AST
open System
open TokenMap

type Ast =
    | Nodo of TokenMap.Token * Ast * Ast
    | Hoja

let rec preorden arbol =
    match arbol with
    | Nodo (t, i, d) -> t.valor + " " + (preorden i) + (preorden d)
    | Hoja -> ""

let rec inorden arbol =
    match arbol with
    | Nodo (t, i, d) -> (inorden i) + t.valor + " " + (inorden d)
    | Hoja -> "" 

type Asoc = L | R


let rec insertarAAST ast token =
    match ast with
    | Hoja -> Nodo (token, Hoja, Hoja)
    | Nodo (t, Hoja, Hoja) ->
        if token.precedencia > t.precedencia && t.precedencia = 0 then
            Nodo (token, ast, Hoja)
        else
            printfn "Token problematico:\n %A \n" token
            printfn "pr token Nuevo: %i -> pr token Actual: %i " token.precedencia t.precedencia
            raise (Exception "Error. No se pueden poner 2 valores juntos v:<")
    | Nodo (t, i, Hoja) ->
        if token.precedencia = 0 then
            Nodo (t, i, Nodo (token, Hoja, Hoja))
        else
            printfn "Token problematico:\n %A \n" token
            raise (Exception "Error. No se pueden poner 2 operadores juntos.")
    | Nodo (t, i, d) ->
        
        if token.precedencia <= t.precedencia && token.precedencia <> 0 then
            Nodo (token, ast, Hoja)
        else if token.precedencia <= t.precedencia && token.precedencia = 0 then
            Nodo (t, i, insertarAAST d token)
        else
            Nodo (t, i, insertarAAST d token)

let construirAst tokens =
    let rec construirAst' ast tokens =
        match tokens with
        | [] -> ast
        | x :: xs -> 
             let nuevoAst = insertarAAST ast x
             construirAst' nuevoAst xs

    construirAst' Hoja tokens
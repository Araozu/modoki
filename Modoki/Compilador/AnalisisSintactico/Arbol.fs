module Arbol
open AnalisisLexico

type Token = {
    valor: string
    posInicio: datosPosicion
    posFinal: datosPosicion
    tipo: tipoToken
    precedencia: int
}

type Ast =
    | Nodo of Token * Ast * Ast
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

let obtPrecedencia token =
    if token.tipo = tipoToken.Operador then
        match token.valor with
        | "<|" | "|>" -> 1
        | "+" -> 2
        | "-" -> 3
        | "*" | "/" | "%" | "%%" -> 4
        | "**" -> 5
        | "f" -> 6
        | "==" | "!=" | "!" | "€" -> 7
        | "." | "?" | "?:" | "?." -> 8
        | _ -> 0
    else
        0

let rec insertarAAST ast token =
    match ast with
    | Hoja -> Nodo (token, Hoja, Hoja)
    | Nodo (t, i, d) ->
        if t.precedencia > token.precedencia && t.precedencia = 0 then
            Nodo (token, ast, Hoja)
        else if t.precedencia > token.precedencia && t.precedencia <> 0 && token.precedencia <> 0 then
            Nodo (token, ast, Hoja)
        else match d with
             | Hoja -> Nodo (t, i, Nodo (token, Hoja, Hoja))
             | Nodo (t', i', d') -> Nodo (t', i', insertarAAST d token)



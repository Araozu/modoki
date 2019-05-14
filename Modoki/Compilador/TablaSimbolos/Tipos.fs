module Tipos

type Tipo =
    | Identificador
    | FunAppl
    | Tipo of string
    | Lista of Tipo
    | Funcion of Tipo * Tipo

let sinTipo = Tipo "null"
let vacio = Tipo "vacio"

let rec obtSignature tipo =
    match tipo with
    | FunAppl -> "funAppl"
    | Identificador -> "identificador"
    | Tipo nombre -> nombre
    | Lista tipo -> "[" + obtSignature tipo + "]"
    | Funcion (t1, t2) ->
        match t1 with
        | Funcion _ -> "(" + obtSignature t1 + ") -> " + obtSignature t2
        | _ -> obtSignature t1 + " -> " + obtSignature t2

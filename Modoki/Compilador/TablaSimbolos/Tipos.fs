module Tipos

type Tipo =
    | Tipo of string
    | Funcion of Tipo * Tipo

let sinTipo = Tipo "null"
let vacio = Tipo "vacio"

let rec obtSignature tipo =
    match tipo with
    | Tipo nombre -> nombre
    | Funcion (t1, t2) ->
        match t1 with
        | Tipo _ -> obtSignature t1 + " -> " + obtSignature t2
        | Funcion _ -> "(" + obtSignature t1 + ") -> " + obtSignature t2



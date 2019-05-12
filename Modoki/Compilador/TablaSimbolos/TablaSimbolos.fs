module TablaSimbolos
open Tipos

type TipoAnotado = TipoAnotado of string * Tipo

let entero = Tipo "Ent"
let caracter = Tipo "Carac"
let string = Lista caracter

let entFunBuilder x = TipoAnotado (x, Funcion (entero, Funcion (entero, entero)))

let funsEnteros = ["+"; "-"; "*"; "/"; "%"; "**"]

let funConcatenarTxt = TipoAnotado ("++", Funcion (string, Funcion (string, string)))
let funMultTxt = TipoAnotado (".+.", Funcion (string, Funcion (entero, string)))
let tablaSimbolos = [funConcatenarTxt; funMultTxt] @ List.map (fun s -> entFunBuilder s) funsEnteros

let buscarEnTablaSimbolos nombreFun =
    try
        tablaSimbolos
        |> List.find
            (
                fun t ->
                    let (TipoAnotado (nombre, _)) = t
                    nombre = nombreFun
            )
        |> Some
    with _ -> None








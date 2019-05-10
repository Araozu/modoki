module TablaSimbolos
open Tipos

type TipoAnotado = TipoAnotado of string * Tipo

let entero = Tipo "ent"

let entFunBuilder x = TipoAnotado (x, Funcion (entero, Funcion (entero, entero)))

let funsEnteros = ["+"; "-"; "*"; "/"; "%"; "**"]

let tablaSimbolos = List.map (fun s -> entFunBuilder s) funsEnteros

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








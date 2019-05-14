module TablaSimbolos
open Tipos

type TipoAnotado = TipoAnotado of string * Tipo

let entero = Tipo "Ent"
let caracter = Tipo "Carac"
let string = Lista caracter

let identificador = Identificador
let funAppl = FunAppl

let entFunBuilder x = TipoAnotado (x, Funcion (entero, Funcion (entero, entero)))

let funsEnteros = ["+"; "-"; "*"; "/"; "%"; "**"]

let ( >-> ) f g = Funcion (f, g)

let largoTxt = TipoAnotado ("largoTxt", string >-> caracter) 

let vacio = Tipo "()"
let funConcatenarTxt = TipoAnotado ("++", string >-> (string >-> string))
let funMultTxt = TipoAnotado (".+.", string >-> (entero >-> string))
let funImpr = TipoAnotado ("impr", string >-> vacio)
let funImprMult = TipoAnotado ("imprMult", string >-> (string >-> entero))

let tablaSimbolos = [funConcatenarTxt; funMultTxt; funImpr; funImprMult]
                    @ List.map (fun s -> entFunBuilder s) funsEnteros

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








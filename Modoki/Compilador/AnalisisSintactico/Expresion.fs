module Expresion
open Alfabeto
open System

(*  
sea h = 
    sea m =
        sea i = 30
        i * 2
    m + 10
    
// <=>
sea h =
    sea m =
        sea i = 30 en i * 2
    en m + 10
    
// <==>
[<Modulo>]
nombre: Expresion
archivo: expresion.fs
declaraciones: [
    [<Declaracion>]
    nombre: h
    valor: 
        [<Binding>]
        nombre: m
        valor: 
            [<Binding>]
            nombre: i
            valor: 30
            en:
                [<AppFun>]
                    param1: 
                        [<AppFun>]
                        param1: *
                        param2: i
                    param2: 2
        en:
            [<AppFun>]
            param1:
                [<AppFun>]
                param1: +
                param2: m
            param2: 10
]    
*)

type AppFun = { param1: string
              ; param2: string
              ; tipo: string
              }

type Fun = { nombre: string
           ; tipo: string
           ; param: string
           }

type Valor = { tipo: string
             ; valor: string
             }

type Binding = { nombre: string option
               ; tipo: string
               ; valor: Expr
               ; en: Expr
               }

and Expr = Binding of Binding | Valor of Valor | Fun of Fun | AppFun of AppFun


type Declaracion = { nombre: string
                   ; tipo: string
                   ; valor: Expr
                   }

type Modulo = { nombre: string
              ; archivo: string
              ; declaraciones: Declaracion list
              }
    
let agregarDeclaracion modulo declaracion = 
    { modulo with declaraciones = [declaracion] @ modulo.declaraciones }
    

let obtenerExpr lista =
    
    (lista, Valor { tipo="?"; valor="1" })



let construirDeclaracion (lista: AnalisisLexico.Token list) modActual nombre =
    match lista with
    | [] -> raise (Exception "Error. La declaracion esta incompleta.")
    | x :: xs ->
        match x.valor with
        | "=" -> 
            let (nuevaLista, expr) = obtenerExpr xs
            let nuevaDeclaracion = { nombre=nombre; tipo="?"; valor = expr}
            (nuevaLista, agregarDeclaracion modActual nuevaDeclaracion)
        | _ ->
            raise (Exception "Error. La declaracion no tiene valor.") 


// AnalisisLexico.Token list -> Modulo
let rec construir (entrada: AnalisisLexico.Token list) modActual =
    match entrada with
    | [] -> modActual
    | x :: y :: xs ->
        match (x.valor, y.tipo) with
        | ("sea", AnalisisLexico.Identificador) ->
            let (nuevaLista, nuevoMod) = construirDeclaracion xs modActual y.valor
            construir nuevaLista nuevoMod
        | (_, _) -> raise ( Exception "Error. En top-level solo se pueden tener declaraciones." )
    | _ :: _ -> raise ( Exception "Error. La declaracion no tiene nombre." )

let construirParseTree (entrada: AnalisisLexico.Token list) =
    let moduloInicial = { nombre = "Ejemplo"; archivo = "ejemplo.gi"; declaraciones = [] }
    
    construir entrada moduloInicial

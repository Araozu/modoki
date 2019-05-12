module AST
open System

type Ast =
    | Nodo of AnalisisLexico.Token * Ast * Ast
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

let imprimirAst ast =
    let rec impr ast =
        match ast with
        | Hoja -> ("", "", 0)
        | Nodo (token, Hoja, Hoja) ->
            let res = " " + token.valor + " "
            let resLargo = res.Length
            let espBlanco = List.map (fun _ -> " ") [0 .. resLargo] |> List.fold (+) ""
            (res, espBlanco, resLargo)
        | Nodo (token, izq, der) ->
            let tokenEsMod2 = token.valor.Length % 2 = 0
            let tokenValor = if tokenEsMod2 then token.valor + " " else token.valor
            let largoRetr = (tokenValor.Length - 1) / 2
            
            let (txtizqSup, txtizqInf, largoIzq) = impr izq
            let (txtderSup, txtderInf, largoDer) = impr der
            
            let espBlancoIzq =
                List.map (fun _ -> " ") [0 .. (largoIzq - largoRetr - 1)]
                |> List.fold (+) ""
                
            let espBlancoDer =
                List.map (fun _ -> " ") [0 .. (largoDer - largoRetr - 1)]
                |> List.fold (+) ""
            
            let parteSuperior = espBlancoIzq + tokenValor + espBlancoDer
            let parteInferior = txtizqSup + "|" + txtderSup + "\n" + txtizqInf + txtderInf
             
            (parteSuperior, parteInferior, parteSuperior.Length)

    let (sup, inf, _) = impr ast
    sup + "\n" + inf

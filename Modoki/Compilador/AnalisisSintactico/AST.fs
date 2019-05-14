module AST

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

let tokenFunAppl =
    { AnalisisLexico.tokenVacio with valor = "|f|"; precedencia = 0; tipo = AnalisisLexico.FunAppl }

let crearNodo token = Nodo (token, Hoja, Hoja)

let rec insertarAAST ast (token: AnalisisLexico.Token) =
    match ast with
    | Hoja ->
        match token.tipo with
        | AnalisisLexico.Operador -> failwith "Los operadores infijos no se pueden usar como funciones."
        | _ -> crearNodo token
    | Nodo (_, Hoja, Hoja) ->
        match token.tipo with
        | AnalisisLexico.Operador -> Nodo (token, ast, Hoja)
        | _ -> Nodo (tokenFunAppl, ast, crearNodo token)
    | Nodo (vToken, izq, Hoja) ->
        match vToken.tipo with
        | AnalisisLexico.Operador ->
            match token.tipo with
            | AnalisisLexico.Operador -> failwith "Se encontraron 2 operadores juntos"
            | _ -> Nodo (vToken, izq, crearNodo token)
        | _ -> failwith "Err tec. FunAppl no tiene valor al que applicarse. (?)"
    | Nodo (vToken, izq, der) ->
        match vToken.tipo with
        | AnalisisLexico.Operador ->
            match token.tipo with
            | AnalisisLexico.Operador when token.precedencia > vToken.precedencia ->
                Nodo (vToken, izq, insertarAAST der token)
            | AnalisisLexico.Operador ->
                Nodo (token, ast, Hoja)
            | _ -> Nodo (vToken, izq, insertarAAST der token)
        | AnalisisLexico.FunAppl ->
            match token.tipo with
            | AnalisisLexico.Operador -> Nodo (token, ast, Hoja)
            | _ -> Nodo (tokenFunAppl, ast, crearNodo token)
        | _ -> failwith "¿Qué pasó aquí?"

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

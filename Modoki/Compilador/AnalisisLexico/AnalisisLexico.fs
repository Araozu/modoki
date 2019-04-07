module AnalisisLexico
open System.Text.RegularExpressions
open Alfabeto

type tipoToken =
    | NuevaLinea
    | Identificador
    | IdentificadorObj
    | TextoLiteral
    | CaracterLiteral
    | NumeroLiteral
    | BoolLiteral
    | Operador
    | EspacioBlanco
    | Ninguno
    | Identacion

type tipoCaracActual =
    | A_NuevaLinea
    | A_EspacioBlanco
    | A_Mayuscula
    | A_Minuscula
    | A_GuionBajo
    | A_ComillaDoble
    | A_ComillaSimple
    | A_Numero
    | A_Operador
    | A_Otros

type datosPosicion = {
    col: int
    fil: int
    posAbs: int
    posAbsFil: int
}

type Token = {
    valor: string
    posInicio: datosPosicion
    posFinal: datosPosicion
    tipo: tipoToken
}

type fms =
    | Inicio
    | Identificador
    | Texto
    | Caracter
    | Numero
    | Identacion
    | Operador

let detectarToken str =

    let regex texto patron =
        let rx = new Regex (patron)
        rx.IsMatch texto
    
    match true with
    | _ when regex str nuevaLinea -> A_NuevaLinea
    | _ when regex str espacioBlanco -> A_EspacioBlanco
    | _ when regex str mayuscula -> A_Mayuscula
    | _ when regex str minuscula -> A_Minuscula
    | _ when regex str guionBajo -> A_GuionBajo
    | _ when regex str comillaDoble -> A_ComillaDoble
    | _ when regex str comilla -> A_ComillaSimple
    | _ when regex str operadores -> A_Operador
    | _ when regex str numero -> A_Numero
    | _ -> A_Otros


type estadoDeFms = Terminado | Error of string | Continua of string

let obtenerTokens (entrada: string) =

    let posVacia = { fil = 0; col= 0; posAbs = 0; posAbsFil = 0 };

    let tokenVacio = {
        valor = ""
        posInicio = posVacia
        posFinal = posVacia
        tipo = Ninguno
    }
    
    let crearToken valor posInicio posFinal tipo =
        { valor = valor; posInicio = posInicio; posFinal = posFinal; tipo = tipo }
        
    let rec reconocerToken tokenActual posActual estadoActual valorActual =
    
        let aumentarPosCol () = 
            { 
                col = posActual.col + 1
                fil = posActual.fil
                posAbs = posActual.posAbs + 1
                posAbsFil = posActual.posAbsFil
            }
        let aumentarPosFil () = 
            {
                col = 0
                fil = posActual.fil + 1
                posAbs = posActual.posAbs + 1
                posAbsFil = posActual.posAbs + 1
            }
    
        let caracActual = 
            try entrada.[posActual.posAbs]
            with 
            | _ -> ' '
            
        let tipoCarac = detectarToken <| caracActual.ToString ()
        
        (* Maquina de estado finito *)
        let (tokenActual, nuevoEstado, nuevaPos, estado) =
            match estadoActual with
            | Inicio ->
                match tipoCarac with 
                | A_NuevaLinea ->
                    (
                        crearToken "\n" posActual posActual NuevaLinea,
                        Inicio,
                        aumentarPosFil(),
                        Continua("")
                    )
                // Esta cosa tiene que diferenciar espacios al inicio y entre tokens
                | A_EspacioBlanco ->
                    (
                        crearToken " " posActual posActual tipoToken.Identacion,
                        Inicio,
                        aumentarPosCol (),
                        Continua ""
                    );
                | A_Mayuscula -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia IdentificadorObj,
                        Identificador,
                        aumentarPosCol (),
                        Continua ""
                    );
                | A_Minuscula | A_GuionBajo -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Identificador, 
                        Identificador, 
                        aumentarPosCol (),
                        Continua ""
                    );
                | A_ComillaDoble -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia TextoLiteral, 
                        Texto, 
                        aumentarPosCol (),
                        Continua ""
                    );
                | A_ComillaSimple ->
                    (
                        crearToken (caracActual.ToString()) posActual posVacia CaracterLiteral, 
                        Caracter, 
                        aumentarPosCol (), 
                        Continua ""
                    );
                | A_Numero -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia NumeroLiteral, 
                        Numero, 
                        aumentarPosCol (), 
                        Continua("")
                    );
                // Hacerlo más granular haciendo que los operadores sean más específicos?
                | A_Operador ->
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Operador,
                        Operador,
                        aumentarPosCol (), 
                        Continua ""
                    )
                | A_Otros ->
                    let largoLista = [0..(posActual.posAbs - posActual.posAbsFil)]
                    let indicadorError =
                        let unirLista ls = ls @ ['^']
                        let res = match largoLista with
                                  | _::xs -> xs
                                  | [] -> []
                        List.map (fun _ -> '_') res
                        |> unirLista
                        |> List.toArray
                        |> System.String
                        |> sprintf "%s" 
                        
                    let textoError = 
                        largoLista
                        |> List.mapi (fun pos _ ->
                            entrada.[posActual.posAbsFil + pos]
                        )
                        |> List.toArray
                        |> System.String
                        |> sprintf "%s"
                        
                    let razonError = 
                        sprintf "Error al crear los tokens. No se esperaba el caracter %c en la pos. %i,%i :\n%s \n%s \n"
                        <| caracActual
                        <| posActual.fil + 1
                        <| posActual.col + 1
                        <| textoError
                        <| indicadorError
                    
                    (tokenActual, estadoActual, posActual, Error razonError)
                
            | _ -> (tokenActual, estadoActual, posActual, Terminado)
        
        match estado with 
        | Terminado -> Some (tokenActual, nuevaPos)
        | Error razon->
            printf "%s" razon
            None
        | Continua valorActual -> reconocerToken tokenActual nuevaPos nuevoEstado valorActual

    let rec crearListaTokens tokensActuales posActual =
        if posActual.posAbs >= String.length entrada then
            Some tokensActuales
        else
            let res = reconocerToken tokenVacio posActual Inicio ""
            match res with
            | Some (nuevoToken, pos) -> crearListaTokens (tokensActuales @ [nuevoToken]) pos
            | None -> None
        
    crearListaTokens [] posVacia
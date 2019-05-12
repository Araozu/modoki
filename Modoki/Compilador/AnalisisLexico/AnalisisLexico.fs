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
    | Comentario
    | ComentarioMult // Son tokens porque al transpilar se conservan las comentarios.

type tipoCaracActual =
    | A_NuevaLinea
    | A_EspacioBlanco
    | A_Mayuscula
    | A_Minuscula
    | A_GuionBajo
    | A_ComillaDoble
    | A_ComillaSimple
    | A_Numero
    | A_Barra
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
    precedencia: int
    signature: Tipos.Tipo
}

type fms =
    | InicioLinea
    | Inicio
    | Identificador
    | IdentificadorObj
    | Texto
    | Caracter
    | Caracter_Final
    | Numero
    | Identacion
    | Operador
    | PreComentario
    | Comentario

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
    | _ when regex str barra -> A_Barra
    | _ when regex str operadores -> A_Operador
    | _ when regex str numero -> A_Numero
    | _ -> A_Otros


type estadoDeFms = Terminado | Error of string | Continua

let obtenerTokens (entrada: string) =

    let posVacia =
        { fil = 0; col= 0; posAbs = 0; posAbsFil = 0; };

    let tokenVacio = {
        valor = ""
        posInicio = posVacia
        posFinal = posVacia
        tipo = Ninguno
        precedencia = -1
        signature = Tipos.sinTipo
    }
    
    let crearToken valor posInicio posFinal tipo =
        { tokenVacio with valor = valor; posInicio = posInicio; posFinal = posFinal; tipo = tipo; }
        
    let rec reconocerToken tokenActual posActual estadoActual =
    
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
        let aumentarValor token valor = { token with valor = token.valor + valor.ToString (); }
        let terminarToken token = { token with posFinal = posActual; }
    
        let caracActual =
            // Cambiado a revision del largo del str porque... JavaScript... undefined...
            if posActual.posAbs < entrada.Length then 
                try entrada.[posActual.posAbs]
                with 
                | _ -> ' '
            else ' '
            
        let tipoCarac = detectarToken <| caracActual.ToString ()
        
        (* Maquina de estado finito *)
        // TODO: Agregar soporte para comentarios.
        let (tokenActual, nuevoEstado, nuevaPos, estado) =
            match estadoActual with
            | InicioLinea ->
                match tipoCarac with 
                | A_NuevaLinea ->
                    (
                        crearToken "\n" posActual posActual NuevaLinea,
                        InicioLinea,
                        aumentarPosFil(),
                        Terminado
                    )
                | A_EspacioBlanco ->
                    (
                        crearToken " " posActual posActual tipoToken.Identacion,
                        Identacion,
                        aumentarPosCol (),
                        Continua
                    )
                | A_Mayuscula -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.IdentificadorObj,
                        IdentificadorObj,
                        aumentarPosCol (),
                        Continua
                    )
                | A_Minuscula | A_GuionBajo -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Identificador, 
                        Identificador, 
                        aumentarPosCol (),
                        Continua
                    )
                | A_ComillaDoble -> 
                    (
                        crearToken "\"" posActual posVacia TextoLiteral, 
                        Texto, 
                        aumentarPosCol (),
                        Continua
                    );
                | A_ComillaSimple ->
                    (
                        crearToken "'" posActual posVacia CaracterLiteral, 
                        Caracter, 
                        aumentarPosCol (), 
                        Continua
                    )
                | A_Numero -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia NumeroLiteral, 
                        Numero, 
                        aumentarPosCol (), 
                        Continua
                    )
                | A_Barra ->
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Comentario,
                        PreComentario,
                        aumentarPosCol (),
                        Continua
                    )
                // TODO: Hacerlo más granular haciendo que los operadores sean más específicos?
                | A_Operador ->
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Operador,
                        Operador,
                        aumentarPosCol (), 
                        Continua
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
            | Inicio ->
                match tipoCarac with 
                | A_NuevaLinea ->
                    (
                        crearToken "\n" posActual posActual NuevaLinea,
                        InicioLinea,
                        aumentarPosFil(),
                        Terminado
                    )
                | A_EspacioBlanco ->
                    (
                        tokenVacio,
                        Inicio,
                        aumentarPosCol (),
                        Continua
                    )
                | A_Mayuscula -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.IdentificadorObj,
                        IdentificadorObj,
                        aumentarPosCol (),
                        Continua
                    )
                | A_Minuscula | A_GuionBajo -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Identificador, 
                        Identificador, 
                        aumentarPosCol (),
                        Continua
                    )
                | A_ComillaDoble -> 
                    (
                        crearToken "\"" posActual posVacia TextoLiteral,
                        Texto, 
                        aumentarPosCol (),
                        Continua
                    )
                | A_ComillaSimple ->
                    (
                        crearToken "'" posActual posVacia CaracterLiteral, 
                        Caracter, 
                        aumentarPosCol (), 
                        Continua
                    )
                | A_Numero -> 
                    (
                        crearToken (caracActual.ToString()) posActual posVacia NumeroLiteral, 
                        Numero, 
                        aumentarPosCol (), 
                        Continua
                    )
                | A_Barra ->
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Comentario,
                        PreComentario,
                        aumentarPosCol (),
                        Continua
                    )
                // TODO: Hacerlo más granular haciendo que los operadores sean más específicos?
                | A_Operador ->
                    (
                        crearToken (caracActual.ToString()) posActual posVacia tipoToken.Operador,
                        Operador,
                        aumentarPosCol (), 
                        Continua
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
            | Identificador | IdentificadorObj ->
                match tipoCarac with
                | A_Mayuscula |  A_Minuscula | A_GuionBajo | A_Numero ->
                    (
                        aumentarValor tokenActual caracActual,
                        estadoActual,
                        aumentarPosCol (),
                        Continua
                    )
                | _ ->
                    (
                        terminarToken tokenActual, 
                        Inicio, 
                        posActual, 
                        Terminado
                    )
            // TODO: Agregar soporte para escapar caracteres, e interpolacion.
            | Texto ->
                match tipoCarac with 
                | A_ComillaDoble ->
                    (
                        aumentarValor tokenActual "\"" |> terminarToken,
                        Inicio,
                        aumentarPosCol (),
                        Terminado
                    ) 
                | _ ->
                    (
                        aumentarValor tokenActual caracActual,
                        estadoActual,
                        aumentarPosCol (),
                        Continua
                    )
            // TODO: Agregar soporte para escapar caracteres, e UTF-8 escapado (\uXXX)
            | Caracter ->
                match tipoCarac with 
                | A_ComillaSimple ->
                    ( tokenActual, estadoActual, posActual, Error <| sprintf "Error. El caracter está vacio en %A" posActual )
                | _ ->
                    (
                        aumentarValor tokenActual caracActual,
                        Caracter_Final,
                        aumentarPosCol (),
                        Continua
                    )
            | Caracter_Final ->
                match tipoCarac with 
                | A_ComillaSimple ->
                    (
                        aumentarValor tokenActual "'" |> terminarToken,
                        Inicio,
                        aumentarPosCol (),
                        Terminado
                    )
                | _ -> ( tokenActual, estadoActual, posActual, Error <| sprintf "Error. El caracter tiene más de un caracter %A" posActual )
            // TODO: Soporte para punto flotante.
            | Numero ->
                match tipoCarac with 
                | A_Numero ->
                    (
                        aumentarValor tokenActual caracActual,
                        estadoActual,
                        aumentarPosCol (),
                        Continua
                    )
                | _ ->
                    (
                        terminarToken tokenActual, 
                        Inicio, 
                        posActual, 
                        Terminado
                    )
            | Identacion ->
                match  tipoCarac with 
                | A_EspacioBlanco ->
                    (
                        aumentarValor tokenActual caracActual,
                        estadoActual,
                        aumentarPosCol (),
                        Continua
                    )
                | _ ->
                    (
                        terminarToken tokenActual, 
                        Inicio, 
                        posActual, 
                        Terminado
                    )
            | Operador ->
                match tipoCarac with  
                | A_Operador ->
                    (
                        aumentarValor tokenActual caracActual,
                        estadoActual,
                        aumentarPosCol (),
                        Continua
                    )
                | _ ->
                    (
                        terminarToken tokenActual, 
                        Inicio, 
                        posActual, 
                        Terminado
                    )
            | PreComentario ->
                match tipoCarac with 
                | A_Barra ->
                    (
                        aumentarValor tokenActual caracActual,
                        Comentario,
                        aumentarPosCol (),
                        Continua
                    )
                | _ ->
                    let nuevoToken = { tokenActual with tipo = tipoToken.Operador }
                    ( nuevoToken, Operador, posActual, Continua )
            | Comentario ->
                match tipoCarac with 
                | A_NuevaLinea ->
                    (
                        tokenActual,
                        InicioLinea,
                        posActual,
                        Terminado
                    )
                | _ -> 
                    (
                        aumentarValor tokenActual caracActual,
                        estadoActual,
                        aumentarPosCol (),
                        Continua
                    )
        
        match estado with 
        | Terminado -> Some (tokenActual, nuevaPos, nuevoEstado)
        | Error razon->
            printf "%s" razon
            None
        | Continua -> reconocerToken tokenActual nuevaPos nuevoEstado

    let rec crearListaTokens tokensActuales posActual estadoFSM =
        if posActual.posAbs >= String.length entrada then
            Some tokensActuales
        else
            let res = reconocerToken tokenVacio posActual estadoFSM
            match res with
            | Some (nuevoToken, pos, nuevoEstado) -> crearListaTokens (tokensActuales @ [nuevoToken]) pos nuevoEstado
            | None -> None

    crearListaTokens [] posVacia Inicio

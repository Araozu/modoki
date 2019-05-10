module TokenMap
open AnalisisLexico

type Token = {
    valor: string
    posInicio: datosPosicion
    posFinal: datosPosicion
    tipo: tipoToken
    precedencia: int
    signature: Tipos.Tipo
}

let obtPrecedencia (token: AnalisisLexico.Token) =
    if token.tipo = tipoToken.Operador then
        match token.valor with
        | "<|" | "|>" -> 2
        | "+" -> 3
        | "-" -> 4
        | "*" | "/" | "%" | "%%" -> 5
        | "**" -> 6
        | "f" -> 7
        | "==" | "!=" | "!" | "€" -> 8
        | "." | "?" | "?:" | "?." -> 9
        | _ -> 1
    else
        0

(* Convierte los tokens v: *)
// AnalisisLexico.Token list -> Token list
let tokenMap tokens =
    List.map (fun (t: AnalisisLexico.Token) ->
        {
            valor = t.valor
            posInicio = t.posInicio
            posFinal = t.posFinal
            tipo = t.tipo
            precedencia = obtPrecedencia t
            signature = Tipos.sinTipo
        }
    ) tokens

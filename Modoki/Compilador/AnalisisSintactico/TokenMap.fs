module TokenMap
open AnalisisLexico

let obtPrecedencia (token: Token) =
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
        { t with precedencia = obtPrecedencia t }
    ) tokens

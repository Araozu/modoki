sea express = importar "express"

sea app = express ()
sea puerto = 8080

app.use express.json
app.use <| express.urlencoded {extended := true}

app.get "/usuarios" <| fn req res next ->
    sea nombre = req.body.nombre
    si nombre?

app.listen puerto <| fn err ->
    console.log
        cuando err es
        | Algo e -> "Hubo un error al crear el servidor.\n${e}"
        | Nada -> "Escuchando el puerto ${puerto}"



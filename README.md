# Tendencias médicas (Shiny)

Aplicación local para explorar el expediente histórico y tendencias de laboratorio.

Desde la carpeta raíz del expediente:

```sh
Rscript -e "shiny::runApp('medical_trends_app', launch.browser = TRUE)"
```

Lee el libro en `outputs/expediente_medico_historico/` y no sube ni publica datos.

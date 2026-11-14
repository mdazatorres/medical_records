library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)


data_file <- file.path("data/expediente_medico_historico.xlsx")
if (!file.exists(data_file)) stop("No se encontró el Excel histórico: ", data_file)

raw <- read_excel(data_file, sheet = "Laboratorios", skip = 3, na = c("", "NA"))
labs <- data.frame(
  fecha = as.Date(raw[["Fecha"]]),
  categoria = raw[["Categoría"]],
  prueba = raw[["Prueba"]],
  valor = as.numeric(raw[["Valor numérico"]]),
  unidad = raw[["Unidad"]],
  rango = raw[["Rango de referencia"]],
  resultado = raw[["Bandera / resultado"]],
  operador = raw[["Operador"]],
  fuente = raw[["Archivo fuente (ruta relativa)"]]
) |>
  filter(!is.na(fecha)) |>
  arrange(fecha, prueba)

history_raw <- read_excel(data_file, sheet = "Cronologia", skip = 2, na = c("", "NA"))
history <- data.frame(
  fecha = as.Date(history_raw[["Fecha"]]),
  categoria = history_raw[["Categoría"]],
  estudio = history_raw[["Estudio / intervención"]],
  resumen = history_raw[["Resumen del documento"]],
  fuente = history_raw[["Archivo fuente (ruta relativa)"]]
) |>
  filter(!is.na(fecha)) |>
  arrange(fecha)

anti_tg <- data.frame(
  fecha = as.Date(c("2021-11-12", "2022-02-16", "2026-03-20", "2026-11-12")),
  categoria = "Tiroides",
  prueba = "Anticuerpos anti-tiroglobulina",
  valor = c(11, 14, 29, 27.7),
  unidad = "UI/mL",
  rango = "<84",
  resultado = "",
  operador = "",
  fuente = c(
    "Carcinoma Papilar/Nov 12 2021_Calcium_Tiroglobulina_Thyroid profile.pdf",
    "Carcinoma Papilar/Feb 16 2022_ Calcium_TSH_tiroglobulina.pdf",
    "Health Records 2022-2025/IL75137-67768362-111530.pdf",
    "Health Records 2022-2025/IL77716-71459796-080034.pdf"
  )
)
labs <- bind_rows(labs, anti_tg) |> arrange(fecha, prueba)

medications <- data.frame(
  Inicio = rep("Desde 2025", 5),
  Medicamento = c("Calcio", "Calcitriol", "Calcio", "Calcitriol", "Levotiroxina"),
  Dosis = c(
    "2 tabletas de 600 mg",
    "0.50 mcg",
    "2 tabletas de 600 mg",
    "0.50 mcg",
    "145 mg"
  ),
  Horario = c("Mediodía", "Mediodía", "Noche", "Noche", "Horario no indicado"),
  Estado = "Actual",
  Fuente = "Información proporcionada por la paciente",
  check.names = FALSE
)

available_tests <- sort(unique(labs$prueba))
default_tests <- intersect(c("Calcio", "TSH", "Tiroglobulina", "Anticuerpos anti-tiroglobulina", "PTH intacta", "Hemoglobina", "Fósforo"), available_tests)

reference_lines <- function(data) {
  unique_rows <- data |> distinct(prueba, rango)
  out <- lapply(seq_len(nrow(unique_rows)), function(i) {
    values <- regmatches(unique_rows$rango[i], gregexpr("[0-9.]+", unique_rows$rango[i]))[[1]]
    values <- suppressWarnings(as.numeric(values))
    if (!length(values) || all(is.na(values))) return(NULL)
    data.frame(prueba = unique_rows$prueba[i], limite = values, tipo = "Rango de referencia")
  })
  bind_rows(out)
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f7fafc; font-family: Arial, sans-serif; }
    .navbar { background: #1f4e78; border: 0; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: white; }
    .panel-default > .panel-heading { background: #d9eaf7; font-weight: 700; }
    .small-note { color: #596775; font-size: 12px; }
  "))),
  navbarPage(
    title = "Tendencias médicas",
    tabPanel("Historial médico",
      fluidRow(
        column(3, wellPanel(
          checkboxGroupInput(
            "history_categories", "Mostrar categorías",
            choices = sort(unique(history$categoria)),
            selected = sort(unique(history$categoria))
          ),
          checkboxInput("history_source", "Mostrar archivo fuente", FALSE),
          tags$p(class = "small-note",
            "Cronología consolidada de estudios, procedimientos, tratamientos y resultados documentados.")
        )),
        column(9,
          h3("Historial médico por fecha"),
          DTOutput("history_table")
        )
      )
    ),
    tabPanel("Medicamentos actuales",
      br(),
      h3("Medicamentos reportados actualmente"),
      tags$p(class = "small-note",
        "Registro proporcionado por la paciente. Confirma dosis, unidad y horario con el médico o farmacia antes de usarlo como indicación."),
      DTOutput("medications_table")
    ),
    tabPanel("Tendencias",
      fluidRow(
        column(3, wellPanel(
          selectizeInput("tests", "Marcadores", choices = available_tests, selected = default_tests,
            multiple = TRUE, options = list(plugins = list("remove_button"))),
          dateRangeInput("dates", "Rango de fechas", start = min(labs$fecha), end = max(labs$fecha)),
          checkboxInput("facets", "Separar gráficos por marcador", TRUE),
          checkboxInput("show_reference", "Mostrar rangos de referencia", TRUE),
          tags$p(class = "small-note", "Los valores con “<” se grafican con el límite numérico reportado. Consulte la tabla para el operador original.")
        )),
        column(9, h3("Resultados por fecha"), plotlyOutput("trend_plot", height = "620px"), uiOutput("reference_note"))
      )
    ),
    tabPanel("Tabla de resultados", br(), DTOutput("lab_table")),
    tabPanel("Acerca de", br(), h3("Expediente médico histórico"),
      p("Aplicación local de consulta. No interpreta resultados ni reemplaza la valoración de un profesional de salud."),
      tags$p(class = "small-note", paste("Archivo de datos:", data_file)))
  )
)

server <- function(input, output, session) {
  output$medications_table <- renderDT({
    datatable(
      medications, rownames = FALSE,
      options = list(pageLength = 10, dom = "t", scrollX = TRUE),
      colnames = c("Inicio", "Medicamento", "Dosis", "Horario", "Estado", "Fuente")
    )
  })
  filtered_history <- reactive({
    req(input$history_categories)
    history |>
      filter(categoria %in% input$history_categories) |>
      arrange(fecha)
  })
  output$history_table <- renderDT({
    df <- filtered_history()
    out <- data.frame(
      Fecha = df$fecha,
      Categoría = df$categoria,
      Estudio.o.intervención = df$estudio,
      Resumen.documental = df$resumen,
      Archivo.fuente = df$fuente,
      check.names = FALSE
    )
    if (!isTRUE(input$history_source)) out <- out[, 1:4, drop = FALSE]
    datatable(
      out, rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE, order = list(list(0, "asc")),
        columnDefs = list(list(targets = 3, width = "520px")))
    )
  })
  filtered <- reactive({
    req(input$tests)
    labs |> filter(prueba %in% input$tests, fecha >= input$dates[1], fecha <= input$dates[2]) |> arrange(prueba, fecha)
  })
  output$trend_plot <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No hay resultados para los filtros seleccionados."))
    df$tooltip <- paste0("<b>", df$prueba, "</b><br>Fecha: ", df$fecha, "<br>Valor: ",
      ifelse(is.na(df$operador), "", df$operador), df$valor, " ", df$unidad,
      "<br>Rango: ", df$rango, "<br>Resultado: ", ifelse(is.na(df$resultado), "", df$resultado))
    p <- ggplot(df, aes(fecha, valor, color = prueba, group = prueba, text = tooltip)) +
      geom_line(linewidth = 0.5, lineend = "round", na.rm = TRUE) +
      geom_point(size = 1.6, na.rm = TRUE) +
      labs(x = NULL, y = "Valor") + theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
    refs <- reference_lines(df)
    if (nrow(refs)) {
      p <- p + geom_hline(
        data = refs, aes(yintercept = limite),
        inherit.aes = FALSE, color = "#7F7F7F", linetype = "dashed", linewidth = 0.55
      )
    }
    if (isTRUE(input$facets)) p <- p + facet_wrap(~prueba, scales = "free_y", ncol = 2)
    ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  output$reference_note <- renderUI({
    req(input$show_reference)
    x <- filtered() |> distinct(prueba, unidad, rango) |> filter(!is.na(rango), rango != "")
    if (!nrow(x)) return(NULL)
    tags$div(class = "small-note", tags$b("Rangos reportados: "), paste(paste0(x$prueba, " (", x$unidad, "): ", x$rango), collapse = " · "))
  })
  output$lab_table <- renderDT({
    df <- filtered()
    out <- data.frame(Fecha = df$fecha, Categoría = df$categoria, Prueba = df$prueba,
      Valor = paste0(ifelse(is.na(df$operador), "", df$operador), ifelse(is.na(df$valor), "", format(df$valor, trim = TRUE, scientific = FALSE))),
      Unidad = df$unidad, Rango = df$rango, Resultado = df$resultado, Fuente = df$fuente)
    datatable(out, rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE, order = list(list(0, "desc"))))
  })
}
shinyApp(ui, server)

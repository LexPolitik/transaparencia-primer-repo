# =========================
# LIBRERÍAS
# =========================
library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(geodata)
library(ggiraph)
library(readr)

# =========================
# CARGA DE DATOS MGA
# =========================
ruta_csv <- "G:/Mi unidad/Transparencia/MGA/base_de_datos_por_entidad_metrica_2023.csv"

mga <- read.csv(
  ruta_csv,
  stringsAsFactors = FALSE
)

# =========================
# NORMALIZACIÓN MGA
# =========================
mga <- mga %>%
  mutate(
    estado = case_when(
      estado %in% c("Distrito Federal", "DF") ~ "Ciudad de México",
      estado == "México" ~ "Estado de México",
      TRUE ~ estado
    )
  )

# =========================
# REGIONES (para promedio regional)
# =========================
regiones <- tibble::tribble(
  ~estado, ~region,
  "Baja California", "Norte",
  "Sonora", "Norte",
  "Chihuahua", "Norte",
  "Nuevo León", "Norte",
  "Ciudad de México", "Centro",
  "Estado de México", "Centro",
  "Jalisco", "Centro",
  "Puebla", "Centro",
  "Oaxaca", "Sur",
  "Chiapas", "Sur",
  "Guerrero", "Sur"
)

# =========================
# MAPA DE MÉXICO (GADM)
# =========================
mx <- geodata::gadm(
  country = "MEX",
  level = 1,
  path = tempdir()
) |>
  st_as_sf()

# =========================
# NORMALIZACIÓN GADM
# =========================
mx$estado_map <- mx$NAME_1

mx$estado_map <- case_when(
  mx$estado_map == "México" ~ "Estado de México",
  mx$estado_map == "Distrito Federal" ~ "Ciudad de México",
  TRUE ~ mx$estado_map
)

# =========================
# UI
# =========================
ui <- fluidPage(
  
  titlePanel("MGA 2023 – Mapa de calor por entidad federativa"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "variable",
        "Selecciona un indicador:",
        choices = c(
          "Índice de Gobierno Abierto (IGA)" = "ga_indice",
          "Transparencia proactiva" = "t_indice",
          "Participación ciudadana" = "pc_indice"
        )
      ),
      
      checkboxInput("resaltar_tb", "Resaltar Top / Bottom 5", FALSE),
      
      checkboxInput("mostrar_promedio", "Comparar con promedio", FALSE),
      
      selectInput(
        "tipo_promedio",
        "Tipo de promedio:",
        choices = c("Nacional" = "nacional", "Regional" = "regional"),
        selected = "nacional"
      ),
      
      # 🔎 Leyenda explicativa (aparece solo cuando se activa comparar con promedio)
      uiOutput("leyenda_promedio"),
      
      uiOutput("explicacion"),
      downloadButton("descargar_estado", "Descargar datos")
    ),
    
    mainPanel(
      girafeOutput("mapa", height = "700px")
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  # ---- Ranking dinámico ----
  mga_ranked <- reactive({
    
    req(input$variable)
    
    mga %>%
      left_join(regiones, by = "estado") %>%
      filter(!is.na(.data[[input$variable]])) %>%
      mutate(
        ranking = rank(-.data[[input$variable]], ties.method = "min"),
        promedio_nacional = mean(.data[[input$variable]], na.rm = TRUE),
        promedio_region = ave(
          .data[[input$variable]],
          region,
          FUN = function(x) mean(x, na.rm = TRUE)
        )
      )
  })
  
  # ---- Leyenda / anotación de comparación con promedio ----
  output$leyenda_promedio <- renderUI({
    
    req(input$tipo_promedio)
    
    if (!isTRUE(input$mostrar_promedio)) return(NULL)
    
    etiqueta <- ifelse(
      input$tipo_promedio == "regional",
      "promedio regional",
      "promedio nacional"
    )
    
    tags$div(
      style = "background-color:#f7f7f7; padding:10px; border-left:5px solid #444; margin-top:10px;",
      tags$strong("Interpretación del borde (comparación con ", etiqueta, "):"),
      tags$ul(
        style = "margin:8px 0 0 18px;",
        tags$li(
          tags$span(style = "color:#1B7837; font-weight:bold;", "Borde verde"),
          " = valor por arriba del ", etiqueta
        ),
        tags$li(
          tags$span(style = "color:#762A83; font-weight:bold;", "Borde morado"),
          " = valor por debajo del ", etiqueta
        )
      ),
      tags$div(
        style = "font-size:12px; color:#666; margin-top:6px;",
        "Nota: el relleno (color interior) sigue mostrando el valor del indicador; el borde solo indica la comparación con el promedio."
      )
    )
  })
  
  # ---- Mapa ----
  output$mapa <- renderGirafe({
    
    mx_data <- mx |>
      left_join(
        mga_ranked(),
        by = c("estado_map" = "estado")
      ) |>
      mutate(
        grupo_tb = case_when(
          !input$resaltar_tb ~ "normal",
          ranking <= 5 ~ "top",
          ranking >= max(ranking, na.rm = TRUE) - 4 ~ "bottom",
          TRUE ~ "otros"
        ),
        benchmark = ifelse(
          input$tipo_promedio == "regional",
          promedio_region,
          promedio_nacional
        ),
        sobre_promedio = .data[[input$variable]] >= benchmark,
        tooltip_text = paste0(
          "Estado: ", estado_map, "\n",
          "IGA: ", round(ga_indice, 3), "\n",
          "Transparencia proactiva: ", round(t_indice, 3), "\n",
          "Participación ciudadana: ", round(pc_indice, 3), "\n",
          "Lugar nacional: ", ranking, " de 32\n",
          "Promedio (", input$tipo_promedio, "): ",
          round(benchmark, 3)
        )
      )
    
    p <- ggplot(mx_data) +
      geom_sf_interactive(
        aes(
          fill = .data[[input$variable]],
          alpha = grupo_tb,
          tooltip = tooltip_text,
          color = ifelse(
            input$mostrar_promedio & sobre_promedio,
            "arriba",
            "abajo"
          )
        ),
        size = 0.3
      ) +
      scale_alpha_manual(
        values = c(top = 1, bottom = 1, otros = 0.25, normal = 1),
        guide = "none"
      ) +
      scale_color_manual(
        values = c(arriba = "#1B7837", abajo = "#762A83"),
        guide = "none"
      ) +
      scale_fill_gradient(
        low = "#FEE5D9",
        high = "#A50F15",
        na.value = "grey90",
        name = "Índice"
      ) +
      labs(
        title = "Métrica de Gobierno Abierto 2023",
        caption = "Fuente: MGA 2023"
      ) +
      theme_void()
    
    girafe(ggobj = p)
  })
  
  # ---- Descarga ----
  output$descargar_estado <- downloadHandler(
    filename = function() {
      paste0("MGA_2023_", input$variable, ".csv")
    },
    content = function(file) {
      write_csv(
        mga_ranked() %>%
          select(
            estado,
            !!sym(input$variable),
            ranking,
            promedio_nacional,
            promedio_region
          ),
        file
      )
    }
  )
  
  # ---- Texto explicativo ----
  output$explicacion <- renderUI({
    
    texto <- switch(
      input$variable,
      "ga_indice" =
        "El Índice de Gobierno Abierto resume el desempeño general en transparencia y participación.",
      "t_indice" =
        "La Transparencia Proactiva mide si la información se publica sin necesidad de solicitarla.",
      "pc_indice" =
        "La Participación Ciudadana evalúa si existen mecanismos reales para incidir en decisiones públicas."
    )
    
    tags$p(
      style = "background-color:#f7f7f7; padding:10px; border-left:5px solid #A50F15;",
      texto
    )
  })
}

# =========================
# APP
# =========================
shinyApp(ui, server)

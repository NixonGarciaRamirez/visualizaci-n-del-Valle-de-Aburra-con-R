library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(raster)

# Cargar el shapefile
shp_path <- "Valle de Aburra.shp"
shp_data <- st_read(shp_path)

# Obtener los nombres de los atributos del shapefile
attribute_names <- names(shp_data)


# Lista con información adicional para los atributos
info_list <- list(
  DPTO_CCDGO = "Este atributo representa el codigo del departamento a nivel Nacional",
  MPIO_CCDGO = "Este atributo representa el codigo de cada munidicio a nivel nacional",
  MPIO_CDPMP = "Código DANE concatenado departamento y municipio",
  DPTO_CNMBR = "Nombre del departamento visualizado",
  MPIO_CNMBR = "Nombre del municipio Visualizado",
  MPIO_CRSLC = "Año documentado de la creación del municipio",
  MPIO_NAREA = "Este atributo representa el área total de cada municipio en km².",
  MPIO_CSMBL = "Simbolo polígono municipio",
  MPIO_VGNC =  "Año vigencia DANE",
  MPIO_TIPO =  "Tipo municipio según DIVIPOLA",
  MPIO_CNMBR = "Este atributo contiene el nombre de cada municipio.",
  Shape_Leng = "Longitud en Km del perimetro del shp",
  Shape_Area = "Area en km² del shp"
  # Agrega más atributos según sea necesario
)


ui <- fluidPage(
  titlePanel("Mapa Base con Esri World Imagery"),
  sidebarLayout(
    sidebarPanel(
      h3("Filtros"),
      selectInput("attribute", "Selecciona un atributo:", choices = attribute_names, multiple = FALSE),
      uiOutput("value_ui"),
      #actionButton("apply", "Aplicar Filtros"),
      br(), # Espacio adicional
      h3("Leyenda para el Atributo"),
      uiOutput("legend_ui") # Usar renderUI correctamente para la leyenda
    ),
    mainPanel(
      leafletOutput("map"),
      h3("Información Adicional"),
      verbatimTextOutput("info")
    )
  )
)

server <- function(input, output, session) {
  
  # Observador para actualizar los valores cuando cambia el atributo seleccionado
  observeEvent(input$attribute, {
    updateSelectInput(session, "value", choices = unique(shp_data[[input$attribute]]))
  })
  
  # Renderizar la UI de los valores del atributo
  output$value_ui <- renderUI({
    selectInput("value", "Selecciona un valor:", choices = unique(shp_data[[input$attribute]]), multiple = TRUE)
  })
  
  # Filtrar los datos según el atributo y valor seleccionados
  filtered_data <- reactive({
    req(input$attribute, input$value)
    shp_data %>% 
      filter(!!sym(input$attribute) %in% input$value)  # Filtrar para selecciones múltiples
  })
  
  # Crear una paleta de colores para la leyenda basada en el atributo seleccionado
  observeEvent(input$attribute, {
    unique_values <- unique(shp_data[[input$attribute]])
    pal <- colorFactor(palette = "Set1", domain = unique_values) # Paleta de colores
    
    # Crear la leyenda usando etiquetas de HTML
    legend_html <- paste(
      "<div style='line-height: 1.5;'>",
      paste0("<span style='background-color:", pal(unique_values), 
             ";width: 20px; height: 20px; display: inline-block; border: 1px solid black;'></span> ",
             unique_values, collapse = "<br/>"),
      "</div>"
    )
    
    # Insertar la leyenda en el UI
    output$legend_ui <- renderUI({
      HTML(legend_html)
    })
    
    # Actualizar el mapa con los colores de la leyenda
    output$map <- renderLeaflet({
      leaflet(shp_data) %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setView(lng = -75.45738, lat = 6.30, zoom = 9) %>%
        addPolygons(
          data = filtered_data(),
          color = ~pal(get(input$attribute)), # Usar la paleta de colores para colorear los polígonos
          weight = 2,
          opacity = 1.0,
          fillOpacity = 0.5,
          popup = ~paste0("<strong>Atributos del Polígono:</strong><br>",
                          "DPTO_CCDGO: ", DPTO_CCDGO, "<br>",
                          "MPIO_CCDGO: ", MPIO_CCDGO, "<br>",
                          "MPIO_CDPMP: ", MPIO_CDPMP, "<br>",
                          "DPTO_CNMBR: ", DPTO_CNMBR, "<br>",
                          "MPIO_CNMBR: ", MPIO_CNMBR, "<br>",
                          "MPIO_CRSLC: ", MPIO_CRSLC, "<br>",
                          "MPIO_NAREA: ", MPIO_NAREA, "<br>",
                          "MPIO_CSMBL: ", MPIO_CSMBL, "<br>",
                          "MPIO_VGNC: ", MPIO_VGNC, "<br>",
                          "MPIO_TIPO: ", MPIO_TIPO, "<br>",
                          "MPIO_CNMBR: ", MPIO_CNMBR, "<br>",
                          "Shape_Leng: ", Shape_Leng,"<br>",
                          "Shape_Area: ", Shape_Area
                          )
        )
    
    })
  })

# Mostrar la información adicional
output$info <- renderPrint({
  attribute_info <- info_list[[input$attribute]]  # Obtener información del atributo
  if (is.null(attribute_info)) {
    attribute_info <- "No hay información disponible para este atributo."
  }
  paste(# "Atributo seleccionado:", input$attribute, 
    #"\nValor del atributo:", paste(input$value, collapse = ", "),
    attribute_info)
})
}

shinyApp(ui, server)



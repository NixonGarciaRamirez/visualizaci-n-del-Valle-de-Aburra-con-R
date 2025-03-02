library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(raster)

# Cargar el shapefile
shp_path <- "Valle_Analisis.shp"# Aqui de ir la UBICACION DEL shp
shp_data <- st_read(shp_path)

# Obtener los nombres de los atributos del shapefile
attribute_names <- names(shp_data)


# Lista con información adicional para los atributos
info_list <- list(
  Nombre = "Nombre del municipio Visualizado",
  Area= "Este atributo representa el área total de cada municipio en km².",
  Poblacion = "Este campo representa la cantidad de poblacion censada por el DANE en el año 2023",
  CO = "Este campo representa la cantidad de monoxido de carbono presente en cada municipio en el año 2023 en partes por millon medido por el SISAIRE",
  ICA_Pm2.5 = "Este campo representa el Indice de Calidad de Aire para materiar particulado de 2.5 micrometros por municipio, donde la concentracion en micro gramos por metro cubico: 0-12 es Buena, 13- 37 Aceptable, 38 - 55 Dañina a grupos sencibles, 56 - 150 Dañina a la salud, 151 - 250 Muy dañina a la salud y 251 - 500 Peligrosa. Datos: SIATA ",
  ICA_Pm10 = "Este campo representa el Indice de Calidad de Aire para materiar particulado de 2.5 micrometros por municipio, donde la concentracion en micro gramos por metro cubico: 0-54 es Buena, 55- 154 Aceptable, 155 - 254 Dañina a grupos sencibles, 255 - 354 Dañina a la salud, 355 - 424 Muy dañina a la salud y 425 - 604 Peligrosa. Datos: SIATA ",
  IRCA = "Este campo representa el Indice de Riesgo de la Calidad del Agua para consumo Humano, este valor es basicamente un promedio ponderado de varias caracterisisticas de sanidad del agua, si el puntaje da entre 0-5 es agua sin riesgo, si es entre 5.1-14 riesgo bajo, 14.1-35 riesgo medio, 35.1-80 riesgo alto y 80.1-100 es agua inviable para consumo humano  "
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
                          "Nombre: ", Nombre, "<br>",
                          "Area: ", Area, "<br>",
                          "Poblacion: ", Poblacion, "<br>",
                          "CO:", CO, "<br>",
                          "ICA_Pm2.5:", ICA_Pm2.5, "<br>",
                          "ICA_Pm10:", ICA_Pm10,"<br>",
                          "IRCA:",IRCA
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





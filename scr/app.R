library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)

datos_pm10 <- read_excel("C:/Users/olive/Desktop/proyecto robos/ad_viz_plotval_data.csv.xlsx",
                         col_types = c(
                           "date",     # Date
                           "text",     # Source
                           "text",     # SiteID
                           "numeric",  # POC
                           "numeric",  # SampleMeasurement 
                           "text",     # Units
                           "numeric",  # Daily AQI Value
                           "text",     # Local Site Name
                           "numeric",  # Daily Obs Count
                           "numeric",  # Percent Complete
                           "text",     # AQS Parameter Code
                           "text",     # AQS Parameter Description
                           "text",     # Method Code
                           "text",     # CBSA Code
                           "text",     # CBSA Name
                           "text",     # State FIPS Code
                           "text",     # State 
                           "text",     # County FIPS Code
                           "text",     # County 
                           "numeric",  # Latitude 
                           "numeric"   # Longitude 
                         ))
# UI

ui <- fluidPage(
  titlePanel("Dashboard Calidad del Aire - PM10"),
  
  fluidRow(
    column(3,
           h4("KPIs"),
           verbatimTextOutput("promedio_pm10"),
           verbatimTextOutput("max_pm10"),
           verbatimTextOutput("promedio_aqi"),
           verbatimTextOutput("obs_dia"),
           verbatimTextOutput("pct_completo")
    ),
    column(9,
           plotOutput("serie_tiempo"),
           leafletOutput("mapa_sitios", height = 400)
    )
  )
)

server <- function(input, output) {
  
  datos_filtrados <- reactive({
    datos_pm10 %>%
      filter(SampleMeasurement <= 500)
  })
  
  datos_resumen <- reactive({
    datos_filtrados() %>%
      group_by(Date) %>%
      summarise(
        promedio_pm10 = mean(SampleMeasurement, na.rm=TRUE),
        max_pm10 = max(SampleMeasurement, na.rm=TRUE),
        promedio_aqi = mean(`Daily AQI Value`, na.rm=TRUE),
        obs_dia = sum(`Daily Obs Count`, na.rm=TRUE),
        pct_completo = mean(`Percent Complete`, na.rm=TRUE)
      )
  })
  
  output$promedio_pm10 <- renderText({
    paste("Promedio PM10:", round(mean(datos_filtrados()$SampleMeasurement, na.rm=TRUE), 2), "µg/m³")
  })
  
  output$max_pm10 <- renderText({
    paste("Máximo PM10:", max(datos_filtrados()$SampleMeasurement, na.rm=TRUE), "µg/m³")
  })
  
  output$promedio_aqi <- renderText({
    paste("Promedio AQI:", round(mean(datos_filtrados()$`Daily AQI Value`, na.rm=TRUE), 1))
  })
  
  output$obs_dia <- renderText({
    paste("Observaciones por día:", round(mean(datos_filtrados()$`Daily Obs Count`, na.rm=TRUE), 0))
  })
  
  output$pct_completo <- renderText({
    paste("Porcentaje Completo:", round(mean(datos_filtrados()$`Percent Complete`, na.rm=TRUE), 1), "%")
  })
  
  output$serie_tiempo <- renderPlot({
    ggplot(datos_resumen(), aes(x = Date, y = promedio_pm10)) +
      geom_line(color = "blue") +
      labs(title = "Promedio diario de PM10 (valores <= 500)", x = "Fecha", y = "PM10 (µg/m³)") +
      theme_minimal()
  })
  
  output$mapa_sitios <- renderLeaflet({
    sitios <- datos_filtrados() %>%
      group_by(`Local Site Name`, Latitude, Longitude) %>%
      summarise(pm10_promedio = mean(SampleMeasurement, na.rm=TRUE))
    
    leaflet(sitios) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        radius = ~pm10_promedio / 10,
        color = ~colorNumeric("Reds", sitios$pm10_promedio)(pm10_promedio),
        label = ~paste0(`Local Site Name`, ": ", round(pm10_promedio, 1), " µg/m³")
      )
  })
}

shinyApp(ui, server)



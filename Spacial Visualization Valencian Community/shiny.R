
library(shiny)
library(leaflet)
library(tmap)
load(file='data_original.RData')

varlist <-unique(SpainRegPoblacion_copia@data$NAME_2)

runApp(list(
  ui =  fluidPage(
    titlePanel("Poblaciones por comunidad"),
    sidebarLayout(
      sidebarPanel(
        selectInput("comu", label = "Comunidad", choices = varlist),
        selectInput("anyo", label = "Año", choices = numero)
      ),
      mainPanel(
        leafletOutput("mapa_comu_anyo")
      )
    )
  ),
  server = function(input, output) {
    
    
    output$mapa_comu_anyo <- renderLeaflet({
      mapita <- tm_shape(SpainRegPoblacion_copia[SpainRegPoblacion_copia@data$NAME_2 == input$comu,])+
        tm_fill(col = input$anyo,
                style = "fixed", 
                breaks = breaks ,palette="viridis")+
        tm_borders(alpha =  0.7)+
        tm_layout(main.title = paste("Comunidad Valenciana", input$anyo),
                  title.size = 2,legend.outside = T,
                  legend.outside.position = c("left","top"))+tm_polygons(input$comu)
      mapitaleaf <- tmap_leaflet(mapita)
      mapitaleaf
      
    })
    
  }
))


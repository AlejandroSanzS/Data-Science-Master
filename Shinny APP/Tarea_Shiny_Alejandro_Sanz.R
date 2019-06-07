

library(shinythemes)
library(shiny)
library(rsconnect)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

ui <-
  navbarPage(
    id = "TabApp ",
    theme = shinytheme("slate"),
    title = "App Máster Ciencia de Datos",
    #1a pestaña
    tabPanel(
      "Selección de Máquina",
      sidebarLayout(
        sidebarPanel(
          h5('MÁQUINA'),
          fileInput("DatosFichero", "Selecciona un fichero", accept = NULL),
          uiOutput('Seleccion')
        ),
        mainPanel(h5('Probabilidad de orden
                     '), plotOutput("grafico"))
        )
      ),
    #2a pestaña
    navbarMenu("Estado de la máquina",
               tabPanel("Evolución temporal alarmas",sidebarLayout(
                 sidebarPanel(h5("Alarmas radiobuttons"), uiOutput('Alarma')
                 ),
                 
                 mainPanel(h5("Evolución temporal Alarmas"),plotOutput('grafico_ALARMA'))
               )),
               
               tabPanel("Registros de la máquina",sidebarLayout(
                 sidebarPanel(h5("Alarmas checkbox"),uiOutput('Alarma2')
                 ),
                 mainPanel(h5("Registros de la máquina seleccionada"),column(12, dataTableOutput('tbl')))
               ))
               
    ),
    #3a pestaña
    tabPanel("Estadísticas globales temporales",
             sidebarLayout(
               sidebarPanel(
                 uiOutput('fecha'),
                 h5("Alarma"),
                 uiOutput('SeleccionAlarma'),
                 uiOutput("slide"),
                 h5("BOXPLOT"),
                 uiOutput('Checkboolbox'),
                 plotOutput("plot2")),
               
               mainPanel(h5("Histograma de la alarma seleccionada"),plotOutput('histo'),h5("Boxplot de la alarma seleccionada"),plotOutput("boxplot"))
               
             )
    )
    )


server <- function(input, output) {
  Maquinas_R<-reactive({
    a<-input$DatosFichero
    load(a$ datapath)
    nombre_variable<-load(a$datapath)
    vars=list()
    vars$datos <- eval(parse(text = nombre_variable))
    vars$Matriculas<-unique(Datos$matricula) #Esto es para evaluar el dataframe 
    vars$Alarma<- names(Datos)[grepl("^a",names(Datos))]
    return(vars)
  })
  #Para seleccionar la máquina a mostrar en la grafica de la primera pestaña.
  output$Seleccion<-renderUI({
    selectInput("Selectbox1", "Selecciona máquina", 
                Maquinas_R()$Matriculas)
    
  })
  
  #Para seleccionar la alarma a visualizar.
  output$Alarma<-renderUI({
    radioButtons("Radiobuttons1", "Selecciona la alarma a visualizar", 
                 Maquinas_R()$Alarma)
    
  })
  #Para la seleccion de las alarmas a mostrar en la tabla
  output$Alarma2<-renderUI({
    checkboxGroupInput("CheckBOX", "Selecciona las alarmas para ver en la tabla", 
                       Maquinas_R()$Alarma)
    
  })
  #Para la seleccion de alarma a la hora de crear el histograma
  output$SeleccionAlarma<-renderUI({
    selectInput("Alarma_hist", "Selecciona máquina", 
                Maquinas_R()$Alarma)
    
  })
  #En el caso que sea posible y funcione se guardaran los inputs reactivos que da el usuario en variables, para mayor claridad.
  #para el plot de la primera pestaña de probabilidad de orden.
  output$grafico <- renderPlot({
    input<-input$Selectbox1
    data <- Maquinas_R()$datos %>%
      filter(matricula == input) %>%
      dplyr::select(p_orden, dia)
    ggplot(data , aes(dia, p_orden, color = p_orden)) + geom_point() + geom_line(aes(colour = p_orden, width = 20))+
      scale_color_gradient(low ="blue", high = "red")
  })
  
  #Para la grafica de las alarmas por radiobutton, si pongo los inputs en variables, no termina de funcionar
  output$grafico_ALARMA <- renderPlot({
    datapoigub <- Maquinas_R()$datos %>%
      filter(matricula == input$Selectbox1) %>%
      dplyr::select(input$Radiobuttons1, dia)
    ggplot(data=datapoigub , aes(dia, datapoigub[, 1], color = datapoigub[, 1])) + ylab(input$Radiobuttons1) +
      geom_point(size = 2) + geom_line(aes(colour = datapoigub[, 1], width = 20))+
      scale_color_gradient(low ="blue", high = "red")
    
    
  })
  # Para la creación de la tabla reactiva.
  output$tbl <-  renderDataTable({
    inputn<-input$Selectbox1
    checkn <- input$CheckBOX
    Maquinas_R()$datos %>%
      filter(matricula == inputn) %>%
      dplyr::select(matricula, dia,checkn,p_orden)
    
  })
  
  #Para estimar el rango de fechas a seleccionar.
  output$fecha <- renderUI({
    dateRangeInput("RangoFechas", "Selecciona el periodo", 
                 start = "2016-01-01", end = "2016-12-14", min = NULL, 
                 max = NULL, format = "yyyy-mm-dd", weekstart = 1,
                 language = "es", separator = "a")
  })
  
  
  #Para seleccionar la anchura del bin del histograma.
  output$slide <- renderUI({
    sliderInput("obs", "Tamaño del bin",  
                min = 1, max = 50, value = 20)
  })
    
    
    
  # Plot del histograma
  output$histo <- renderPlot({
    input_hist<-input$Alarma_hist
    fini <- input$RangoFechas[1]
    ffin <- input$RangoFechas[2]
    bin <- input$obs
    inputn<-input$Selectbox1
    data_hist <- Maquinas_R()$datos %>%
      filter(fini<dia,ffin>dia,matricula==inputn) %>% 
      dplyr::select(input_hist)
    ggplot(data=data_hist )  +
      geom_histogram(aes_string(x=input_hist),binwidth=bin)+
      scale_color_gradient(low ="blue", high = "red")
    
    
  })
  # Para poder seleccionar si mostrar todas las maquinas o solo 1 en el boxpplot.
  output$Checkboolbox <- renderUI({
    checkboxInput("Caja1", "Todas las máquinas", value = FALSE, width = NULL)
  })
  
  # Grafica del boxplot.
  output$boxplot <- renderPlot({
    input_hist<-input$Alarma_hist
    fini <- input$RangoFechas[1]
    ffin <- input$RangoFechas[2]
    bin <- input$obs
    inputn<-input$Selectbox1
    if(input$Caja1==FALSE){
    data_box <- Maquinas_R()$datos %>%
      filter(fini<dia,ffin>dia,matricula==inputn) 
    ggplot( data=data_box)  +
      geom_boxplot(aes_string(x="matricula",y=input$Alarma_hist))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }else{
      data_box <- Maquinas_R()$datos %>%
        filter(fini<dia,ffin>dia)
        ggplot(data=data_box)  +
        geom_boxplot(aes_string(x="matricula",y=input$Alarma_hist))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
  })

  
}



shinyApp(ui, server)

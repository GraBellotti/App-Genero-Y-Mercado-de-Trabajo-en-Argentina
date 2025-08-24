library(shiny)
library(bslib)
library(shinythemes)
library(shinipsum)
library(bsicons)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(reactable)
library(gfonts)
library(thematic)

#get_all_fonts()
TasasML <- read.table("TasasML.txt", sep = ";", dec = ",", header = T) 
ITI <- read.table("IOP.txt", sep = ";", dec = ",", header = T) %>% 
 mutate(Año = as.integer(Año),
        IOP_Mujeres = format(round(as.numeric(IOP_Mujeres)),big.mark = "."),
        IOP_Varones = format(round(as.numeric(IOP_Varones)),big.mark = "."))

thematic::thematic_shiny()

ui <- page_navbar(title = "Género y Mercado de Trabajo",
                  theme = bslib::bs_theme(bootswatch = "materia"),
                  tags$style("#MetodoTS, #MetodoTD {
text-align: justify;
text-justify: inter-word;
}"),
      nav_panel(title = "Inserción en el ML",
            page_sidebar(
              sidebar = sidebar(
                sliderInput(inputId = "AÑOa",
                            label = "AÑO",
                            value=c(min(TasasML$Año),max(TasasML$Año)),
                            min = min(TasasML$Año),
                            max = max(TasasML$Año),
                            round = TRUE,
                            step = 1
                            ),
                selectInput(inputId = "GRUPO_EDAD",
                            label = "GRUPO DE EDAD",
                            choices = unique(TasasML$GRUPO_EDAD),
                            selected = "Total",
                            multiple = F),
                selectInput(inputId = "NIVEL_EDUCATIVO",
                            label = "NIVEL EDUCATIVO",
                            choices = unique(TasasML$NIVEL_EDUCATIVO),
                            selected = "Total",
                            multiple = F)),
              navset_card_tab(
                nav_panel(title = "Presentación",
                          uiOutput(outputId ="Metodo"),
                          imageOutput("des")),
                nav_panel(title = "Tasa de Actividad",
                          fluidRow(column(12,plotlyOutput(outputId ="Activ"))),
                          fluidRow(column(1,""),column(10, uiOutput("MetodoTA"),column(1,"")))),
                nav_panel(title = "Tasa de Desocupación",
                          fluidRow(column(12, plotlyOutput(outputId = "Desoc"))),
                          fluidRow(column(1,""),column(10, uiOutput("MetodoTD"),column(1,"")))),
                nav_panel(title = "Tasa de Subocupación",
                          fluidRow(column(12, plotlyOutput(outputId = "Suboc"))),
                          fluidRow(column(1,""),column(10, uiOutput("MetodoTS"),column(1,"")))),
                nav_panel(title = "Trabajo No Registrado",
                          fluidRow(column(12, plotlyOutput(outputId = "TNR"))),
                          fluidRow(column(1,""),column(10, uiOutput("MetodoTNR"),column(1,"")))),
              ))),
            nav_panel("Ingresos",
                    fluidRow(
                        column(4,selectInput(inputId = "CatOc",
                                    label = "CATEGORIA OCUPACIONAL",
                                    choices = unique(ITI$CATEGORIA_OCUPACIONAL),
                                    selected = "Total",
                                    multiple = T)),
                         column(4,selectInput(inputId = "NIVED",
                                   label = "NIVEL EDUCATIVO",
                                   choices = unique(ITI$NIVEL_EDUCATIVO),
                                   selected = "Total",
                                   multiple = F)),
                      column(4,
                             sliderInput(inputId = "AÑOb",
                                         label = "AÑO",
                                         value=c(min(ITI$Año),max(ITI$Año)),
                                         min = min(ITI$Año),
                                         max = max(ITI$Año),
                                         round = TRUE,
                                         step = 1))
                              ),
                       fluidRow(
                          column(4, uiOutput("MetodoY"),imageOutput("des2")),
                          column(8,plotlyOutput(outputId ="ITI"))
                               )
                    ),
      nav_panel("Tablas",
                accordion(
                  accordion_panel("Tasas Mercado Laboral",
                                  fluidRow(column(3,selectInput(inputId = "AnioML",
                                                                label = "AÑO",
                                                                choices = unique(TasasML$Año),
                                                                selected = max(TasasML$Año),
                                                                multiple = T)), 
                                           column(3,selectInput(inputId = "NE",
                                                                label = "NIVEL EDUCATIVO",
                                                                choices = unique(TasasML$NIVEL_EDUCATIVO),
                                                                selected = "Total",
                                                                multiple = F)),
                                           column(3,selectInput(inputId = "GE",
                                                                label = "GRUPO DE EDAD",
                                                                choices = unique(TasasML$GRUPO_EDAD),
                                                                selected = "Total",
                                                                multiple = F)),
                                           column(3,downloadButton("DescargaML","Descargar Tabla",class = "btn-sm btn-primary"))),
                                  fluidRow(column(12,tableOutput("TasasML")))
                                  ),
                  accordion_panel("Brecha de Ingresos",
                                  fluidRow(column(3,selectInput(inputId = "AnioI",
                                                                label = "AÑO",
                                                                choices = unique(ITI$Año),
                                                                selected = max(ITI$Año),
                                                                multiple = T)), 
                                           column(3,selectInput(inputId = "NEI",
                                                                label = "NIVEL EDUCATIVO",
                                                                choices = unique(ITI$NIVEL_EDUCATIVO),
                                                                selected = "Total",
                                                                multiple = F)),
                                           column(3,selectInput(inputId = "CO",
                                                          label = "CATEGORIA OCUPACIONAL",
                                                          choices = unique(ITI$CATEGORIA_OCUPACIONAL),
                                                          selected = "Total",
                                                          multiple = F)),
                                           column(3,downloadButton("DescargaY","Descargar Tabla", class = "btn-sm btn-primary"))),
                                  fluidRow(column(12,tableOutput("TasasY")))
                                  )
                  )
                ),
    
      nav_panel(title = "Info y Contacto",
                fluidRow(
                  column(12, uiOutput("Info")
                  )))
      )
            
# Server
server <- function(input, output) {
  #si quiero crear una sola vez filtros que sirvan para todos los outpus tengo que crear por fuera 
  #del render de los outputs un objeto reactivo, y para ello una funcion reactiva como reactive
  #armo los inputs y los outputs
  output$Metodo <- renderUI({
    HTML(paste("La información se basa en los datos recopilados por la Encuesta 
               Permanente de Hogares, publicadas por el INDEC.<br>",
               "<br>",               
               "El universo de análisis se conforma por la población mayor a 14
                años.<br>",
               "<br>",  
               "Las tasas muestran porcentajes agrupados por sexo.",
               sep = ""))
    
  })
  output$des <- renderImage({
    list(
      src = "desigualdadY2.jpg",
      width = 300,
      height = 250
      #,
      #alt = "desigualdad"
    )
  }, deleteFile = F)
  output$MetodoTA <- renderUI({HTML(paste("La tasa de actividad expresa el porcentaje de la Población Economicamente Activa (PEA: Desocupadas/os + Ocupadas/os)
                                          en relación al total de la población. En este caso se calcula, PEA Mujeres / Total Mujeres y PEA Varones / Total Varones.<br>",
                                          "<br>",               
                                          "En otras palabras nos indica que porcentaje de la población se encuentra participando del mercado de trabajo, ya sea como 
               ocupadas/os o desocupadas/os.<br>",
                                          "<br>",  
                                          "Historicamente las mujeres tienen tasas de actividad menores a los hombres, si bien la diferencia se fue reduciendo con el
               pasar de los años. Para dar un paso más, y comprender porque las mujeres participan menos, es importante analizar otros indicadores como las 
               tasas de desocupación, subocupación y trabajo no registrado",
                                          sep = ""))
  })
  Tasas <- reactive({TasasML %>%
      filter(
        GRUPO_EDAD == input$GRUPO_EDAD,
        NIVEL_EDUCATIVO == input$NIVEL_EDUCATIVO,
        Año >= input$AÑOa[1] & Año <= input$AÑOa[2]
      )
  })
  
  output$Activ <- renderPlotly({
    plot_ly(Tasas(),
            x = ~Año,
            y = ~TA_Mujeres, 
            type = 'bar', 
            name = 'Mujeres', 
            text = ~TA_Mujeres,
            values = ~TA_Mujeres,
            textposition = 'inside',
            marker = list(color = "#d300a9")
    ) %>% 
      add_trace(y = ~TA_Varones, 
                name = 'Varones', 
                text = ~TA_Varones,
                values = ~TA_Varones,
                textposition = 'inside',
                marker = list(color = "#00d394")) %>%
      layout(barmode = 'dodge',
             xaxis = list(title = "", tickangle = -45,showline= T, linewidth=1, linecolor="grey"),
             yaxis = list(title = "", showgrid=F, showticklabels=F),
             margin = list(b = 100),
             barmode = 'group'
      ) 
  })  
  output$MetodoTD <- renderUI({HTML(paste("La tasa de desocupación expresa el porcentaje de desocupadas/os en relación al total de la población economicamente activa (PEA). 
               El cálculo se realiza a nivel sexo: Desocupadas Mujeres / Total PEA Mujeres y Desocupados Varones / Total PEA Varones.<br>",
                                          "<br>",               
                                          "Los niveles de desocupación son mayores en el caso de las mujeres. Este es uno de los motivos que explican una menor participación de las mujeres, por ser 
               un factor que desalienta la búsqueda de trabajo.<br>",
                                          sep = ""))
  })
  output$Desoc <- renderPlotly({
    plot_ly(Tasas(),
            x = ~Año,
            y = ~TD_Mujeres, 
            type = 'bar', 
            name = 'Mujeres',
            text = ~TD_Mujeres,
            values = ~TD_Mujeres,
            textposition = 'inside',
            marker = list(color = "#d300a9")) %>% 
      add_trace(y = ~TD_Varones, 
                name = 'Varones', 
                text = ~TD_Varones,
                values = ~TD_Varones,
                textposition = 'inside',
                marker = list(color = "#00d394")) %>% 
      layout(uniformtext=list(minsize=8, mode='hide'),
             xaxis = list(title = "", tickangle = -45,showline= T, linewidth=1, linecolor="grey"),
             yaxis = list(title = "", showgrid=F, showticklabels=F),
             margin = list(b = 100),
             barmode = 'group')
  })   
  output$MetodoTS <- renderUI({HTML(paste("La tasa de subocupación expresa el porcentaje de personas que trabajan menos de 35 horas 
               semanales por causas involuntarias y están dispuestas a trabajar más. Se calcula en relación al total de la población economicamente activa. 
               El cálculo se realiza a nivel sexo: Subocupadas Mujeres / Total PEA Mujeres y Subocupados Varones / Total PEA Varones.<br>",
                                          "<br>",               
                                          "Los niveles de subocupación son mayores en el caso de las mujeres. Esta diferencia esta relacionada, entre otras cuestiones, con una mayor precariedad de los trabajos donde se
               encuentran empleadas las mujeres.<br>",
                                          sep = "")) })
  output$Suboc <- renderPlotly({
    plot_ly(Tasas(),
            x = ~Año,
            y = ~TS_Mujeres, 
            type = 'bar', 
            name = 'Mujeres', 
            text = ~TS_Mujeres,
            values = ~TS_Mujeres,
            textposition = 'inside',
            marker = list(color = "#d300a9")) %>% 
      add_trace(y = ~TS_Varones, 
                name = 'Varones', 
                text = ~TS_Varones,
                values = ~TS_Varones,
                textposition = 'inside',
                marker = list(color = "#00d394")) %>% 
      layout(xaxis = list(title = "", tickangle = -45,showline= T, linewidth=1, linecolor="grey"),
             yaxis = list(title = "", showgrid=F, showticklabels=F),
             margin = list(b = 100),
             barmode = 'group')
  })
  output$MetodoTNR <- renderUI({HTML(paste("La tasa de Trabajo No Registrado, expresa el porcentaje de personas ocupadas en trabajos que no poseen descuentos jubilatorios, empleadas en trabajos informales. 
               El cálculo se realiza a nivel sexo: Ocupadas en Trabajo No Registrado Mujeres / Total Ocupadas Mujeres y Ocupados en Trabajo No Registrado Varones / Total Ocupados Varones.<br>",
                                           "<br>",               
                                           "Nuevamente es más elevado el nivel de trabajo no registrado de las mujeres que de los hombres, reflejando una mayor precariedad de sus empleos.<br>",
                                           sep = "")) })
  output$TNR <- renderPlotly({
    plot_ly(Tasas(),
            x = ~Año,
            y = ~TNR_Mujeres, 
            type = 'bar', 
            name = 'Mujeres', 
            text = ~TNR_Mujeres,
            values = ~TNR_Mujeres,
            textposition = 'inside',
            marker = list(color = "#d300a9")) %>% 
      add_trace(y = ~TNR_Varones, 
                name = 'Varones', 
                text = ~TNR_Varones,
                values = ~TNR_Varones,
                textposition = 'inside',
                marker = list(color = "#00d394")) %>% 
      layout(xaxis = list(title = "", tickangle = -45,showline= T, linewidth=1, linecolor="grey"),
             yaxis = list(title = "", showgrid=F, showticklabels=F),
             margin = list(b = 100),
             barmode = 'group')
  })  
  output$MetodoY <- renderUI({
    HTML(paste(h4("Metodología"),
               "<br>",               
               "El gráfico muestra la diferencia de ingresos de las mujeres, en relación a los hombres, expresada como diferencia porcentual.<br>",
               "<br>",  
               sep = ""))})
  output$des2 <- renderImage({
    list(
      src = "desigualdadY.jpg",
      width = 300,
      height = 250
    )}, deleteFile = F)
  
  Ingresos <- reactive({ITI %>%
      filter(
        CATEGORIA_OCUPACIONAL %in% input$CatOc,
        NIVEL_EDUCATIVO == input$NIVED,
        Año >= input$AÑOb[1] & Año <= input$AÑOb[2]
      )
  })
  
  output$ITI <- renderPlotly({
    plot_ly(Ingresos(),
            x = ~Año,
            y = ~BrechaIOP, 
            type = 'bar', 
            name = ~CATEGORIA_OCUPACIONAL, 
            text = ~BrechaIOP,
            split = ~CATEGORIA_OCUPACIONAL,
            values = ~BrechaIOP,
            textposition = 'inside',
            #marker = list(color = "#3fd300")
            colors = c("#003fd3", "#d39400", "#00d394", "#d300a9")) %>% 
      #layout(xaxis = list(title = "", tickangle = -90, linewidth=1, linecolor="grey"),
       #      yaxis = list(title = "",  visible = F, showgrid=F, showticklabels=F),
        #     margin = list(b = 100),
         #    barmode = 'group') 
    layout(xaxis = list(title = "", tickangle = -45),
           yaxis = list(title = "",  visible = F),
           margin = list(b = 100),
           barmode = 'group')
  })
  
  TasasDL <- reactive({TasasML %>%
      filter(
        GRUPO_EDAD == input$GE,
        NIVEL_EDUCATIVO == input$NE,
        Año %in% input$AnioML
      )
  })
  output$TasasML<- renderTable(TasasDL())

  IngresosDL <- reactive({ITI %>%
         filter(
           CATEGORIA_OCUPACIONAL == input$CO,
           NIVEL_EDUCATIVO == input$NEI,
           Año %in% input$AnioI
         )
     })
     
     
  output$TasasY<- renderTable(IngresosDL())

  output$DescargaML <- downloadHandler(
    filename = "TasasML.tsv",
    content = function(file) {
      write.csv(TasasDL(), file, row.names = F)
    }
  )

  output$DescargaY <- downloadHandler(
    filename = "ITI.tsv",
    content = function(file) {
      write.csv(IngresosDL(), file, row.names = F)
    }
  )

       output$Info <- renderUI({
        HTML(paste( "Elaborado por:<br>",
                    "<br>",
                    "Gra Bellotti<br>",
                    "<br>",
                    "Lic. en Sociología (UNLP)<br>",
                    "Especialista en Economía Política (FLACSO) <br>",
                    "Magister en Explotación de Datos y Gestión del Conocimiento (Universidad Austral)<br>",
                    "<br>",
                    "Contacto: <br>",
                    "<br>",
                    "www.linkedin.com/in/graciela-bellotti <br>",
                    "https://github.com/GraBellotti  <br>",
                    "grabellotti@gmail.com <br>",
                    sep = ""))
      })
  
  }

# Run the application
shinyApp(ui = ui, server = server)


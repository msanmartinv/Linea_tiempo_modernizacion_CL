# Librerias a cargar para esta app ####
library(shiny)
library(shinythemes)
library(readr)
library(plotly)
library(lubridate)
library(vistime)
library(rsconnect)
library(tidyverse)
library(htmlwidgets)

# Lectura del archivo CSV con casos de modernización del Estado de Chile ####
URL_CSV <- "https://transformacionpublica.cl/wp-content/uploads/2021/06/210621_Linea_Tiempo_a_Publicar_v8.csv"
Linea_Tiempo_a_Publicar <- read_delim(URL_CSV, ";", escape_double = FALSE, trim_ws = TRUE)

# Defino fecha de inicio para Vistime, a partir del año del evento
inicio <- lubridate::ymd(Linea_Tiempo_a_Publicar$Año, truncated = 2L)

# Defino etiqueta a mostrar para cada evento
etiqueta <- paste0(Linea_Tiempo_a_Publicar$Año, "<br>", 
                   Linea_Tiempo_a_Publicar$Evento_corto, "<br>", 
                   Linea_Tiempo_a_Publicar$Ambito)

# Defino paleta de colores para cada ámbito de incidencia
ambitos <- unique(Linea_Tiempo_a_Publicar[c("Ambito")])
paleta <- c("#e7dcd9","#d9afca","#bc6ca7","#eb9687","#c5ae91","#eb3c27","#8e3c36","#684832",
            "#afb1b4","#6ea2d5","#5f4b8b","#0e3a53","#ead94e","#c0d725","#00a28a","#f0ede5",
            "#ffffff")
color_ambitos <- cbind(ambitos, paleta)

# Defino fechas y años relevantes para mostrar en los textos
fecha_act <- Linea_Tiempo_a_Publicar$Publicada_el[1]
min_yr <- min(Linea_Tiempo_a_Publicar$Año)
max_yr <- max(Linea_Tiempo_a_Publicar$Año)
mide_yrs <- max_yr - min_yr

# Defino tabla con acciones por año 
acciones <- Linea_Tiempo_a_Publicar %>% select(-c(2:4, 6:10)) %>% group_by(Año, Accion) %>% summarize(n())

# Defino etiqueta a mostrar para cada evento para Vistime
Linea_Tiempo_a_Publicar <- left_join(Linea_Tiempo_a_Publicar, color_ambitos, by=c("Ambito", "Ambito"))
linea_tiempo <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                           start=inicio, 
                           end=inicio, 
                           group=Linea_Tiempo_a_Publicar$Accion, 
                           color=Linea_Tiempo_a_Publicar$paleta,
                           tooltip=etiqueta)
ambito_inc <- "Accesibilidad"

# Defino UI para la app que dibuja una línea de tiempo
ui <- fluidPage(
  
  theme = shinytheme("united"),
  navbarPage(paste0("Acciones de modernización en el Estado chileno entre ", min_yr, " y ", max_yr),  
             
             tabPanel("Línea de tiempo",
             # Título de la app 
             titlePanel("Línea de tiempo con modernización por tipo de acción realizada"),
             
             # Panel de la izquierda con un selector de ámbitos de incidencia 
             sidebarLayout(
               sidebarPanel(
                 strong("Ámbito de incidencia de las acciones de modernización realizadas"),
                 br(),
                 br(),
                 selectInput(inputId = "ambito_inc", 
                             label = strong("Elija uno o más ámbitos (si desea verlos todos deje en blanco el selector)"),
                             selected = NULL,
                             choices = sort(unique(Linea_Tiempo_a_Publicar$Ambito), decreasing = FALSE),
                             multiple = TRUE),
                 
                 helpText("Este visor permite ver las principales acciones de modernización realizadas en los últimos ", mide_yrs, " años."),
                 helpText(paste("Datos actualizados al", fecha_act)),
                 br(),
                 strong("Nota:"),
                 p("Esta es una recopilación personal de distintos eventos de modernización que he encontrado
                    en distintos sitios web. El criterio de agrupación y del tipo de incidencia es personal y si tiene
                    una opinión distinta o una propuesta estaré encantada de conversarla. Si considera que falta algún 
                    evento, feliz lo incorporaré (con el agradecimiento correspondiente)."),
                 br(),
                 a("Enlace al archivo CSV de datos", href = URL_CSV),
                 br(),
                 a("Enlace al GitHub de este proyecto", href = "https://msanmartinv.shinyapps.io/Linea_Tiempo_v1"),
                 br(),
                 p(em("Elaborado por:"),br("Macarena San Martín Vergara"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 
                 tabsetPanel(
                   id = "tabset1",
                   
                   tabPanel("Gráfico", 
                            br(),
                            "Gráfico de eventos de modernización", 
                            br(),
                            br(),
                            plotly::plotlyOutput("myVistime")),
                            
                   tabPanel("Datos", 
                            br(),
                            "Tabla de datos",
                            br(),
                            br(),
                            DT::dataTableOutput("table"),
                            downloadButton(outputId = "download_data", label = "Descargar datos"))
                 )
               )
             )
            ),
            
            tabPanel("Otros gráficos",
                     # Título de la app 
                     titlePanel("Cantidad y tipo de acciones de modernización por año"),
                     
                     # Panel de la izquierda con un selector de ámbitos de incidencia 
                     sidebarLayout(
                       sidebarPanel(
                         #strong("Elija rango de años para el histograma"),
                         #br(),
                         #sliderInput("selector", "",
                        #             min = min_yr, max = max_yr, value = c(min_yr, max_yr)),
                         
                         helpText("Este visor permite ver las principales acciones de modernización realizadas en los últimos ", mide_yrs, " años."),
                         helpText(paste("Datos actualizados al", fecha_act)),
                         br(),
                         br(),
                         strong("Nota:"),
                         p("Esta es una recopilación personal de distintos eventos de modernización que he encontrado
                           en distintos sitios web. El criterio de agrupación y del tipo de incidencia es personal y si tiene
                           una opinión distinta o una propuesta estaré encantada de conversarla. Si considera que falta algún 
                           evento, feliz lo incorporaré (con el agradecimiento correspondiente)."),
                         br(),
                         a("Enlace al archivo CSV de datos", href = URL_CSV),
                         br(),
                         a("Enlace al GitHub de este proyecto", href = "https://msanmartinv.shinyapps.io/Linea_Tiempo_v1"),
                         br(),
                         p(em("Elaborado por:"),br("Macarena San Martín Vergara"))
                         ),
                       
                       # Show a plot of the generated distribution
                       mainPanel(
                        
                         tabsetPanel(
                           id = "tabset2",
                           
                           tabPanel("Circular", 
                                    br(),
                                    "Gráfico de porcentaje de eventos de modernización por tipo de acción", 
                                    br(),
                                    br(),
                                    plotly::plotlyOutput("Torta")),
                           
                           tabPanel("Barras", 
                                    br(),
                                    "Gráfico de eventos de modernización por tipo y por año", 
                                    br(),
                                    br(),
                                    plotly::plotlyOutput("Histograma"))
                           
                            )
                          )
                       )
                     )
            
))

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$myVistime <- plotly::renderPlotly({
     
     # Gráfico todo si no hay selección de ambito_inc
     if (is.null(input$ambito_inc)) {
       linea_tiempo <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                                  start=inicio, 
                                  end=inicio, 
                                  group=Linea_Tiempo_a_Publicar$Accion, 
                                  color=Linea_Tiempo_a_Publicar$paleta,
                                  tooltip=etiqueta)       
     } 
     else {
       linea_tiempo <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                                  start=inicio, 
                                  end=inicio, 
                                  group=Linea_Tiempo_a_Publicar$Accion, 
                                  color=Linea_Tiempo_a_Publicar$paleta,
                                  tooltip=etiqueta) %>% filter(Linea_Tiempo_a_Publicar$Ambito == input$ambito_inc) 
     }
     vistime(linea_tiempo, show_labels = FALSE, linewidth = 15)
   })

   output$Histograma <- plotly::renderPlotly({ 
     plot_ly(acciones, 
             y = acciones$`n()`, 
             x = acciones$Año, 
             type = 'bar', 
             name = ~Accion) %>% layout(yaxis = list(title = 'Cantidad'), 
                                       barmode = 'stack')
     
   })
   
   output$Torta <- plotly::renderPlotly({
     colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
     plot_ly(Linea_Tiempo_a_Publicar, 
             labels = acciones$Accion, 
             values = table(Linea_Tiempo_a_Publicar$Accion), 
             type = 'pie',
             textposition = 'inside',
             textinfo = 'number+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = acciones$Accion,
             marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>% layout(title = ' ',
                                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
   })
   
   output$table <- DT::renderDataTable({
     if (is.null(input$ambito_inc)) {
       tabla <- Linea_Tiempo_a_Publicar %>% select(-c(2,4,11))
     }
     else {
       tabla <- Linea_Tiempo_a_Publicar %>% filter(Linea_Tiempo_a_Publicar$Ambito == input$ambito_inc) %>% select(-c(2,4,11))
     }
     DT::datatable(tabla, options = list(orderClasses = FALSE))
   })

   output$download_data <- downloadHandler(
     filename = function() {
       paste0("Acciones_Modernizacion_Chile_", Sys.Date(), ".csv")
     },
     content = function(file) {
       if (is.null(input$ambito_inc)) {
         tabla <- Linea_Tiempo_a_Publicar %>% select(-c(2,4,11))
       }
       else {
         tabla <- Linea_Tiempo_a_Publicar %>% filter(Linea_Tiempo_a_Publicar$Ambito == input$ambito_inc) %>% select(-c(2,4,11))
       }
       write.csv(tabla, file, row.names = FALSE)
     }
   )
   
   
   }

# Run the application 
shinyApp(ui = ui, server = server)




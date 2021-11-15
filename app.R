# Este programa fue desarrollado en R versión 3.6.1 con RStudio v 99.9.9 ejecutándose en Conda (Anaconda v3) 
# Elaborado por Macarena San Martín Vergara
#

# Librerias a cargar para esta app ####
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(readr)
library(plotly)
library(lubridate)
library(vistime)
library(rsconnect)
library(tidyverse)
library(htmlwidgets)

# Lectura del archivo CSV con casos de modernización del Estado de Chile ####
URL_CSV <- "https://transformacionpublica.cl/wp-content/uploads/2021/11/211112_Linea_Tiempo_a_Publicar_v10.csv"
URL_GH <- "https://github.com/msanmartinv/Linea_tiempo_modernizacion_CL"
Linea_Tiempo_a_Publicar <- read_delim(URL_CSV, ";", escape_double = FALSE, trim_ws = TRUE)

# Defino fecha de inicio para Vistime, a partir del año del evento
inicio <- lubridate::ymd(Linea_Tiempo_a_Publicar$Año, truncated = 2L)

# Defino etiqueta a mostrar para cada evento (criterio MSM)
etiqueta_msm <- paste0(Linea_Tiempo_a_Publicar$Año, "<br>", 
                   Linea_Tiempo_a_Publicar$Evento_corto, "<br>", 
                   Linea_Tiempo_a_Publicar$Ambito_Impacto)

# Defino etiqueta a mostrar para cada evento (criterio OCDE)
etiqueta_ocde <- paste0(Linea_Tiempo_a_Publicar$Año, "<br>", 
                   Linea_Tiempo_a_Publicar$Evento_corto, "<br>", 
                   Linea_Tiempo_a_Publicar$Dim_Gob_Dig_OECD)

# Defino las paletas de colores
paleta17_a <- c("#e7dcd9","#d9afca","#bc6ca7","#eb9687","#c5ae91","#eb3c27","#8e3c36","#684832",
             "#afb1b4","#6ea2d5","#5f4b8b","#0e3a53","#ead94e","#c0d725","#00a28a","#f0ede5",
             "#ffffff")
paleta17_b <- c("#a50026","#d7191c","#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8",
             "#abd9e9","#74add1","#4575b4","#313695","#762a83","#c2a5cf","#a6dba0","#5aae61",
             "#1b7837")
paleta6 <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb","#4575b4")
paleta6_b <- c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2c7bb6","#4575b4")
paleta5 <- c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2c7bb6")
paleta4 <- c("#d7191c","#fdae61","#abd9e9","#2c7bb6")
paleta4_b <- c("#d7191c","#fdae61","#abdda4","#2b83ba")

# Defino paleta de colores para cada ámbito de incidencia
ambitos <- unique(Linea_Tiempo_a_Publicar[c("Ambito_Impacto")])
color_ambitos <- cbind(ambitos, paleta17_b)

# Defino paleta de colores para cada dimensión OCDE
dimensiones <- unique(Linea_Tiempo_a_Publicar[c("Dim_Gob_Dig_OECD")])
color_dimensiones <- cbind(dimensiones, paleta6)

# Para dejar las URL en enlaces a clickear en la tabla (cambio el contenido de la columna URL)
Linea_Tiempo_a_Publicar$URL <- paste0("<a href='",Linea_Tiempo_a_Publicar$URL,"'>Ir al documento o sitio</a>")

# Defino fechas, años y datos relevantes para mostrar en los textos
fecha_act <- max(as.Date(Linea_Tiempo_a_Publicar$Recuperada_el, "%d-%m-%Y"))
min_yr <- min(Linea_Tiempo_a_Publicar$Año)
max_yr <- max(Linea_Tiempo_a_Publicar$Año)
mide_yrs <- max_yr - min_yr
cant_acc_tot <- Linea_Tiempo_a_Publicar %>% select(c(2)) %>% summarize(n())
cant_acciones <- Linea_Tiempo_a_Publicar %>% select(-c(1:4, 6:12)) %>% group_by(Tipo_Accion) %>% summarize(n())
cant_aspectos <- Linea_Tiempo_a_Publicar %>% select(-c(1:5, 7:12)) %>% group_by(Aspectos_Transv_OECD) %>% summarize(n())
cant_ambitos <- Linea_Tiempo_a_Publicar %>% select(-c(1:6, 8:12)) %>% group_by(Ambito_Impacto) %>% summarize(n())
cant_dimension <- Linea_Tiempo_a_Publicar %>% select(-c(1:7, 9:12)) %>% group_by(Dim_Gob_Dig_OECD) %>% summarize(n())

# Defino tablas con acciones y aspectos por año 
acciones <- Linea_Tiempo_a_Publicar %>% select(-c(2:4, 6:12)) %>% group_by(Año, Tipo_Accion) %>% summarize(n())
aspectos <- Linea_Tiempo_a_Publicar %>% select(-c(2:5, 7:12)) %>% group_by(Año, Aspectos_Transv_OECD) %>% summarize(n())
acc_amb <- Linea_Tiempo_a_Publicar %>% select(-c(1:4, 6, 8:12)) %>% group_by(Tipo_Accion, Ambito_Impacto) %>% summarize(n())
ambitos_yr <- Linea_Tiempo_a_Publicar %>% select(-c(2:6, 8:12)) %>% group_by(Año, Ambito_Impacto) %>% summarize(n())
dimension_yr <- Linea_Tiempo_a_Publicar %>% select(-c(2:7, 9:12)) %>% group_by(Año, Dim_Gob_Dig_OECD) %>% summarize(n())
asp_dim <- Linea_Tiempo_a_Publicar %>% select(-c(1:5, 7, 9:12)) %>% group_by(Aspectos_Transv_OECD, Dim_Gob_Dig_OECD) %>% summarize(n())

# Agrego columnas con colores por Ámbito y por Dimensión
Linea_Tiempo_a_Publicar <- left_join(Linea_Tiempo_a_Publicar, color_ambitos, by=c("Ambito_Impacto", "Ambito_Impacto"))
Linea_Tiempo_a_Publicar <- left_join(Linea_Tiempo_a_Publicar, color_dimensiones, by=c("Dim_Gob_Dig_OECD", "Dim_Gob_Dig_OECD"))

# Defino etiqueta a mostrar para cada evento para Linea Tiempo A
etiqueta_a <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                           start=inicio, 
                           end=inicio, 
                           group=Linea_Tiempo_a_Publicar$Tipo_Accion, 
                           color=Linea_Tiempo_a_Publicar$paleta17_b,
                           tooltip=etiqueta_msm)

# Defino etiqueta a mostrar para cada evento para Linea Tiempo B
etiqueta_b <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                           start=inicio, 
                           end=inicio, 
                           group=Linea_Tiempo_a_Publicar$Aspectos_Transv_OECD, 
                           color=Linea_Tiempo_a_Publicar$paleta6,
                           tooltip=etiqueta_ocde)

# Defino UI para la app que dibuja una línea de tiempo
ui <- fluidPage(
  
  theme = shinytheme("united"),
  navbarPage(paste0("Acciones de modernización en el Estado chileno entre ", min_yr, " y ", max_yr),  

# Panel de Mirada General ####              
             tabPanel("Mirada general",
                      # Título de la app 
                      titlePanel("Cantidad y tipo de acciones de modernización por año"),
                      
                      # Panel de la izquierda 
                      sidebarLayout(
                        sidebarPanel(
                          p("En este artículo se presenta una recopilación de acciones o implementaciones de políticas públicas 
                            ocurridos en Chile entre el año ", min_yr, " y ", max_yr, " que provocaron cambios en la forma en que el Estado 
                            entrega servicios a la ciudadanía, ya sea con leyes, nuevas políticas, orgánicas institucionales, 
                            mejoras de procesos y muchas otras que en su momento fueron llamadas modernizaciones, y que hoy se 
                            consideran transformaciones"),
                          br(),
                          p("Este visor permite ver las principales acciones de modernización realizadas en los últimos ", mide_yrs, " años."),
                          p("Para ello, se han analizado", cant_acc_tot, " acciones."),
                          p(paste("Datos actualizados al", fecha_act)),
                          br(),
                          helpText("Nota:"),
                          helpText("Esta es una recopilación personal de distintos eventos de modernización que he encontrado
                            en distintos sitios web. El criterio de agrupación y del tipo de incidencia es de la autora y si tiene
                            una opinión distinta o una propuesta puede escribir a macarena_sanm {at} yahoo.com. Si considera que falta algún 
                            evento, feliz lo incorporaré (con el agradecimiento correspondiente)."),
                          br(),
                          a("Enlace al archivo CSV de datos", href = URL_CSV),
                          br(),
                          a("Enlace a GitHub de este proyecto", href = URL_GH),
                          hr(),
                          p(em("Elaborado por"),br("Macarena San Martín Vergara"),style="text-align:left; font-family: times")
                        ),
                        
                        # Panel principal
                        mainPanel(
                          
                          tabsetPanel(
                            id = "tabset2",
                            
                            tabPanel("Acciones por criterio", 
                                     br(),
                                     "Acciones de Modernización según criterio", 
                                     br(),
                                     br(),
                                     plotly::plotlyOutput("Torta") %>% withSpinner(type = 4)
                                     ),
                            
                            tabPanel("Acciones por tipo y año", 
                                     br(),
                                     "Eventos de modernización por tipo de acción y por año", 
                                     br(),
                                     br(),
                                     plotly::plotlyOutput("Histog_acc_yr") %>% withSpinner(type = 4),
                                     br(),
                                     "Tabla", 
                                     br(),
                                     br(),
                                     fluidRow(column(DT::dataTableOutput("tablaAccYr"),width=8))
                                     ),
                            
                            tabPanel("Acciones por aspecto y año", 
                                     br(),
                                     "Eventos de modernización por aspecto y año", 
                                     br(),
                                     br(),
                                     plotly::plotlyOutput("Histog_asp_yr") %>% withSpinner(type = 4),
                                     br(),
                                     "Tabla", 
                                     br(),
                                     br(),
                                     fluidRow(column(DT::dataTableOutput("tablaAspYr"),width=8))
                                     ),
                            
                            tabPanel("Acciones por ámbito", 
                                     br(),
                                     "Eventos de modernización por tipo de acción y aspecto", 
                                     br(),
                                     br(),
                                     plotly::plotlyOutput("Bubble_acc_amb") %>% withSpinner(type = 4),
                                     br(),
                                     br(),
                                     br()
                            ),
                            
                            tabPanel("Acciones por ámbito y año", 
                                     br(),
                                     "Eventos de modernización por ámbito y año", 
                                     br(),
                                     br(),
                                     plotly::plotlyOutput("Histog_amb_yr") %>% withSpinner(type = 4),
                                     br(),
                                     "Tabla", 
                                     br(),
                                     br(),
                                     fluidRow(column(DT::dataTableOutput("tablaAmbYr"),width=8))
                                     ),
                            
                            tabPanel("Acciones por dimensión y año", 
                                     br(),
                                     "Eventos de modernización por dimensión y año", 
                                     br(),
                                     br(),
                                     plotly::plotlyOutput("Histog_dim_yr") %>% withSpinner(type = 4),
                                     br(),
                                     "Tabla", 
                                     br(),
                                     br(),
                                     fluidRow(column(DT::dataTableOutput("tablaDimYr"),width=8))
                                     ),
                            
                            tabPanel("Aspectos por dimensión OCDE", 
                                     br(),
                                     "Eventos de modernización por tipo de acción y aspecto", 
                                     br(),
                                     br(),
                                     plotly::plotlyOutput("Bubble_asp_dim") %>% withSpinner(type = 4),
                                     br(),
                                     br(),
                                     br()
                            )

                          )
                        )
                        )
                      ),
             
# Panel de Linea de Tiempo MSM ####
        tabPanel("Línea de tiempo por tipo de acción",
             # Título de la app 
             titlePanel("Línea de tiempo con modernización por tipo de acción realizada"),
             
             # Panel de la izquierda 
             sidebarLayout(
               sidebarPanel(
                 strong("Ámbito de incidencia de las acciones de modernización realizadas (según definiciones de la autora)"),
                 br(),
                 br(),
                 selectInput(inputId = "ambito_inc", 
                             label = strong("Elija uno o más ámbitos (si desea verlos todos deje en blanco el selector)"),
                             selected = NULL,
                             choices = sort(unique(Linea_Tiempo_a_Publicar$Ambito_Impacto), decreasing = FALSE),
                             multiple = TRUE),
                 
                 p("Este visor permite ver las principales acciones de modernización realizadas en los últimos ", mide_yrs, " años."),
                 p(paste("Datos actualizados al", fecha_act)),
                 br(),
                 helpText("Nota:"),
                 helpText("Esta es una recopilación personal de distintos eventos de modernización que he encontrado
                    en distintos sitios web. El criterio de agrupación y del tipo de incidencia es personal y si tiene
                    una opinión distinta o una propuesta estaré encantada de conversarla. Si considera que falta algún 
                    evento, feliz lo incorporaré (con el agradecimiento correspondiente)."),
                 br(),
                 a("Enlace al archivo CSV de datos", href = URL_CSV),
                 br(),
                 a("Enlace a GitHub de este proyecto", href = URL_GH),
                 hr(),
                 p(em("Elaborado por"),br("Macarena San Martín Vergara"),style="text-align:left; font-family: times")
               ),
               
               # Panel principal
               mainPanel(
                 
                 tabsetPanel(
                   id = "tabsetA",
                   
                   tabPanel("Línea de tiempo por tipo de acción", 
                            br(),
                            " ", 
                            br(),
                            br(),
                            plotly::plotlyOutput("LineaTiempoA") %>% withSpinner(type = 4)
                            ),
                            
                   tabPanel("Tabla de datos por tipo de acción", 
                            br(),
                            " ",
                            br(),
                            br(),
                            downloadButton(outputId = "download_dataA", label = "Descargar datos"),
                            DT::dataTableOutput("tablaA"))
                 )
               )
             )
            ),

# Panel de Linea de Tiempo OCDE ####
            tabPanel("Línea de tiempo por aspectos",
                     # Título de la app 
                     titlePanel("Línea de tiempo de modernización por aspectos transversales y dimensiones definidos por OCDE 2019"),
                     
                     # Panel de la izquierda 
                     sidebarLayout(
                       sidebarPanel(
                         strong("Ámbito de incidencia de las acciones de modernización realizadas (según definiciones OCDE)"),
                         br(),
                         br(),
                         selectInput(inputId = "dimension_ocde", 
                                     label = strong("Elija una o más dimensiones (si desea verlas todas deje en blanco el selector)"),
                                     selected = NULL,
                                     choices = sort(unique(Linea_Tiempo_a_Publicar$Dim_Gob_Dig_OECD), decreasing = FALSE),
                                     multiple = TRUE),
                         
                         p("Este visor permite ver las principales acciones de modernización realizadas en los últimos ", mide_yrs, " años."),
                         p(paste("Datos actualizados al", fecha_act)),
                         br(),
                         helpText("Nota:"),
                         helpText("Esta es una recopilación personal de distintos eventos de modernización que he encontrado
                           en distintos sitios web. El criterio de agrupación y del tipo de dimensión está basado en el usado  
                           por la OCDE para construir el Índice de Gobiernos Digitales 2019 que se referencia abajo. Si considera  
                           que falta algún evento, feliz lo incorporaré (con el agradecimiento correspondiente)."),
                         br(),
                         a("Enlace al Índice de Gobiernos Digitales 2019 OCDE", href = "https://www.oecd.org/gov/digital-government-index-4de9f5bb-en.htm"),
                         br(),
                         a("Enlace al archivo CSV de datos", href = URL_CSV),
                         br(),
                         a("Enlace a GitHub de este proyecto", href = URL_GH),
                         hr(),
                         p(em("Elaborado por"),br("Macarena San Martín Vergara"),style="text-align:left; font-family: times")
                       ),
                       
                       # Panel principal
                       mainPanel(
                         
                         tabsetPanel(
                           id = "tabsetB",
                           
                           tabPanel("Línea de tiempo por aspectos", 
                                    br(),
                                    "Tardará un poco en cargar...", 
                                    br(),
                                    br(),
                                    plotly::plotlyOutput("LineaTiempoB") %>% withSpinner(type = 4)
                                    ),
                           
                           tabPanel("Tabla de datos por aspectos", 
                                    br(),
                                    " ",
                                    br(),
                                    br(),
                                    downloadButton(outputId = "download_dataB", label = "Descargar datos"),
                                    DT::dataTableOutput("tablaB")
                                    )
                         )
                       )
                       )
                     )

))

# Define server logic required to draw a histogram
server <- function(input, output) {

# GRÁFICO LINEAS DE TIEMPO ####
  output$LineaTiempoA <- plotly::renderPlotly({
 
     # Gráfico incluye todo si no hay selección de ambito_inc
     if (is.null(input$ambito_inc)) {
       linea_tiempo_a <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                                  start=inicio, 
                                  end=inicio, 
                                  group=Linea_Tiempo_a_Publicar$Tipo_Accion, 
                                  color=Linea_Tiempo_a_Publicar$paleta17_b,
                                  tooltip=etiqueta_msm)       
     } 
     else {
       linea_tiempo_a <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                                  start=inicio, 
                                  end=inicio, 
                                  group=Linea_Tiempo_a_Publicar$Tipo_Accion, 
                                  color=Linea_Tiempo_a_Publicar$paleta17_b,
                                  tooltip=etiqueta_msm) %>% filter(Linea_Tiempo_a_Publicar$Ambito_Impacto == input$ambito_inc) 
     }
     vistime(linea_tiempo_a, show_labels = FALSE, linewidth = 15)
   })

   output$LineaTiempoB <- plotly::renderPlotly({
     
     # Gráfico incluye todo si no hay selección de dimension_ocde
     if (is.null(input$dimension_ocde)) {
       linea_tiempo_b <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                                  start=inicio, 
                                  end=inicio, 
                                  group=Linea_Tiempo_a_Publicar$Aspectos_Transv_OECD, 
                                  color=Linea_Tiempo_a_Publicar$paleta6,
                                  tooltip=etiqueta_ocde)       
     } 
     else {
       linea_tiempo_b <- data.frame(event=Linea_Tiempo_a_Publicar$Titulo_evento, 
                                  start=inicio, 
                                  end=inicio, 
                                  group=Linea_Tiempo_a_Publicar$Aspectos_Transv_OECD, 
                                  color=Linea_Tiempo_a_Publicar$paleta6,
                                  tooltip=etiqueta_ocde) %>% filter(Linea_Tiempo_a_Publicar$Dim_Gob_Dig_OECD == input$dimension_ocde) 
     }
     vistime(linea_tiempo_b, show_labels = FALSE, linewidth = 15)
   })

# GRÁFICO HISTOGRAMAS ####
   output$Histog_acc_yr <- plotly::renderPlotly({ 
     plot_ly(acciones, 
             y = acciones$`n()`, 
             x = acciones$Año, 
             type = 'bar',
             #marker = list(color = paleta4_b),
             name = ~Tipo_Accion) %>% layout(yaxis = list(title = 'Cantidad'), 
                                       barmode = 'stack')
   })
   
   output$Histog_asp_yr <- plotly::renderPlotly({ 
     plot_ly(aspectos, 
             y = aspectos$`n()`, 
             x = aspectos$Año, 
             type = 'bar', 
             name = ~Aspectos_Transv_OECD) %>% layout(yaxis = list(title = 'Cantidad'), 
                                             barmode = 'stack')
   })
   
   output$Histog_amb_yr <- plotly::renderPlotly({ 
     plot_ly(ambitos_yr, 
             y = ambitos_yr$`n()`, 
             x = ambitos_yr$Año, 
             type = 'bar', 
             name = ~Ambito_Impacto) %>% layout(yaxis = list(title = 'Cantidad'), 
                                                      barmode = 'stack')
   })
   
   output$Histog_dim_yr <- plotly::renderPlotly({ 
     plot_ly(dimension_yr, 
             y = dimension_yr$`n()`, 
             x = dimension_yr$Año, 
             type = 'bar', 
             name = ~Dim_Gob_Dig_OECD) %>% layout(yaxis = list(title = 'Cantidad'), 
                                                      barmode = 'stack')
   })
   
# GRÁFICO CIRCULARES ####
   output$Circular1 <- plotly::renderPlotly({
     plot_ly(Linea_Tiempo_a_Publicar, 
             labels = cant_acciones$Tipo_Accion, 
             values = cant_acciones$`n()`, 
             type = 'pie',
             textposition = 'inside',
             insidetextfont = list(color = '#FFFFFF'),
             hovertemplate = "%{label} <br> Cantidad: %{value} de %{cant_acc_tot} </br> Porcentaje: %{percent}",
             marker = list(colors = paleta4, line = list(color = '#FFFFFF', width = 1)),
             showlegend = TRUE) %>% layout(title = ' ',
                                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
   })

   output$Torta <- plotly::renderPlotly({
     fig <- plot_ly()
     fig <- fig %>% add_pie(data = cant_acciones, labels = ~Tipo_Accion, values = ~`n()`,
                            hovertemplate = "%{label} <br> Cantidad: %{value} </br> Porcentaje: %{percent}",
                            marker = list(colors = paleta4, line = list(color = '#FFFFFF', width = 1)),
                            showlegend = T, name = "Acciones", domain = list(row = 0, column = 0))
     fig <- fig %>% add_pie(data = cant_ambitos, labels = ~Ambito_Impacto, values = ~`n()`,
                            hovertemplate = "%{label} <br> Cantidad: %{value} </br> Porcentaje: %{percent}",
                            marker = list(colors = paleta17_b, line = list(color = '#FFFFFF', width = 1)),
                            showlegend = TRUE, name = "Ambitos", domain = list(row = 0, column = 1))
     fig <- fig %>% add_pie(data = cant_aspectos, labels = ~Aspectos_Transv_OECD, values = ~`n()`,
                            hovertemplate = "%{label} <br> Cantidad: %{value} </br> Porcentaje: %{percent}",
                            marker = list(colors = paleta4_b, line = list(color = '#FFFFFF', width = 1)),
                            showlegend = TRUE, name = "Aspectos", domain = list(row = 1, column = 0))
     fig <- fig %>% add_pie(data = cant_dimension, labels = ~Dim_Gob_Dig_OECD, values = ~`n()`,
                            hovertemplate = "%{label} <br> Cantidad: %{value} </br> Porcentaje: %{percent}",
                            marker = list(colors = paleta6, line = list(color = '#FFFFFF', width = 1)),
                            showlegend = TRUE, name = "Dimensiones", domain = list(row = 1, column = 1))
     fig <- fig %>% layout(title = " ", showlegend = F,
                           grid=list(rows=2, columns=2),
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     fig
   })

# GRÁFICO BURBUJAS ####
   output$Bubble_acc_amb <- plotly::renderPlotly({
     fig <- plot_ly(acc_amb, x = ~Tipo_Accion, y = ~Ambito_Impacto, text = ~`n()`, type = 'scatter', mode = 'markers',
                    marker = list(size = ~`n()`, opacity = 0.7))
     fig <- fig %>% layout(title = ' ',
                           xaxis = list(showgrid = TRUE),
                           yaxis = list(showgrid = TRUE))
     fig
     })

   output$Bubble_asp_dim <- plotly::renderPlotly({
     fig <- plot_ly(asp_dim, x = ~Aspectos_Transv_OECD, y = ~Dim_Gob_Dig_OECD, text = ~`n()`, type = 'scatter', mode = 'markers',
                    marker = list(size = ~`n()`, opacity = 0.7))
     fig <- fig %>% layout(title = ' ', 
                           xaxis = list(showgrid = TRUE),
                           yaxis = list(showgrid = TRUE))
     fig
   })
   
   
# TABLAS ####
   output$tablaAccYr <- DT::renderDataTable({
     DT::datatable(acciones, options = list(searchHighlight = TRUE), filter = 'top', escape = FALSE)
   })
   
   output$tablaAspYr <- DT::renderDataTable({
     DT::datatable(aspectos, options = list(searchHighlight = TRUE), filter = 'top', escape = FALSE)
   })
   
   output$tablaAmbYr <- DT::renderDataTable({
     DT::datatable(ambitos_yr, options = list(searchHighlight = TRUE), filter = 'top', escape = FALSE)
   })
   
   output$tablaDimYr <- DT::renderDataTable({
     DT::datatable(dimension_yr, options = list(searchHighlight = TRUE), filter = 'top', escape = FALSE)
   })
   
   output$tablaA <- DT::renderDataTable({
     if (is.null(input$ambito_inc)) {
       tabla_a <- Linea_Tiempo_a_Publicar %>% select(-c(2,4,8,11))
     }
     else {
       tabla_a <- Linea_Tiempo_a_Publicar %>% filter(Linea_Tiempo_a_Publicar$Ambito_Impacto == input$ambito_inc) %>% select(-c(2,4,8,11))
     }
     #DT::datatable(tabla, options = list(orderClasses = FALSE))
     DT::datatable(tabla_a, options = list(searchHighlight = TRUE), filter = 'top', escape = FALSE)
   })

   output$tablaB <- DT::renderDataTable({
     if (is.null(input$dimension_ocde)) {
       tabla_b <- Linea_Tiempo_a_Publicar %>% select(-c(2,4,8,11))
     }
     else {
       tabla_b <- Linea_Tiempo_a_Publicar %>% filter(Linea_Tiempo_a_Publicar$Dim_Gob_Dig_OECD == input$dimension_ocde) %>% select(-c(2,4,8,11))
     }
     #DT::datatable(tabla, options = list(orderClasses = FALSE))
     DT::datatable(tabla_b, options = list(searchHighlight = TRUE), filter = 'top', escape = FALSE)
   })
   
# Descarga de Tablas ####
   output$download_dataA <- downloadHandler(
     filename = function() {
       paste0("Acciones_Modernizacion_Chile_", Sys.Date(), ".csv")
     },
     content = function(file) {
       if (is.null(input$ambito_inc)) {
         tablaA <- Linea_Tiempo_a_Publicar %>% select(-c(2,4,11))
       }
       else {
         tablaA <- Linea_Tiempo_a_Publicar %>% filter(Linea_Tiempo_a_Publicar$Ambito_Impacto == input$ambito_inc) %>% select(-c(2,4,11))
       }
       write.csv(tablaA, file, row.names = FALSE)
     }
   )
   
   output$download_dataB <- downloadHandler(
     filename = function() {
       paste0("Acciones_Modernizacion_Chile_", Sys.Date(), ".csv")
     },
     content = function(file) {
       if (is.null(input$dimension_ocde)) {
         tablaA <- Linea_Tiempo_a_Publicar %>% select(-c(2,4,11))
       }
       else {
         tablaA <- Linea_Tiempo_a_Publicar %>% filter(Linea_Tiempo_a_Publicar$Ambito_Impacto == input$dimension_ocde) %>% select(-c(2,4,11))
       }
       write.csv(tablaA, file, row.names = FALSE)
     }
   )
   
   
   }

# Run the application 
shinyApp(ui = ui, server = server)




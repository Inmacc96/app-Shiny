
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(shiny)
library(shinythemes)
library(dygraphs)
library(vars)
library(visNetwork)

var_texto = "
Esta aplicación shiny tiene el objetivo de demostrar
los conocimientos adquiridos
en la construcción de un cuadro de mandos básico con R.
"

var_texto_mitfm = "
Mi TFM consiste en obtener un dataset suficientemente enriquecido a partir de 3 datasets sobre vehículos.
Para ello, se utilizarán técnicas de Data Science con el fin de que la fusión sea lo más optima posible.
"

# Datos
GDatasets = c("advertising.csv","datos_salarios.csv","datos_ordenadores.csv")
datos <- read.csv("data/advertising.csv")
names(datos) = c("Id","TVPubl","RadioPubl","PeriodicosPubl","Ventas")
var_selec = c(2:5)

# Mapas
GLugares <- c("Sevilla","Córdoba","Granada","Londres","India", "Vaticano")
GLatLong <- data.frame(
    Lat = c(37.35945,37.879100,37.176564,51.500818,27.173560,41.90234465),
    Long = c(-5.98814,-4.779483, -3.588987,-0.124510,78.042743,12.4568083)
) 

GLatLongPopUps <- data.frame( 
    Lat = c(37.359443,37.879626,37.176266,51.5006895, 27.175011, 41.902191),
    Long = c(-5.988027, -4.779906, -3.588195, -0.1245838,  78.042131 ,  12.453709) 
)

Gcontents <- c(paste(sep = "<br/>", "<b><a href='https://matematicas.us.es/'>Facultad de Matemáticas(US)</a></b>",
                        "Universidad de Sevilla", "Calle Tarfia, s/n, 41012 Sevilla") ,
               paste(sep = "<br/>", "<b><a href='https://mezquita-catedraldecordoba.es/descubre-el-monumento/el-edificio/patio-de-los-naranjos/'>Patio de los Naranjos</a></b>",
                      "Mezquita-Catedral de Córdoba", "Calle Cardenal Herrero, 1, 14003 Córdoba") ,
               paste(sep = "<br/>", "<b><a href='https://www.alhambra-patronato.es/'>Alhambra</a></b>",
                       "Monumento", "Realejo-San Matias, 18009 Granada") ,
               paste(sep = "<br/>", "<b><a href='https://es.wikipedia.org/wiki/Big_Ben'>Big Ben</a></b>",
                     "Palacio de Westminster", "Westminster, London SW1A 0AA, Reino Unido"),
               paste(sep = "<br/>", "<b><a href='https://www.tajmahal.gov.in/'>Taj Mahal</a></b>",
                     "Monumento", "Dharmapuri, Forest Colony, Tajganj, Agra, Uttar Pradesh 282001, India"),
               paste(sep = "<br/>", "<b><a href='http://www.vatican.va/various/basiliche/san_pietro/index_it.htm'>Basílica de San Pedro</a></b>",
                     "Vaticano", "Piazza San Pietro, 00120 Città del Vaticano")
               )

#Series temporales

data("co2")
Seriestemporalesnames <- c("co2","lungDeaths")
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)

#Grafo

nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))



#Registros

#Define los campos que queremos grabar del formulario
fields <-  c("Nombre", "Nombre_restaurante", "Localidad",
             "Puntuación_comida", "Puntuación_servicio",
             "Puntuación_local", "Puntuación_precio")

#Guardamos los datos en el data.frame: "responses"
saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}




# fluidPage -----
ui <- fluidPage( #o: shinyUI(
    theme=shinythemes::shinytheme(theme = "superhero"),
    includeCSS("www/estilos.css"),
    # titlePanel -----
    titlePanel(strong("Inteligencia de Negocio con R: DSBI",
                      style="color:light blue;"),
               windowTitle = "Inteligencia de Negocio con R: DSBI"),
    # navlistPanel -----
    navlistPanel(widths=c(2,10),
                 # tabPanel: Información ----
                 tabPanel("Información",icon = icon("info"),
                          div(class="center",
                              img(src="portadashiny.png",height="200px"),
                              img(src="hex_shiny.jpg",height="200px")
                          ),
                          br(),br(),br(),
                          hr(),
                          h3("Objetivo:",class="estiloh3"),
                          var_texto,
                          h3("Autor:",class="estiloh3"),
                          strong("Mª Inmaculada Caballero Carrero"),
                          h3("Resumen de mi Trabajo Fin de Máster:",class="estiloh3"),
                          var_texto_mitfm
                 ), #Cierre navlistpanel-tabPanel-Informacion ----
                 
                 # tabPanel: Datos -----
                 tabPanel("Datos",icon = icon("database"),
                         # VerticalLayout: Seleccionar datos de entrada y representación tabla ----
                          verticalLayout(selectInput("SelDataset", strong("Selecciona un dataset"),
                                                     GDatasets),
                          DT::dataTableOutput('tbl'))
                ), #Cierre navlistpanel-tabPanel-Datos ----
                 
                # tabPanel: Estudio Descriptivo ----
                 tabPanel("Estudio Descriptivo",icon = icon("chart-bar"),
                          # fluidRow: selecciona variable
                          fluidRow(
                              column(width=4,
                                     shiny::selectInput("Selvariable01Uni","Selecciona variable",
                                                        choices = names(datos)[c(var_selec)],
                                                        selected = names(datos)[var_selec[1]])
                              ) #Cierre column fluidRow Tabpanel Estudio Descriptivo ----
                            ), #Cierre fluidRow TabPanel Estudio Descriptivo ----
                          # tabsetPanel: Unidimensional-----
                          tabsetPanel(
                              # tabPanel: Resumen Numérico ----
                              tabPanel("Resumen Numérico",
                                       shiny::verbatimTextOutput("OUTResNum") # verbatimText: OUTResNum
                                        ), #Cierre tabpanel Resumen numérico ----
                              
                              # tabPanel: Gráficos Unidimensionales ----
                              tabPanel("Gráficos Unidimensionales",
                                       tabsetPanel(
                                           tabPanel("Histograma",
                                                    conditionalPanel("!['estado.civil','raza','nivel.educacion','tipo.trabajo','salud',
                                                                     'seguro.medico','cd','multi','buenfabricante'].includes(input.Selvariable01Uni)",
                                                                     plotlyOutput("plotHist")),
                                                    conditionalPanel("['estado.civil','raza','nivel.educacion','tipo.trabajo','salud',
                                                                     'seguro.medico','cd','multi','buenfabricante'].includes(input.Selvariable01Uni)",
                                                                     verbatimTextOutput("avisoplotHist"))
                                                    ), #Cierre tabPanel Histograma ----
                                           tabPanel("Diagrama de Barras",
                                                    conditionalPanel("['estado.civil','raza','nivel.educacion','tipo.trabajo','salud',
                                                                     'seguro.medico','cd','multi','buenfabricante'].includes(input.Selvariable01Uni)",
                                                                     plotlyOutput("plotDiagBar")),
                                                    conditionalPanel("!['estado.civil','raza','nivel.educacion','tipo.trabajo','salud',
                                                                     'seguro.medico','cd','multi','buenfabricante'].includes(input.Selvariable01Uni)",
                                                                     verbatimTextOutput("avisoplotDiagBar"))
                                                  
                                                    ), #Cierre tabPanel Diagrama de barras ----
                                           tabPanel("Boxplot",
                                                    conditionalPanel("!['estado.civil','raza','nivel.educacion','tipo.trabajo','salud',
                                                                     'seguro.medico','cd','multi','buenfabricante'].includes(input.Selvariable01Uni)",
                                                                     plotlyOutput("plotboxplot")),
                                                    conditionalPanel("['estado.civil','raza','nivel.educacion','tipo.trabajo','salud',
                                                                     'seguro.medico','cd','multi','buenfabricante'].includes(input.Selvariable01Uni)",
                                                                     verbatimTextOutput("avisoplotboxplot"))
                                                    ) #Cierre tabPanel Boxplot ----
                                       ) #Cierre tabsetPanel Gráficos unidimensionales ----
                                       ),#Cierre tabpanel Gráficos unidimensionales ----
                              
                              # tabPanel: Regresión Lineal ----
                              tabPanel("Regresión Lineal",
                                       fluidRow(
                                            column(width=4,
                                                    shiny::selectInput("SelvariableDepY","Selecciona variable dependiente (Y)",
                                                                        choices = names(datos)[c(var_selec)],
                                                                        selected = names(datos)[var_selec[1]])
                                                    ), #Cierre column1 fluidRow Regresión lineal ----
                                            column(width=1,
                                                   style='padding:20px;',
                                                   actionButton("Invertir","",icon=icon("arrows-alt-h"))

                                                    ), #Cierre column2 fluidRow Regresión Lineal ----
                                            column(width=4,
                                                    shiny::selectInput("SelvariableIndepX","Selecciona variable independiente (X)",
                                                                        choices = names(datos)[c(var_selec)],
                                                                        selected = names(datos)[var_selec[1]])
                                                    ) #Cierre column3 fluidRow Regresión Lineal ----
                              
                                          ), #Cierre fluidRow Regresión Lineal ----
                                       shiny::actionButton("BotonCal","Calcular",icon=icon("calculator"),width="97px",height=1),
                                       fluidRow(
                                           column(width=5,
                                                  shiny::verbatimTextOutput("OUTModLineal"),
                                                  tags$head(tags$style("#OUTModLineal{font-size: 12px;
                                                                                    }"))
                                                  ), #Cierre column1 salida Regresión Lineal ----
                                           column(width=5,
                                                 shiny::plotOutput("OUTGrafDispersion") 
                                                 ) #Cierre column2 diagrama de dispersión ----
                                       ) #Cierre fluidRow salida y diagrama de dispersión RegresiónLineal ----
                                       ) #Cierre tabPanel Regresión Lineal ----
                              ) #Cierre tabsetpanel Resumen Numérico ----
                 ), #Cierre tabPanel Estudio Descriptivo ----
         
                 # tabPanel: Mapas ----
                 tabPanel("Mapas", icon = icon("globe"),
                          fluidRow(
                                    column(width=3,offset=1,shiny::selectInput("SelLugar01","Selecciona Lugar de visita",
                                                                      choices=GLugares,
                                                                      selected=GLugares[1])
                                          ), #Cierre column1 fluidRow Mapas ----
                                   column(width=5,offset=1, shiny::tags$ul(
                                       shiny::tags$li(tags$a('¿Cómo obtener latitud-longitud?',target='_blank',href="https://www.bufa.es/google-maps-latitud-longitud/")),
                                       shiny::tags$li(tags$a('¿Cómo obtener latitud-longitud(youtube)?',target='_blank', href="https://www.youtube.com/watch?v=AInC_XTANrQ")))
                                       ) #Cierre column2 fluidRow Mapas ----
                                    ), #Cierre fluidRow Mapas ----
                          br(),
                          leafletOutput("map", height="600px")
                          ), #cierre tabPanel Mapas ----
                
                # tabPanel: Series Temporales ----
                tabPanel("Series temporales",icon=icon("far fa-clock"),
                         fluidRow( 
                                  column(width=4 , selectInput("Selsertemp","Selecciona la serie temporal",
                                                            choices= Seriestemporalesnames,
                                                            selected= Seriestemporalesnames[1])
                                    ), #Cierre column1 fluidRow Series Temporales ----
                             conditionalPanel("input.Selsertemp != 'lungDeaths'",
                                   column(width=2,numericInput("months", label = "Meses a Predecir",
                                                                         value = 72, min = 12, max = 144, step = 12),
                                          ), #Cierre column2 conditionalPanel1 fluidRow Series Temporales ----
                                   column(width=2, checkboxInput("IC", label = "Mostrar Intervalo de confianza", value = TRUE),
                                          checkboxInput("showgrid", label = "Mostrar Grid", value = TRUE)
                                          ), #Cierre column3 conditionalPanel1 fluidRow Series Temporales ----
                                   column(width=2, checkboxInput("stepchart", label = "Mostrar gráfico escalonado", value = FALSE),
                                          checkboxInput("area", label = "Mostrar área", value = FALSE)
                                          ), #Cierre column4 conditionalPanel1 fluidRow Series Temporales ----
                                   column(width=1, checkboxInput("points", label = "Mostrar puntos serie", value = FALSE)       
                                          ) #Cierre column5 conditionalPanel1 fluidRow Series Temporales ----
                             ) #Cierre conditionalPanel1 fluidRow Series Temporales ----
                             
                         ), #Cierre fluidRow Series Temporales ----
                         conditionalPanel("input.Selsertemp != 'lungDeaths'",
                                sidebarLayout(
                                     sidebarPanel(
                                         shiny::verbatimTextOutput("OUTSerTemp")
                                      ), #Cierre sidebarPanel Series Temporales ----
                                     mainPanel(
                                         dygraphOutput("dygraphSerTemp1")
                                     ) #Cierre mainPanel Series Temporales ----
                                  )#Cierre sidebarLayout conditionalPanel3 Series Temporales ----
                         ), #Cierre conditionalPanel2 Series Temporales ----
                         conditionalPanel("input.Selsertemp == 'lungDeaths'",
                                          dygraphOutput("dygraphSerTemp2")
                                          ) #Cierre conditionalPanel3 Series Temporales ----
                         ),#Cierre tabPanel Series Temporales ----
                
                #tabPanel: Grafos ----
                tabPanel("Grafos",icon = icon("fas fa-project-diagram"),
                         fluidRow(
                           column(width = 3,
                                  selectInput("color", "Color :",
                                               choices = c("Blanco" = "white",
                                                 "Azul" = "blue",
                                                 "Rojo" = "red",
                                                 "Verde" = "green"))
                                  ), #Cierre column1 fluidRow Grafos ----
                           column(width = 3,
                                  selectInput("forma","Forma:",
                                              choices = c("Círculo" = "circle",
                                                "Cuadrado" = "square",
                                                "Triángulo" = "triangle"))
                                  ), #Cierre column2 fluidRow Grafos ----
                           column(width = 3,
                                  selectInput("direccion","Dirección:",
                                                          choices = c("to","from","No dirigido" = "NULL"),
                                                          selected = "NULL")
                           
                                  ), #Cierre column3 fluidRow Grafos ----
                         ), #Cierre fluidRow Grafos ----
                         mainPanel(
                             visNetworkOutput("network_proxy_nodes", height = "400px")
                           ) #Cierre mainPanel Grafos ----
                         ), #Cierre tabPanel Grafos ----
                
                #tabPanel: Regristro ---
                tabPanel("Registro",icon=icon("fas fa-table"),
                         sidebarPanel(width=3,
                               textInput("Nombre", "Nombre Cliente", ""),
                               textInput("Nombre_restaurante", "Nombre restaurante", ""),
                               textInput("Localidad", "Localidad", ""),
                               sliderInput("Puntuación_comida", "Valoración comida",
                                           0, 10,value=0,ticks = FALSE),
                               sliderInput("Puntuación_servicio", "Valoración servicio",
                                           0, 10,value=0,ticks = FALSE),
                               sliderInput("Puntuación_local", "Valoración local",
                                           0, 10,value=0, ticks = FALSE),
                               selectInput("Puntuación_precio","Valoración precio",
                                           choices=c("Alto","Bajo","Medio")),
                               tags$hr(),
                               actionButton("submit", "Enviar"),
                               actionButton("borrar","Borrar"),
                               actionButton("limpiar", "Limpiar inputs")
                         ), #Cierre sidebarPanel Registro ----
                         mainPanel(width = 9,
                           DT::dataTableOutput("responses")
                         ) #Cierre mainPanel Registro ----
                         ) #Cierre tabPanel Registro ---
    ) #Cierre navlistPanel ----
)#Cierre fluidPage ----


server <- function(input, output, session) {

    Gdatos <- shiny::reactiveValues()
   # ObserveEvent: input$SelDataset ----
    observeEvent(input$SelDataset, {
        if (input$SelDataset==GDatasets[1]) {
            Gdatos$datos = read.csv(paste("data/",GDatasets[1],sep=""))
            Gdatos$Nombres = c("Id","TVPubl","RadioPubl","PeriodicosPubl","Ventas")
            Gdatos$var_selec = c(2:5)
            names(Gdatos$datos) = Gdatos$Nombres
        }

        if (input$SelDataset==GDatasets[2]){
            Gdatos$datos = read.csv2(paste("data/",GDatasets[2],sep=""))
            Gdatos$Nombres = c("X","ano.inicio","edad","estado.civil","raza","nivel.educacion",
                               "tipo.trabajo","salud","seguro.medico","log.salario","salario")
            Gdatos$var_selec=c(2:11)
        }

        if (input$SelDataset==GDatasets[3]){
            Gdatos$datos = read.csv(paste("data/",GDatasets[3],sep=""))
            Gdatos$Nombres = c("X","precio","velocidad","hd","ram","pantalla","cd","multi",
                               "buenfabricante","numprecios","numes.ene1993")
            Gdatos$var_selec=c(2:11)
        }
        
      
        shiny::updateSelectInput(session,"Selvariable01Uni","Selecciona variable",
                                 choices = Gdatos$Nombres[Gdatos$var_selec],
                                 selected = Gdatos$Nombres[Gdatos$var_selec[1]])
        shiny::updateSelectInput(session,"SelvariableDepY","Selecciona variable dependiente (Y)",
                                 choices = Gdatos$Nombres[Gdatos$var_selec],
                                 selected = Gdatos$Nombres[Gdatos$var_selec[1]])
        shiny::updateSelectInput(session,"SelvariableIndepX","Selecciona variable independiente (X)",
                                 choices = Gdatos$Nombres[Gdatos$var_selec],
                                 selected = Gdatos$Nombres[Gdatos$var_selec[1]])

    })
 
    
    # Salida tabla de datos ----
   
    output$tbl <- DT::renderDataTable(Gdatos$datos,filter="top",
                                      style="bootstrap",
                                      extensions = c('Buttons','ColReorder','Responsive'),
                                      options = list(pageLength = 15, autoWidth = TRUE,
                                                     dom = 'Bfrtip',
                                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                     colReorder = TRUE,
                                                     search = list(regex = TRUE),
                                                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                                     ),
                                      class="cell-border stripe compact"
                                      )
    
    # Salida Resumen Numérico ---
    
    output$OUTResNum <- renderPrint({
        varUni <- input$Selvariable01Uni
        cat(paste("Variable = ",varUni))
        cat("\n")
        cat("\n")
        summary(Gdatos$datos[,varUni])
    })


    
    #Salida Gráfico Unidimensional-Histograma ----
    
   observeEvent(input$Selvariable01Uni,{
        varUni <- input$Selvariable01Uni
        varX <- Gdatos$datos[,varUni]
        if ( !is.factor(varX) ) {
            output$plotHist <- renderPlotly({
                histggplot <- ggplot(data = Gdatos$datos, aes(varX)) +
                    geom_histogram(fill = "blue",bins = 10,col = "white") +
                    labs(title = paste("Histograma de ",varUni), x = varUni, y = "count")
                ggplotly(histggplot)
           })
            output$avisoplotHist <- renderPrint({
                cat('El histograma se ha representado correctamente.')
            })
        } else {
            output$plotHist <- renderPlotly({
            })
            
            output$avisoplotHist <- renderPrint({
                cat("¡Estás seleccionando una variable categórica. Representa mejor un diagrama de barras!")
            })
        }
    })
    
    #Salida Gráfico Unidimensional-Diagrama de barras ----
    
    observeEvent(input$Selvariable01Uni,{
        varUni <- input$Selvariable01Uni
        varX <- Gdatos$datos[,varUni]
        if ( is.factor(varX) ) {
            output$plotDiagBar <- renderPlotly({
                barggplot <- ggplot(data = Gdatos$datos, aes(varX)) +
                    geom_bar(fill = "seagreen4", col = "white") +
                    labs(title = paste("Diagrama de Barras de ",varUni), x = varUni)
                ggplotly(barggplot)
            })
            output$avisoplotDiagBar <- renderPrint({
                cat('El diagrama de barras se ha representado correctamente.')
            })
            

        } else {
            output$plotDiagBar <- renderPlotly({
            })
            
            output$avisoplotDiagBar <- renderPrint({
                cat("¡Estás seleccionando una variable numérica. Representa mejor un histograma!")
            })
        }
    })

    #Salida Gráfico Unidimensional-Boxplot ----
    
    observeEvent(input$Selvariable01Uni,{
        varUni <- input$Selvariable01Uni
        varX <- Gdatos$datos[,varUni]
        if ( !is.factor(varX) ) {
            output$plotboxplot <- renderPlotly({
                boxplotggplot <- ggplot(data = Gdatos$datos, aes(1,varX)) +
                                        geom_boxplot(fill = "light blue") + coord_flip()+
                                        scale_x_continuous(breaks = NULL)+
                                        labs(x = "",y = varUni)
                ggplotly(boxplotggplot)
            })
            output$avisoplotboxplot <- renderPrint({
                cat('El gráfico boxplot se ha representado correctamente.')
            })
            
            
        } else {
            output$plotboxplot <- renderPlotly({
            })
            
            output$avisoplotboxplot <- renderPrint({
                cat("¡Estás seleccionando una variable categórica!")
            })
        }
    })
    
    #Cambio de variables independientes y dependientes ----
    
    observeEvent(input$Invertir,
                 {updateNumericInput(session, "SelvariableDepY", value = input$SelvariableIndepX)
                 updateNumericInput(session, "SelvariableIndepX", value = input$SelvariableDepY)})

    
    #Salida Modelo Lineal tras activar el botón ----
    
    modelo_lineal <- eventReactive(input$BotonCal,{
        
        cualY = which(Gdatos$Nombres %in%  input$SelvariableDepY)
        if (is.factor(Gdatos$datos[,cualY])){
            stop("An error has occurred. Check your logs or contact the app author for clarification.")
        }
        else{
        cualX = which(Gdatos$Nombres %in%  input$SelvariableIndepX)
        df = Gdatos$datos[,c(cualY,cualX)]
        names(df) = c("varY","varX")
        summary(lm(varY~varX,data = df))
        }
    })

    reglineal <- eventReactive(input$BotonCal,{
        paste("Regresión Lineal.","Variable Y = ", input$SelvariableDepY, ", " 
              ,"Variable X = ",input$SelvariableIndepX)
    })
    output$OUTModLineal <- renderPrint({
        cat(reglineal()) 
        cat("\n")
        cat("\n")
        modelo_lineal()
    }) 
    
    #Salida Gráfico de dispersión ----
    
    grafdispersion <- eventReactive(input$BotonCal,{
        cualY = which(Gdatos$Nombres %in%  input$SelvariableDepY)
        cualX = which(Gdatos$Nombres %in%  input$SelvariableIndepX)
        df = Gdatos$datos[,c(cualY,cualX)]
        names(df) = c("varY","varX")
        if (!is.factor(Gdatos$datos[,cualX]) & !is.factor(Gdatos$datos[,cualY])){
            ggplot(df, aes(varX,varY)) +
                geom_point(size=2) +
                geom_smooth(method = "lm", se = FALSE) + #Recta de regresión 
                labs(title ="Diagrama de dispersión - Recta de regresión", 
                     x = input$SelvariableIndepX, y = input$SelvariableDepY)
        } else {
             ggplot(df, aes(varX,varY)) +
                    geom_point(size=2) +
                    labs(title ="Diagrama de dispersión", x = input$SelvariableIndepX, y = input$SelvariableDepY)
        }
    })
    output$OUTGrafDispersion <- renderPlot({
        grafdispersion()
    })

    # renderLeaflet: map -----
    
    output$map <- renderLeaflet({
        cual = which(input$SelLugar01==GLugares)
        LAT = GLatLong$Lat[cual]
        LONG = GLatLong$Long[cual]
        ZOOM=18
        lat_popups = GLatLongPopUps$Lat[cual]
        long_popups = GLatLongPopUps$Long[cual]
        content = Gcontents[cual]
        
        
        leaflet() %>%
            setView(lng=LONG, lat=LAT, zoom=ZOOM )  %>%
            addProviderTiles("OpenStreetMap.Mapnik") %>%
            addPopups(lng= long_popups,lat= lat_popups, content,
                       options = popupOptions(closeButton = FALSE)
             )
            
    })
    
    # RenderPrint: Serie Temporal ----
    
    output$OUTSerTemp <- renderPrint({
          HoltWinters((x = co2))
    })

    predicted <- reactive({
            hw <- HoltWinters(x = co2)
            predict(hw, n.ahead = input$months,
                    prediction.interval = input$IC,
                    level = 0.95)
    })
    
    # renderDygraph:co2 ----
    
    output$dygraphSerTemp1 <- renderDygraph({
        if (input$IC){
        dygraph(predicted()) %>%
            dySeries(c("lwr", "fit", "upr")) %>%
            dyOptions(drawGrid = input$showgrid, stepPlot = input$stepchart,
                      fillGraph = input$area, fillAlpha = 0.4,
                      drawPoints = input$points, pointSize = 2)
        } else {
          dygraph(predicted()) %>%
            dyOptions(drawGrid = input$showgrid, stepPlot = input$stepchart,
                      fillGraph = input$area, fillAlpha = 0.4,
                      drawPoints = input$points, pointSize = 2)
        }
    })

     #renderDygraph2: lungDeaths ----
    
    output$dygraphSerTemp2 <- renderDygraph({
          dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
            dyRangeSelector() %>%
            dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),
                      stackedGraph = TRUE) %>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
    })
    
    # renderVisNetwork : Grafos ----
    
    output$network_proxy_nodes <- renderVisNetwork({
      visNetwork(nodes, edges) %>% visNodes(color = "white",shadow = TRUE) %>%
        visOptions(manipulation = TRUE) %>%
        visInteraction(navigationButtons = TRUE)
    })
    
    observe({
      visNetworkProxy("network_proxy_nodes") %>%
        visNodes(color = input$color,
                 shape = input$forma)
    })
    
    observe({
      visNetworkProxy("network_proxy_nodes") %>%
        visEdges(arrows=input$direccion) 
    })
    

    #Registros ----
  
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    
    loadData <- reactiveValues()
    
    observeEvent(input$borrar,
                   if (exists("responses")) {
                     responses <<- responses [-c(dim(responses)[1]),]
                     loadData$data <- responses
                   }
                 )
    
    observeEvent(input$submit,
                 if (exists("responses")) {
                     loadData$data <- responses
                   }
                 )
    
    output$responses <- DT::renderDataTable({
      loadData$data
    }, style="bootstrap",
       options = list(pageLength = 15, autoWidth = TRUE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
       class="cell-border stripe compact"
    )

    
    #Limpieza inputs---
    
    observe({input$limpiar
                 updateTextInput(session, "name", value = "")
                 updateTextInput(session, "namerest", value = "")
                 updateTextInput(session, "localidad", value = "")
                 updateNumericInput(session, "punt_comida", value = 0)
                 updateNumericInput(session, "punt_servicio", value = 0)
                 updateNumericInput(session, "punt_local", value = 0)
                 updateVarSelectInput(session,"punt_precio",selected = "Alto")})
    


}

# Run the application 
shinyApp(ui = ui, server = server)

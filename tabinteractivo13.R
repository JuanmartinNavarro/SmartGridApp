library(shiny)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(shinyBS)
library(ggplot2)

# load("nwt_locations.RData")
# n<-0
#i<<-1

# lon <- -119.25
# lat <- 69.333

# @knitr ui01
ui <- fluidPage(mainPanel(
  tabsetPanel(id = "inTabset",
              tabPanel(title = "Registro",  value = "panel1",

                       bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                     leafletOutput("Mapclientes"),

                                     absolutePanel(top = 10, right = -300,
                                                   numericInput("numcliente_reg", "Numero de Cliente",10000),
                                                   numericInput("Latitud_reg", "Latitud",37.29204),
                                                   numericInput("Longitud_reg", "Longitud",-6.382060),
                                                   textInput("text", label = h3("Localidad"), value = ""),
                                                   radioButtons("radio_reg", label = h3("Tipo de Cliente"),
                                                                choices = list("Vivienda" = 1, "Negocio" = 2
                                                                ),selected = ""),
                                                   conditionalPanel(condition="(input.radio_reg == 2)",
                                                                    numericInput("CNAE", "CNAE",9820)),
                                                   checkboxInput("check_con", label = "Consumidor", value = FALSE),
                                                   conditionalPanel(condition="(input.check_con)",
                                                                    numericInput("pot_reg1", "Potencia Contratada en KW",5)),
                                                   checkboxInput("check_gen", label = "Generacion", value = FALSE),
                                                   conditionalPanel(condition="(input.check_gen)",
                                                                    numericInput("pot_reg2", "Potencia Dispositivo Generador en KW",5)),
                                                   checkboxInput("check_alm", label = "Almacenamiento", value = FALSE),
                                                   conditionalPanel(condition="(input.check_alm)",
                                                                    selectInput("bateria_reg", "Baterías disponibles", c("", Bateria$Nombre), selected="")
                                                   ),
                                                   checkboxInput("check_coche", label = "Coche Electrico", value = FALSE),
                                                   conditionalPanel(condition="(input.check_coche)",
                                                                    selectInput("coche", "Coches Electricos disponibles", c("", Coches_Electricos$Nombre), selected="")
                                                   ),

                                                   actionButton("Registrar", label = "Registrar"),
                                                   actionButton("clientes", label = "Ver Mapa")
                                     )
                       )
              ),
              tabPanel(title = "Mercado",  value = "panel2",

                       bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                     # verbatimTextOutput("txt"),
                                     htmlOutput("text2"),
                                     leafletOutput("Map"),

                                     absolutePanel(top = 10, right = -300,
                                                   selectInput("numcliente_cons", "Numero de Cliente del Usuario", c("", datosclientes$num_cliente), selected=""),
                                                   radioButtons("radio", label = h3("Crear Nuevo Intercambio"),
                                                                choices = list("Vender Energia" = 1, "Comprar Energia" = 2
                                                                ),selected = ""),
                                                   conditionalPanel(condition="(input.radio == 2)||(input.radio == 1)",

                                                                    actionButton("Intercambio", label = "Intercambio")),

                                                   dateInput("date_mercado",
                                                             label = h3("Date input"),
                                                             value = "2017-01-01"),
                                                   selectInput("select", label = h3("Hora"),
                                                               choices = list("1:00" = 1, "2:00" = 2,
                                                                              "3:00" = 3, "4:00" = 4,
                                                                              "5:00" = 5,"6:00" = 6,
                                                                              "7:00" = 7,"8:00" = 8,
                                                                              "9:00" = 9,"10:00" = 10,
                                                                              "11:00" = 11,"12:00" = 12,
                                                                              "13:00" = 13,"14:00" = 14,
                                                                              "15:00" = 15,"16:00" = 16,
                                                                              "17:00" = 17,"18:00" = 18,
                                                                              "19:00" = 19,"20:00" = 20,
                                                                              "21:00" = 21,"22:00" = 22,
                                                                              "23:00" = 23,"24:00" = 24,
                                                                              "Dia Completo" = 25), selected = 1),
                                                   numericInput("inter", "Energia Intercambiada",500),
                                                   selectInput("numcliente", "Numero Cliente Seleccionado", c("", datosclientes$num_cliente), selected=""),
                                                   conditionalPanel("input.numcliente !== null && input.numcliente !== ''",
                                                                    actionButton("button_plot_and_table", "Energia Ofertada en un dia", class="btn-block")),
                                                   conditionalPanel("input.numcliente !== null && input.numcliente !== ''",
                                                                    actionButton("button_plot_and_table2", "Energia Demandada en un dia", class="btn-block")),


                                                   numericInput("preciomer", "Precio (Euros)",0),
                                                   actionButton("Ver", label = "Ver"),
                                                   actionButton("Mercado", label = "Mercado")

                                     ),
                                     bsModal("Plot_and_table", "Energia ofertada", "button_plot_and_table", size = "large",
                                             plotOutput("TestPlot"),
                                             dataTableOutput("TestTable")
                                     ),
                                     bsModal("Plot_and_table2", "Energia demandada", "button_plot_and_table2", size = "large",
                                             plotOutput("TestPlot2"),
                                             dataTableOutput("TestTable2")
                                     )
                       )
              ),
              tabPanel(title = "Puntual", value = "panel3",
                       mainPanel( tabsetPanel(
                         tabPanel("Generacion",
                                  bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                                plotOutput("plot"),
                                                absolutePanel(top = 10, right = -500,

                                                              selectInput("numcliente2", "Numero Cliente Seleccionado", c("", datosclientes$num_cliente), selected=""),

                                                              # numericInput("pot", "Potencia en KW",5),
                                                              numericInput("precio", "Precio (Euros)",0),
                                                              dateInput("date2",
                                                                        label = h3("Date input"),
                                                                        value = "2017-01-01"),
                                                              actionButton("fotovoltaica", "Fotovoltaica"),
                                                              actionButton("eolica", "Eolica")
                                                ))),
                         tabPanel("Consumo",
                                  bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                                plotOutput("plot2"),
                                                sliderInput("range", "Rango:",
                                                            min = 1, max = 24, value = c(2,5)),
                                                absolutePanel(top = 10, right = -500,
                                                              selectInput("numcliente3", "Numero Cliente Seleccionado", c("", datosclientes$num_cliente), selected=""),
                                                              numericInput("energia", "Energía requerida (W)",1000),
                                                              numericInput("preciodem", "Precio (Euros)",0),
                                                              dateInput("date3",
                                                                        label = h3("Date input"),
                                                                        value = "2017-01-01"),
                                                              actionButton("Consumo", "Crear nuevo consumo")
                                                )
                                  )),

                         tabPanel("Almacenamiento",
                                  bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                                plotOutput("plot3"),
                                                sliderInput("range2", "Rango:",
                                                            min = 1, max = 24, value = c(2,5)),
                                                absolutePanel(top = 10, right = -500,
                                                              selectInput("numcliente4", "Numero Cliente Seleccionado", c("", datosclientes$num_cliente), selected=""),
                                                              numericInput("porcentaje", "Porcentaje de Energía disponible (%)",50),
                                                              numericInput("precioalm1", "Precio Demandado(Euros)",0),
                                                              numericInput("precioalm2", "Precio Ofertado(Euros)",0),
                                                              dateInput("date4",
                                                                        label = h3("Date input"),
                                                                        value = "2017-01-01"),
                                                              actionButton("bateria", "Bateria Domestica")
                                                              # actionButton("Coche", "Coche Electrico")

                                                ))
                         )
                       ))
              ),
              tabPanel(title = "Periodo", value = "panel4",
                       mainPanel( tabsetPanel(

                         tabPanel("Consumo",
                                  bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                                plotOutput("plot_sem2"),
                                                absolutePanel(top = -40, right = -700,
                                                              radioButtons("radio_sem2", label = h3("Tipo de Familia"),
                                                                           choices = list("Numerosa" = 1, "Unifamiliar" = 2,"Negocio Oficina" = 3, "Negocio Restauracion" = 4
                                                                           ),selected = ""),
                                                              checkboxInput("Lavadora", label = "Lavadora", value = FALSE),
                                                              checkboxInput("Cocina", label = "Cocina Electrica", value = FALSE),
                                                              checkboxInput("Calentador", label = "Calentador Agua Electrico", value = FALSE),
                                                              checkboxInput("Calefaccion", label = "Calefaccion Electrica", value = FALSE),
                                                              checkboxInput("Ac", label = "Aire Acondicionado", value = FALSE),
                                                              conditionalPanel(condition="(input.Lavadora)",
                                                                               numericInput("pot_sem21", "Potencia de Lavadora en KW",1)
                                                              ),
                                                              conditionalPanel(condition="(input.Cocina)",
                                                                               numericInput("pot_sem22", "Potencia de Cocina en KW",1)
                                                              ),
                                                              conditionalPanel(condition="(input.Calentador)",
                                                                               numericInput("pot_sem23", "Potencia de Calentador en KW",1)
                                                              ),
                                                              conditionalPanel(condition="(input.Calefaccion)",
                                                                               numericInput("pot_sem24", "Potencia de Calefaccion en KW",1)
                                                              ),
                                                              conditionalPanel(condition="(input.Ac)",
                                                                               numericInput("pot_sem25", "Potencia de Aire Acondicionado en KW",1)
                                                              ),
                                                              selectInput("num_cliente_sem2", "Numero Cliente", c("", datosclientes$num_cliente), selected=""),
                                                              dateRangeInput("dates_sem2", label = h3("Intervalo fechas")),
                                                              numericInput("preciocon", "Precio Demandado(Euros)",0),
                                                              actionButton("Consumo_sem", "Crear nuevo consumo")
                                                )
                                  )),
                         tabPanel("Generacion",
                                  bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                                plotOutput("plot_sem1"),
                                                absolutePanel(top = -40, right = -700,
                                                              checkboxInput("check_consu", label = "Consumidor", value = FALSE),
                                                              radioButtons("radio_sem1", label = h4("Tipo Dispositivo Generador"),
                                                                           choices = list("Fotovoltaica" = 1, "Eolico" = 2
                                                                           ),selected = ""),
                                                              conditionalPanel(condition="(input.check_consu)",
                                                                               radioButtons("radio_sem11", label = h4("Tipo de Consumidor"),
                                                                                            choices = list("Familia Numerosa" = 1, "Unifamiliar" = 2,"Negocio Oficina" = 3, "Negocio Restauracion" = 4
                                                                                            ),selected = "")),
                                                              dateRangeInput("dates_sem1", label = h4("Intervalo fechas")),
                                                              selectInput("num_cliente_sem1", "Numero Cliente", c("", datosclientes$num_cliente), selected=""),
                                                              conditionalPanel(condition="(input.check_consu)",
                                                                               numericInput("preciogen1", "Precio Demandado(Euros)",0)),
                                                              numericInput("preciogen2", "Precio Ofertado(Euros)",0),
                                                              actionButton("Generacion_sem", "Crear Oferta/demanda")
                                                ))),

                         tabPanel("Generacion y Almacenamiento",
                                  bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                                plotOutput("plot_sem3"),
                                                plotOutput("plot_sem3a"),
                                                # sliderInput("range_sem3", "Rango:",
                                                #             min = 1, max = 24, value = c(2,5)),
                                                absolutePanel(top = -40, right = -700,
                                                              checkboxInput("check_consu2", label = "Consumidor", value = FALSE),
                                                              checkboxInput("check_gen2", label = "Generador", value = FALSE),
                                                              conditionalPanel(condition="(input.check_gen2)",
                                                                               radioButtons("radio_sem3", label = h4("Tipo Dispositivo Generador"),
                                                                                            choices = list("Fotovoltaica" = 1, "Eolico" = 2
                                                                                            ),selected = "")),
                                                              conditionalPanel(condition="(input.check_consu2)",
                                                                               radioButtons("radio_sem31", label = h4("Tipo de Consumidor"),
                                                                                            choices = list("Familia Numerosa" = 1, "Unifamiliar" = 2,"Negocio Oficina" = 3, "Negocio Restauracion" = 4
                                                                                            ),selected = "")),
                                                              selectInput("num_cliente_sem3", "Numero Cliente", c("", datosclientes$num_cliente), selected=""),

                                                              dateRangeInput("dates_sem3", label = h3("Intervalo fechas")),
                                                              conditionalPanel(condition="(input.check_consu2)",
                                                                               numericInput("precioalmcon", "Precio Demandado(Euros)",0)),
                                                              conditionalPanel(condition="(input.check_gen2)",
                                                                               numericInput("precioalmgen", "Precio Ofertado(Euros)",0)),
                                                              numericInput("estado", "Estado inicial Bateria (%)",100),
                                                              actionButton("almac_sem", "Crear nuevo consumo")
                                                              # actionButton("Coche", "Coche Electrico")

                                                ))
                         ),
                         tabPanel("Generacion y Coche Electrico",
                                  bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                                # plotOutput("plot_sem3"),
                                                # plotOutput("plot_sem3a"),
                                                # sliderInput("range_sem3", "Rango:",
                                                #             min = 1, max = 24, value = c(2,5)),
                                                absolutePanel(top = -40, right = -700,
                                                              checkboxInput("check_consu3", label = "Consumidor", value = FALSE),
                                                              checkboxInput("check_gen3", label = "Generador", value = FALSE),
                                                              conditionalPanel(condition="(input.check_gen3)",
                                                                               radioButtons("radio_sem4", label = h4("Tipo Dispositivo Generador"),
                                                                                            choices = list("Fotovoltaica" = 1, "Eolico" = 2
                                                                                            ),selected = "")),
                                                              conditionalPanel(condition="(input.check_consu3)",
                                                                               radioButtons("radio_sem41", label = h4("Tipo de Consumidor"),
                                                                                            choices = list("Familia Numerosa" = 1, "Unifamiliar" = 2,"Negocio Oficina" = 3, "Negocio Restauracion" = 4
                                                                                            ),selected = "")),
                                                              selectInput("num_cliente_sem4", "Numero Cliente", c("", datosclientes$num_cliente), selected=""),

                                                              dateRangeInput("dates_sem4", label = h3("Intervalo fechas")),
                                                              selectInput("tipoconector", "Tipo de Conector", c("", Conectores$Tipo_Conector), selected=""),

                                                              conditionalPanel(condition="(input.check_consu3)",
                                                                               numericInput("precioalmcon2", "Precio Demandado(Euros)",0)),
                                                              conditionalPanel(condition="(input.check_gen3)",
                                                                               numericInput("precioalmgen2", "Precio Ofertado(Euros)",0)),
                                                              numericInput("estado2", "Estado salida Bateria (%)",100),
                                                              selectInput("selectsalida", label = h3("Hora Salida"),
                                                                          choices = list("1:00" = 1, "2:00" = 2,
                                                                                         "3:00" = 3, "4:00" = 4,
                                                                                         "5:00" = 5,"6:00" = 6,
                                                                                         "7:00" = 7,"8:00" = 8,
                                                                                         "9:00" = 9,"10:00" = 10,
                                                                                         "11:00" = 11,"12:00" = 12,
                                                                                         "13:00" = 13,"14:00" = 14,
                                                                                         "15:00" = 15,"16:00" = 16,
                                                                                         "17:00" = 17,"18:00" = 18,
                                                                                         "19:00" = 19,"20:00" = 20,
                                                                                         "21:00" = 21,"22:00" = 22,
                                                                                         "23:00" = 23,"24:00" = 24), selected = 1),
                                                              numericInput("estado3", "Estado llegada Bateria (%)",100),
                                                              selectInput("selectllegada", label = h3("Hora Llegada"),
                                                                          choices = list("1:00" = 1, "2:00" = 2,
                                                                                         "3:00" = 3, "4:00" = 4,
                                                                                         "5:00" = 5,"6:00" = 6,
                                                                                         "7:00" = 7,"8:00" = 8,
                                                                                         "9:00" = 9,"10:00" = 10,
                                                                                         "11:00" = 11,"12:00" = 12,
                                                                                         "13:00" = 13,"14:00" = 14,
                                                                                         "15:00" = 15,"16:00" = 16,
                                                                                         "17:00" = 17,"18:00" = 18,
                                                                                         "19:00" = 19,"20:00" = 20,
                                                                                         "21:00" = 21,"22:00" = 22,
                                                                                         "23:00" = 23,"24:00" = 24), selected = 1),

                                                              actionButton("almac_sem2", "Crear nuevo consumo")
                                                              # actionButton("Coche", "Coche Electrico")

                                                ))
                         )
                       ))
              ),


              tabPanel(title = "Historico",  value = "panel5",

                       bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                     # leafletOutput("mapa"),
                                     plotOutput("plot_his"),
                                     absolutePanel(top = 10, right = -300,
                                                   selectInput("numcliente_his", "Numero Cliente", c("", datosclientes$num_cliente), selected=""),
                                                   dateRangeInput("dates_his", label = h3("Intervalo fechas")),
                                                   numericInput("preciohisdem", "Precio Demandado(Euros)",0),
                                                   numericInput("preciohisofe", "Precio Ofertado(Euros)",0),
                                                   actionButton("mostrar", label = "Mostrar"),
                                                   actionButton("guardar", label = "Guardar")
                                     )
                       )),
              tabPanel(title = "Registro Mercado",  value = "panel6",

                       bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                     # leafletOutput("mapa"),
                                     dataTableOutput("tabla"),
                                     htmlOutput("text"),
                                     plotOutput("plotenergiames"),
                                     plotOutput("plotsaldo"),
                                     plotOutput("plotsaldo2"),
                                     absolutePanel(top = 10, right = -300,
                                                   selectInput("numcliente_reg", "Numero Cliente", c("", datosclientes$num_cliente), selected=""),
                                                   # dateRangeInput("dates_his", label = h3("Intervalo fechas")),
                                                   actionButton("mostrar2", label = "Mostrar")
                                                   # actionButton("guardar", label = "Guardar")
                                     )
                       )),
              tabPanel(title = "Mapa Historicos",  value = "panel7",

                       bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                     leafletOutput("mapa"),

                                     absolutePanel(top = 10, right = -300,
                                                   selectInput("colors", "Color Scheme",
                                                               rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                                   ),

                                                   dateInput("date",
                                                             label = h3("Date input"),
                                                             value = "2014-12-15"),
                                                   selectInput("select", label = h3("Hora"),
                                                               choices = list("1:00" = 1, "2:00" = 2,
                                                                              "3:00" = 3, "4:00" = 4,
                                                                              "5:00" = 5,"6:00" = 6,
                                                                              "7:00" = 7,"8:00" = 8,
                                                                              "9:00" = 9,"10:00" = 10,
                                                                              "11:00" = 11,"12:00" = 12,
                                                                              "13:00" = 13,"14:00" = 14,
                                                                              "15:00" = 15,"16:00" = 16,
                                                                              "17:00" = 17,"18:00" = 18,
                                                                              "19:00" = 19,"20:00" = 20,
                                                                              "21:00" = 21,"22:00" = 22,
                                                                              "23:00" = 23,"24:00" = 24,
                                                                              "Dia Completo" = 25), selected = 1),
                                                   checkboxInput("legend", "Show legend", TRUE),
                                                   actionButton("action", label = "Action")
                                     )
                       ))
  )
)
)


# @knitr server01
server <- function(input, output, session) {
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")

  output$Map <- renderLeaflet({
    leaflet(datosclientes) %>% setView(Longitud, Latitud, 4) %>% addTiles() %>%
      fitBounds(~min(Longitud), ~min(Latitud), ~max(Longitud), ~max(Latitud))
  })

  output$Mapclientes <- renderLeaflet({
    leaflet(datosclientes) %>% setView(Longitud, Latitud, 4) %>% addTiles() %>%
      fitBounds(~min(Longitud), ~min(Latitud), ~max(Longitud), ~max(Latitud))
  })

  filteredData <- reactive({
    pruebamapa[pruebamapa$Fecha == input$date,]
  })


  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, hora)
  })

  output$text2 <- renderUI({
    if (is.null(vprecio$data)) return()
    str1 <- paste("Fecha", input$date_mercado)
    str2 <- paste("Hora",input$select)
    str3 <- paste("Precio",vprecio$data)
    HTML(paste(str1, str2, str3, sep = '<br/>'))

  })

  output$mapa <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(pruebamapa) %>% addTiles() %>%
      fitBounds(~min(Longitud), ~min(Latitud), ~max(Longitud), ~max(Latitud))
  })


  # @knitr server01observer

  # @knitr server01remainder
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
      # proxy %>% removeMarker(layerId="Lineas")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })

  observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$numcliente) || input$numcliente!=p$id) updateSelectInput(session, "numcliente", selected=p$id)
    }
  })

  observeEvent(input$numcliente, { # update the map markers and view on location selectInput changes
    p <- input$Map_marker_click
    p2 <- subset(datosclientes, num_cliente==input$numcliente)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
      # proxy %>% removeMarker(layerId="Lineas")
    } else if(length(p$id) && input$numcliente!=p$id){
      proxy %>% setView(lng=p2$Longitud, lat=p2$Latitud, input$Map_zoom) %>% acm_defaults(p2$Longitud, p2$Latitud)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$Longitud, lat=p2$Latitud, input$Map_zoom) %>% acm_defaults(p2$Longitud, p2$Latitud)
    }
  })

  observeEvent(input$Intercambio,{
    if (input$select!=25) {
      tam<-dim(mercadomapa)
      ult<-tam[1]
      ult<-ult+1

      Fecha<-input$date_mercado
      indice<-as.numeric(input$select)
      cantidad<-input$inter
      CH<-data.frame(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
      CH[1,indice]<-cantidad
      if(input$radio == 1){
        numcliente<-as.integer(input$numcliente_cons)
        numcliente_cons<-as.integer(input$numcliente)
        longitude<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,5]
        latitude<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,4]
        Longitud<-datosclientes[datosclientes$num_cliente==input$numcliente,5]
        Latitud<-datosclientes[datosclientes$num_cliente==input$numcliente,4]
        cantidadofe<-oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente_cons),6+indice]
        cantidaddem<-demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente),6+indice]
        oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente_cons),6+indice]<<-cantidadofe-cantidad
        demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente),6+indice]<<-cantidaddem-cantidad
      }
      if(input$radio == 2){
        numcliente<-as.integer(input$numcliente)
        numcliente_cons<-as.integer(input$numcliente_cons)
        longitude<-datosclientes[datosclientes$num_cliente==input$numcliente,5]
        latitude<-datosclientes[datosclientes$num_cliente==input$numcliente,4]
        Longitud<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,5]
        Latitud<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,4]
        cantidadofe<-oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente),6+indice]
        cantidaddem<-demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente_cons),6+indice]
        oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente),6+indice]<<-cantidadofe-cantidad
        demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente_cons),6+indice]<<-cantidaddem-cantidad
      }
      mercadomapa[ult,]<<-data.frame(numcliente,latitude,longitude,numcliente_cons,Latitud,Longitud,Fecha,input$preciomer,CH)

    }else{
      Fecha<-as.Date(input$date_mercado)
      CH<-data.frame(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
      if(input$radio == 1){
        numcliente<-as.integer(input$numcliente_cons)
        numcliente_cons<-as.integer(input$numcliente)
        longitude<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,5]
        latitude<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,4]
        Longitud<-datosclientes[datosclientes$num_cliente==input$numcliente,5]
        Latitud<-datosclientes[datosclientes$num_cliente==input$numcliente,4]
        for (i in 1:24) {
          cantidadofe<-oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente_cons),6+i]
          cantidaddem<-demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente),6+i]
          resta<-cantidadofe-cantidaddem
          if (resta>0) {
            cantidad<-cantidaddem
            oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente_cons),6+i]<<-resta
            demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente),6+i]<<-0
          }
          if (resta<=0) {
            cantidad<-cantidadofe
            oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente_cons),6+i]<<-0
            demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente),6+i]<<-(resta)*(-1)
          }
          CH[1,i]<-cantidad
        }
      }
      if(input$radio == 2){
        numcliente<-as.numeric(input$numcliente)
        numcliente_cons<-as.numeric(input$numcliente_cons)
        longitude<-datosclientes[datosclientes$num_cliente==input$numcliente,5]
        latitude<-datosclientes[datosclientes$num_cliente==input$numcliente,4]
        Longitud<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,5]
        Latitud<-datosclientes[datosclientes$num_cliente==input$numcliente_cons,4]
        for (i in 1:24) {
          cantidadofe<-oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente),6+i]
          cantidaddem<-demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente_cons),6+i]
          resta<-cantidadofe-cantidaddem
          if (resta>0) {
            cantidad<-cantidaddem
            oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente),6+i]<<-resta
            demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente_cons),6+i]<<-0
          }
          if (resta<=0) {
            cantidad<-cantidadofe
            oferta[(oferta$Fecha==Fecha)&(oferta$IDCliente==numcliente),6+i]<<-0
            demanda[(demanda$Fecha==Fecha)&(demanda$IDCliente==numcliente_cons),6+i]<<-(resta)*(-1)
          }
          CH[1,i]<-cantidad
        }
      }
      tam<-dim(mercadomapa)
      ult<-tam[1]
      ult<-ult+1
      mercadomapa[ult,]<<-data.frame(numcliente,latitude,longitude,numcliente_cons,Latitud,Longitud,Fecha,input$preciomer,CH)


    }

  })

  Data <- reactive({

    oferta %>% filter(IDCliente==input$numcliente) %>% filter(Fecha==input$date_mercado)

  })

  output$TestPlot <- renderPlot({

    barplot(as.matrix(Data()[,7:30]),main=input$numcliente,
            ylab="Energia ofertada",
            xlab="Hora", col="blue",xlim=c(0,25))

  })

  output$TestTable <- renderDataTable({
    Data()
  }, options = list(pageLength=5))

  Data2 <- reactive({

    demanda %>% filter(IDCliente==input$numcliente) %>% filter(Fecha==input$date_mercado)

  })

  output$TestPlot2 <- renderPlot({
    barplot(as.matrix(Data2()[,7:30]),main=input$numcliente,
            ylab="Energia demandada",
            xlab="Hora", col="blue",xlim=c(0,25))
  })

  output$TestTable2 <- renderDataTable({
    Data2()
  }, options = list(pageLength=5))

  observeEvent(input$Ver,{
    fecha<-input$date_mercado

    gener[1,1]<-0
    gener[1,2]<-0
    gener[1,3]<-0
    gener[1,4]<-0
    consu[1,1]<-0
    consu[1,2]<-0
    consu[1,3]<-0
    consu[1,4]<-0
    # almac[1,1]<-0
    # almac[1,2]<-0
    # almac[1,3]<-0
    # almac[1,4]<-0

    if (input$select!=25) {

      PrecioHIS<-PrecioHIS[PrecioHIS$Fecha==fecha,]
      vprecio$data<-PrecioHIS[PrecioHIS$Hora==input$select,"Precio"]

      j<-0
      fil_gener<-oferta[,6+as.numeric(input$select)]
      ind_gen<-which(as.logical(fil_gener))
      tam_gen<-length(ind_gen)
      for (i in 1:tam_gen) {
        if(oferta[ind_gen[i],"Fecha"]==fecha){
          j<-j+1
          gener[j,]<-oferta[ind_gen[i],]
          energia_gener[j]<-oferta[ind_gen[i],6+as.numeric(input$select)]
        }
      }
      j<-0
      fil_consu<-demanda[,6+as.numeric(input$select)]
      ind_con<-which(as.logical(fil_consu))
      tam_con<-length(ind_con)
      for (i in 1:tam_con) {
        if(demanda[ind_con[i],"Fecha"]==fecha){
          j<-j+1
          consu[j,]<-demanda[ind_con[i],]
          energia_consu[j]<-demanda[ind_con[i],6+as.numeric(input$select)]
        }
      }
      j<-0
      fil_merca<-mercadomapa[,8+as.numeric(input$select)]
      ind_merca<-which(as.logical(fil_merca))
      tam_merca<-length(ind_merca)
      if (tam_merca!=0) {
        for (i in 1:tam_merca) {
          if(mercadomapa[ind_merca[i],"Fecha"]==fecha){
            j<-j+1
            merca[j,]<-mercadomapa[ind_merca[i],]
            energia_merca[j]<-mercadomapa[ind_merca[i],8+as.numeric(input$select)]
          }
        }
      }
      total<-gener
      tam_t<-dim(total)
      n<-tam_t[1]
      tam_c<-dim(consu)
      m<-tam_c[1]
      for (i in 1:m) {
        n<-n+1
        total[n,]<-consu[i,]
      }
      # tam_t<-dim(total)
      # n<-tam_t[1]
      # tam_a<-dim(almac)
      # k<-tam_a[1]
      # for (i in 1:k) {
      #   n<-n+1
      #   total[n,]<-consu[i,]
      # }

      leafletProxy("Map", data = total) %>%
        clearShapes() %>%
        clearMarkers() %>%
        # removeShape(layerId="clientes") %>%
        removeShape(layerId="Principal") %>%
        removeShape(layerId="Lineas") %>%
        addCircleMarkers(data=consu, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"green", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~IDCliente, popup = ~paste(sep = "<br/>",
                                                                                                                                                                                       "<b><a>Numero Cliente</a></b>",
                                                                                                                                                                                       IDCliente,
                                                                                                                                                                                       "<b><a>Energia demandada</a></b>",
                                                                                                                                                                                       energia_consu,
                                                                                                                                                                                       "<b><a>Precio</a></b>",
                                                                                                                                                                                       Precio,
                                                                                                                                                                                       "<b><a>Energia Intercambiada</a></b>",
                                                                                                                                                                                       energia_merca)) %>%
        addCircleMarkers(data=gener, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"red", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~IDCliente, popup = ~paste(sep = "<br/>",
                                                                                                                                                                                     "<b><a>Numero Cliente</a></b>",
                                                                                                                                                                                     IDCliente,
                                                                                                                                                                                     "<b><a>Energia ofrecida</a></b>",
                                                                                                                                                                                     energia_gener,
                                                                                                                                                                                     "<b><a>Precio</a></b>",
                                                                                                                                                                                     Precio,
                                                                                                                                                                                     "<b><a>Energia Intercambiada</a></b>",
                                                                                                                                                                                     energia_merca) )
      # addCircleMarkers(data=almac, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"blue", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~IDCliente, popup = ~paste(IDCliente,energia_almac))
    }else{

      PrecioHIS<-PrecioHIS[PrecioHIS$Fecha==fecha,]
      mediaprecio<-mean(as.numeric(PrecioHIS$Precio))
      vprecio$data<-mediaprecio

      gener<-oferta[oferta$Fecha==fecha,]
      consu<-demanda[demanda$Fecha==fecha,]
      total<-gener
      tam_t<-dim(total)
      n<-tam_t[1]
      tam_c<-dim(consu)
      m<-tam_c[1]
      for (i in 1:m) {
        n<-n+1
        total[n,]<-consu[i,]
      }

      leafletProxy("Map", data = total) %>%
        clearShapes() %>%
        clearMarkers() %>%
        # removeShape(layerId="clientes") %>%
        removeShape(layerId="Principal") %>%
        removeShape(layerId="Lineas") %>%
        addCircleMarkers(data=consu, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"green", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~IDCliente, popup = ~paste(sep = "<br/>",
                                                                                                                                                                                       "<b><a>Numero Cliente</a></b>",
                                                                                                                                                                                       IDCliente,

                                                                                                                                                                                       "<b><a>Precio</a></b>",
                                                                                                                                                                                       Precio
        )) %>%
        addCircleMarkers(data=gener, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"red", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~IDCliente, popup = ~paste(sep = "<br/>",
                                                                                                                                                                                     "<b><a>Numero Cliente</a></b>",
                                                                                                                                                                                     IDCliente,

                                                                                                                                                                                     "<b><a>Precio</a></b>",
                                                                                                                                                                                     Precio) )


    }

  })

  observeEvent(input$clientes,{
    consu_data<-datosclientes[datosclientes$tipo_cliente==1,]
    gener_data<-datosclientes[datosclientes$tipo_cliente==2,]
    almac_data<-datosclientes[datosclientes$tipo_cliente==3,]

    leafletProxy("Mapclientes", data = datosclientes) %>%
      clearShapes() %>%
      # removeShape(layerId=~num_cliente) %>%
      # removeShape(layerId="Principal") %>%
      addCircleMarkers(data=consu_data, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"green", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~num_cliente, popup = ~paste(sep = "<br/>",
                                                                                                                                                                                            "<b><a>Numero Cliente</a></b>",
                                                                                                                                                                                            num_cliente,
                                                                                                                                                                                            "<b><a>Potencia Contratada</a></b>",
                                                                                                                                                                                            potencia_con)) %>%
      addCircleMarkers(data=gener_data, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"red", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~num_cliente, popup = ~paste(sep = "<br/>",
                                                                                                                                                                                          "<b><a>Numero Cliente</a></b>",
                                                                                                                                                                                          num_cliente,
                                                                                                                                                                                          "<b><a>Potencia Contratada</a></b>",
                                                                                                                                                                                          potencia_con,
                                                                                                                                                                                          "<b><a>Potencia Generacion</a></b>",
                                                                                                                                                                                          potencia_gen )) %>%
      addCircleMarkers(data=almac_data, radius=6,lat = ~ Latitud,lng = ~ Longitud, color= ~"blue", stroke=FALSE, fillOpacity=0.5,group="locations", layerId = ~num_cliente, popup = ~paste(sep = "<br/>",
                                                                                                                                                                                           "<b><a>Numero Cliente</a></b>",
                                                                                                                                                                                           num_cliente,
                                                                                                                                                                                           "<b><a>Potencia Contratada</a></b>",
                                                                                                                                                                                           potencia_con,
                                                                                                                                                                                           "<b><a>Potencia Generacion</a></b>",
                                                                                                                                                                                           potencia_gen,
                                                                                                                                                                                           "<b><a>Potencia Almacenamiento</a></b>",
                                                                                                                                                                                           potencia_alm ))

  })

  observeEvent(input$Mercado,{

    # pal <- colorpal()
    hora<-mercadomapa[,8+as.numeric(input$select)]
    indicesini<-which(as.logical(hora))
    tamini<-length(indicesini)
    if (tamini!=0) {

      j<-0
      for (i in 1:tamini) {
        fecha<-mercadomapa[indicesini[i] ,7]
        if(fecha==input$date_mercado){
          j<-j+1
          indices[j]<-indicesini[i]
        }

      }
      tam<-length(indices)

      for (i in 1:tam) {
        cliente_gen[i]<-mercadomapa[indices[i],1]
        cliente_con[i]<-mercadomapa[indices[i],4]
        # energia_gen[i]<-pruebamapa[(pruebamapa$Fecha == input$date)&(pruebamapa$IDCliente == cliente_gen[i]),27+as.numeric(input$select)]
        # energia_con[i]<-pruebamapa[(pruebamapa$Fecha == input$date)&(pruebamapa$IDCliente == cliente_con[i]),27+as.numeric(input$select)]
      }
      if((length(which(mercadomapa$IDCliente_Gen==input$numcliente))!=0)||(length(which(mercadomapa$IDCliente_Cons==input$numcliente))!=0)){ #is.null(input$numcliente)
        #   #Mirar si se cambian los indices al filtrar por Cliente
        for (i in 1:tam) {
          mercadom[i,]<-mercadomapa[indices[i],]
        }
        mercadom<-mercadom[(mercadom$IDCliente_Gen==input$numcliente)|(mercadom$IDCliente_Cons==input$numcliente),]
        for (i in 1:tam) {
          merca[i,]<-mercadom[i,]
          merca[i,33]<-1
        }
        # mercadomapa<-mercadomapa[indices[j],]
      } else{
        for (i in 1:tam) {
          merca[i,]<-mercadomapa[indices[i],]
          merca[i,33]<-1
        }
      }
      consumidores<-datosclientes[(datosclientes$tipo_cliente==1|datosclientes$tipo_cliente==3)&(datosclientes$num_cliente==cliente_con),]
      generadores<-datosclientes[(datosclientes$tipo_cliente==2|datosclientes$tipo_cliente==3)&(datosclientes$num_cliente==cliente_gen),]

      for(i in 1:nrow(merca)){
        if(i<2){
          leafletProxy("Map") %>%
            clearShapes() %>%
            removeShape(layerId="Lineas") %>%
            removeShape(layerId="Principal") %>%
            addPolylines(data = merca, lat = as.numeric(merca[i, c(2,5)]), lng = as.numeric(merca[i, c(3,6)]),color="red",group=~grupo ,layerId="Principal")  %>%
            addCircles(data=consumidores,lat = ~ Latitud,lng = ~ Longitud,radius = ~potencia_con,
                       color = "green", fillOpacity = 0.7) %>% #,popup = ~paste(num_cliente,Potencia,hora,energia_con)
            addCircles(data=generadores,lat = ~ Latitud,lng = ~ Longitud,radius = ~potencia_gen,
                       color = "red", fillOpacity = 0.7)#,popup = ~paste(num_cliente,Potencia,hora,energia_gen)
        }else{
          leafletProxy("Map") %>%
            # clearShapes() %>%
            addPolylines(data = merca, lat = as.numeric(merca[i, c(2,5)]), lng = as.numeric(merca[i, c(3,6)]),color="red",group=~grupo ,layerId="Lineas")  %>%
            addCircles(data=consumidores,lat = ~ Latitud,lng = ~ Longitud,radius = ~potencia_con,
                       color = "green", fillOpacity = 0.7) %>% #,popup = ~paste(num_cliente,Potencia,hora,energia_con)
            addCircles(data=generadores,lat = ~ Latitud,lng = ~ Longitud,radius = ~potencia_gen,
                       color = "red", fillOpacity = 0.7) #,popup = ~paste(num_cliente,Potencia,hora,energia_gen)
        }
      }
    }
  })

  v <- reactiveValues(data = NULL)
  valm <- reactiveValues(data = NULL)
  vcon <- reactiveValues(data = NULL)
  vhis<-reactiveValues(data = NULL)
  w <- reactiveValues(data = NULL)
  w_g <- reactiveValues(data = NULL)
  w_1 <- reactiveValues(data = NULL)
  w_11 <- reactiveValues(data = NULL)
  w_a <- reactiveValues(data = NULL)
  w_3 <- reactiveValues(data = NULL)
  w_31 <- reactiveValues(data = NULL)
  w_3a <- reactiveValues(data = NULL)
  w_ac <- reactiveValues(data = NULL)
  w_4 <- reactiveValues(data = NULL)
  w_41 <- reactiveValues(data = NULL)
  w_4a <- reactiveValues(data = NULL)
  vtabla<- reactiveValues(data = NULL)
  vsaldo<-reactiveValues(data = NULL)
  vsaldomes<-reactiveValues(data = NULL)
  vsaldoreal<-reactiveValues(data = NULL)
  vsaldorealmes<-reactiveValues(data = NULL)
  venergiamesreal<-reactiveValues(data = NULL)
  venergiames<-reactiveValues(data = NULL)
    vprecio<-reactiveValues(data = NULL)

  observeEvent(input$Registrar, {
    cliente<-as.numeric(input$numcliente_reg)
    latitud<-input$Latitud_reg
    longitud<-input$Longitud_reg
    if(input$check_con == 'TRUE'){
      potencia_con<-input$pot_reg1
    }
    if(input$check_gen == 'TRUE'){
      potencia_gen<-input$pot_reg2
    }
    if((input$check_gen == 'TRUE')&&(input$check_con == 'TRUE')){
      tipo_cliente<-3
    }
    if(input$check_alm== 'TRUE'){
      # potencia_alm<-input$pot_reg3
      potencia_alm<-Bateria[Bateria$Nombre==input$bateria_reg,"Potencia"]
      tipo_cliente<-3
    }
    if(input$check_coche== 'TRUE'){
      # potencia_alm<-input$pot_reg3
      potencia_alm<-Coches_Electricos[Coches_Electricos$Nombre==input$coche,"Acumulacion"]
      tipo_cliente<-3
    }
    if((input$check_gen == 'FALSE')&&(input$check_alm == 'FALSE')&&(input$check_coche == 'FALSE')){
      potencia_gen<-0
      potencia_alm<-0
      tipo_cliente<-1
    }
    if((input$check_con == 'FALSE')&&(input$check_alm == 'FALSE')&&(input$check_coche == 'FALSE')){
      potencia_con<-0
      potencia_alm<-0
      tipo_cliente<-2
    }
    if(input$radio_reg == 2){
      CNAE<-input$CNAE
    }else{
      CNAE<-0
    }
    tam_inter<-dim(datosclientes)
    m<-tam_inter[1]
    m<-m+1
    Localidad<-as.character(input$text)
    # Localidad<-as.character(Localidad)
    datosclientes[m,]<<-data.frame(cliente,CNAE,Localidad,latitud,longitud,potencia_con,potencia_gen,potencia_alm,tipo_cliente)

  })

  observeEvent(input$fotovoltaica, {
    precio<-input$precio
    mes<-as.numeric(format(input$date2, format="%m" ))
    if ((mes>3)||(mes<10)){
      v$data <-(basefotovoltaica*input$pot*1000)/100
    }else{
      v$data <-((basefotovoltaica*input$pot*1000)/100)*0.6
    }

    tam<-dim(oferta)
    n<-tam[1]
    n<-n+1

    cliente<-as.numeric(input$numcliente2)
    latitud<-datosclientes[datosclientes$num_cliente==input$numcliente2,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==input$numcliente2,"Longitud"]
    potencia<-datosclientes[datosclientes$num_cliente==input$num_cliente2,"potencia_gen"]

    oferta[n,]<<-data.frame(cliente,latitud,longitud,potencia, input$date2,precio,v$data[1],v$data[2],v$data[3],v$data[4],v$data[5],v$data[6],v$data[7],v$data[8],v$data[9],v$data[10],v$data[11],v$data[12],v$data[13],v$data[14],v$data[15],v$data[16],v$data[17],v$data[18],v$data[19],v$data[20],v$data[21],v$data[22],v$data[23],v$data[24])
  })

  observeEvent(input$eolica, {
    mes<-as.numeric(format(input$date2, format="%m" ))
    precio<-input$precio
    if ((mes>3)||(mes<10)){
      v$data <-((baseeolica*input$pot*1000)/100)*0.8
    }else{
      v$data <-((baseeolica*input$pot*1000)/100)*1.2
    }
    tam<-dim(oferta)
    n<-tam[1]
    n<-n+1

    cliente<-as.numeric(input$numcliente2)
    latitud<-datosclientes[datosclientes$num_cliente==input$numcliente2,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==input$numcliente2,"Longitud"]
    potencia<-datosclientes[datosclientes$num_cliente==input$num_cliente2,"potencia_gen"]

    oferta[n,]<<-data.frame(cliente,latitud,longitud,potencia, input$date2,precio,v$data[1],v$data[2],v$data[3],v$data[4],v$data[5],v$data[6],v$data[7],v$data[8],v$data[9],v$data[10],v$data[11],v$data[12],v$data[13],v$data[14],v$data[15],v$data[16],v$data[17],v$data[18],v$data[19],v$data[20],v$data[21],v$data[22],v$data[23],v$data[24])
  })

  observeEvent(input$Consumo, {
    cero <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    rango<- input$range
    precio<-input$preciodem
    indice_inic<-rango[1]
    indice_final<-rango[2]
    cero[indice_inic:indice_final]<-input$energia
    vcon$data<-cero
    tam<-dim(demanda)
    n<-tam[1]
    n<-n+1

    cliente<-as.numeric(input$numcliente3)
    latitud<-datosclientes[datosclientes$num_cliente==input$numcliente3,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==input$numcliente3,"Longitud"]
    potencia<-datosclientes[datosclientes$num_cliente==input$numcliente3,"potencia_con"]

    demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia, input$date3,precio,vcon$data[1],vcon$data[2],vcon$data[3],vcon$data[4],vcon$data[5],vcon$data[6],vcon$data[7],vcon$data[8],vcon$data[9],vcon$data[10],vcon$data[11],vcon$data[12],vcon$data[13],vcon$data[14],vcon$data[15],vcon$data[16],vcon$data[17],vcon$data[18],vcon$data[19],vcon$data[20],vcon$data[21],vcon$data[22],vcon$data[23],vcon$data[24])
  })

  observeEvent(input$bateria, {
    cero <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    rango<- input$range2
    indice_inic<-rango[1]
    indice_final<-rango[2]
    cero[indice_inic:indice_final]<-input$porcentaje
    valm$data<-cero
    tam<-dim(almacenamiento)
    n<-tam[1]
    n<-n+1

    cliente<-as.numeric(input$numcliente4)
    latitud<-datosclientes[datosclientes$num_cliente==input$numcliente4,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==input$numcliente4,"Longitud"]
    potencia<-datosclientes[datosclientes$num_cliente==input$numcliente4,"potencia_alm"]

    almacenamiento[n,]<<-data.frame(cliente,latitud,longitud,potencia, input$date4,valm$data[1],valm$data[2],valm$data[3],valm$data[4],valm$data[5],valm$data[6],valm$data[7],valm$data[8],valm$data[9],valm$data[10],valm$data[11],valm$data[12],valm$data[13],valm$data[14],valm$data[15],valm$data[16],valm$data[17],valm$data[18],valm$data[19],valm$data[20],valm$data[21],valm$data[22],valm$data[23],valm$data[24])

    tam_con<-dim(demanda)
    m<-tam_con[1]
    m<-m+1
    demanda[m,]<<-data.frame(cliente,latitud,longitud,potencia, input$date4,input$precioalm1,valm$data[1]*potencia*1000/100,valm$data[2]*potencia*1000/100,valm$data[3]*potencia*1000/100,valm$data[4]*potencia*1000/100,valm$data[5]*potencia*1000/100,valm$data[6]*potencia*1000/100,valm$data[7]*potencia*1000/100,valm$data[8]*potencia*1000/100,valm$data[9]*potencia*1000/100,valm$data[10]*potencia*1000/100,valm$data[11]*potencia*1000/100,valm$data[12]*potencia*1000/100,valm$data[13]*potencia*1000/100,valm$data[14]*potencia*1000/100,valm$data[15]*potencia*1000/100,valm$data[16]*potencia*1000/100,valm$data[17]*potencia*1000/100,valm$data[18]*potencia*1000/100,valm$data[19]*potencia*1000/100,valm$data[20]*potencia*1000/100,valm$data[21]*potencia*1000/100,valm$data[22]*potencia*1000/100,valm$data[23]*potencia*1000/100,valm$data[24]*potencia*1000/100)
    tam_gen<-dim(oferta)
    k<-tam_gen[1]
    k<-k+1
    oferta[k,]<<-data.frame(cliente,latitud,longitud,potencia, input$date4,input$precioalm2,valm$data[1]*potencia*1000/100,valm$data[2]*potencia*1000/100,valm$data[3]*potencia*1000/100,valm$data[4]*potencia*1000/100,valm$data[5]*potencia*1000/100,valm$data[6]*potencia*1000/100,valm$data[7]*potencia*1000/100,valm$data[8]*potencia*1000/100,valm$data[9]*potencia*1000/100,valm$data[10]*potencia*1000/100,valm$data[11]*potencia*1000/100,valm$data[12]*potencia*1000/100,valm$data[13]*potencia*1000/100,valm$data[14]*potencia*1000/100,valm$data[15]*potencia*1000/100,valm$data[16]*potencia*1000/100,valm$data[17]*potencia*1000/100,valm$data[18]*potencia*1000/100,valm$data[19]*potencia*1000/100,valm$data[20]*potencia*1000/100,valm$data[21]*potencia*1000/100,valm$data[22]*potencia*1000/100,valm$data[23]*potencia*1000/100,valm$data[24]*potencia*1000/100)

  })

  observeEvent(input$Consumo_sem, {
    cliente<-as.numeric(input$num_cliente_sem2)
    potencia<-datosclientes[datosclientes$num_cliente==cliente,"potencia_con"]
    latitud<-datosclientes[datosclientes$num_cliente==cliente,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==cliente,"Longitud"]
    fechas<-input$dates_sem2
    mes1<-as.numeric(format(fechas[1], format="%m" ))
    mes2<-as.numeric(format(fechas[2], format="%m" ))

    if(input$radio_sem2 == 1){
      if(input$Lavadora){
        if((mes1<3)||(mes1>10)){
          basenumerosa[12]<-basenumerosa[12]*2*(input$pot_sem21)
          basenumerosa[11]<-basenumerosa[11]*2*(input$pot_sem21)
        }else{
          basenumerosa[12]<-basenumerosa[12]*1.5*(input$pot_sem21)
          basenumerosa[12]<-basenumerosa[12]*2*(input$pot_sem21)
        }
      }
      if(input$Cocina){
        basenumerosa[14]<-basenumerosa[14]*2*(input$pot_sem22)
        basenumerosa[22]<-basenumerosa[22]*2*(input$pot_sem22)
      }
      if(input$Calentador){
        if((mes1<3)||(mes1>10)){
          basenumerosa[6]<-basenumerosa[6]*3*(input$pot_sem23)
          basenumerosa[7]<-70
          basenumerosa[20]<-70
          basenumerosa[21]<-90
        }else{
          basenumerosa[6]<-basenumerosa[6]*2*(input$pot_sem23)
          basenumerosa[7]<-60
          basenumerosa[20]<-60
          basenumerosa[21]<-85
        }
      }
      if(input$Calefaccion){
        if((mes1<3)||(mes1>10)){
          basenumerosa[1]<-basenumerosa[1]*2*(input$pot_sem24)
          basenumerosa[2]<-basenumerosa[2]*2*(input$pot_sem24)
          basenumerosa[5]<-basenumerosa[5]*2*(input$pot_sem24)
          basenumerosa[6]<-basenumerosa[6]*2*(input$pot_sem24)
          basenumerosa[23]<-basenumerosa[23]*2*(input$pot_sem24)
          basenumerosa[24]<-basenumerosa[24]*2*(input$pot_sem24)
        }
      }
      if(input$Ac){
        if((mes1<3)||(mes1>10)){
        }else{
          basenumerosa[1]<-basenumerosa[1]*1.5*(input$pot_sem25)
          basenumerosa[2]<-basenumerosa[2]*1.5*(input$pot_sem25)
          basenumerosa[5]<-basenumerosa[5]*1.5*(input$pot_sem25)
          basenumerosa[6]<-basenumerosa[6]*1.5*(input$pot_sem25)
          basenumerosa[23]<-basenumerosa[23]*1.5*(input$pot_sem25)
          basenumerosa[24]<-basenumerosa[24]*1.5*(input$pot_sem25)
        }
      }
      w$data <- (basenumerosa*potencia*1000)/100
    }
    if(input$radio_sem2 == 2){
      if(input$Lavadora){
        if((mes1<3)||(mes1>10)){
          baseunifamiliar[12]<-baseunifamiliar[12]*2*(input$pot_sem21)
        }else{
          baseunifamiliar[12]<-baseunifamiliar[12]*1.5*(input$pot_sem21)
        }
      }
      if(input$Cocina){
        baseunifamiliar[14]<-baseunifamiliar[14]*2*(input$pot_sem22)
        baseunifamiliar[22]<-baseunifamiliar[22]*1.5*(input$pot_sem22)
      }
      if(input$Calentador){
        if((mes1<3)||(mes1>10)){
          baseunifamiliar[6]<-baseunifamiliar[6]*3*(input$pot_sem23)
          baseunifamiliar[7]<-50
          baseunifamiliar[20]<-80
          baseunifamiliar[21]<-80
        }else{
          baseunifamiliar[6]<-baseunifamiliar[6]*2*(input$pot_sem23)
          baseunifamiliar[7]<-40
          baseunifamiliar[20]<-75
          baseunifamiliar[21]<-75
        }
      }
      if(input$Calefaccion){
        if((mes1<3)||(mes1>10)){
          baseunifamiliar[1]<-baseunifamiliar[1]*2*(input$pot_sem24)
          baseunifamiliar[2]<-baseunifamiliar[2]*2*(input$pot_sem24)
          baseunifamiliar[5]<-baseunifamiliar[5]*2*(input$pot_sem24)
          baseunifamiliar[6]<-baseunifamiliar[6]*2*(input$pot_sem24)
          baseunifamiliar[23]<-baseunifamiliar[23]*2*(input$pot_sem24)
          baseunifamiliar[24]<-baseunifamiliar[24]*2*(input$pot_sem24)
        }
      }
      if(input$Ac){
        if((mes1>3)||(mes1<10)){
        }else{
          baseunifamiliar[1]<-baseunifamiliar[1]*1.5*(input$pot_sem25)
          baseunifamiliar[2]<-baseunifamiliar[2]*1.5*(input$pot_sem25)
          baseunifamiliar[5]<-baseunifamiliar[5]*1.5*(input$pot_sem25)
          baseunifamiliar[6]<-baseunifamiliar[6]*1.5*(input$pot_sem25)
          baseunifamiliar[23]<-baseunifamiliar[23]*1.5*(input$pot_sem25)
          baseunifamiliar[24]<-baseunifamiliar[24]*1.5*(input$pot_sem25)
        }
      }
      w$data <- (baseunifamiliar*potencia*1000)/100
    }
    if(input$radio_sem2 == 3){
      if(input$Calefaccion){
        if((mes1<3)||(mes1>10)){
          basenegocio[1]<-basenegocio[1]*2*(input$pot_sem24)
          basenegocio[2]<-basenegocio[2]*2*(input$pot_sem24)
          basenegocio[5]<-basenegocio[5]*2*(input$pot_sem24)
          basenegocio[6]<-basenegocio[6]*2*(input$pot_sem24)
          basenegocio[23]<-basenegocio[23]*2*(input$pot_sem24)
          basenegocio[24]<-basenegocio[24]*2*(input$pot_sem24)
        }
      }
      if(input$Ac){
        if((mes1<3)||(mes1>10)){
        }else{
          basenegocio[1]<-basenegocio[1]*1.5*(input$pot_sem25)
          basenegocio[2]<-basenegocio[2]*1.5*(input$pot_sem25)
          basenegocio[5]<-basenegocio[5]*1.5*(input$pot_sem25)
          basenegocio[6]<-basenegocio[6]*1.5*(input$pot_sem25)
          basenegocio[23]<-basenegocio[23]*1.5*(input$pot_sem25)
          basenegocio[24]<-basenegocio[24]*1.5*(input$pot_sem25)
        }
      }
      w$data <- (basenegocio*potencia*1000)/100
    }
    if(input$radio_sem2 == 4){
      if(input$Calefaccion){
        if((mes1<3)||(mes1>10)){
          basebar[1]<-basebar[1]*2*(input$pot_sem24)
          basebar[2]<-basebar[2]*2*(input$pot_sem24)
          basebar[5]<-basebar[5]*2*(input$pot_sem24)
          basebar[6]<-basebar[6]*2*(input$pot_sem24)
          basebar[23]<-basebar[23]*2*(input$pot_sem24)
          basebar[24]<-basebar[24]*2*(input$pot_sem24)
        }
      }
      if(input$Ac){
        if((mes1<3)||(mes1>10)){
        }else{
          basebar[1]<-basebar[1]*1.5*(input$pot_sem25)
          basebar[2]<-basebar[2]*1.5*(input$pot_sem25)
          basebar[5]<-basebar[5]*1.5*(input$pot_sem25)
          basebar[6]<-basebar[6]*1.5*(input$pot_sem25)
          basebar[23]<-basebar[23]*1.5*(input$pot_sem25)
          basebar[24]<-basebar[24]*1.5*(input$pot_sem25)
        }
      }
      w$data <- (basebar*potencia*1000)/100
    }
    dif<-as.numeric(fechas[2]-fechas[1])
    for(i in 1:dif){
      tam<-dim(demanda)
      n<-tam[1]
      n<-n+1
      fecha<-fechas[1]+i-1
      demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia, fecha,as.numeric(input$preciocon) ,w$data[1],w$data[2],w$data[3],w$data[4],w$data[5],w$data[6],w$data[7],w$data[8],w$data[9],w$data[10],w$data[11],w$data[12],w$data[13],w$data[14],w$data[15],w$data[16],w$data[17],w$data[18],w$data[19],w$data[20],w$data[21],w$data[22],w$data[23],w$data[24])
    }
  })

  observeEvent(input$Generacion_sem, {
    # potencia<-input$pot_sem11
    # potencia_gen<-input$pot_sem1
    cliente<-as.numeric(input$num_cliente_sem1)
    potencia<-datosclientes[datosclientes$num_cliente==cliente,"potencia_con"]
    potencia_gen<-datosclientes[datosclientes$num_cliente==cliente,"potencia_gen"]
    latitud<-datosclientes[datosclientes$num_cliente==cliente,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==cliente,"Longitud"]
    fechas<-input$dates_sem1
    mes<-as.numeric(format(fechas[1], format="%m" ))
    cero <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    w_11$data <-cero
    w_1$data <-cero
    if(input$check_consu){
      if(input$radio_sem11 == 1){
        w_11$data <- (basenumerosa*potencia*1000)/100
      }
      if(input$radio_sem11 == 2){
        w_11$data <- (baseunifamiliar*potencia*1000)/100
      }
      if(input$radio_sem11 == 3){
        w_11$data <- (basenegocio*potencia*1000)/100
      }
      if(input$radio_sem11 == 4){
        w_11$data <- (basebar*potencia*1000)/100
      }
    }
    if(input$radio_sem1 == 1){
      if ((mes>3)||(mes<10)){
        w_1$data <- (basefotovoltaica*potencia_gen*1000)/100
      }else{
        w_1$data <- ((basefotovoltaica*potencia_gen*1000)/100)*0.6
      }
    }
    if(input$radio_sem1 == 2){
      if ((mes>3)||(mes<10)){
        w_1$data <- ((baseeolica*potencia_gen*1000)/100)*0.8
      }else{
        w_1$data <- ((baseeolica*potencia_gen*1000)/100)*1.2
      }
    }
    w_g$data<-w_11$data-w_1$data
    for(i in 1:24){
      if(w_g$data[i]<0){
        w_11$data[i]<-0
        w_1$data[i]<-(w_g$data[i])*(-1)
      }
      if(w_g$data[i]>0){
        w_11$data[i]<-w_g$data[i]
        w_1$data[i]<-0
      }
      if(w_g$data[i]==0){
        w_1$data[i]<-0
        w_11$data[i]<-0
      }
    }
    dif<-as.numeric(fechas[2]-fechas[1])
    if(input$check_consu){
      for(i in 1:dif){
        tam11<-dim(demanda)
        n<-tam11[1]
        n<-n+1
        tam1<-dim(oferta)
        l<-tam1[1]
        l<-l+1
        fecha<-fechas[1]+i-1
        demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia, fecha,input$preciogen1,w_11$data[1],w_11$data[2],w_11$data[3],w_11$data[4],w_11$data[5],w_11$data[6],w_11$data[7],w_11$data[8],w_11$data[9],w_11$data[10],w_11$data[11],w_11$data[12],w_11$data[13],w_11$data[14],w_11$data[15],w_11$data[16],w_11$data[17],w_11$data[18],w_11$data[19],w_11$data[20],w_11$data[21],w_11$data[22],w_11$data[23],w_11$data[24])
        oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen, fecha,input$preciogen2,w_1$data[1],w_1$data[2],w_1$data[3],w_1$data[4],w_1$data[5],w_1$data[6],w_1$data[7],w_1$data[8],w_1$data[9],w_1$data[10],w_1$data[11],w_1$data[12],w_1$data[13],w_1$data[14],w_1$data[15],w_1$data[16],w_1$data[17],w_1$data[18],w_1$data[19],w_1$data[20],w_1$data[21],w_1$data[22],w_1$data[23],w_1$data[24])
      }

    }else{
      for(i in 1:dif){
        tam1<-dim(oferta)
        l<-tam1[1]
        l<-l+1
        fecha<-fechas[1]+i-1
        oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen, fecha,input$preciogen2,w_1$data[1],w_1$data[2],w_1$data[3],w_1$data[4],w_1$data[5],w_1$data[6],w_1$data[7],w_1$data[8],w_1$data[9],w_1$data[10],w_1$data[11],w_1$data[12],w_1$data[13],w_1$data[14],w_1$data[15],w_1$data[16],w_1$data[17],w_1$data[18],w_1$data[19],w_1$data[20],w_1$data[21],w_1$data[22],w_1$data[23],w_1$data[24])
      }
    }

  })

  observeEvent(input$almac_sem, {

    cliente<-as.numeric(input$num_cliente_sem3)
    potencia<-datosclientes[datosclientes$num_cliente==cliente,"potencia_con"]
    potencia_gen<-datosclientes[datosclientes$num_cliente==cliente,"potencia_gen"]
    potencia_alm<-datosclientes[datosclientes$num_cliente==cliente,"potencia_alm"]
    latitud<-datosclientes[datosclientes$num_cliente==cliente,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==cliente,"Longitud"]
    fechas<-input$dates_sem3
    mes<-as.numeric(format(fechas[1], format="%m" ))
    cero <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    w_31$data <-cero

    w_3$data <-cero

    if(input$check_consu2){
      if(input$radio_sem31 == 1){
        w_31$data <- (basenumerosa*potencia*1000)/100

      }
      if(input$radio_sem31 == 2){
        w_31$data <- (baseunifamiliar*potencia*1000)/100
      }
      if(input$radio_sem31 == 3){
        w_31$data <- (basenegocio*potencia*1000)/100
      }
      if(input$radio_sem31 == 4){
        w_31$data <- (basebar*potencia*1000)/100
      }
    }
    if(input$check_gen2){
      if(input$radio_sem3 == 1){
        if ((mes>3)||(mes<10)){
          w_3$data <- (basefotovoltaica*potencia_gen*1000)/100
        }else{
          w_3$data <- ((basefotovoltaica*potencia_gen*1000)/100)*0.6
        }
      }
      if(input$radio_sem3 == 2){
        if ((mes>3)||(mes<10)){
          w_3$data <- ((baseeolica*potencia_gen*1000)/100)*0.8
        }else{
          w_3$data <- ((baseeolica*potencia_gen*1000)/100)*1.2
        }
      }
    }

    basealmac[1]<-input$estado
    w_3a$data <- (basealmac*potencia_alm*1000)/100

    w_a$data<-w_3$data-w_31$data

    for(i in 1:24){

      if(w_a$data[i]<0){
        if(w_3a$data[i]==0){
          w_3$data[i]<-0
          w_31$data[i]<-(w_a$data[i])*(-1)
        }else{
          if(w_3a$data[i]>=((w_a$data[i])*(-1))){
            w_31$data[i]<-0
            w_3$data[i]<-0
            w_3a$data[i+1]<-w_3a$data[i]+w_a$data[i]
          }else{
            w_3$data[i]<-0
            w_31$data[i]<-((w_a$data[i])*(-1))-w_3a$data[i]
            w_3a$data[i+1]<-0
          }
        }
      }
      if(w_a$data[i]>0){
        if(w_3a$data[i]==0){
          if(w_a$data[i]<=(potencia_alm*1000)){
            w_3a$data[i+1]<-w_a$data[i]
            w_3$data[i]<-0
            w_31$data[i]<-0
          }else{
            w_3a$data[i+1]<-potencia_alm*1000
            w_31$data[i]<-0
            w_3$data[i]<-w_a$data[i]-(potencia_alm*1000)
          }
        }else{
          if(w_3a$data[i]<(potencia_alm*1000)){
            if(w_a$data[i]+w_3a$data[i]>=(potencia_alm*1000)){
              w_3a$data[i+1]<-potencia_alm*1000
              w_31$data[i]<-0
              w_3$data[i]<-w_a$data[i]+w_3a$data[i]-(potencia_alm*1000)
            }else{
              w_3$data[i]<-0
              w_31$data[i]<-0
              w_3a$data[i+1]<-w_a$data[i]+w_3a$data[i]
            }
          }else{
            w_3$data[i]<-w_a$data[i]
            w_31$data[i]<-0
            w_3a$data[i+1]<-w_3a$data[i]
          }
        }
      }
      if(w_a$data[i]==0){
        w_3$data[i]<-0
        w_31$data[i]<-0
        w_3a$data[i+1]<-w_3a$data[i]
      }
    }
    dif<-as.numeric(fechas[2]-fechas[1])

    if(input$check_consu2&&input$check_gen2){
      for(i in 1:dif){
        tam31<-dim(demanda)
        n<-tam31[1]
        n<-n+1
        tam3<-dim(oferta)
        l<-tam3[1]
        l<-l+1
        tam3a<-dim(almacenamiento)
        k<-tam3a[1]
        k<-k+1
        fecha<-fechas[1]+i-1
        demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia,fecha,input$precioalmcon,w_31$data[1],w_31$data[2],w_31$data[3],w_31$data[4],w_31$data[5],w_31$data[6],w_31$data[7],w_31$data[8],w_31$data[9],w_31$data[10],w_31$data[11],w_31$data[12],w_31$data[13],w_31$data[14],w_31$data[15],w_31$data[16],w_31$data[17],w_31$data[18],w_31$data[19],w_31$data[20],w_31$data[21],w_31$data[22],w_31$data[23],w_31$data[24])
        oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen,fecha,input$precioalmgen,w_3$data[1],w_3$data[2],w_3$data[3],w_3$data[4],w_3$data[5],w_3$data[6],w_3$data[7],w_3$data[8],w_3$data[9],w_3$data[10],w_3$data[11],w_3$data[12],w_3$data[13],w_3$data[14],w_3$data[15],w_3$data[16],w_3$data[17],w_3$data[18],w_3$data[19],w_3$data[20],w_3$data[21],w_3$data[22],w_3$data[23],w_3$data[24])
        almacenamiento[k,]<<-data.frame(cliente,latitud,longitud,potencia_alm, fecha,w_3a$data[1]*100/(potencia_alm*1000),w_3a$data[2]*100/(potencia_alm*1000),w_3a$data[3]*100/(potencia_alm*1000),w_3a$data[4]*100/(potencia_alm*1000),w_3a$data[5]*100/(potencia_alm*1000),w_3a$data[6]*100/(potencia_alm*1000),w_3a$data[7]*100/(potencia_alm*1000),w_3a$data[8]*100/(potencia_alm*1000),w_3a$data[9]*100/(potencia_alm*1000),w_3a$data[10]*100/(potencia_alm*1000),w_3a$data[11]*100/(potencia_alm*1000),w_3a$data[12]*100/(potencia_alm*1000),w_3a$data[13]*100/(potencia_alm*1000),w_3a$data[14]*100/(potencia_alm*1000),w_3a$data[15]*100/(potencia_alm*1000),w_3a$data[16]*100/(potencia_alm*1000),w_3a$data[17]*100/(potencia_alm*1000),w_3a$data[18]*100/(potencia_alm*1000),w_3a$data[19]*100/(potencia_alm*1000),w_3a$data[20]*100/(potencia_alm*1000),w_3a$data[21]*100/(potencia_alm*1000),w_3a$data[22]*100/(potencia_alm*1000),w_3a$data[23]*100/(potencia_alm*1000),w_3a$data[24]*100/(potencia_alm*1000))
      }
    }else{
      if(input$check_gen2){
        for(i in 1:dif){
          tam3<-dim(oferta)
          l<-tam3[1]
          l<-l+1
          tam3a<-dim(almacenamiento)
          k<-tam3a[1]
          k<-k+1
          fecha<-fechas[1]+i-1
          oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen,fecha,input$precioalmgen,w_3$data[1],w_3$data[2],w_3$data[3],w_3$data[4],w_3$data[5],w_3$data[6],w_3$data[7],w_3$data[8],w_3$data[9],w_3$data[10],w_3$data[11],w_3$data[12],w_3$data[13],w_3$data[14],w_3$data[15],w_3$data[16],w_3$data[17],w_3$data[18],w_3$data[19],w_3$data[20],w_3$data[21],w_3$data[22],w_3$data[23],w_3$data[24])
          almacenamiento[k,]<<-data.frame(cliente,latitud,longitud,potencia_alm, fecha,w_3a$data[1]*100/(potencia_alm*1000),w_3a$data[2]*100/(potencia_alm*1000),w_3a$data[3]*100/(potencia_alm*1000),w_3a$data[4]*100/(potencia_alm*1000),w_3a$data[5]*100/(potencia_alm*1000),w_3a$data[6]*100/(potencia_alm*1000),w_3a$data[7]*100/(potencia_alm*1000),w_3a$data[8]*100/(potencia_alm*1000),w_3a$data[9]*100/(potencia_alm*1000),w_3a$data[10]*100/(potencia_alm*1000),w_3a$data[11]*100/(potencia_alm*1000),w_3a$data[12]*100/(potencia_alm*1000),w_3a$data[13]*100/(potencia_alm*1000),w_3a$data[14]*100/(potencia_alm*1000),w_3a$data[15]*100/(potencia_alm*1000),w_3a$data[16]*100/(potencia_alm*1000),w_3a$data[17]*100/(potencia_alm*1000),w_3a$data[18]*100/(potencia_alm*1000),w_3a$data[19]*100/(potencia_alm*1000),w_3a$data[20]*100/(potencia_alm*1000),w_3a$data[21]*100/(potencia_alm*1000),w_3a$data[22]*100/(potencia_alm*1000),w_3a$data[23]*100/(potencia_alm*1000),w_3a$data[24]*100/(potencia_alm*1000))
        }
      }
      if(input$check_consu2){
        for(i in 1:dif){
          tam31<-dim(demanda)
          n<-tam31[1]
          n<-n+1
          tam3a<-dim(almacenamiento)
          k<-tam3a[1]
          k<-k+1
          fecha<-fechas[1]+i-1
          demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia,fecha,input$precioalmcon,w_31$data[1],w_31$data[2],w_31$data[3],w_31$data[4],w_31$data[5],w_31$data[6],w_31$data[7],w_31$data[8],w_31$data[9],w_31$data[10],w_31$data[11],w_31$data[12],w_31$data[13],w_31$data[14],w_31$data[15],w_31$data[16],w_31$data[17],w_31$data[18],w_31$data[19],w_31$data[20],w_31$data[21],w_31$data[22],w_31$data[23],w_31$data[24])
          almacenamiento[k,]<<-data.frame(cliente,latitud,longitud,potencia_alm, fecha,w_3a$data[1]*100/(potencia_alm*1000),w_3a$data[2]*100/(potencia_alm*1000),w_3a$data[3]*100/(potencia_alm*1000),w_3a$data[4]*100/(potencia_alm*1000),w_3a$data[5]*100/(potencia_alm*1000),w_3a$data[6]*100/(potencia_alm*1000),w_3a$data[7]*100/(potencia_alm*1000),w_3a$data[8]*100/(potencia_alm*1000),w_3a$data[9]*100/(potencia_alm*1000),w_3a$data[10]*100/(potencia_alm*1000),w_3a$data[11]*100/(potencia_alm*1000),w_3a$data[12]*100/(potencia_alm*1000),w_3a$data[13]*100/(potencia_alm*1000),w_3a$data[14]*100/(potencia_alm*1000),w_3a$data[15]*100/(potencia_alm*1000),w_3a$data[16]*100/(potencia_alm*1000),w_3a$data[17]*100/(potencia_alm*1000),w_3a$data[18]*100/(potencia_alm*1000),w_3a$data[19]*100/(potencia_alm*1000),w_3a$data[20]*100/(potencia_alm*1000),w_3a$data[21]*100/(potencia_alm*1000),w_3a$data[22]*100/(potencia_alm*1000),w_3a$data[23]*100/(potencia_alm*1000),w_3a$data[24]*100/(potencia_alm*1000))
        }
      }
    }
  })

  observeEvent(input$almac_sem2, {

    cliente<-as.numeric(input$num_cliente_sem4)
    potencia<-datosclientes[datosclientes$num_cliente==cliente,"potencia_con"]
    potencia_gen<-datosclientes[datosclientes$num_cliente==cliente,"potencia_gen"]
    potencia_alm<-datosclientes[datosclientes$num_cliente==cliente,"potencia_alm"]
    latitud<-datosclientes[datosclientes$num_cliente==cliente,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==cliente,"Longitud"]
    fechas<-input$dates_sem4
    mes<-as.numeric(format(fechas[1], format="%m" ))
    cero <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    w_41$data <-cero

    w_4$data <-cero

    if(input$check_consu3){
      if(input$radio_sem41 == 1){
        w_41$data <- (basenumerosa*potencia*1000)/100

      }
      if(input$radio_sem41 == 2){
        w_41$data <- (baseunifamiliar*potencia*1000)/100
      }
      if(input$radio_sem41 == 3){
        w_41$data <- (basenegocio*potencia*1000)/100
      }
      if(input$radio_sem41 == 4){
        w_41$data <- (basebar*potencia*1000)/100
      }
    }
    if(input$check_gen3){
      if(input$radio_sem4 == 1){
        if ((mes>3)||(mes<10)){
          w_4$data <- (basefotovoltaica*potencia_gen*1000)/100
        }else{
          w_4$data <- ((basefotovoltaica*potencia_gen*1000)/100)*0.6
        }
      }
      if(input$radio_sem4 == 2){
        if ((mes>3)||(mes<10)){
          w_4$data <- ((baseeolica*potencia_gen*1000)/100)*0.8
        }else{
          w_4$data <- ((baseeolica*potencia_gen*1000)/100)*1.2
        }
      }
    }

    potencia_conector<-Conectores[Conectores$Tipo_Conector==input$tipoconector,"Potencia"]
    horasalida<-as.numeric(input$selectsalida)
    horallegada<-as.numeric(input$selectllegada)
    energia_salida<-(input$estado2*potencia_alm*1000)/100
    energia_llegada<-(input$estado3*potencia_alm*1000)/100
    energia_necesaria<-energia_salida-energia_llegada
    tiempo_disponible<-24-(horallegada-horasalida)
    tiempo_recarga<-ceiling(energia_necesaria/(potencia_conector*1000))
    hora_recarga<-horasalida-tiempo_recarga
    energia_necesitada<-cero
    cero[horasalida]<-input$estado2
    # cero[horallegada]<-input$estado3
    acumulado<-0
    for (i in hora_recarga:(horasalida-1)) {
      acumulado<-potencia_conector*1000+acumulado
      energia_necesitada[i]<-potencia_conector*1000
      cero[i]<-(energia_llegada+acumulado)*100/(potencia_alm*1000)
    }
    for (i in horallegada:24) {
      cero[i]<-input$estado3
    }
    if (hora_recarga>1) {
      for (i in 1:hora_recarga) {
        cero[i]<-input$estado3
      }
    }

    w_4a$data<-energia_necesitada
    w_4alm$data<-cero
    w_ac$data<-w_4$data-w_41$data-w4a$data

    for(i in 1:24){
      if(w_ac$data[i]<0){
        w_41$data[i]<-0
        w_4$data[i]<-(w_ac$data[i])*(-1)
      }
      if(w_ac$data[i]>0){
        w_41$data[i]<-w_ac$data[i]
        w_4$data[i]<-0
      }
      if(w_ac$data[i]==0){
        w_4$data[i]<-0
        w_41$data[i]<-0
      }
    }
    dif<-as.numeric(fechas[2]-fechas[1])

    if(input$check_consu3&&input$check_gen3){
      for(i in 1:dif){
        tam31<-dim(demanda)
        n<-tam31[1]
        n<-n+1
        tam3<-dim(oferta)
        l<-tam3[1]
        l<-l+1
        tam3a<-dim(almacenamiento)
        k<-tam3a[1]
        k<-k+1
        fecha<-fechas[1]+i-1
        demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia,fecha,input$precioalmcon2,w_41$data[1],w_41$data[2],w_41$data[3],w_41$data[4],w_41$data[5],w_41$data[6],w_41$data[7],w_41$data[8],w_41$data[9],w_41$data[10],w_41$data[11],w_41$data[12],w_41$data[13],w_41$data[14],w_41$data[15],w_41$data[16],w_41$data[17],w_41$data[18],w_41$data[19],w_41$data[20],w_41$data[21],w_41$data[22],w_41$data[23],w_41$data[24])
        oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen,fecha,input$precioalmgen2,w_4$data[1],w_4$data[2],w_4$data[3],w_4$data[4],w_4$data[5],w_4$data[6],w_4$data[7],w_4$data[8],w_4$data[9],w_4$data[10],w_4$data[11],w_4$data[12],w_4$data[13],w_4$data[14],w_4$data[15],w_4$data[16],w_4$data[17],w_4$data[18],w_4$data[19],w_4$data[20],w_4$data[21],w_4$data[22],w_4$data[23],w_4$data[24])
        almacenamiento[k,]<<-data.frame(cliente,latitud,longitud,potencia_alm, fecha,w_4alm$data[1],w_4alm$data[2],w_4alm$data[3],w_4alm$data[4],w_4alm$data[5],w_4alm$data[6],w_4alm$data[7],w_4alm$data[8],w_4alm$data[9],w_4alm$data[10],w_4alm$data[11],w_4alm$data[12],w_4alm$data[13],w_4alm$data[14],w_4alm$data[15],w_4alm$data[16],w_4alm$data[17],w_4alm$data[18],w_4alm$data[19],w_4alm$data[20],w_4alm$data[21],w_4alm$data[22],w_4alm$data[23],w_4alm$data[24])
      }
    }else{
      if(input$check_gen2){
        for(i in 1:dif){
          tam3<-dim(oferta)
          l<-tam3[1]
          l<-l+1
          tam3a<-dim(almacenamiento)
          k<-tam3a[1]
          k<-k+1
          fecha<-fechas[1]+i-1
          oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen,fecha,input$precioalmgen2,w_4$data[1],w_4$data[2],w_4$data[3],w_4$data[4],w_4$data[5],w_4$data[6],w_4$data[7],w_4$data[8],w_4$data[9],w_4$data[10],w_4$data[11],w_4$data[12],w_4$data[13],w_4$data[14],w_4$data[15],w_4$data[16],w_4$data[17],w_4$data[18],w_4$data[19],w_4$data[20],w_4$data[21],w_4$data[22],w_4$data[23],w_4$data[24])
          almacenamiento[k,]<<-data.frame(cliente,latitud,longitud,potencia_alm, fecha,w_3a$data[1]*100/(potencia_alm*1000),w_3a$data[2]*100/(potencia_alm*1000),w_3a$data[3]*100/(potencia_alm*1000),w_3a$data[4]*100/(potencia_alm*1000),w_3a$data[5]*100/(potencia_alm*1000),w_3a$data[6]*100/(potencia_alm*1000),w_3a$data[7]*100/(potencia_alm*1000),w_3a$data[8]*100/(potencia_alm*1000),w_3a$data[9]*100/(potencia_alm*1000),w_3a$data[10]*100/(potencia_alm*1000),w_3a$data[11]*100/(potencia_alm*1000),w_3a$data[12]*100/(potencia_alm*1000),w_3a$data[13]*100/(potencia_alm*1000),w_3a$data[14]*100/(potencia_alm*1000),w_3a$data[15]*100/(potencia_alm*1000),w_3a$data[16]*100/(potencia_alm*1000),w_3a$data[17]*100/(potencia_alm*1000),w_3a$data[18]*100/(potencia_alm*1000),w_3a$data[19]*100/(potencia_alm*1000),w_3a$data[20]*100/(potencia_alm*1000),w_3a$data[21]*100/(potencia_alm*1000),w_3a$data[22]*100/(potencia_alm*1000),w_3a$data[23]*100/(potencia_alm*1000),w_3a$data[24]*100/(potencia_alm*1000))
        }
      }
      if(input$check_consu2){
        for(i in 1:dif){
          tam31<-dim(demanda)
          n<-tam31[1]
          n<-n+1
          tam3a<-dim(almacenamiento)
          k<-tam3a[1]
          k<-k+1
          fecha<-fechas[1]+i-1
          demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia,fecha,input$precioalmcon2,w_41$data[1],w_41$data[2],w_41$data[3],w_41$data[4],w_41$data[5],w_41$data[6],w_41$data[7],w_41$data[8],w_41$data[9],w_41$data[10],w_41$data[11],w_41$data[12],w_41$data[13],w_41$data[14],w_41$data[15],w_41$data[16],w_41$data[17],w_41$data[18],w_41$data[19],w_41$data[20],w_41$data[21],w_41$data[22],w_41$data[23],w_41$data[24])
          almacenamiento[k,]<<-data.frame(cliente,latitud,longitud,potencia_alm, fecha,w_4alm$data[1],w_4alm$data[2],w_4alm$data[3],w_4alm$data[4],w_4alm$data[5],w_4alm$data[6],w_4alm$data[7],w_4alm$data[8],w_4alm$data[9],w_4alm$data[10],w_4alm$data[11],w_4alm$data[12],w_4alm$data[13],w_4alm$data[14],w_4alm$data[15],w_4alm$data[16],w_4alm$data[17],w_4alm$data[18],w_4alm$data[19],w_4alm$data[20],w_4alm$data[21],w_4alm$data[22],w_4alm$data[23],w_4alm$data[24])
        }
      }
    }
  })


  observeEvent(input$guardar, {
    cliente<-as.numeric(input$numcliente_his)
    fechas<-input$dates_his
    latitud<-datosclientes[datosclientes$num_cliente==cliente,"Latitud"]
    longitud<-datosclientes[datosclientes$num_cliente==cliente,"Longitud"]
    tipo<-datosclientes[datosclientes$num_cliente==cliente,"tipo_cliente"]
    if (tipo==1) {
      potencia_con<-datosclientes[datosclientes$num_cliente==cliente,"potencia_con"]
    }else{
      if(tipo==2){
        potencia_gen<-datosclientes[datosclientes$num_cliente==cliente,"potencia_gen"]
      }else{
        potencia_gen<-datosclientes[datosclientes$num_cliente==cliente,"potencia_gen"]
        potencia_con<-datosclientes[datosclientes$num_cliente==cliente,"potencia_con"]
      }
    }
    # potencia<-datosclientes[datosclientes$num_cliente==cliente,"Potencia"]
    for(i in 1:24){
      if(vhis$data[i]==0){
        gener[i]<-0
        consu[i]<-0
      }
      if(vhis$data[i]>0){
        gener[i]<-0
        consu[i]<-vhis$data[i]
      }
      if(vhis$data[i]<0){
        gener[i]<-(vhis$data[i])*(-1)
        consu[i]<-0
      }
    }
    dif<-as.numeric(fechas[2]-fechas[1])
    if((max(gener)!=0)&&(max(consu)!=0)){
      for(i in 1:dif){
        tam31<-dim(demanda)
        n<-tam31[1]
        n<-n+1
        tam3<-dim(oferta)
        l<-tam3[1]
        l<-l+1
        fecha<-fechas[1]+i-1
        demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia_con, fecha,input$preciohisdem,consu[1],consu[2],consu[3],consu[4],consu[5],consu[6],consu[7],consu[8],consu[9],consu[10],consu[11],consu[12],consu[13],consu[14],consu[15],consu[16],consu[17],consu[18],consu[19],consu[20],consu[21],consu[22],consu[23],consu[24])
        oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen, fecha,input$preciohisofe,gener[1],gener[2],gener[3],gener[4],gener[5],gener[6],gener[7],gener[8],gener[9],gener[10],gener[11],gener[12],gener[13],gener[14],gener[15],gener[16],gener[17],gener[18],gener[19],gener[20],gener[21],gener[22],gener[23],gener[24])
      }
    }
    if((max(gener)==0)&&(max(consu)!=0)){
      for(i in 1:dif){
        tam31<-dim(demanda)
        n<-tam31[1]
        n<-n+1
        fecha<-fechas[1]+i-1
        demanda[n,]<<-data.frame(cliente,latitud,longitud,potencia_con, fecha,input$preciohisdem,consu[1],consu[2],consu[3],consu[4],consu[5],consu[6],consu[7],consu[8],consu[9],consu[10],consu[11],consu[12],consu[13],consu[14],consu[15],consu[16],consu[17],consu[18],consu[19],consu[20],consu[21],consu[22],consu[23],consu[24])
      }
    }
    if((max(gener)!=0)&&(max(consu)==0)){
      for(i in 1:dif){
        tam3<-dim(oferta)
        l<-tam3[1]
        l<-l+1
        fecha<-fechas[1]+i-1
        oferta[l,]<<-data.frame(cliente,latitud,longitud,potencia_gen, fecha,input$preciohisofe,gener[1],gener[2],gener[3],gener[4],gener[5],gener[6],gener[7],gener[8],gener[9],gener[10],gener[11],gener[12],gener[13],gener[14],gener[15],gener[16],gener[17],gener[18],gener[19],gener[20],gener[21],gener[22],gener[23],gener[24])
      }
    }

  })

  observeEvent(input$mostrar, {
    cliente<-input$numcliente_his
    consu_med<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    gener_med<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    if(length(which(oferta$IDCliente==cliente))!=0){
      gener_his<-oferta[oferta$IDCliente==cliente,7:30]
      gener_med<-apply(gener_his[,1:24],2,mean)
    }
    if(length(which(demanda$IDCliente==cliente))!=0){
      consu_his<-demanda[demanda$IDCliente==cliente,7:30]
      consu_med<-apply(consu_his[,1:24],2,mean)
    }
    vhis$data<-consu_med-gener_med
  })

  observeEvent(input$mostrar2, {
    cliente<-input$numcliente_reg
    mercadomapa<-mercadomapa[(mercadomapa$IDCliente_Gen==cliente)|(mercadomapa$IDCliente_Cons==cliente),]
    vtabla$data<-mercadomapa
    tam<-dim(mercadomapa)
    distintas<-unique(mercadomapa$Fecha)
    tamfecha<-length(distintas)
    fechaini<-mercadomapa[1,"Fecha"]
    tamsaldo<-dim(saldo)

    total<-0
    k<-1
    for (j in 1:tam[1]) {
      if (mercadomapa[j,"IDCliente_Gen"]==cliente) {
        for (i in 1:24) {
          total<-total+(mercadomapa[j,8+i]*mercadomapa[j,"Precio"]/1000)

        }
      }else{
        for (i in 1:24) {
          total<-total-(mercadomapa[j,8+i]*mercadomapa[j,"Precio"]/1000)

        }
      }
      if((j==tam[1])||(mercadomapa[j+1,"Fecha"]!=fechaini)){
        saldo[k,]<<-data.frame(as.numeric(cliente),fechaini,total)  #saldo[k+tamsaldo[1],]
        if(j!=tam[1]){
          total<-0
          fechaini<-mercadomapa[j+1,"Fecha"]
          k<-k+1
        }
      }
    }
    vsaldo$data<-saldo[saldo$IDCliente==cliente,2:3]
    fechaini<-mercadomapa[1,"Fecha"]
    mes<- as.numeric(format(fechaini, "%m"))
    totalmes<-0
    totalenergiaInter<-0
    k<-1
    for (j in 1:tam[1]) {
      if (mercadomapa[j,"IDCliente_Gen"]==cliente) {
        for (i in 1:24) {
          totalmes<-totalmes+(mercadomapa[j,8+i]*mercadomapa[j,"Precio"]/1000)
          totalenergiaInter<-totalenergiaInter+mercadomapa[j,8+i]
        }
      }else{
        for (i in 1:24) {
          totalmes<-totalmes-(mercadomapa[j,8+i]*mercadomapa[j,"Precio"]/1000)
          totalenergiaInter<-totalenergiaInter-mercadomapa[j,8+i]
        }
      }
      fechasig<-mercadomapa[j+1,"Fecha"]
      messig<- as.numeric(format(fechasig, "%m"))
      if((j==tam[1])||(mes!=messig)){
        saldomes[k,]<<-data.frame(as.numeric(cliente),mes,totalmes)  #saldo[k+tamsaldo[1],]
        energiamesinter[k,]<<-data.frame(as.numeric(cliente),mes,totalenergiaInter)
        if(j!=tam[1]){
          totalmes<-0
          totalenergiaInter<-0
          fechaini<-mercadomapa[j+1,"Fecha"]
          mes<- as.numeric(format(fechaini, "%m"))
          k<-k+1
        }
      }
    }
    vsaldomes$data<-saldomes[saldomes$IDCliente==cliente,2:3]
    venergiames$data<-energiamesinter[energiamesinter$IDCliente==cliente,2:3]

    demandamodif<-demanda[demanda$IDCliente==cliente,]
    # demandamod<-demanda[order(as.Date(demanda$Fecha)),]
    tamdema<-dim(demandamodif)
    # demandamodificado<-demandamod[1:tamdema[1],]
    totalreal<-0

    if (tamdema[1]!=0) {
      fechaini<-demandamodif[1,"Fecha"]
      k<-1
      for (j in 1:tamdema[1]) {
        fecha<-demandamodif[j,"Fecha"]
        # precioreal<-as.numeric(PrecioHIS[(PrecioHIS$Fecha==fecha)|(as.numeric(PrecioHIS$Hora)==i),"Precio"])
        Precio<-PrecioHIS[(PrecioHIS$Fecha==fecha),]
          for (i in 1:24) {
            precioreal<-as.numeric(Precio[(as.numeric(Precio$Hora)==i-1),"Precio"])
            totalreal<-totalreal-(demandamodif[j,6+i]*precioreal/1000)
          }

        if((j==tamdema[1])||(demandamodif[j+1,"Fecha"]!=fechaini)){
          saldoreal[k,]<<-data.frame(as.numeric(cliente),fechaini,totalreal)  #saldo[k+tamsaldo[1],]
          if(j!=tam[1]){
            totalreal<-0
            fechaini<-demandamodif[j+1,"Fecha"]
            k<-k+1
          }
        }
      }
      vsaldoreal$data<-saldoreal[saldoreal$IDCliente==cliente,2:3]

      fechaini<-demandamodif[1,"Fecha"]
      mes<- as.numeric(format(fechaini, "%m"))
      totalrealmes<-0
      totalenergiamesreal<-0
      k<-1
      for (j in 1:tamdema[1]) {
        fecha<-demandamodif[j,"Fecha"]
        Precio<-PrecioHIS[(PrecioHIS$Fecha==fecha),]
          for (i in 1:24) {
            precioreal<-as.numeric(Precio[(as.numeric(Precio$Hora)==i-1),"Precio"])
            totalrealmes<-totalrealmes-(demandamodif[j,6+i]*precioreal/1000)
            totalenergiamesreal<-totalenergiamesreal-demandamodif[j,6+i]
          }

        fechasig<-demandamodif[j+1,"Fecha"]
        messig<- as.numeric(format(fechasig, "%m"))
        if((j==tamdema[1])||(mes!=messig)){
          saldorealmes[k,]<<-data.frame(as.numeric(cliente),mes,totalrealmes)  #saldo[k+tamsaldo[1],]
          energiamesreal[k,]<<-data.frame(as.numeric(cliente),mes,totalenergiamesreal)
          if(j!=tamdema[1]){
            totalrealmes<-0
            totalenergiamesreal<-0
            fechaini<-demandamodif[j+1,"Fecha"]
            mes<- as.numeric(format(fechaini, "%m"))
            k<-k+1
          }
        }
      }
      vsaldorealmes$data<-saldorealmes[saldorealmes$IDCliente==cliente,2:3]
      venergiamesreal$data<-energiamesreal[energiamesreal$IDCliente==cliente,2:3]
    }
  })

  output$plotsaldo <- renderPlot({
    if (is.null(vsaldo$data)) return()
    cliente<-as.numeric(input$numcliente_reg)
    fechas<-vsaldo$data[,1]
    if (is.null(vsaldoreal$data)){
      y<-vsaldo$data[,2]
      barplot(y,main=cliente, col = c( "blue"),names.arg = c(vsaldoreal$data[,1]),
              ylab="Saldo (Euros)",
              xlab="Fecha")
    }else{
      y<-rbind(vsaldo$data[,2],vsaldoreal$data[,2])
      barplot(y,main=cliente, col = c("blue", "red"),names.arg = c(vsaldoreal$data[,1]),
              ylab="Saldo (Euros)",
              xlab="Fecha",legend = colnames(saldocomparado[2:3]))
    }


  })

  output$plotsaldo2 <- renderPlot({
    if (is.null(vsaldomes$data)) return()
    cliente<-as.numeric(input$numcliente_reg)
    if (is.null(vsaldorealmes$data)) {
      y<-vsaldomes$data[,2]
      barplot(y,main=cliente, col = c("blue"),names.arg = c(vsaldomes$data[,1]),
              ylab="Saldo (Euros)",
              xlab="Mes")
    }else{
      y<-rbind(vsaldomes$data[,2],vsaldorealmes$data[,2])
      barplot(y,main=cliente, col = c("blue", "red"),names.arg = c(vsaldomes$data[,1]),
              ylab="Saldo (Euros)",
              xlab="Mes",legend = colnames(saldocomparado[2:3]))
    }



  })

  output$plotenergiames <- renderPlot({
    if (is.null(venergiames$data)) return()

    cliente<-as.numeric(input$numcliente_reg)
    if (is.null(venergiamesreal$data)){
      x<-venergiames$data[,2]/1000
      barplot(x,main=cliente, col = c("blue"),names.arg = c(venergiames$data[,1]),
              ylab="Energia Total(kW)",
              xlab="Fecha")
    }else{
      x<-rbind(venergiames$data[,2]/1000,venergiamesreal$data[,2]/1000)
      barplot(x,main=cliente, col = c("blue", "red"),names.arg = c(venergiames$data[,1]),
              ylab="Energia Total(kW)",
              xlab="Fecha")
    }



  })

  output$text <- renderUI({
    if (is.null(venergiames$data)) return()
    if (is.null(venergiamesreal$data)){
    # energiamesinter<-energiamesinter[energiamesinter$IDCliente==input$numcliente_reg,]
    # energiamesreal<-energiamesreal[energiamesreal$IDCliente==input$numcliente_reg,]
      energiamesinter<-energiamesinter[energiamesinter$IDCliente==input$numcliente_reg,]
      saldomes<-saldomes[saldomes$IDCliente==input$numcliente_reg,]
      tamenergiames<-dim(energiamesinter)
      for (i in 1:tamenergiames[1]) {
        str1 <- paste("Mes", energiamesinter[i,2])
        str2 <- paste("Energia Intercambiada (KW)",energiamesinter[i,3]/1000)
        str3 <- paste("Precio Total (Euros)",abs(saldomes[i,3]))

      }

      HTML(paste(str1, str2, str3, sep = '<br/>'))
    }else{
      energiamesinter<-energiamesinter[energiamesinter$IDCliente==input$numcliente_reg,]
      tamenergiames<-dim(energiamesinter)
      for (i in 1:tamenergiames[1]) {
        str1 <- paste("Mes", energiamesinter[i,2])
        str2 <- paste("Energia Intercambiada",energiamesinter[i,3])
        str3 <- paste("Energia no Intercambiada",energiamesreal[i,3])
        str4 <- paste("Precio Total (Euros)",abs(saldomes[i,3])+abs(saldorealmes[i,3]))
      }

      HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    }
  })

  output$tabla = renderDataTable({
    if (is.null(vtabla$data)) return()
    vtabla$data
  }, options = list(pageLength=5))

  output$plot_sem2 <- renderPlot({
    if (is.null(w$data)) return()
    if (is.null(input$radio_sem2)) return()
    cliente<-input$num_cliente_sem2
    barplot(w$data,main=cliente,
            ylab="Energia Consumida en un dia(W)",
            xlab="Hora", col="blue",xlim=c(0,25))
  })

  output$plot_sem1 <- renderPlot({
    if (is.null(w_g$data)) return()
    if (is.null(input$radio_sem1)) return()
    # if (is.null(input$radio_sem11)) return()
    cliente<-input$num_cliente_sem1
    barplot(w_g$data,main=cliente,
            ylab="Energia Consumida/Producida en un dia(W)",
            xlab="Hora", col="blue",xlim=c(0,25))
  })
  output$plot_sem3 <- renderPlot({
    if (is.null(w_a$data)) return()
    # if (is.null(input$radio_sem3)) return()
    # if (is.null(input$radio_sem31)) return()
    cliente<-input$num_cliente_sem3
    barplot(w_a$data,main=cliente,
            ylab="Energia Consumida/Producida en un dia(W)",
            xlab="Hora", col="blue",xlim=c(0,25))
  })
  output$plot_sem3a <- renderPlot({
    if (is.null(w_3a$data)) return()
    # if (is.null(input$radio_sem3)) return()
    # if (is.null(input$radio_sem31)) return()
    cliente<-input$num_cliente_sem3
    potencia_alm<-datosclientes[datosclientes$num_cliente==cliente,"potencia_alm"]
    barplot(w_3a$data*100/(potencia_alm*1000),main=cliente,
            ylab="Energia almacenada en un dia(W)",
            xlab="Hora", col="blue",xlim=c(0,25))
  })

  output$plot <- renderPlot({
    if (is.null(v$data)) return()

    cliente<-as.numeric(input$numcliente2)

    barplot(v$data,main=cliente,
            ylab="Energia Generada (W)",
            xlab="Hora", col="blue",xlim=c(0,25))
  })

  output$plot2 <- renderPlot({
    # if (is.null(input$radio3)) return()
    # if((input$radio3 != 1)&(input$radio3 != 2)){
    # v$data <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    # }
    if (is.null(vcon$data)) return()

    cliente<-as.numeric(input$numcliente3)

    barplot(vcon$data,main=cliente,
            ylab="Energia Consumida (W)",
            xlab="Hora", col="blue",xlim=c(0,25))
  })
  output$plot3 <- renderPlot({
    # if (is.null(input$radio4)) return()
    # v$data <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    if (is.null(valm$data)) return()


    cliente<-as.numeric(input$numcliente4)

    barplot(valm$data,main=cliente,
            ylab="Estado de Almacenamiento (%)",
            xlab="Hora", col="blue",xlim=c(0,25))
  })

  output$plot_his <- renderPlot({
    if (is.null(vhis$data)) return()
    # if (is.null(input$radio2)) return()
    cliente<-input$numcliente_his
    barplot(vhis$data,main=cliente,
            ylab="Curva Media (W)",
            xlab="Hora", col="blue",xlim=c(0,24))
  })


  observeEvent(input$action,{
    pal <- colorpal()
    if (input$select==1){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,28]
    }
    if (input$select==2){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,29]
    }
    if (input$select==3){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,30]
    }
    if (input$select==4){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,31]
    }
    if (input$select==5){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,32]
    }
    if (input$select==6){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,33]
    }
    if (input$select==7){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,34]
    }
    if (input$select==8){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,35]
    }
    if (input$select==9){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,36]
    }
    if (input$select==10){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,37]
    }
    if (input$select==11){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,38]
    }
    if (input$select==12){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,39]
    }
    if (input$select==13){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,40]
    }
    if (input$select==14){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,41]
    }
    if (input$select==15){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,42]
    }
    if (input$select==16){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,43]
    }
    if (input$select==17){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,44]
    }
    if (input$select==18){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,45]
    }
    if (input$select==19){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,46]
    }
    if (input$select==20){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,47]
    }
    if (input$select==21){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,48]
    }
    if (input$select==22){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,49]
    }
    if (input$select==23){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,50]
    }
    if (input$select==24){
      hora<-pruebamapa[pruebamapa$Fecha == input$date,51]
    }
    if (input$select==25){
      hora<-apply(pruebamapa[pruebamapa$Fecha==input$date,28:51],2,sum)
    }

    leafletProxy("mapa", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(lat = ~ Latitud,lng = ~ Longitud,radius = ~PC01*2, weight = 1, color = "#777777",
                 fillColor = ~pal(hora), fillOpacity = 0.7, popup = ~paste(IDCliente,PC01,hora)
      )
  })

}

shinyApp(ui, server)
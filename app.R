#Cargue las librerias necesaria
library(DT)
library(sf)
library(sp)
library(maps)
library(png)
library(tmap)
library(dplyr)
library(ggmap)
library(shiny)
library(tidyr)
library(ggtext)
library(plotly)
library(ggplot2)
library(stringr)
library(ggthemes)
library(rsconnect)
library(tidyverse)
library(ggcorrplot)
library(rstudioapi)
library(tidygeocoder)
library(rgeoboundaries)
library(shinydashboard)
library(shinycssloaders)# to add a loader while graph is populating

conct_catacter<- function(caracteres ){
  caracteres<- unique(caracteres)
  caracteres_<-caracteres[1]  
  if (length(caracteres)>1 ){
    
    for (i in caracteres[2:length(caracteres)] ){
      caracteres_<-paste(caracteres_,i,sep="," )
    }
  }
  return(caracteres_)
}

nombres_c<-c("IdInmueble"        ,     "Referencia" ,           "NoContrato"         ,   "NoCCostos"    ,
             "CentroCostos"     ,      "Ciudad"      ,          "Direccion"          ,    "palabras_apa",           "Lugar"    ,             
             "localizaciones.address" ,"localizaciones.lat" ,    "localizaciones.long" ,  
             "Vr.Canon"          ,     "Vr.Administracion" ,     "Valor.Iva"    ,         
             "Propietario"     ,       "P.Cedula"  ,             "Arrendatario"   ,       
             "A.Cedula"    ,           "Aseguradora"     ,       "NoSolicitud",           
             "Direcciones_c")
#datos_completos<-read.csv2("data_orden/inmuebles_23.csv",sep=";", header =TRUE)
datos_completos<-read.csv2("BD/inmuebles_app.csv",sep=";", header =TRUE)
datos_completos$Lugar<-str_trim(gsub("  "," ",datos_completos$Lugar))
localizaciones_join<-read.csv2("BD/localizacion_inmuebles.csv",header=TRUE)
localizaciones_join<-unique(localizaciones_join[,names(localizaciones_join)])
localizaciones_join$conector<-paste(localizaciones_join$Direcciones_prueba,
                                    localizaciones_join$Ciudad,sep=' ')
localizaciones_join<-localizaciones_join %>%
  group_by(conector)%>%
  summarise(localizaciones.lat=mean(localizaciones.lat),
            localizaciones.long=mean(localizaciones.long))
  

datos_completos$conector<-paste(datos_completos$Direcciones_prueba,
                                datos_completos$Ciudad,sep=' ')
datos_completos<-merge(x = datos_completos, y = localizaciones_join, 
                       by = "conector")
datos_completos$localizaciones.address<-datos_completos$conector

#datos_completos$Direcciones_c<-datos_completos$conector
filtro<-datos_completos$Admon..Inc.=='True'
datos_completos[filtro,'Vr.Canon'] <- datos_completos[filtro,'Vr.Canon']-datos_completos[filtro,'Vr.Administracion']
datos<-datos_completos[datos_completos$Bloqueado=='False',nombres_c]
datos_b<-datos_completos[datos_completos$Bloqueado=='True',nombres_c]

direccion_unique <- datos %>%
  group_by(Direcciones_c) %>%
  summarise( Nombre_del_Lugar = conct_catacter(Lugar),Total_de_apartamentos = n(),
             Centro_de_Costos = conct_catacter(CentroCostos),
             Ciudad = conct_catacter(Ciudad),
             Valor.min = min(Vr.Canon),Valor.max = max(Vr.Canon), 
             Valor.prom = mean(Vr.Canon),
             Valor.promad = mean(Vr.Administracion),
             Latitud = mean(localizaciones.lat),
             Longitud = mean(localizaciones.long)) # cuenta la cantidad de filas que hay por esa agrupación

direccion_unique_b <- datos_b %>%
  group_by(Direcciones_c) %>%
  summarise( Nombre_del_Lugar = conct_catacter(Lugar),Total_de_apartamentos = n(),
             Centro_de_Costos = conct_catacter(CentroCostos),
             Ciudad = conct_catacter(Ciudad),
             Valor.min = min(Vr.Canon),Valor.max = max(Vr.Canon), 
             Valor.prom = mean(Vr.Canon),
             Valor.promad = mean(Vr.Administracion),
             Latitud = mean(localizaciones.lat),
             Longitud = mean(localizaciones.long)) # cuenta la cantidad de filas que hay por esa agrupación



# Cambiar las variables a factores
datos$IdInmueble <- as.character(datos$IdInmueble)
# creamos Indices
direccion_unique$Indice<- 1:nrow(direccion_unique)
col_t<-ncol(direccion_unique)
direccion_unique<-direccion_unique[,c(col_t,1:col_t-1)]
direccion_unique_b$Indice<- 1:nrow(direccion_unique_b)
col_t<-ncol(direccion_unique_b)
direccion_unique_b<-direccion_unique_b[,c(col_t,1:col_t-1)]
datos$NoContrato <- as.character(datos$NoContrato)
datos$NoCCostos <- as.factor(datos$NoCCostos)
datos$NoSolicitud <- as.character(datos $NoSolicitud)
datos$CentroCostos <- as.factor(datos$CentroCostos)
datos$Ciudad <- as.factor(datos$Ciudad)
datos$Aseguradora <- as.factor(datos$Aseguradora)
datos <- datos %>%
  rename(Latitud = localizaciones.lat)
datos <- datos %>%
  rename(Longitud = localizaciones.long) 
datos <- datos %>% rename(c( "Apt/Casa" = "palabras_apa" ,  "localizaciones" =
                               "localizaciones.address"))
datos$Tipo_de_Inmueble <- (datos$ Valor.Iva >0)

filtro=datos$Tipo_de_Inmueble
datos$Tipo_de_Inmueble[filtro==F]<-'Vivienda'
datos$Tipo_de_Inmueble[filtro]<-'Comercial'
datos_b$Tipo_de_Inmueble <- (datos_b$ Valor.Iva >0)

filtro=datos_b$Tipo_de_Inmueble
datos_b$Tipo_de_Inmueble[filtro==F]<-'Vivienda'
datos_b$Tipo_de_Inmueble[filtro]<-'Comercial'
#  Choices for selectInput 
c1 = datos %>% select(c(13:15))%>%
  names()

#####################
c3= unique(direccion_unique$Centro_de_Costos)
c4= names(direccion_unique)
n1<-length(c4)
c4=c4[c(1:(n1-2) ) ]
#Definición de la interfaz de usuario
ui <- fluidPage(
  
  dashboardPage(
    skin = "blue",
    title = "Inmuebles de Portada Inmobiliaria",
    dashboardHeader(title=span(img(src = "logo Portada.png", height = 35), "Inmuebles de Portada Inmobiliaria"),
                    titleWidth = 800, 
                    tags$li(class="dropdown",tags$a(href="https://portadainmobiliaria.com/", icon("building"), "Portada", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/estefania-echeverry-franco-932597232/" ,icon("linkedin"), "My Profile", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://github.com/EstefaniaEcheverry/Inmuebles", icon("github"), "Source Code", target="_blank"))
    ),
    dashboardSidebar(
      #sidebarmenu
      sidebarMenu(
        id="sidebar",
        fluidRow(style='height:5vh'),
        #first menuitem
        menuItem("Dataset", tabName="data", icon=icon("database")),
        conditionalPanel("input.sidebar == 'data'&& input.t1 == 'datos.1' ",
                         selectInput(inputId ="var0" ,label= "Seleccione por tipo de inmueble",
                                     choices =c('Activos','Bloqueados'))),  
        conditionalPanel("input.sidebar == 'data'&& input.t1 == 'datos.2' ",
                         selectInput(inputId ="var3" ,label= "Seleccione por tipo de inmueble",
                                     choices =c('Activos','Bloqueados'))),  
        conditionalPanel("input.sidebar == 'data'&& input.t1 == 'datos.3' ",
                         selectInput(inputId ="var4" ,label= "Seleccione por tipo de inmueble",
                                     choices =c('Activos','Bloqueados'))),  
        menuItem(text= "Visualization",tabName = "viz",icon = icon("chart-line")),
        conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trendscc'",
                         selectInput(inputId ="var7" ,label= "Seleccione por tipo de inmueble",
                                     choices =c('Activos','Bloqueados')), selected="Activos"),
        conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trendsc'",
                         selectInput(inputId ="var8" ,label= "Seleccione por tipo de inmueble",
                                     choices =c('Activos','Bloqueados')), selected="Activos"), 
        conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trendsap'",
                         selectInput(inputId ="var9" ,label= "Seleccione por tipo de inmueble",
                                     choices =c('Activos','Bloqueados')), selected="Activos"), 
        conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'",
                         selectInput(inputId ="var1" ,label= "Seleccione la variable X",
                                     choices =c1 ,selected="Vr.Canon")),
        conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' " ,
                         selectInput(inputId ="varx" ,label= "Seleccione la variable X",
                                     choices =c1 ,selected="Vr.Canon")),
        
        conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation'" ,
                         selectInput(inputId ="vary" ,label= "Seleccione la variable Y",
                                     choices =c1 ,selected="Vr.Administracion")),
        conditionalPanel("input.sidebar == 'viz' && input.t2 == 'boxpl'",
                         selectInput(inputId ="var2" ,label= "Seleccione la variable",
                                     choices =c1 ,selected="Vr.Canon")),
        menuItem(text= "Inmuebles Map",tabName = "map",icon = icon("map")),
        conditionalPanel("input.sidebar == 'map'",
                         selectInput(inputId ="var5" ,label= "Seleccione el Centro de Costos",
                                     choices =c3,multiple = TRUE )) ,
        conditionalPanel("input.sidebar == 'map'",
                         selectInput(inputId ="var6" ,label= "Seleccione una columna de descripción por punto",
                                     choices =c4,multiple = TRUE ))   
      )
    ),  
    dashboardBody(
      tabItems(
        # first tab item
        tabItem(tabName = "data",
                tabBox(id="t1",width= 12,
                       fluidRow(style='height:5vh'),
                       tabPanel(title="Información",icon = icon("address-card"),fluidRow(
                         column(width = 8, tags$img(src="image.png", width =400 , height = 200,alt ="Something went wrong",deleteFile=FALSE),
                                align = "center"),
                         column(width = 4, tags$br() ,
                                tags$p(" Son 5769 inmuebles controlados por los diferentes Centros de costos , 
                                     segun los datos 24 de estos inmuebles estan bloqueados o desactivados,
                                     es decir, existen 5745 inmuebles activos distribuidos por los centros 
                                     de costos; Los Colores maneja 1078 de estos inmuebles y Laureles 888
                                     inmuebles. ")
                         )
                       )
                       
                       
                       ), 
                       tabPanel(title="Datos",icon = icon("table"),value= "datos.1", DT::dataTableOutput("dataT")),
                       tabPanel(title="Estructura",icon = icon("uncharted"),value= "datos.2",verbatimTextOutput("structure")),
                       tabPanel(title="Resumen estadísticas",icon = icon("chart-pie"),value= "datos.3",verbatimTextOutput("summary"))
                )
        ),
        # second tab item or landing page here ..
        tabItem(tabName = "viz",
                tabBox(id="t2", width=20,
                       fluidRow(style='height:5vh'),
                       tabPanel(title="Centro de Costos",icon = icon("building"), value="trendscc",
                                fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1"),
                                                                      collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                         tags$div(align="center", box(tableOutput("low6"), title = textOutput("head2") ,
                                                                      collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))),
                                withSpinner(plotlyOutput("barcc")),
                                DT::dataTableOutput('canon_general')),
                       tabPanel(title="Ciudad",icon = icon("city"), value="trendsc",
                                fluidRow(tags$div(align="center", box(tableOutput("top5.1"), title = textOutput("head3"),
                                                                      collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                         tags$div(align="center", box(tableOutput("low5"), title = textOutput("head4") ,
                                                                      collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))),
                                withSpinner(plotlyOutput("barciu"))),
                       tabPanel(title="Aseguradora",icon = icon("address-book"), value="trendsap",
                                fluidRow(tags$div(align="center", box(tableOutput("top3"), title = textOutput("head5"),
                                                                      collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                         tags$div(align="center", box(tableOutput("low4"), title = textOutput("head6") ,
                                                                      collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))),
                                withSpinner(plotlyOutput("baraseg"))),
                       tabPanel(title="Distribución",icon = icon("chart-line"),target="", value="distro",
                                radioButtons(inputId ="inmueble_dis" , label = "Selecione tipo de inmueble"
                                             , choices = unique(datos$Tipo_de_Inmueble) , inline = TRUE),
                                withSpinner(plotlyOutput("histplot", height = "350px"))),
                       tabPanel(title="Box Plots",icon = icon("chart-line"),value="boxpl",
                                radioButtons(inputId ="inmueble_box" , label = "Selecione tipo de inmueble"
                                             , choices = unique(datos$Tipo_de_Inmueble) , inline = TRUE),
                                radioButtons(inputId ="inmueble_ab_box" , label = "Seleccione por tipo de inmueble"
                                             , choices = c('Activos','Bloqueados') , inline = TRUE),
                                withSpinner(plotlyOutput("boxplot", height = "350px"))
                       ),
                       tabPanel(title="Scatterplot ",icon =icon("chart-line"),value="relation",
                                radioButtons(inputId ="inmueble_scatter" , label = "Selecione tipo de inmueble"
                                             ,choices = unique(datos$Tipo_de_Inmueble) , inline = TRUE),
                                withSpinner(plotlyOutput("scatter")))
                )
        ),
        tabItem(tabName="map",
                tabBox(id="t3",width= 12,
                       tabPanel(title="Inmuebles",
                                fluidPage(
                                  fluidRow(
                                    br(),
                                    withSpinner(tmapOutput( outputId = "map_plot"))),br(),
                                  actionButton("reset_input", "Base de Datos", icon("fa-light fa-table-list"), 
                                               style="color: #fff; background-color:  #3c8dbc; border-color: #2e6da4"),
                                  br(),br(),
                                  
                                  fluidRow(
                                    ( DT::dataTableOutput('data_filtro')) )
                                  
                                )),
                       tabPanel(title="Bloqueados",
                                fluidPage(fluidRow(br(),
                                                   withSpinner(tmapOutput(outputId = "map_plot_b"))),br(),
                                          actionButton("reset_input_b", "Base de Datos", icon("fa-light fa-table-list"), 
                                                       style="color: #fff; background-color:  #3c8dbc; border-color: #2e6da4"),
                                          br(),br(),
                                          fluidRow(
                                            ( DT::dataTableOutput('data_filtro_b')) )
                                )),
                       tabPanel(title='Conjuntos',
                                fluidPage(fluidRow(DT::dataTableOutput('top_desc') )  ))
                )
        )
      )
    )
  )
  
)

# Definción de la funcionalidad lógica de la aplicación (servidor)

server <- function(input, output, session) {
  # stucture 
  #####################
  ################################################################
  # Crear mapa con geoboundaries escogiendo solo los municipios
  
  area_metropolitana <- geoboundaries(country = "COLOMBIA", adm_lvl = 2)%>%
    dplyr::filter(is.element(shapeName,c("MEDELLÃN", # para adm_lvl =2 los municipios
                                         "BELLO",    # estan en mayuscula y no estan
                                         "COPACABANA",# en UTF-8 
                                         "ENVIGADO",
                                         "CALDAS", # hay dos caldas en colombia
                                         "ITAGÃœÃ", # itagui
                                         "LA ESTRELLA", 
                                         "SABANETA",
                                         "SAN JERÃ“NIMO",
                                         "RIONEGRO",
                                         "BOGOTÃ, D.C.",
                                         "LA CEJA",
                                         "MARINILLA"
    ) ) 
    ) %>%
    st_transform(crs = 3857)
  # Cambiandoles los nombres por como se escribe
  area_metropolitana$shapeName<-c("CALDAS_NO",# no son de antioquia
                                  "MARINILLA",
                                  "RIONEGRO_NO",
                                  "ENVIGADO",
                                  "ITAGUI",
                                  "LA CEJA",
                                  "CALDAS",
                                  "SAN JERONIMO",
                                  "RIONEGRO" ,
                                  "BOGOTA",
                                  "MEDELLIN",
                                  "COPACABANA",
                                  "BELLO",
                                  "SABANETA",
                                  "LA ESTRELLA"
  )
  # Eliminar el Caldas que no es de Antioquia
  area_metropolitana <- area_metropolitana[!(area_metropolitana$shapeName == "CALDAS_NO" & 
                                               area_metropolitana$shapeName == "RIONEGRO_NO"),]
  
  
  ################## Mapas
  
  # Pasar los datos a formato sf tomando algunos datos de la base
  direcciones_sf <- direccion_unique %>% 
    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
    st_set_crs(value = 4326) %>%
    st_transform(crs = 3857) %>%
    st_intersection(area_metropolitana)
  
  output$top_desc<-DT::renderDataTable({
    direccion_unique %>%
      dplyr::filter(direccion_unique$Nombre_del_Lugar!='' &
               (is.null(input$var5) |
                  is.element(direccion_unique$Centro_de_Costos, input$var5) ))%>%
      select(c(Nombre_del_Lugar,Direcciones_c,Centro_de_Costos,Total_de_apartamentos) ) %>%
      dplyr::arrange(desc(Total_de_apartamentos) ) 
    
  }, options = list(scrollX = TRUE,lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#ffffff' });",
                      "}"),
                    columnDefs=list(list(className='dt-center',targets="_all"))
  ),
  filter = "top",
  selection = 'multiple',
  style = 'bootstrap',
  class = 'cell-border stripe',
  rownames = FALSE)
  
  output$map_plot<- renderTmap({
    if (is.null(input$var5) ){
      direcciones_sf_filtro<-direcciones_sf
    }
    else{
      direcciones_sf_filtro<-direcciones_sf[is.element(direcciones_sf$Centro_de_Costos,input$var5),]      
    }
    if(is.null(input$var6)){
      columnas_mostrar<-c4
    }
    else{
      columnas_mostrar<-input$var6
      
    }
    # Mapa del aréa metropolitana con los inmuebles 
    tmap_mode('view') %>%
      tm_shape(shp = direcciones_sf_filtro)+ # coordenadas lat long
      tm_dots(size = 0.05,col = "Centro_de_Costos",popup.vars=columnas_mostrar)+
      tmap_options(max.categories = 32)
    
  })  
  observeEvent(input$map_plot_marker_click,{
    output$data_filtro<- DT::renderDataTable({
      click<-input$map_plot_marker_click
      direccion_print<-direccion_unique[paste('X',direccion_unique$Indice,sep="")==click$id ,
                                        'Direcciones_c']
      datos_print<- datos[datos$Direcciones_c==direccion_print$Direcciones_c,]  
      datos_print    
    }, options = list(scrollX = TRUE,lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#ffffff' });",
                        "}"),
                      columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE )
    
  })
  observeEvent(input$reset_input,{
    output$data_filtro<- DT::renderDataTable({
      datos
    }, options = list(scrollX = TRUE,lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#ffffff' });",
                        "}"),
                      columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE)
  })
  
  
  # Pasar los datos a formato sf tomando algunos datos bloqueados
  direcciones_sf_b <- direccion_unique_b %>% 
    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
    st_set_crs(value = 4326) %>%
    st_transform(crs = 3857) %>%
    st_intersection(area_metropolitana)
  
  output$map_plot_b<-renderTmap({
    if (is.null(input$var5) ){
      direcciones_sf_filtro_b<-direcciones_sf_b
    }
    else{
      direcciones_sf_filtro_b<-direcciones_sf_b[is.element(direcciones_sf_b$Centro_de_Costos,input$var5),]      
    }
    if(is.null(input$var6)){
      columnas_mostrar<-c4
    }
    else{
      columnas_mostrar<-input$var6
      
    }
    # Mapa del aréa metropolitana con los inmuebles 
    tmap_mode('view') %>%
      tm_shape(shp = direcciones_sf_filtro_b)+ # coordenadas lat long
      tm_dots(size = 0.05,col = "Centro_de_Costos",popup.vars=columnas_mostrar)+
      tmap_options(max.categories = 32)
  })
  observeEvent(input$map_plot_b_marker_click,{
    output$data_filtro_b<- DT::renderDataTable({
      click<-input$map_plot_b_marker_click
      direccion_print_b<-direccion_unique_b[paste('X',direccion_unique_b$Indice,sep="")==click$id ,
                                            'Direcciones_c']
      datos_print_b<- datos_b[datos_b$Direcciones_c==direccion_print_b$Direcciones_c,]  
      datos_print_b
    }, options = list(scrollX = TRUE,lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#ffffff' } );",
                        "}"),
                      columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE )
    
  })
  observeEvent(input$reset_input_b,{
    output$data_filtro_b<- DT::renderDataTable({
      datos_b
    }, options = list(scrollX = TRUE,lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#ffffff' });",
                        "}"),
                      columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE)
  })
  
  
  
  
  # reactive values for map
  
  
  output$structure <- renderPrint(
    if (input$var3=='Activos'){
      datos %>% str()
      
    }
    else{
      datos_b    %>% str()
    }
    
  )
  
  # Summary 
  output$summary <- renderPrint(
    # Resumen de los datos 
    if (input$var4=='Activos'){
      datos %>% summary()
    }
    else{
      datos_b    %>% summary()
    }
  )
  # DataTable
  output$dataT <- DT::renderDataTable(
    DT::datatable({
      if (input$var0=='Activos'){
        datos
      }
      else{
        datos_b
        
      }}
      , options = list(scrollX = TRUE,lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#ffffff' });",
                         "}"),
                       columnDefs=list(list(className='dt-center',targets="_all"))
      ),
      filter = "top",
      selection = 'multiple',
      style = 'bootstrap',
      class = 'cell-border stripe',
      rownames = FALSE ))
  # DataTable
  output$dataInmu <- renderDataTable(
    direccion_unique
  )
  
  
  # Stacked histogram and boxplot
  output$histplot <- renderPlotly({
    
    datos_<-datos[ datos[,input$var1]>0 & is.element(datos$Tipo_de_Inmueble, input$inmueble_dis), ]
    n_re<-nrow(datos[is.element(datos$Tipo_de_Inmueble, input$inmueble_dis), ])
    registos_0<-n_re-nrow(datos_)
    print(registos_0)
    texto_add<-''
    if (registos_0>0){
      texto_add<-paste(as.character(registos_0),' registros en 0 de ',as.character(n_re),sep='')
    }
    if (nrow(datos_)>0){
      p1 <- datos_ %>%
        plot_ly( ) %>%
        add_histogram(~get(input$var1)) %>%
        layout(xaxis = list(title= ~get(input$var1)))
      # Box plot Vr. Canon
      p2 <- datos_ %>% 
        plot_ly() %>% 
        add_boxplot(~get(input$var1)) %>%
        layout(yaxis =list(showticklabels = F))
      # Stacking Plots Vr. Canon
      subplot(p2, p1, nrows=2,shareX = TRUE) %>%
        hide_legend() %>%
        layout(title=paste("Gráfico de distribución - Histograma y Boxplot \n ",texto_add,sep=''  ) ,
               yaxis=list(title="Frecuencia"))
    }
    else{
      ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = texto_add) + 
        theme_void()
    }
    
  })
  
  
  
  
  ### Bar Charts - State wise trend
  
  #################
  sum_pesos<-function(vector_){
    total=sum(vector_)
    paste('$' ,formatC(total,big.mark = '.',decimal.mark=',',format='fg'))
  }
  var_condicion<-function(vector_,vector_condicion,condicion){
    vector_resultado<-rep(0,length(vector_))
    filtro=vector_condicion==condicion
    vector_resultado[filtro]<-vector_[filtro]
    return(vector_resultado)
  }
  
  # Tabla de los Centros de costos Vivienda y Comercial (Acitvos)
  tabla_centroc <- datos %>% 
    group_by(CentroCostos) %>%
    dplyr::mutate(Vr.canon_c=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Comercial'),
                  Vr.canon_v=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Vivienda')) %>%
    summarise(Total.c=n(), Vr.Canon=sum_pesos(Vr.Canon), 
              N_Vivienda = sum(Tipo_de_Inmueble=="Vivienda"),
              N_Comercial = sum(Tipo_de_Inmueble=="Comercial"),
              Vr.canon_c=sum_pesos(Vr.canon_c),
              Vr.canon_v=sum_pesos(Vr.canon_v))  %>%  
    dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1))%>%
    dplyr::mutate(General=paste('\n Total canon: ',Vr.Canon,'.\n Vivienda: \n Frec: ',
                                N_Vivienda,'. \n Canon Vivienda: ',Vr.canon_v,'.\n Comercial: \n Frec: ',N_Comercial,
                                '. \n Canon Comercial: ',Vr.canon_c,'.\n Porcentaje: ',Porcentaje,'.',sep=''),
                  Var_prop=paste("   ",Total.c," ", "", "\n",Porcentaje,"%"))
  
  ##################################################
  # Activos general por tipo de inmueble
  tabla_centroc_general <- datos %>% 
    dplyr::mutate(Vr.canon_c=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Comercial'),
                  Vr.canon_v=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Vivienda'),
                  Vr.Administracion_c=var_condicion(Vr.Administracion,Tipo_de_Inmueble,'Comercial'),
                  Vr.Administracion_v=var_condicion(Vr.Administracion,Tipo_de_Inmueble,'Vivienda'),
                  Valor.Iva_c=var_condicion(Valor.Iva,Tipo_de_Inmueble,'Comercial'),
                  Valor.Iva_v=var_condicion(Valor.Iva,Tipo_de_Inmueble,'Vivienda')) %>%
    summarise(Total.c=n(), Vr.Canon=sum_pesos(Vr.Canon), 
              Vr.Administracion=sum_pesos(Vr.Administracion),
              Valor.Iva =sum_pesos(Valor.Iva),
              N_Vivienda = sum(Tipo_de_Inmueble=="Vivienda"),
              N_Comercial = sum(Tipo_de_Inmueble=="Comercial"),
              Vr.canon_c=sum_pesos(Vr.canon_c),
              Vr.canon_v=sum_pesos(Vr.canon_v),
              Vr.Administracion_c=sum_pesos(Vr.Administracion_c),
              Vr.Administracion_v=sum_pesos(Vr.Administracion_v),
              Valor.Iva_c=sum_pesos(Valor.Iva_c))
  tabla_centro_general<-data.frame(Discriminado=c('Vivienda:', '','', 'Comercial:', '','','', 'Total:','','',''),
                                   Medida=c(c('Frecuencia:','Valor Canon:','Valor Administracion:'),
                                            rep(c('Frecuencia:','Valor Canon:','Valor Administracion:', 'Valor Iva:'),2)),
                                   Valor= c(tabla_centroc_general$N_Vivienda,tabla_centroc_general$Vr.canon_v,
                                            tabla_centroc_general$Vr.Administracion_v,
                                            #tabla_centroc_general$Valor.Iva_v,
                                            tabla_centroc_general$N_Comercial , tabla_centroc_general$Vr.canon_c ,
                                            tabla_centroc_general$Vr.Administracion_c,
                                            tabla_centroc_general$Valor.Iva_c,
                                            tabla_centroc_general$Total.c , tabla_centroc_general$Vr.Canon,
                                            tabla_centroc_general$Vr.Administracion,
                                            tabla_centroc_general$Valor.Iva) )
  ############################################
  # Tabla de los Centros de costos Vivienda y Comercial (Bloqueados)
  tabla_centroc1 <- datos_b %>% group_by(CentroCostos) %>%
    summarise(Total.c=n(),Vr.Canon=sum_pesos(Vr.Canon))  %>%  
    dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1))
  
  # Bloqueados general por tipo de inmueble
  tabla_centroc_general_b <- datos_b %>% 
    summarise(Total.c=n(), Vr.Canon=sum_pesos(Vr.Canon), 
              Vr.Administracion=sum_pesos(Vr.Administracion))
  tabla_centro_general_b<-data.frame(Discriminado=c('Total:','',''),
                                     Medida=c('Frecuencia:','Valor Canon:','Valor Administracion:'),
                                     Valor= c(
                                       tabla_centroc_general_b$Total.c , tabla_centroc_general_b$Vr.Canon,
                                       tabla_centroc_general_b$Vr.Administracion) )
  ############################################
  
  # Barplot de Frecuencia de la variable Centro de Costos
  output$barcc <- renderPlotly({
    if (input$var7=='Activos'){
      # Gráfica de Centro total de Costos
      p4 <- tabla_centroc %>% ggplot(aes(x=CentroCostos, y =Total.c, 
                                         fill=(CentroCostos),label=General )) +
        
        geom_bar(width = 0.9, stat="identity")+  
        
        ylim(c(0,1150))+
        labs(x="Centros de costos" , y= "Frecuencia",title = "Diagrama de barras para la variable Centro de costos") +   
        labs(fill = "")+                                         
        
        geom_text(aes(label=Var_prop),  
                  vjust=1.3,                         
                  color="black",                     
                  hjust=0.5,                         
                  position = position_dodge(0.9),    
                  angle=0,                           
                  size=4.0                            
        ) +  
        scale_fill_discrete(name = "Centro de costos", labels = c("Laureles", "Sabaneta", "Poblado" ,"Colores", "Envigado", "Itagui", "Centro", "Bello",
                                                                  "La estrella", "San Antonio Pr" , "Calasanz","Fontibon","Rionegro")) +    
        
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) + 
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      
      ggplotly(p4)
    }
    else{
      
      # Gráfica de Centro total de Costos
      p4 <- tabla_centroc1 %>% ggplot(aes(x=CentroCostos, y =Total.c,
                                          fill=CentroCostos,label=Vr.Canon )) +
        geom_bar(width = 0.9, stat="identity")+  
        
        ylim(c(0,25))+
        labs(x="Centros de costos", y= "Frecuencia",title = "Diagrama de barras para la variable Centro de costos") +   
        labs(fill = "")+                                         
        
        geom_text(aes(label=paste0(Total.c," ", "", "\n", Porcentaje, "%")),  
                  vjust=1.3,                         
                  color="black",                     
                  hjust=0.5,                         
                  position = position_dodge(0.9),    
                  angle=0,                           
                  size=4.0                            
        ) +  
        scale_fill_discrete(name = "Centro de costos", labels = c("Laureles", "Sabaneta", "Poblado" ,"Colores", "Envigado", "Itagui", "Centro", "Bello",
                                                                  "La estrella", "San Antonio Pr" , "Calasanz","Fontibon","Rionegro")) +    
        
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) + 
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      
      ggplotly(p4)
      
    }
    
  })
  #############
  output$canon_general<-DT::renderDataTable(
    if (input$var7=='Activos'){
      tabla_centro_general
    }
    else{
      tabla_centro_general_b
    }
  )
  
  # Tabla total de cada ciudad y porcentajes por Aseguradora
  tabla_ciudad <- datos %>% group_by(Ciudad) %>%
    dplyr::mutate(Vr.canon_c=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Comercial'),
                  Vr.canon_v=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Vivienda')) %>%
    summarise(Total.ciudad=n(), Vr.Canon=sum_pesos(Vr.Canon), 
              N_Vivienda = sum(Tipo_de_Inmueble=="Vivienda"),
              N_Comercial = sum(Tipo_de_Inmueble=="Comercial"),
              Vr.canon_c=sum_pesos(Vr.canon_c),
              Vr.canon_v=sum_pesos(Vr.canon_v))  %>%  
    dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1))%>%
    dplyr::mutate(General=paste('\n Total canon: ',Vr.Canon,'.\n Vivienda: \n Frec: ',
                                N_Vivienda,'. \n Canon Vivienda: ',Vr.canon_v,'.\n Comercial: \n Frec: ',N_Comercial,
                                '. \n Canon Comercial: ',Vr.canon_c,'.\n Porcentaje: ',Porcentaje,'.',sep=''),
                  Var_prop=paste("   ",Total.ciudad," ", "", "\n",Porcentaje,"%"))
  
  
  # Tabla total de cada ciudad y porcentajes por Aseguradora
  tabla_ciudad1 <- datos_b %>% group_by(Ciudad) %>%
    summarise(Total.ciudad=n(),Vr.Canon=sum_pesos(Vr.Canon))  %>%  
    dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1)) %>%
    dplyr::mutate(Var_prop=paste("   ",Total.ciudad," ", "", "\n",Porcentaje,"%"))
  
  # Barplot de Frecuencia de la variable Ciudad
  output$barciu <- renderPlotly({
    if (input$var8=='Activos'){
      
      # Gráfica de Centro total de Costos
      p5 <- tabla_ciudad %>% ggplot(aes(x=Ciudad, y = Total.ciudad, fill=Ciudad,
                                        label=General)) + 
        geom_bar( stat="identity"              
        )+  
        
        ylim(c(0,3050))+
        labs(x="Ciudad", y= "Frecuencia", title= "Diagrama de barras para la variable Ciudad") +   
        
        scale_fill_discrete(name = "Ciudad", labels = c("Medellin", "Sabaneta", "Envigado", "Itagui", "Bello", 
                                                        "La estrella", "San Jeronimo", "Caldas", "Copacabana",
                                                        "San Antonio Pr","Bogota","Rionegro", "La Ceja", "Marinilla")) +                                            
        
        geom_text(aes(label=Var_prop),  
                  vjust=1.3,                         
                  color="black",                     
                  hjust=0.5,                         
                  position = position_dodge(0.9),    
                  angle=0,                           
                  size=3.0                           
        ) +
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) + 
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      ggplotly(p5)
    }
    else{
      # Gráfica de Centro total de Costos
      p6 <- tabla_ciudad1 %>% ggplot(aes(x=Ciudad, y = Total.ciudad, fill=Ciudad,
                                         label=Vr.Canon )) + 
        geom_bar( stat="identity"              
        )+  
        
        ylim(c(0,60))+
        labs(x="Ciudad", y= "Frecuencia", title= "Diagrama de barras para la variable Ciudad") +   
        
        scale_fill_discrete(name = "Ciudad", labels = c("Medellin", "Sabaneta", "Envigado", "Itagui", "Bello", 
                                                        "La estrella", "San Jeronimo", "Caldas", "Copacabana", 
                                                        "San Antonio Pr","Bogota","Rionegro","La Ceja", "Marinilla")) +                                            
        
        geom_text(aes(label=Var_prop),  
                  vjust=1.3,                         
                  color="black",                     
                  hjust=0.5,                         
                  position = position_dodge(0.9),    
                  angle=0,                           
                  size=3.0                           
        ) +
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) + 
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      ggplotly(p6)
    }
    
  })
  
  ####################
  # Tabla total de cada ciudad y porcentajes por Aseguradora
  tabla_aseg <- datos %>% group_by(Aseguradora) %>%
    dplyr::mutate(Vr.canon_c=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Comercial'),
                  Vr.canon_v=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Vivienda')) %>%
    summarise(Total.aseg=n(), Vr.Canon=sum_pesos(Vr.Canon), 
              N_Vivienda = sum(Tipo_de_Inmueble=="Vivienda"),
              N_Comercial = sum(Tipo_de_Inmueble=="Comercial"),
              Vr.canon_c=sum_pesos(Vr.canon_c),
              Vr.canon_v=sum_pesos(Vr.canon_v))  %>%  
    dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1))%>%
    dplyr::mutate(General=paste('\n Total canon: ',Vr.Canon,'.\n Vivienda: \n Frec: ',
                                N_Vivienda,'. \n Canon Vivienda: ',Vr.canon_v,'.\n Comercial: \n Frec: ',N_Comercial,
                                '. \n Canon Comercial: ',Vr.canon_c,'.\n Porcentaje: ',Porcentaje,'%.',sep=''),
                  Var_prop=paste("   ",Total.aseg," ", "", "\n",Porcentaje,"%"))
  
  # Tabla total de cada ciudad y porcentajes por Aseguradora
  tabla_aseg1 <- datos_b %>% group_by(Aseguradora) %>%
    summarise(Total.aseg=n(),Vr.Canon=sum_pesos(Vr.Canon))  %>%  
    dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1))
  
  # Barplot de Frecuencia de la variable Aseguradora
  output$baraseg <- renderPlotly({
    
    if (input$var9=='Activos'){
      
      
      # Gráfica de Aseguradora
      p8 <- tabla_aseg %>% ggplot(aes(x=Aseguradora, y = Total.aseg, fill=Aseguradora ,label=General)) + 
        geom_bar(width = 0.9, stat="identity",position = position_dodge())+  
        ylim(c(0,5000))+
        labs(x="Aseguradoras", y= "Frecuencia",title= "Diagrama de barras para la variable Aseguradora") +   
        labs(fill = "")+                                         
        
        geom_text(aes(label=Var_prop),  
                  vjust=1.3,                         
                  color="black",                     
                  hjust=0.5,                         
                  position = position_dodge(0.9),    
                  angle=0,                           
                  size=3.0                            
        ) +  
        scale_fill_discrete(name = "Aseguradora", labels = c("El libertador" , "El libertador 14 lim", "El libertador lim 12", "El libertador lim 6", "El libertador lim  18", "Portada Inmobiliaria SAS" , "sin Seguro")) +    
        
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +  
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
      ggplotly(p8)
    }
    else{
      
      # Gráfica de Aseguradora
      p9 <- tabla_aseg1 %>% ggplot(aes(x=Aseguradora, y = Total.aseg, fill=Aseguradora,label=Vr.Canon )) + 
        geom_bar(width = 0.9, stat="identity",position = position_dodge())+  
        ylim(c(0,100))+
        labs(x="Aseguradoras", y= "Frecuencia",title= "Diagrama de barras para la variable Aseguradora") +   
        labs(fill = "")+                                         
        
        geom_text(aes(label=paste0(Total.aseg," ", "", "\n", Porcentaje, "%")),  
                  vjust=1.3,                         
                  color="black",                     
                  hjust=0.5,                         
                  position = position_dodge(0.9),    
                  angle=0,                           
                  size=3.0                            
        ) +  
        scale_fill_discrete(name = "Aseguradora", labels = c("El libertador" , "El libertador 14 lim", "El libertador lim 12", "El libertador lim 6", "El libertador lim  18", "Portada Inmobiliaria SAS" , "sin Seguro")) +    
        
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +  
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
      ggplotly(p9)
    }
    
  })
  
  # Codigo del server para los resumenes que se abren
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("Los Centros de Costos con mas Inmuebles")
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("Los Centros de Costos con menos Inmuebles")
  )
  
  # Top 5  con mayor # de inmuebles Centro Costos 
  output$top5 <- renderTable({
    if (input$var7=='Activos'){
      # Tabla total de cada ciudad y porcentajes por Centro de Costos
      tabla_centroc  %>%
        select(CentroCostos, Total.c, Vr.Canon) %>%
        arrange(desc((Total.c))) %>%
        dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1)) %>%
        head(5)
      
      
    }
    else{
      # Tabla total de cada ciudad y porcentajes por Centro de Costos
      tabla_centroc1  %>%
        select(CentroCostos, Total.c, Vr.Canon) %>%
        arrange(desc((Total.c))) %>%
        dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1)) %>%
        head(2)
      
    }
    
  })
  
  # Top 6 con menor # de inmuebles Centro Costos 
  output$low6 <- renderTable({
    if (input$var7=='Activos'){
      # Tabla total de cada ciudad y porcentajes por Centro de Costos
      tabla_centroc %>%
        select(CentroCostos, Total.c, Vr.Canon) %>%
        arrange(Total.c) %>%
        dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1)) %>%
        head(7)
      
    }
    else{
      # Tabla total de cada ciudad y porcentajes por Centro de Costos
      tabla_centroc1 %>%
        select(CentroCostos, Total.c, Vr.Canon) %>%
        arrange(Total.c) %>%
        dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1)) %>%
        head(7)
    } 
    
  })
  
  # Rendering the box header  
  output$head3 <- renderText(
    paste("Los municipios con mas inmuebles")
  )
  
  # Rendering the box header 
  output$head4 <- renderText(
    paste("Los municipios con menos inmuebles")
  )
  
  # Top 5  con mayor # de inmuebles Ciudad
  output$top5.1 <- renderTable({
    if (input$var8=='Activos'){
      tabla_ciudad  %>%
        select(Ciudad, Total.ciudad, Vr.Canon) %>%
        arrange(desc((Total.ciudad))) %>%
        dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1)) %>%
        head(5)
    }
    else{
      tabla_ciudad1  %>%
        select(Ciudad, Total.ciudad, Vr.Canon) %>%
        arrange(desc((Total.ciudad))) %>%
        dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1)) %>%
        head(1) 
    }
  })
  
  # Top 5 con menor # de inmuebles Ciudad
  output$low5 <- renderTable({
    if (input$var8=='Activos'){
      tabla_ciudad %>%
        select(Ciudad, Total.ciudad, Vr.Canon) %>%
        arrange(Total.ciudad) %>%
        dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1)) %>%
        head(6)
    }
    else{
      tabla_ciudad1 %>%
        select(Ciudad, Total.ciudad, Vr.Canon) %>%
        arrange(Total.ciudad) %>%
        dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1)) %>%
        head(5)
    }
    
  })
  
  # Rendering the box header  
  output$head5 <- renderText(
    paste("Las aseguradoras con mas inmuebles")
  )
  
  # Rendering the box header 
  output$head6 <- renderText(
    paste("Las aseguradoras con menos inmuebles")
  )
  
  # Top 3 con mayor # de inmuebles Aseguradoras 
  output$top3 <- renderTable({
    if (input$var9=='Activos'){
      tabla_aseg  %>%
        select(Aseguradora, Total.aseg, Vr.Canon) %>%
        arrange(desc((Total.aseg))) %>%
        dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1)) %>%
        head(3)
    }
    else{
      tabla_aseg1  %>%
        select(Aseguradora, Total.aseg, Vr.Canon) %>%
        arrange(desc((Total.aseg))) %>%
        dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1)) %>%
        head(2)
    }
    
  })
  
  
  
  # Top 4 con menor # de inmuebles Aseguradoras
  output$low4 <- renderTable({
    if (input$var9=='Activos'){
      tabla_aseg  %>%
        select(Aseguradora, Total.aseg, Vr.Canon) %>%
        arrange((Total.aseg)) %>%
        dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1)) %>%
        head(4)
    }
    else{
      tabla_aseg1  %>%
        select(Aseguradora, Total.aseg, Vr.Canon) %>%
        arrange((Total.aseg)) %>%
        dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1)) %>%
        head(2)
    }
  })
  
  #Scatter plot
  output$scatter <- renderPlotly({
    
    datos_plot<-datos[is.element(datos$Tipo_de_Inmueble,input$inmueble_scatter),]
    datos_plot$Total<-datos_plot[,input$varx]+datos_plot[,input$vary]
    datos_plot$Total<-paste(datos_plot$Total,'\n ID inmueble:', datos_plot$IdInmueble,sep=''  )
    sc <- datos_plot %>%
      ggplot(aes(x=get(input$varx), y =get(input$vary), label= Total)) +
      geom_point(aes(colour = (CentroCostos))) +
      labs(title = paste("Diagrama de dispersión entre", input$varx, "y", input$vary),
           x= input$varx,
           y= input$vary) +
      theme(plot.title = element_textbox_simple(size=10,
                                                halign = 0.5))
    ggplotly(sc)
  })
  
  output$boxplot <- renderPlotly({
    # Box Plot
    if (input$inmueble_ab_box=='Activos'){
      datos_box<-datos
    }
    else{
      datos_box<-datos_b
    }
    filtro<-(is.element(datos_box$Tipo_de_Inmueble,input$inmueble_box) )
    
    datos_<-datos_box[filtro  & datos_box[,input$var2]>0, ]
    n_re<-nrow(datos_box[filtro,])
    registos_0<-n_re-nrow(datos_)
    texto_add<-''
    if (registos_0>0){
      texto_add<-paste(as.character(registos_0),' registros en 0 de ',as.character(n_re),sep='')
    }
    else if(  nrow(datos_)==0){
      texto_add<- 'No hay registros \n que cumplan la condicón'
      
    }
    print(registos_0)
    if (nrow(datos_)>0){
      datos_ %>%
        plot_ly() %>%
        add_boxplot(x = datos_$CentroCostos,y=datos_[,input$var2],# data = datos_,
                    color= datos_$CentroCostos)%>%
        layout(title=paste('Gráfico de box-plot de ',input$var2, ' por Centro de Costos \n' ,texto_add,sep=''),
               yaxis=list(title="Frecuencia"))
      
    }
    else{
      ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = texto_add) + 
        theme_void()
      
    }
  })
  
  
}

#Definición de la aplicación
shinyApp(ui = ui, server = server)

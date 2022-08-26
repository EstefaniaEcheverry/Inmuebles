#Cargue las librerias necesarias
library(DT)
library(gt)
library(sf)
library(sp)
library(maps)
library(png)
library(tmap)
library(bslib)
library(dplyr)
library(ggmap)
library(shiny)
library(tidyr)
library(ggtext)
library(plotly)
library(readxl)
library(ggplot2)
library(leaflet)
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
#Crear el objeto de base de datos 
  datos <- read.csv2("data/direccion_actn.csv", header= TRUE)
datos_b<-read.csv2("data/inmuebles_bloqn.csv", header= TRUE)
direccion_unique <- read.csv2("data/direccion_uniquen.csv", header= TRUE)
direccion_unique_b <- read.csv2("data/direccion_bloqn.csv", header= TRUE)

# Cambiar las variables a factores
datos$IdInmueble <- as.character(datos$IdInmueble)
# creamos indices
direccion_unique$indice<- 1:nrow(direccion_unique)
col_t<-ncol(direccion_unique)
direccion_unique<-direccion_unique[,c(col_t,1:col_t-1)]
direccion_unique_b$indice<- 1:nrow(direccion_unique_b)
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
                  titleWidth = 400, 
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
                       selectInput(inputId ="var0" ,label= "Seleccione por tipo de inmueble",
                                   choices =c('Activos','Bloqueados'))),  
      conditionalPanel("input.sidebar == 'data'&& input.t1 == 'datos.3' ",
                       selectInput(inputId ="var0" ,label= "Seleccione por tipo de inmueble",
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
                              tags$p("Son 5483 inmuebles controlados por los diferentes Centros de costos , 
                                     segun los datos 24 de estos inmuebles estan bloqueados o desactivados,
                                     es decir, existen 5459 inmuebles activos distribuidos por los centros 
                                     de costos; Los Colores maneja 1056 de estos inmuebles y Laureles 871 
                                     inmuebles. ")
                       )
                     )
                     
                     
                     ), 
                     tabPanel(title="Datos",icon = icon("address-card"),value= "datos.1", DT::dataTableOutput("dataT")),
                     tabPanel(title="Estructura",icon = icon("address-card"),value= "datos.2",verbatimTextOutput("structure")),
                     tabPanel(title="Resumen estadísticas",icon = icon("address-card"),value= "datos.3",verbatimTextOutput("summary"))
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
                              withSpinner(plotlyOutput("barcc"))),
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
                                  (tmapOutput( "map_plot"))),                                
                                fluidRow(
                                  ( DT::dataTableOutput('data_filtro')) )

                      )),
                     tabPanel(title="Bloqueados",
                              fluidPage(fluidRow(br(),
                                withSpinner(tmapOutput(outputId = "map_plot_b"))),
                                fluidRow(
                                  ( DT::dataTableOutput('data_filtro_b')) )
                              )
                     
                     ))
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
    filter(is.element(shapeName,c("MEDELLÃN", # para adm_lvl =2 los municipios
                                  "BELLO",    # estan en mayuscula y no estan
                                  "COPACABANA",# en UTF-8 
                                  "ENVIGADO",
                                  "CALDAS", # hay dos caldas en colombia
                                  "ITAGÃœÃ", # itagui
                                  "LA ESTRELLA", 
                                  "SABANETA",
                                  "SAN JERÃ“NIMO",
                                  "BOGOTÃ, D.C."
    ) ) 
    ) %>%
    st_transform(crs = 3857)
  # Cambiandoles los nombres por como se escribe
  area_metropolitana$shapeName<-c("CALDAS_NO",# no son de antioquia        
                                  "ENVIGADO",
                                  "ITAGÜÍ",
                                  "CALDAS",
                                  "SAN JERÓNIMO",
                                  "BOGOTÁ",
                                  "MEDELLIN",
                                  "COPACABANA",
                                  "BELLO",
                                  "SABANETA",
                                  "LA ESTRELLA"  
  )
  # Eliminar el Caldas que no es de Antioquia
  area_metropolitana <- area_metropolitana[!(area_metropolitana$shapeName == "CALDAS_NO"),]
  
  
  ################## Mapas
  
  # Pasar los datos a formato sf tomando algunos datos de la base
  direcciones_sf <- direccion_unique %>% 
    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
    st_set_crs(value = 4326) %>%
    st_transform(crs = 3857) %>%
    st_intersection(area_metropolitana)
  

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
        tm_dots(size = 0.05,col = "Centro_de_Costos",popup.vars=columnas_mostrar)
 
  })  
  
  output$data_filtro<- DT::renderDataTable({
   if ( sum(names(input) == 'map_plot_marker_click')==1 ){

    click<-input$map_plot_marker_click
    direccion_print<-direccion_unique[paste('X',direccion_unique$indice,sep="")==click$id ,
                                      'Direcciones_c']
    datos_print<- datos[datos$Direcciones_c==direccion_print,]  
    datos_print
   }
    else{
      datos
    }
  }, options = list(scrollX = TRUE) )
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
      tm_dots(size = 0.05,col = "Centro_de_Costos",popup.vars=columnas_mostrar)
  })
  output$data_filtro_b<- DT::renderDataTable({
    if ( sum(names(input) == 'map_plot_b_marker_click')==1 ){
      
      click<-input$map_plot_b_marker_click
      direccion_print<-direccion_unique_b[paste('X',direccion_unique$indice,sep="")==click$id ,
                                        'Direcciones_c']
      datos_print<- datos_b[datos_b$Direcciones_c==direccion_print,]  
      datos_print
    }
    else{
      datos_b
    }
  }, options = list(scrollX = TRUE) )
  # reactive values for map
  

  

  output$structure <- renderPrint(
    if (input$var0=='Activos'){
      datos %>% str()
      
    }
    else{
      datos_b    %>% str()
    }
    
  )

  # Summary 
  output$summary <- renderPrint(
    # Resumen de los datos 
    if (input$var0=='Activos'){
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
    , options = list(scrollX = TRUE) 
    
  ))
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

  # Tabla total de cada ciudad y porcentajes por Centro de Costos
  tabla_centroc <- datos %>% group_by(CentroCostos) %>%
    dplyr::mutate(Vr.canon_c=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Comercial'),
                  Vr.canon_v=var_condicion(Vr.Canon,Tipo_de_Inmueble,'Vivienda')) %>%
    summarise(Total.c=n(), Vr.Canon=sum_pesos(Vr.Canon), 
              N_Vivienda = sum(Tipo_de_Inmueble=="Vivienda"),
              N_Comercial = sum(Tipo_de_Inmueble=="Comercial"),
              Vr.canon_c=sum_pesos(Vr.canon_c),
              Vr.canon_v=sum_pesos(Vr.canon_v))  %>%  
    dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1))%>%
    dplyr::mutate(General=paste('\n Total canon: ',Vr.Canon,'.\n Vivienda: \n Frec: ',
                            N_Vivienda,'. \n Total canon: ',Vr.canon_v,'.\n Comercial: \n Frec: ',N_Comercial,
                            '. \n Total canon: ',Vr.canon_c,'.\n Porcentaje: ',Porcentaje,'.',sep=''),
                  Var_prop=paste("   ",Total.c," ", "", "\n",Porcentaje,"%"))
 
  # Tabla total de cada ciudad y porcentajes por Centro de Costos
  tabla_centroc1 <- datos_b %>% group_by(CentroCostos) %>%
    summarise(Total.c=n(),Vr.Canon=sum_pesos(Vr.Canon))  %>%  
    dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1))
  
  # Barplot de Frecuencia de la variable Centro de Costos
  output$barcc <- renderPlotly({
    if (input$var7=='Activos'){
  
      # Gráfica de Centro total de Costos
      p4 <- tabla_centroc %>% ggplot(aes(x=CentroCostos, y =Total.c, 
                                         fill=(CentroCostos),label=General )) +
    
        geom_bar(width = 0.9, stat="identity")+  
        
        ylim(c(0,1100))+
        labs(x="Centros de costos", y= "Frecuencia",title = "Diagrama de barras para la variable Centro de costos") +   
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
                                                                  "La estrella", "San Antonio Pr" , "Calasanz","Fontibon")) +    
        
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) + 
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      
      ggplotly(p4)
    }
    else{
    
      # Gráfica de Centro total de Costos
      p4 <- tabla_centroc1 %>% ggplot(aes(x=CentroCostos, y =Total.c,
                                          fill=CentroCostos,label=Vr.Canon )) +
        geom_bar(width = 0.9, stat="identity")+  
        
        ylim(c(0,15))+
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
                                                                  "La estrella", "San Antonio Pr" , "Calasanz","Fontibon")) +    
        
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) + 
        theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      
      ggplotly(p4)
      
    }
    
  })
  #############
 
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
                                N_Vivienda,'. \n Total canon: ',Vr.canon_v,'.\n Comercial: \n Frec: ',N_Comercial,
                                '. \n Total canon: ',Vr.canon_c,'.\n Porcentaje: ',Porcentaje,'.',sep=''),
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
        
        ylim(c(0,3000))+
        labs(x="Ciudad", y= "Frecuencia", title= "Diagrama de barras para la variable Ciudad") +   
        
        scale_fill_discrete(name = "Ciudad", labels = c("Medellin", "Sabaneta", "Envigado", "Itagui", "Bello", 
                                                        "La estrella", "San Jeronimo", "Caldas", "Copacabana", "San Antonio Pr","Bogota")) +                                            
        
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
        
        ylim(c(0,40))+
        labs(x="Ciudad", y= "Frecuencia", title= "Diagrama de barras para la variable Ciudad") +   
        
        scale_fill_discrete(name = "Ciudad", labels = c("Medellin", "Sabaneta", "Envigado", "Itagui", "Bello", 
                                                        "La estrella", "San Jeronimo", "Caldas", "Copacabana", "San Antonio Pr","Bogota")) +                                            
        
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
                                N_Vivienda,'. \n Total canon: ',Vr.canon_v,'.\n Comercial: \n Frec: ',N_Comercial,
                                '. \n Total canon: ',Vr.canon_c,'.\n Porcentaje: ',Porcentaje,'%.',sep=''),
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
        ylim(c(0,55))+
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
    sc <- datos_plot %>%
      ggplot(aes(x=get(input$varx), y =get(input$vary), label= IdInmueble)) +
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
    datos_<-datos[ is.element(datos$Tipo_de_Inmueble,input$inmueble_box) & datos[,input$var2]>0, ]
    n_re<-nrow(datos[is.element(datos$Tipo_de_Inmueble,input$inmueble_box),])
    registos_0<-n_re-nrow(datos_)
    texto_add<-''
    if (registos_0>0){
      texto_add<-paste(as.character(registos_0),' registros en 0 de ',as.character(n_re),sep='')
    }
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

#Configuración tematica de los gráficos al estilo de la aplicación
#thematic_shiny()

#Definición de la aplicación
shinyApp(ui = ui, server = server)


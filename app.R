#Cargue las librerias necesarias
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
library(readxl)
#  library(colmaps)
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
datos <- read.csv2("data/direccion_act.csv", header= TRUE)
datos_b<-read.csv2("data/inmuebles_bloq.csv", header= TRUE)
direccion_unique <- read.csv2("data/direccion_unique.csv", header= TRUE)
direccion_unique_b <- read.csv2("data/direccion_bloq.csv", header= TRUE)
#colnames(datos) <- colnames(datos)[2:ncol(datos)]   
#data <- datos[ , - ncol(datos)]
attach(datos)
# Cambiar las variables a factores
datos$IdInmueble <- as.character(datos$IdInmueble)
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

# Estructura de los datos 
#datos %>% str()

# Resumen de los datos 
#datos %>% summary() 

#Cabeza de los datos 
#datos %>% head() 

#  Choices for selectInput 
c1 = datos %>% select(c(15:17))%>%
  names()

#####################
c2 = datos %>% select(c(5,9:11)) %>% 
  names()
c3= unique(direccion_unique$Centro_de_Costos)
c4= names(direccion_unique)
n1<-length(c4)
c4=c4[c(1:(n1-2) ) ]
#Definición de la interfaz de usuario
ui <- fluidPage(

dashboardPage(
  dashboardHeader(title="Inmuebles de Portada Inmobiliaria", titleWidth = 650, 
                  tags$li(class="dropdown",tags$a(href="https://portadainmobiliaria.com/", icon("building"), "Portada", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/estefania-echeverry-franco-932597232/" ,icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/EstefaniaEcheverry", icon("github"), "Source Code", target="_blank"))
  ),
  dashboardSidebar(
    #sidebarmenu
    sidebarMenu(
      id="sidebar",
      
      #first menuitem
      menuItem("Dataset", tabName="data", icon=icon("database")),
      conditionalPanel("input.sidebar == 'data'",
                       selectInput(inputId ="var0" ,label= "Seleccione una columna de descripción por punto",
                                   choices =c('Bloqueados','Activos'))),  
      menuItem(text= "Visualization",tabName = "viz",icon = icon("chart-line")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'",
                       selectInput(inputId ="var1" ,label= "Seleccione la variable X",
                                   choices =c1 ,selected="Vr.Canon")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' " ,
                       selectInput(inputId ="var3" ,label= "Seleccione la variable X",
                                   choices =c1 ,selected="Vr.Canon")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation'" ,
                       selectInput(inputId ="var4" ,label= "Seleccione la variable Y",
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
                     tabPanel(title="Información",icon = icon("address-card"),fluidRow(
                       column(width = 8, tags$img(src="image.png", width =400 , height = 200,alt ="Something went wrong",deleteFile=FALSE),
                              align = "center"),
                       column(width = 4, tags$br() ,
                              tags$p("Son 5301 inmuebles controlados por los diferentes Centros de costos, segun los datos 66 de estos
                                     inmuebles estan bloqueados, ")
                       )
                     )
                     
                     
                     ), 
                     tabPanel(title="Datos",icon = icon("address-card"),dataTableOutput("dataT")),
                     tabPanel(title="Estructura",icon = icon("address-card"),verbatimTextOutput("structure")),
                     tabPanel(title="Resumen estadísticas",icon = icon("address-card"),verbatimTextOutput("summary"))
              )
      ),
      # second tab item or landing page here ..
      tabItem(tabName = "viz",
              tabBox(id="t2", width=20,
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
                     tabPanel(title="Distribución",icon = icon("chart-line"),target="", value="distro",withSpinner(plotlyOutput("histplot", height = "350px"))),
                     tabPanel(title="Box Plots",icon = icon("chart-line"),value="boxpl",withSpinner(plotlyOutput("boxplot", height = "350px"))),
                     tabPanel(title="Scatterplot ",icon =icon("chart-line"),value="relation", withSpinner(plotlyOutput("scatter")))
              )
      ),
      # third tab item
      tabItem(tabName="map",
              tabBox(id="t3",width= 12,
                     tabPanel(title="Inmuebles",withSpinner(tmapOutput(outputId = "map_plot"))
      
                     ),
                     tabPanel(title="Bloqueados",withSpinner(tmapOutput(outputId = "map_plot_b"))
                     #tabPanel(title="Laureles",withSpinner(tmapOutput(outputId = "maplaureles"))),
                     #tabPanel(title="Sabaneta",withSpinner(tmapOutput(outputId = "mapsabaneta"))),
                     #tabPanel(title="Poblado" ,withSpinner(tmapOutput(outputId = "mappoblado"))),
                     #tabPanel(title="Los Colores",withSpinner(tmapOutput(outputId = "mapcolores"))),
                     #tabPanel(title="Envigado",withSpinner(tmapOutput(outputId = "mapenvigado"))),
                     #tabPanel(title="Itagui",withSpinner(tmapOutput(outputId = "mapitagui"))),
                     #tabPanel(title="Centro",withSpinner(tmapOutput(outputId = "mapcentro"))),
                     #tabPanel(title="Bello" ,withSpinner(tmapOutput(outputId = "mapbello"))),
                     #tabPanel(title="La estrella" ,withSpinner(tmapOutput(outputId = "mapestrella"))),
                     #tabPanel(title="San Antonio Pr.",withSpinner(tmapOutput(outputId = "mapsanantonio"))),
                     #tabPanel(title="Calasanz",withSpinner(tmapOutput(outputId = "mapcalasanz")))
                     ))
      )
    )
  )
)
)

# Definción de la funcionalidad lógica de la aplicación (servidor)

server <- function(input, output, session) {
  # stucture 

    # Estructura de los datos 

  output$structure <- renderPrint(
    if (input$var0=='Bloqueados'){
      datos_b %>% str()
    }
    else{
      datos    %>% str()
    }
    
  )
  # Summary 
  output$summary <- renderPrint(
    # Resumen de los datos 
    if (input$var0=='Bloqueados'){
      datos_b %>% summary()
    }
    else{
      datos    %>% summary()
    }
  )
  # DataTable
  output$dataT <- renderDataTable(
    if (input$var0=='Bloqueados'){
      datos_b
    }
    else{
      datos  
    }
  )
  # DataTable
  output$dataInmu <- renderDataTable(
    direccion_unique
  )
  
  
  
  # Stacked histogram and boxplot
  output$histplot <- renderPlotly({
    
    p1 <- datos %>%
      plot_ly( ) %>%
      add_histogram(~get(input$var1)) %>%
      layout(xaxis = list(title= ~get(input$var1)))
    # Box plot Vr. Canon
    p2 <- datos %>% 
      plot_ly() %>% 
      add_boxplot(~get(input$var1)) %>%
      layout(yaxis =list(showticklabels = F))
    # Stacking Plots Vr. Canon
    subplot(p2, p1, nrows=2,shareX = TRUE) %>%
      hide_legend() %>%
      layout(title="Gráfico de distribución - Histograma y Boxplot",
             yaxis=list(title="Frecuencia"))
    
  })
  
  ### Bar Charts - State wise trend
  
  #################
  
  # Tabla total de cada ciudad y porcentajes por Aseguradora
  tabla_centroc <- datos %>% group_by(CentroCostos) %>%
    summarise(Total.c=n())  %>%  
    dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1))
  
  # Barplot de Frecuencia de la variable Centro de Costos
  output$barcc <- renderPlotly({
    
    # Gráfica de Centro total de Costos
    p4 <- tabla_centroc %>% ggplot(aes(x=CentroCostos, y =Total.c,
                                       fill=CentroCostos )) +
      geom_bar(width = 0.9, stat="identity")+  
      
      ylim(c(0,1100))+
      labs(x="Centros de costos", y= "Frecuencia",title = "Diagrama de barras para la variable Centro de costos") +   
      labs(fill = "")+                                         
      
      geom_text(aes(label=paste0(Total.c," ", "", "\n(", Porcentaje, "%",")")),  
                vjust=1.3,                         
                color="black",                     
                hjust=0.5,                         
                position = position_dodge(0.9),    
                angle=0,                           
                size=4.0                            
      ) +  
      scale_fill_discrete(name = "Centro de costos", labels = c("Laureles", "Sabaneta", "Poblado" ,"Colores", "Envigado", "Itagui", "Centro", "Bello",
                                                                "La estrella", "San Antonio Pr" , "Calasanz")) +    
      
      theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) + 
      theme(legend.position = "left") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    ggplotly(p4)
  })
  #############
  
  
  # Tabla total de cada ciudad y porcentajes por Aseguradora
  tabla_ciudad <- datos %>% group_by(Ciudad) %>%
    summarise(Total.ciudad=n())  %>%  
    dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1))
  
  # Barplot de Frecuencia de la variable Ciudad
  output$barciu <- renderPlotly({
    # Gráfica de Centro total de Costos
    p5 <- tabla_ciudad %>% ggplot(aes(x=Ciudad, y = Total.ciudad, fill=Ciudad )) + 
      geom_bar( stat="identity"              
      )+  
      
      ylim(c(0,3000))+
      labs(x="Ciudad", y= "Frecuencia", title= "Diagrama de barras para la variable Ciudad") +   
      
      scale_fill_discrete(name = "Ciudad", labels = c("Medellin", "Sabaneta", "Envigado", "Itagui", "Bello", 
                                                      "La estrella", "San Jeronimo", "Caldas", "Copacabana", "San Antonio Pr")) +                                            
      
      geom_text(aes(label=paste0(Total.ciudad," ", "", "\n(", Porcentaje, "%",")")),  
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
  })
  
  ####################
  
  # Tabla total de cada ciudad y porcentajes por Aseguradora
  tabla_aseg <- datos %>% group_by(Aseguradora) %>%
    summarise(Total.aseg=n())  %>%  
    dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1))
  
  # Barplot de Frecuencia de la variable Aseguradora
  output$baraseg <- renderPlotly({
    # Gráfica de Aseguradora
    p6 <- tabla_aseg %>% ggplot(aes(x=Aseguradora, y = Total.aseg, fill=Aseguradora )) + 
      geom_bar(width = 0.9, stat="identity",position = position_dodge())+  
      ylim(c(0,5000))+
      labs(x="Aseguradoras", y= "Frecuencia",title= "Diagrama de barras para la variable Aseguradora") +   
      labs(fill = "")+                                         
      
      geom_text(aes(label=paste0(Total.aseg," ", "", "\n(", Porcentaje, "%",")")),  
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
    ggplotly(p6)
  })
  
  # Codigo del server para los resumenes que se abren
  
  # Rendering the box header  
  output$head1 <- renderText(
    paste("los 5 Centros de Costos con mas Inmuebles")
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("Los Centros de Costos con menos Inmuebles")
  )
  
  # Top 5  con mayor # de inmuebles Centro Costos 
  output$top5 <- renderTable({
    
    tabla_centroc %>%
      select(CentroCostos, Total.c) %>%
      arrange(desc((Total.c))) %>%
      dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1)) %>%
      head(6)
    
  })
  
  # Top 6 con menor # de inmuebles Centro Costos 
  output$low6 <- renderTable({
    
    tabla_centroc %>%
      select(CentroCostos, Total.c) %>%
      arrange(Total.c) %>%
      dplyr::mutate(Porcentaje = round(Total.c/sum(Total.c)*100, 1)) %>%
      head(5)
    
    
  })
  
  # Rendering the box header  
  output$head3 <- renderText(
    paste("Los 5 municipios con mas inmuebles")
  )
  
  # Rendering the box header 
  output$head4 <- renderText(
    paste("Los municipios con menos inmuebles")
  )
  
  # Top 5  con mayor # de inmuebles Ciudad
  output$top5.1 <- renderTable({
    
    tabla_ciudad  %>%
      select(Ciudad, Total.ciudad) %>%
      arrange(desc((Total.ciudad))) %>%
      dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1)) %>%
      head(5)
    
  })
  
  # Top 5 con menor # de inmuebles Ciudad
  output$low5 <- renderTable({
    
    tabla_ciudad %>%
      select(Ciudad, Total.ciudad) %>%
      arrange(Total.ciudad) %>%
      dplyr::mutate(Porcentaje = round(Total.ciudad/sum(Total.ciudad)*100, 1)) %>%
      head(5)
    
  })
  
  # Rendering the box header  
  output$head5 <- renderText(
    paste("Las 3 aseguradoras con mas inmuebles")
  )
  
  # Rendering the box header 
  output$head6 <- renderText(
    paste("Las asegiradoras con menos inmuebles")
  )
  
  # Top 3 con mayor # de inmuebles Aseguradoras 
  output$top3 <- renderTable({
    
    tabla_aseg  %>%
      select(Aseguradora, Total.aseg) %>%
      arrange(desc((Total.aseg))) %>%
      dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1)) %>%
      head(3)
    
  })
  
  
  
  # Top 4 con menor # de inmuebles Aseguradoras
  output$low4 <- renderTable({
    
    tabla_aseg  %>%
      select(Aseguradora, Total.aseg) %>%
      arrange((Total.aseg)) %>%
      dplyr::mutate(Porcentaje = round(Total.aseg/sum(Total.aseg)*100, 1)) %>%
      head(4)
    
  })
  
  #Scatter plot
  output$scatter <- renderPlotly({
    
    #Creating scatter plot for relationship using ggplot
    
    sc <- datos %>%
      ggplot(aes(x=get(input$var3), y =get(input$var4), color= CentroCostos)) +
      geom_point() +
      labs(title = paste("Diagrama de dispersión entre", input$var3, "y", input$var4),
           x= input$var3,
           y= input$var4) +
      theme(plot.title = element_textbox_simple(size=10,
                                                halign = 0.5))
    ggplotly(sc)
  })
  
  output$boxplot <- renderPlotly({
    # Box Plot
    datos_<-datos[datos$Vr.Canon>0,]
    
    datos_ %>%
      filter(Vr.Canon>0 ) %>%
      plot_ly() %>%
      add_boxplot(x = datos_$CentroCostos,y=datos_$Vr.Canon, data = datos_,
                  color= datos_$CentroCostos)%>%
      layout(title="Gráfico de disperción del Valor del Canon por Centro de Costos",
             yaxis=list(title="Frecuencia"))
    
  })
  #####################
  ################################################################
  # Crear mapa con geoboundaries escogiendo solo los municipios
  area_metropolitana <- geoboundaries(country = "COLOMBIA", adm_lvl = 2) %>%
    filter(is.element(shapeName,c("MEDELLÃN", # para adm_lvl =2 los municipios
                                  "BELLO",    # estan en mayuscula y no estan
                                  "COPACABANA",# en UTF-8 
                                  "ENVIGADO",
                                  "CALDAS", # hay dos caldas en colombia
                                  "ITAGÃœÃ", # itagui
                                  "LA ESTRELLA", 
                                  "SABANETA",
                                  "SAN JERÃ“NIMO"
    ) ) 
    ) %>%
    st_transform(crs = 3857)
  # Cambiandoles los nombres por como se escribe
  area_metropolitana$shapeName<-c("CALDAS_NO",# no son de antioquia        
                                  "ENVIGADO",
                                  "ITAGÜÍ",
                                  "CALDAS",
                                  "SAN JERÓNIMO",
                                  "MEDELLIN",
                                  "COPACABANA",
                                  "BELLO",
                                  "SABANETA",
                                  "LA ESTRELLA" 
  )
  # Eliminar el Caldas que no es de Antioquia
  area_metropolitana <- area_metropolitana[!(area_metropolitana$shapeName 
                                             == "CALDAS_NO"),]
  ################## Mapas
  
  # Pasar los datos a formato sf tomando algunos datos de la base
  direcciones_sf <- direccion_unique %>% 
    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
    st_set_crs(value = 4326) %>%
    st_transform(crs = 3857) %>%
    st_intersection(area_metropolitana)
  
#    tmap_mode('view') +
#    tm_shape(shp = direcciones_sf)+ # coordenadas lat long
    #tm_markers(size = 0.05,col = "Centro_de_Costos")#
#      tm_dots(size = 0.05,col = "Centro_de_Costos",shape=25,popup.vars=c( "Direcciones_c" , "Nombre_del_Lugar","Total_de_apartamentos"))
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
  
  # Crear la base de datos para sf para visualizar el mapa
  # Pasar los datos a formato sf tomando algunos datos de la base
#  inmuebles_sf1 <- datos %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
##  output$mapcolores<- renderTmap({
    # Mapa del aréa metropolitana con los inmuebles 
#    tmap_mode('view') %>%
#      tm_shape(shp = inmuebles_sf1)+ # coordenadas lat long
#      tm_dots(size = 0.05,col = "CentroCostos")
#  })
  
  # Mapa por cada de uno de los Centros de costos por conjunto
  # Laureles
#  Laureles_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Laureles"), ]
  # Pasar los datos a formato sf tomando datos de Laureles
#  inmulaureles_sf <- Laureles_ %>% 
 #   st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
 #   st_intersection(area_metropolitana)
  
  
#  output$maplaureles<- renderTmap({
    # Mapa de solo centro de costos de Laureles
#    tmap_mode('view')
    
#    tm_shape(shp = inmulaureles_sf)+ # coordenadas lat long
#      tm_dots(size = 0.05,col = "purple")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Sabaneta
 # Sabaneta_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Sabaneta"), ]
  # Pasar los datos a formato sf tomando datos de Sabaneta
#  inmusabaneta_sf <- Sabaneta_ %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
 #   st_transform(crs = 3857) %>%
 #   st_intersection(area_metropolitana)
  
#  output$mapsabaneta<- renderTmap({
#    # Mapa de solo centro de costos de Sabaneta
#    tmap_mode('view')
    
#    tm_shape(shp = inmusabaneta_sf)+ # coordenadas lat long
#      tm_dots(size = 0.05,col = "coral2")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Poblado
#  Poblado_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Poblado"), ]
  # Pasar los datos a formato sf tomando datos de Poblado
#  inmuPoblado_sf <- Poblado_ %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
#  output$mappoblado<- renderTmap({
    # Mapa de solo centro de costos de Sabaneta
#    tmap_mode('view')
    
#    tm_shape(shp = inmuPoblado_sf)+ # coordenadas lat long
#      tm_dots(size = 0.05,col = "mediumseagreen")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Colores
#  Colores_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Colores"), ]
  # Pasar los datos a formato sf tomando datos de Colores
#  inmuColores_sf <- Colores_ %>% 
 #   st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
#  output$mapcolores<- renderTmap({
    # Mapa de solo centro de costos de Sabaneta
#    tmap_mode('view')
    
#    tm_shape(shp = inmuColores_sf)+ # coordenadas lat long
 #     tm_dots(size = 0.05,col = "darkgoldenrod")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Envigado
#  Envigado_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Envigado"), ]
  # Pasar los datos a formato sf tomando datos de Envigado
#  inmuEnvigado_sf <- Envigado_ %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
 #   st_intersection(area_metropolitana)
  
  
#  output$mapenvigado<- renderTmap({
    # Mapa de solo centro de costos de Sabaneta
#    tmap_mode('view')
    
#    tm_shape(shp = inmuEnvigado_sf)+ # coordenadas lat long
#      tm_dots(size = 0.05,col = "firebrick")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Itagui
#  Itagui_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Itagui"), ]
  # Pasar los datos a formato sf tomando datos de Itagui
 # inmuItagui_sf <- Itagui_ %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
  
#  output$mapitagui<- renderTmap({
    # Mapa de solo centro de costos de Sabaneta
#    tmap_mode('view')
    
 #   tm_shape(shp = inmuItagui_sf)+ # coordenadas lat long
  #    tm_dots(size = 0.05,col = "darksalmon")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Centro
 # Centro_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Centro"), ]
  # Pasar los datos a formato sf tomando datos de Centro
 # inmuCentro_sf <- Centro_ %>% 
 #   st_as_sf(coords = c('Longitud', 'Latitud')) %>%
 #   st_set_crs(value = 4326) %>%
  #  st_transform(crs = 3857) %>%
  #  st_intersection(area_metropolitana)
  
 # output$mapcentro<- renderTmap({
    # Mapa de solo centro de costos de Centro
 #   tmap_mode('view')
    
#  tm_shape(shp = inmuCentro_sf)+ # coordenadas lat long
#    tm_dots(size = 0.05,col = "darkslateblue")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Bello
#  Bello_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Bello"), ]
  # Pasar los datos a formato sf tomando datos de Bello
 # inmuBello_sf <- Bello_ %>% 
#  st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
#  output$mapbello<- renderTmap({
    # Mapa de solo centro de costos de Bello
#    tmap_mode('view')
    
#    tm_shape(shp = inmuBello_sf)+ # coordenadas lat long
#      tm_dots(size = 0.05,col = "cornflowerblue")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # La estrella
#  La_estrella_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"La estrella"), ]
  # Pasar los datos a formato sf tomando datos de La estrella
#  inmuLa_estrella_sf <- La_estrella_ %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
#  output$mapestrella<- renderTmap({
    # Mapa de solo centro de costos de La estrella
#    tmap_mode('view')
    
#    tm_shape(shp = inmuLa_estrella_sf)+ # coordenadas lat long
#      tm_dots(size = 0.05,col = "darkmagenta")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # San Antonio Prado
#  San_Antonio_Pr_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"San Antonio Pr."), ]
  # Pasar los datos a formato sf tomando datos de San Antonio Prado
#  inmuSan_Antonio_Pr_sf <- San_Antonio_Pr_ %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
#    st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
#  output$mapsanantonio<- renderTmap({
    # Mapa de solo centro de costos de San Antonio Prado
#    tmap_mode('view')
    
#    tm_shape(shp = inmuSan_Antonio_Pr_sf)+ # coordenadas lat long
 #     tm_dots(size = 0.05,col = "forestgreen")
#  })
  
  # Mapa por cada de uno de los Centros de costos
  # Calasanz
#  Calasanz_ <- direccion_unique[is.element(direccion_unique$Centro_de_Costos ,"Calasanz"), ]
  # Pasar los datos a formato sf tomando datos de Calasanz
#  inmuCalasanz_sf <- Calasanz_ %>% 
#    st_as_sf(coords = c('Longitud', 'Latitud')) %>%
 #   st_set_crs(value = 4326) %>%
#    st_transform(crs = 3857) %>%
#    st_intersection(area_metropolitana)
  
#  output$mapcalasanz<- renderTmap({
    # Mapa de solo centro de costos de Calasanz
#    tmap_mode('view')
    
#    tm_shape(shp = inmuCalasanz_sf)+ # coordenadas lat long
 #     tm_dots(size = 0.05,col = "orange")
 # })
  
}

#Configuración tematica de los gráficos al estilo de la aplicación
#thematic_shiny()

#Definición de la aplicación
shinyApp(ui = ui, server = server)

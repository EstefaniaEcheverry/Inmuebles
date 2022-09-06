# geocodificación

#librerias 
library(readxl) # leer archivos xls
library(stringr) # manipulacion direcciones
library(tidygeocoder) # geocodificacion de direcciones


########################funciones###################################################
separador<-function(texto,palabras){
# texto: un caracter cualquiera
# palabras: un vector de caracteres 
# return: vector con caraceres separados por palabras.
    for (palabra in palabras){
    vector<-unlist( strsplit(texto, palabra),use.names = F)
    texto<-c()
    for (j in vector){
      texto<-c(texto,j)
    }
  }
  
  return(texto ) 
}

separador_def<-function(texto,palabras_eliminar,palabras_inter){
  palabras_inter<- paste(" ",palabras_inter," ",sep="")
  palabras_eliminar<-paste(" ",palabras_eliminar," ",sep="")
  for (palabra in c(palabras_inter,palabras_eliminar) ){
    vector_new<-c() 
    vector<-strsplit(texto,palabra ) # seperar vector por palabra
    for (i in vector ){ # i es un vector
      if(length(i)==1){ # palabra no esta en i
        for (j in i ){  
          vector_new<-c(vector_new,j) #  añadiendo la palabra
        }
      }
      else{ # si i >1 entonces
        contar<-1
        for (j in i ){ # para el primer elemento del vector se deja tal cual
          if(contar==1){ 
            vector_new<-c(vector_new,j)
          }
          else{ # para los otros elementos se añade la palabra separador y el elemento del vector
            vector_new<-c(vector_new,paste(" ",palabra,j,sep=""))            
          }
          
          contar<-contar+1
        }
      }
    }
    
    texto<-c() # vector palabra separada
    for (j in vector_new){
      texto<-c(texto,j)
    }
  }
  texto_final<-c() 
  texto_separado<-c()
  for (i in texto){
    sep_1<-separador(i,palabras_eliminar )
    if (length( sep_1)==1 ){
      texto_final<-c(texto_final,i)
    }
    else{
      texto_separado<-c(texto_separado,i)
    }
  }
  texto_final_<-str_replace_all(str_c(texto_final,collapse = ""),"  "," ") 
  texto_separado_<-str_replace_all(str_c(texto_separado,collapse = ""),"  "," ") 
  return(c(texto_final_,texto_separado_)) 
}

#######################################################################################
# Lectura de datos
inmuebles <- read_excel("data_orden/BaseDatosInmueblesPortada06-09-2022.xls")
# Seleccionar solo los sectores de interes
centro_costos_codigo<-c("01","02","03","04","08","09","10","11","12","19","22","25")
inmuebles <- inmuebles[is.element(inmuebles$CentroCostos, 
                                         centro_costos_codigo ), ]
# correcciones manuales

inmuebles[inmuebles$IdInmueble== 11719, 'Ciudad'] <- "MEDELLIN"

#########################
centro_costos_nombres<- c("Laureles","Sabaneta","Poblado",
                          "Colores","Envigado","Itagui",
                          "Centro","Bello","La estrella", 
                          "San Antonio Pr." , "Calasanz",
                          "Fontibon")
inmuebles$NoCCostos<-as.numeric(inmuebles$CentroCostos)
inmuebles$CentroCostos <- factor(inmuebles$CentroCostos,
                                   labels = centro_costos_nombres)

names(inmuebles)[names(inmuebles)=='Vr Canon']<-'Vr.Canon'
names(inmuebles)[names(inmuebles)=='Vr Administracion']<-'Vr.Administracion'
names(inmuebles)[names(inmuebles)=='P Cedula']<-'P.Cedula'
names(inmuebles)[names(inmuebles)=='A. Cedula']<-'A.Cedula'
names(inmuebles)[names(inmuebles)=='Valor Iva']<-'Valor.Iva'

inmuebles$P.Cedula <- as.character(inmuebles$P.Cedula)
inmuebles$A.Cedula <- as.character(inmuebles$A.Cedula)

## modificacion de direcciones


# Palabras que se junta con cada dirección
palabras_inter <- c("CONJUNTO","CONJUTO","COJUNTO", "CRUCERO", "URBANIZACIÓN","URBANIZACION","URB", 
                    "EDIFICIO","EDIFICO","ED","EDF","MULTIFAMILIAR","MULTIFAMILIARES","LC",
                    "UNIDAD","MIRADOR","LOMA","LOMAS","LOS CABOS","BALCONES","TORRES","TORRE",
                    "SERRANA","SERRANIAS","PARQUE","PARQUES","SETAI", "ABADIA","GUAYACANES",
                    "EL CORTIJO", "OASIS","CALASANZ", "LAS CASAS", "LOS LAURELES", "PORTONES",
                    "SANDIEGO", "SAN DIEGO","TIERRA","PUNTA","ARBOLEDA","ROSEDAL","PORTON","MEDITERRANEA",
                    "ALAMEDA","CEDROS","CAMINO","RECINTO","BOSQUES","BALCON","BULEVAR","LAS VEGAS","REMANSO",
                    "CONQUISTADOR","ALTOS,","COLINAS","CORAZON","LAS ANTILLAS","CONDOMINIO","CIUDADELA","PARIS",
                    "JACARANDAS","VILLA","AGUAS","ARRECIFES","ACUARELA","ACUARELAS","GUADUALES","RESERVA","LA   RESERVA","ENTRE", "PLAZA","LA PLAZA","CASTELLON","NUCLEO","PRADO","PRADOS","NUEVO","PARCELACION",
                    "ESTADIO","VIVARE","LA MOTA","BASALTO","MADEROS","PIZARRA","AMARELLO","CENTRO","VENTURA"," SAN FRANCISCO","LA GRUTA","FARO", "RINCON","AIRES","SANTIVARI","SAN MIGUEL","PUERTA","PROVIDENCIA","FUENTE","SAN TELMO","VEGAS","CASTROPOLO","VIVALTO","SOL","NIQUIA","BOSQUE","PAMPLONA","XUE","BRUJA","CEIBA","ALTOBELO","OPORTO","FLOR","VOLGA","CIGARRAS","NUEVA","CAMPO","SABATTO","JARDIN","LA ALQUERIA","BAHIA","NATIVO","MONTEPARAISO","LA VEGA","VLORE","SIERRA","VALPARAISO","CAPELLA","ALONDRA","ALMENDRA","BRUJAS","ASOCIACION","COLIBRI","PORTAL","GAUDI","CAPELLA","PALMERAS","CANTO","ARCE","COMERCIAL","MAGDALENA","URNANIZACION","NEBRASKA","TRENTTO","LISBOA","MISSISSIPPI","LUNA","URANIA","COLORES","EL ROSAL","JARDINES","QUINTAS","PRIMEIRO","JAZZ","MESSANTIA","ORION","FIRENZZE","ALDEA","MANZANARES","FORETTI","CHIE","KAMELOT","BIOCITY","CENTRAL","FRONTERA","GIRASOLES","CARLOS","SENDERO","LAUREL","LUXURY","GUADUAL","LOS ARBOLES","FELICITY","AMATISTA","MONTE","HUNGRIA","PACIFICA","VIÑA","SENDEROS","ENSENADA","LOS COLORES","CALASANIA","MOCACCINO","CASTILLO","CIUDAD","FLORIDA","BALUARTE","CATTLEYA","CRISTOBAL","KIRIBATI","CANTARES","PARQUE","PROVIDENCIA","VENTUM","MONTEFLOR","OBRA","MONTE","SAN JOAQUIN","TERRITORIO","CENTURY","ALIADAS","MESSANTIA","VENTTO","PUERTO")
# Palabras a eliminar de las direcciones 
palabras_eliminar <- c("AP", "INT","BL","CASA", "CS","IN", "APT")

# direccion y nombre apartamento
direcciones <- c ()
palabras_apa<-c()
for (direccion in inmuebles$Direccion){
  palabrastemp<-separador_def(direccion,
                              palabras_eliminar,
                              palabras_inter)
  direcciones<-c(direcciones,palabrastemp[1])
  palabras_apa<-c(palabras_apa,palabrastemp[2])
}
inmuebles$direcciones <- direcciones
inmuebles$palabras_apa <- palabras_apa
# nombre conjunto 
#################################
direcciones <- c()
palabras_apa<-c()
for (direccion in inmuebles$Direccion){
  palabrastemp<-separador_def(direccion,
                              palabras_inter,
                              palabras_eliminar)
  direcciones<-c(direcciones,palabrastemp[1])
  palabras_apa<-c(palabras_apa,palabrastemp[2])
}
inmuebles$Lugar <- palabras_apa

############################

# Eliminar las palabras de las direcciones para las localizaciones
palabras_no <- c ("CONJUNTO", "AP","CA", "ET", "INT", "ED", "URBANIZACION",
                  "PRIMER", "CASA","LO","CS","PISO", "IN")
Direcciones_c<-c()
for (direccion in inmuebles$Direccion){
  Direcciones_c<-c(Direcciones_c,separador(direccion,palabras_no)[1])
}

##################### geo codificacion ###############################3

datos <- read.csv2("data_orden/inmuebles_2.csv", header= TRUE,
                   fileEncoding = "UTF-8")
#datos_b<-read.csv2("data/inmuebles_bloqn.csv", header= TRUE,
                  # fileEncoding = "UTF-8")

localizaciones <- unique(read.csv("data/localizaciones_2.csv",sep=",",header=T))
id_inmuebles<-c(datos$IdInmueble)  
filtro<-(!is.element( inmuebles$IdInmueble,id_inmuebles ) &
           !is.element(Direcciones_c, localizaciones$address))
inmuebles$Direcciones_c <- Direcciones_c
direcciones_nuevas<-paste (Direcciones_c,inmuebles$Ciudad, ', Colombia',sep=' ' )
direcciones_nuevas<-data.frame(address=(direcciones_nuevas[filtro]), id=inmuebles$IdInmueble[filtro])
localizaciones_nuevas<-geocode(direcciones_nuevas,"address", method = "arcgis" )


write.csv2(x = rbind(unique(localizaciones_nuevas[,c(1,3,4)]),localizaciones), file = "data_orden/localizaciones.csv", 
                            row.names = F, fileEncoding = "UTF-8" )


inmuebles$localizaciones.lat<-0
inmuebles$localizaciones.long<- 0
inmuebles$localizaciones.address <- ''
for (i in  localizaciones_nuevas$id ){
  filtro1<-inmuebles$IdInmueble==i
  filtro2<-localizaciones_nuevas$id==i
  #filtro2[is.na(filtro2)]<-F
  #if (nrow(inmuebles[filtro1,])>0){
    inmuebles[filtro1,]$localizaciones.lat<-(localizaciones_nuevas[filtro2,]$lat)
    inmuebles[filtro1,]$localizaciones.long<-(localizaciones_nuevas[filtro2,]$long)
    inmuebles[filtro1,]$localizaciones.address<-(localizaciones_nuevas[filtro2,]$address)
  #}
}
sum(filtro2)
sum(filtro1)
datos_completo<- datos
datos_completo$localizaciones.lat<-as.numeric(datos_completo$localizaciones.lat)
datos_completo$localizaciones.long<-as.numeric(datos_completo$localizaciones.long)
filtro_<-inmuebles$localizaciones.address==''
for (i in inmuebles$IdInmueble[filtro_]){
  filtro1<-inmuebles$IdInmueble==i
  filtro2<-datos_completo$IdInmueble==i
  #print(1)
  inmuebles[filtro1,]$localizaciones.lat<-(unique(datos_completo[filtro2,]$localizaciones.lat))
  inmuebles[filtro1,]$localizaciones.long<-(unique(datos_completo[filtro2,]$localizaciones.long))
  inmuebles[filtro1,]$localizaciones.address<-(unique(datos_completo[filtro2,]$localizaciones.address))
  
}


write.csv2(x = inmuebles, file = "data_orden/inmuebles_3.csv", row.names = F,
           fileEncoding = "UTF-8")


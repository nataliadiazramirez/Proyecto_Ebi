

library(pdftools)
library(tm)
library(stringr)
library(stringi)
library(data.table)
library(dplyr)


#Descripcion funcion 1: Formato para extraer los datos de la tabla de emisiones
# para los gases de CO2, CH4 y N2O

tabla_emision<-function(filas_sector_energia,gas,doc){
  ##Parametros
  #filas_sector_energia:posiciones del doc donde esta la palabra "sector energia
  #gas: si es CO2,CH4,N2O
  #doc: tiene todo el texto del pdf
  
  ##Extraccion
  
  if(toupper(gas)=="CO2"){
    #Se obtienen los valores de la tabla
    #punto inicial 1
    ini_tabla<-filas_sector_energia[1]
    #quitando encabezado
    ind_datos <-(ini_tabla+7):(filas_sector_energia[2]-1)
  }
  
  if(toupper(gas)=="CH4"){
    #Se obtienen los valores de la tabla
    #punto inicial 2
    ini_tabla<-filas_sector_energia[2]
    #quitando encabezado
    ind_datos <-(ini_tabla+4):(filas_sector_energia[3]-1)
  }
  
  if(toupper(gas)=="N2O"){
    #Se obtienen los valores de la tabla
    #punto inicial 3
    ini_tabla<-filas_sector_energia[3]
    #quitando encabezado
    ind_datos <-(ini_tabla+4):(filas_sector_energia[4]-1)
  }
  
  #datos
  datos_tabla <- doc[ind_datos]
  datos_tabla <- datos_tabla %>%
    str_squish() %>%
    strsplit(split = "[[:space:]]")
  
  #Al separar por espacios hay palabras de la fuente que se deben agrupar para los casos
  #en que el tamaño es mayor a 4 (cantidad de columnas de la tabla)
  datos_tabla<-lapply(datos_tabla,
                          function(x) 
                            if(length(x)>4){
                              data.frame(rbind(c(paste(x[1:(length(x)-3)], collapse = ' '),x[(length(x)-2):length(x)])))
                            }else{
                              data.frame(rbind(x))
                            }
  )
  
  #Une todas en un solo data frame
  tabla<-rbindlist(datos_tabla,use.names = T,fill=TRUE)
  tabla<-tabla %>% 
    select(X1,X2,X3,X4) %>% 
    filter(!is.na(X1)) %>% 
    mutate(Gas=toupper(gas))
  
  colnames(tabla)<-c("Fuente_combustible","Factor_emision","Lim_inf","Lim_sup","Gas")
  
  #Agregar Unidades
  doc2<-str_squish(toupper(doc))
  filas_unidades <- grep("L COMBUSTIBLE", doc2)
  unidades <- doc2[filas_unidades]
  unidades <- unidades %>% 
    str_remove_all(pattern = "INFERIOR") %>% 
    str_remove_all(pattern = "SUPERIOR") %>% 
    str_remove_all(pattern = "\\(") %>% 
    str_remove_all(pattern = "\\)") %>% 
    str_squish()
  
  unidades<-unidades[grep(gas,unidades)]  
  
  tabla<-tabla %>% 
    mutate(Unidad=unidades)
  
  return(tabla)
  
}
  
  
#Descripcion funcion 1: Formato para extraer los datos de la tabla de potencial de calentamiento
# para todos los gases 

tabla_potencial<-function(ini_tabla_potenc,inicio_paginas,doc){
  ##Parametros
  #ini_tabla_potenc :posicion del doc donde esta las palabras "Potencial de calentamiento"
  #inicio_paginas: posiciones de los inicios de pagina de doc
  #doc: tiene todo el texto del pdf
  
  ##Extraccion
  
  #quitando encabezado
  ind_datos <-(ini_tabla_potenc+2):(inicio_paginas[length(inicio_paginas)]-1)
  #datos
  datos_tabla_potenc <- doc[ind_datos]
  datos_tabla_potenc <- datos_tabla_potenc %>%
    str_squish() %>%
    strsplit(split = "[[:space:]]")
  
  #Al separar por espacios hay palabras del gas que se deben agrupar y en el potencial 
  # los miles estan separados por espacios, por lo que los que tienen un largo mayor a 2
  #hay que verificar si la penultima posicion tiene un numero, si si tiene un numero
  #se junta con la ultima posicion, si no tiene un numero se junta con las primeras posiciones
  #que corresponden al gas
  
  datos_tabla_potenc<-
    suppressWarnings(lapply(datos_tabla_potenc,
                            function(x)
                              if(length(x)>2){
                                if(!is.na(as.numeric(x[length(x)-1]))){
                                  data.frame(rbind(c(paste(x[1:(length(x)-2)], collapse = ' '),paste(x[(length(x)-1):length(x)], collapse = ' '))))
                                }else{
                                  data.frame(rbind(c(paste(x[1:(length(x)-1)], collapse = ' '),x[length(x)])))
                                }
                              }else{
                                data.frame(rbind(x))
                              }
    )
    )
  
  #Une todas en un solo data frame
  tabla_potenc<-rbindlist(datos_tabla_potenc,use.names = T,fill=TRUE)
  tabla_potenc<-tabla_potenc %>% 
    select(X1,X2) %>% 
    filter(!is.na(X1)) %>% 
    mutate(X2= as.numeric(str_remove_all(X2, pattern = "\\s")))
  
  colnames(tabla_potenc)<-c("Gas","Potencial_calentamiento")
  
  return(tabla_potenc)
}


#Descripcion funcion 1: Le da formato al PDF para extraer los datos tomando en cuenta
#las dos funciones anteriores

extraer_datos_pdf<-function(pathpdf){
  ##Parametros
  #pathPDF: direccion de carpeta donde esta el pdf
  
  ##1. Preparacion del pdf
  # Establece el controlador para leer el texto
  read <- readPDF(control = list(text = "-layout"))
  
  #Lee el archivo pdf
  documento <- Corpus(URISource(pathpdf), 
                      readerControl = list(reader = read,language = "es"))
  
  #Extrae de la variable documento el contenido asociado al pdf que se leyó. 
  #Se crear un arreglo con tantas entradas como paginas tenga el documento.
  doc_origen <- content(documento[[1]])
  
  #Se une el texto del documento. Se utiliza '\f', como fin de página
  doc_unido <-paste(doc_origen, collapse = '\f')
  
  #Se separa por líneas el documento
  doc_lineas<- strsplit(doc_unido, "\r\n")
  
  #Se deja el contenido en una variable
  doc <- doc_lineas[[1]]
  
  #Se ubican los pie de página
  filas_pie_pagina <- grep("\f", doc) - 1
  
  #Se eliminan las páginas que son pie de página
  doc <- doc[- filas_pie_pagina]
  
  #2.Extraer factor de emision y potencial de calentamiento
  
  #2.1 Factor de emision
  
  #Se busca el inicio de las tablas con la palabra clave "Sector energía"
  filas_sector_energia <- grep("Sector energía", doc)
  
  ##Tabla CO2 - dioxido de carbono
  tabla_CO2<-tabla_emision(filas_sector_energia,"CO2",doc)
  ##Tabla CH4 - Metano
  tabla_CH4<-tabla_emision(filas_sector_energia,"CH4",doc)
  
  ##Tabla N2O - Oxido nitroso
  tabla_N2O<-tabla_emision(filas_sector_energia,"N2O",doc)
  
  #Unir las tablas de CO2, CH4, N2O
  tabla_factor_emision<-data.frame(rbind(tabla_CO2,tabla_CH4,tabla_N2O))
  
  #Limpieza
  tabla_factor_emision<-tabla_factor_emision %>%
    select(Fuente_combustible,Factor_emision,Gas,Unidad) %>% 
    mutate(
      Fuente_combustible=str_replace_all(Fuente_combustible, pattern = "[[:punct:]]", replacement = "_"),
      Fuente_combustible=str_to_sentence(Fuente_combustible),
      Fuente_combustible=stri_trans_general(Fuente_combustible,"Latin-ASCII"),
      Factor_emision=as.numeric(str_replace_all(Factor_emision, pattern = "[[:punct:]]", replacement = "."))
    )
  tabla_factor_emision$Fuente_combustible<-gsub( "contruccion","construccion",tabla_factor_emision$Fuente_combustible)
  
  #2.2 Potencial de calentamiento
  
  #Se busca el inicio de la tabla con la palabra clave "Potencial de calentamiento"
  ini_tabla_potenc <- grep("Potencial de calentamiento", doc)
  #posición de encabezado de la pagina que le sigue que es la ultima
  inicio_paginas<-grep("\f", doc)
  tabla_potenc<-tabla_potencial(ini_tabla_potenc,inicio_paginas,doc)
  
  
  #3. Union de la emision y el potencial
  tabla_factor_emision<-tabla_factor_emision %>% 
    left_join(tabla_potenc,by=c("Gas"="Gas"))
  
  #4. Agregar la version a la tabla
  
  #posicion donde se encuentra la palabra clave edicion
  pagEdicion <- grep("edición", doc)
  #Extraer el texto
  lineaEdicion<- doc[pagEdicion]
  #Quitar signos de puntuacion
  lineaEdicion<-str_squish(str_remove_all(lineaEdicion, pattern = "[[:punct:]]"))
  
  tabla_factor_emision<-tabla_factor_emision %>% 
    mutate(Referencia=paste0("IMN ",lineaEdicion))
  
  return(tabla_factor_emision)
}















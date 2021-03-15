
#Paquetes
library(readxl)
library(here)
library(dplyr)
library(stringr)
library(stringi)


#Descripcion de funcion 1: Limpieza de los diccionarios estandarizando los nombres de columnas y 
#textos tomando en cuenta posibles modificaciones manuales que puedan ocasionar un error 

clean_diccionario<-function(dicc){
  #Parametros de la funcion
  #dicc: es el diccionario que se extrae del excel
  
  
  #Limpieza
  
  #1.Estandarizar el nombre de columnas
  col_dic<-colnames(dicc)
  
  ##Quitar espacios en blanco y Solo primer letra en mayuscula
  col_dic<-str_to_sentence(str_squish(col_dic))
  
  ##Quitar tildes
  col_dic<-stri_trans_general(col_dic,"Latin-ASCII")
  
  ##Palabras con el separador _  
  col_dic[grepl("Sub",col_dic)==T & grepl("fuente",col_dic)==T]<-"Sub_fuente"
  col_dic[grepl("Sub",col_dic)==T & grepl("sector",col_dic)==T]<-"Sub_sector"
  col_dic[grepl("Sub",col_dic)==T & grepl("categoria",col_dic)==T]<-"Sub_categoria"
  col_dic[grepl("Sub",col_dic)==T & grepl("alcance",col_dic)==T]<-"Sub_alcance"
  col_dic[grepl("Cuenta",col_dic)==T & grepl("contable",col_dic)==T]<-"Cuenta_contable"
  col_dic[grepl("Cuenta",col_dic)==T & grepl("descripcion",col_dic)==T]<-"Cuenta_descripcion"
  col_dic[grepl("Fuente",col_dic)==T & grepl("combustible",col_dic)==T]<-"Fuente_combustible"
  
  colnames(dicc)<-col_dic
  
  
  #2.Estandarizar el texto
  
  #2.1.En las columnas: Fuente,Sub_fuente,Sector,Sub_sector,Categoria,Sub_categoria,Sub_alcance
  #2.2.En las columnas: Gas
  #2.3.En las columnas: Fuente_combustible
  
  #3.Verificar el tamaño de la columna cuenta contable deberia tener 17 caracteres
  
  for (i in 1:ncol(dicc)) {
    col_name_t<-colnames(dicc)[i]
    
    if(col_name_t %in% 
       c("Tipo","Fuente","Sub_fuente","Sector","Sub_sector","Categoria","Sub_categoria","Sub_alcance")){
      
      colnames(dicc)[i]<-"temp"
      
      #Quitar espacios en blanco y Solo primer letra en mayuscula
      dicc<-dicc %>% 
        mutate(temp=str_to_sentence(str_squish(temp)))
      
      #Quitar tildes
      dicc<-dicc %>% 
        mutate(temp=stri_trans_general(temp,"Latin-ASCII"))
      
      #Quitar signos de puntuacion 
      dicc<-dicc %>% 
        mutate(temp=str_remove_all(temp, pattern = "[[:punct:]]"))
      
      colnames(dicc)[i]<-col_name_t
    }
    
    if(col_name_t %in% c("Gas")){
      
      #Quitar espacios en blanco y todo en mayuscula
      dicc<-dicc %>% 
        mutate(Gas=toupper(str_squish(Gas)))
      
      #Quitar tildes
      dicc<-dicc %>% 
        mutate(Gas=stri_trans_general(Gas,"Latin-ASCII"))
      
      #Quitar signos de puntuacion 
      dicc<-dicc %>% 
        mutate(Gas=str_remove_all(Gas, pattern = "[[:punct:]]"))
    }
    
    if(col_name_t %in% c("Fuente_combustible")){
      
      #Quitar espacios en blanco y Solo primer letra en mayuscula
      dicc<-dicc %>% 
        mutate(Fuente_combustible=str_to_sentence(str_squish(Fuente_combustible)))
      
      #Quitar tildes
      dicc<-dicc %>% 
        mutate(Fuente_combustible=stri_trans_general(Fuente_combustible,"Latin-ASCII"))
      
      #Reemplazar signos de puntuacion por guion bajo
      dicc<-dicc %>% 
        mutate(Fuente_combustible=str_replace_all(Fuente_combustible, pattern = "[[:punct:]]", replacement = "_"))
    }
    
    if(col_name_t %in% c("Cuenta_contable")){
      
      #Quitar espacios en blanco
      dicc<-dicc %>% 
        mutate(Cuenta_contable=str_squish(Cuenta_contable))
    }
  }
  
  return(data.frame(dicc))

}


#Descripcion de funcion 2: Extraer nombres de las hojas del excel del diccionario, ya que al ser una archivo
#para manipularlo manual puede que se cambien los formatos de los nombres.


hojas_diccionario<-function(path_dicc){
  #Parametros de la funcion
  #path_dicc: path del excel
  
  
  #Nombres de hojas 
  sheets_dic<-excel_sheets(path_dicc)

  #Estandarizar nombre para identificar hojas
  
  #quitar espacios y en minuscula
  sheets_dic_t<-tolower(str_squish(sheets_dic))
  
  ##Quitar tildes
  sheets_dic_t<-stri_trans_general(sheets_dic_t,"Latin-ASCII")
  
  
  hojas<-c(sheets_dic[grep("fuente",sheets_dic_t)],
           sheets_dic[grep("sector",sheets_dic_t)],
           sheets_dic[grep("gas",sheets_dic_t)])
  
  return(hojas)
  
}



#Descripcion de funcion 3: Extraer nombres de las hojas del excel de la base de datos del sistema,
#ya que al ser una archivo para manipularlo manual puede que se cambien los formatos de los nombres.


hojas_datos_sistema<-function(path_dicc){
  #Parametros de la funcion
  #path_dicc: path del excel
  
  
  #Nombres de hojas 
  sheets<-excel_sheets(path_dicc)
  
  #Estandarizar nombre para identificar hojas
  
  #quitar espacios y en minuscula
  sheets_t<-tolower(str_squish(sheets))
  
  ##Quitar tildes
  sheets_t<-stri_trans_general(sheets_t,"Latin-ASCII")
  
  hojas<-sheets[grep("datos",sheets_t)]
  
  return(hojas)
  
}

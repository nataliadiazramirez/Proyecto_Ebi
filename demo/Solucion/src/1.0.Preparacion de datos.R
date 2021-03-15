
#Descripcion: Preparacion de datos. Aqui se realiza la union de la base del sistema
#con los diccionarios y los factores del pdf para realizar los calculos y datos finales



#### Paquetes
library(readxl)
library(writexl)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)



#### Directorios

utilities_data<-paste0(here(),"/demo/Solucion/utilities")
diccionarios_data<-paste0(here(),"/demo/Solucion/data/diccionarios")
external_data<-paste0(here(),"/demo/Solucion/data/external")
interim_data<-paste0(here(),"/demo/Solucion/data/interim")

pdf_path<-paste0(external_data,"/FactoresEmision-GEI-2019.pdf")

#### Archivos requeridos

#Funciones
source(paste0(utilities_data,"/funcion_limpieza_diccionarios_datos.R"))
source(paste0(utilities_data,"/funcion_extraccion_datosPDF.R"))

#Datos sistema
datos_sistema<-read_excel(paste0(external_data,"/Base de Datos Sistema.xlsx"),
                        sheet = hojas_datos_sistema(paste0(external_data,"/Base de Datos Sistema.xlsx")))

#Diccionario fuente
dicc_fuente<-read_excel(paste0(diccionarios_data,"/diccionario_diesel.xlsx"),
                        sheet = hojas_diccionario(paste0(diccionarios_data,"/diccionario_diesel.xlsx"))[1])

#Diccionario sector
dicc_sector<-read_excel(paste0(diccionarios_data,"/diccionario_diesel.xlsx"),
                        sheet = hojas_diccionario(paste0(diccionarios_data,"/diccionario_diesel.xlsx"))[2])

#Diccionario gas
dicc_gas<-read_excel(paste0(diccionarios_data,"/diccionario_diesel.xlsx"),
                        sheet = hojas_diccionario(paste0(diccionarios_data,"/diccionario_diesel.xlsx"))[3])



#### Preparacion de datos

#0. Limpiar diccionarios
dicc_fuente<-clean_diccionario(dicc_fuente)
dicc_sector<-clean_diccionario(dicc_sector)
dicc_gas<-clean_diccionario(dicc_gas)


#1. De la base de datos obtener por año,articulo, bodega y cuenta contable la cantidad
data<-datos_sistema %>% 
  mutate(anio= year(Fecha)) %>% 
  group_by(anio,ARTICULO,BODEGA,CUENTA_CONTABLE) %>% 
  summarise(cantidad=sum(CANTIDAD)) %>% 
  filter(ARTICULO=="050101")


#2. Unir con diccionario fuente por medio de la cuenta contable y agrupar por fuente
data<-data %>% 
  left_join(dicc_fuente,by=c("CUENTA_CONTABLE"="Cuenta_contable")) %>% 
  group_by(anio,ARTICULO,BODEGA,Fuente) %>% 
  summarise(cantidad=sum(cantidad))


#3. Unir con diccionario sector por la fuente
data<-data %>% 
  left_join(dicc_sector,by=c("Fuente"="Fuente")) %>% 
  select(anio,Tipo,ARTICULO,BODEGA,Sector,Sub_sector,Categoria,Sub_categoria,Fuente,Alcance,Sub_alcance,cantidad)


#4. Unir con diccionario gas  por medio de la fuente
data_emision<-data %>% 
  left_join(dicc_gas,by=c("Fuente"="Fuente"))


#5.En datos del pdf calcular el factor de conversion

#Extraer datos del pdf
datos_pdf<- extraer_datos_pdf(pdf_path)

#Calculo del factor de conversion
options(scipen = 999)
datos_pdf<-datos_pdf %>% 
  mutate(
    Factor_conversion=case_when(
      Gas=="CO2"~ Factor_emision*((1/1000)*Potencial_calentamiento),
      Gas=="CH4"|Gas=="N2O"~ Factor_emision*((1/1000000)*Potencial_calentamiento)
    )
  ) %>% 
  select(Fuente_combustible,Gas,Potencial_calentamiento,Factor_emision,Factor_conversion,
         Unidad,Referencia) %>% 
  arrange(Fuente_combustible)


#5. Unir con el PDF por medio del gas y fuente de combustible para obtener el
#Potencial de calentamiento y Factor de emisión para calcular el Factor de conversion 

data_emision<-data_emision %>% 
  left_join(datos_pdf,by=c("Fuente_combustible"="Fuente_combustible","Gas"="Gas"))

#6. calcular las emisiones

data_emision<-data_emision %>% 
  mutate(emision=cantidad*Factor_conversion,
         Gas=paste0(Gas,"_(tCO2e)")) %>% 
  select(
    anio,Tipo,ARTICULO,BODEGA,Sector,Sub_sector,Categoria,Sub_categoria,Fuente,Alcance,Sub_alcance,
    Gas,emision )

#calcular el total
data_emision<-data_emision %>% 
  group_by(anio,Tipo,ARTICULO,BODEGA,Sector,Sub_sector,Categoria,Sub_categoria,Fuente,Alcance,Sub_alcance) %>% 
  mutate(`Total_(tCO2e)`=sum(emision)) 



#transponer gas
data_emision<-data_emision %>%
  pivot_wider(
    id_cols = c(anio,Tipo,ARTICULO,BODEGA,Sector,Sub_sector,Categoria,Sub_categoria,
                Fuente,Alcance,Sub_alcance,`Total_(tCO2e)`),
    names_from = Gas,
    values_from = emision,
    values_fill = 0
  )


if("NA_(tCO2e)" %in% colnames(data_emision)){
  data_emision<-data_emision %>% 
    select(-`NA_(tCO2e)`)
}


#7.Calcular porcentaje del total
total_emision<-sum(data_emision$`Total_(tCO2e)`,na.rm = T)

data_emision<-data_emision %>%
  mutate(Porcentaje=`Total_(tCO2e)`/total_emision)

#Columnas finales
data_emision<-data_emision %>%
  select(anio,Tipo,ARTICULO,BODEGA,Sector,Sub_sector,Categoria,Sub_categoria,
         Fuente,Alcance,Sub_alcance,`CO2_(tCO2e)`,`CH4_(tCO2e)`,`N2O_(tCO2e)`,
         `Total_(tCO2e)`,Porcentaje) %>% 
  ungroup()

#Renombrar la bodega
data_emision<-data_emision %>%
  mutate(BODEGA=case_when(
    BODEGA=="ASE"~"PTA Aczarri",
    BODEGA=="LIM"~"PTA Limón",
    BODEGA=="URU"~"PTA Uruka"
  )) %>% 
  rename(Lugar="BODEGA")

#8. Calcular los totales de emision (no agrupando por bodega)
data_emision_total<-data_emision %>%
  group_by(anio,Tipo,ARTICULO,Sub_categoria) %>% 
  summarise(
    `CO2_(tCO2e)`=sum(`CO2_(tCO2e)`,na.rm = T),
    `CH4_(tCO2e)`=sum(`CH4_(tCO2e)`,na.rm = T),
    `N2O_(tCO2e)`=sum(`N2O_(tCO2e)`,na.rm = T),
    `Total_(tCO2e)`=sum(`Total_(tCO2e)`,na.rm = T) 
  ) %>%
  mutate(Porcentaje=`Total_(tCO2e)`/total_emision)



#9. Exportar la tabla de emisiones,la base de Cuadro de fuentes y sumideros, base para word
write_xlsx(list(Factores_emision=datos_pdf,
                Fuentes_Sumideros_Lugar=data_emision,
                Fuentes_Sumideros_Total=data_emision_total
                ),
           paste0(interim_data,"/Resultados_Preparacion de Datos.xlsx"))









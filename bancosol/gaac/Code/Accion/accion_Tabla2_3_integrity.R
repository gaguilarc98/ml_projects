####____CARGA DE LIBRERIAS Y FUNCIONES_____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ggplot2)
library(ggrepel)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____APERTURA CLIENTES ADRIANA____####
id_masked <- read_excel("D:/!bso/shared/SinfinancialTool_20230815.xlsx")

t2names <- c("Llave_Cliente","Fecha_Crea","Fecha_1Cred","Genero","Fecha_nacimiento",
             "Lugar_nacimiento","Estado_Civil","Nivel_Escolaridad","Num_Hijos","Num_personas_Cargo",
             "Tenencia_Hogar","Tiempo_Lugar_Resd","Tenencia_Local","Antigüedad_Microemp",
             "Antigüedad_Local","Tipo_Ubicacion_Local","Num_Empleados","Experiencia_Microemp")
tabla2 <- fread("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/deliver_final/deliver_tabla_2/TABLA2_2017_2023.csv",
                sep=';',encoding = "UTF-8",col.names = t2names)

lfiles <- list.files("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/deliver_final/deliver_tabla_3/")
t3List <- list()
for (i in 2:length(lfiles)) {
  t3_temp <- read_excel(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/deliver_final/deliver_tabla_3/",
                               lfiles[i])) %>% 
    mutate(year = as.numeric(substr(lfiles[i],8,11)))
  t3List[[i]] <- t3_temp
}
t3Full <- rbindlist(t3List)
saveRDS(t3Full, "D:/!bso/accion/tabla3_2017_2023.rds")
t3Full <- readRDS("D:/!bso/accion/tabla3_2017_2023.rds")
####____JOINS WITH TABLA 2 y 3____####
glimpse(id_masked)
glimpse(tabla2)
glimpse(t3Full)

####____INTEGRIDAD DE TABLAS____####
sapply(tabla2, function(x){sum(is.na(x))})
sapply(t3Full, function(x){sum(is.na(x))})
#TABLA 2
table(tabla2$Estado_Civil, useNA = "always")
table(tabla2$Nivel_Escolaridad, useNA = "always")
table(tabla2$Num_Hijos, useNA = "always")
table(tabla2$Num_personas_Cargo, useNA = "always")
table(tabla2$Tenencia_Hogar, useNA = "always")
table(tabla2$Tenencia_Local, useNA = "always")
table(tabla2$Num_Empleados, useNA = "always")
table(tabla2$, useNA = "always")

#TABLA 3
table(t3Full$Sector, useNA = "always")
####____UNION____####
ids_t2 <- id_masked %>% 
  inner_join(tabla2,by=c("CustomerId"="Llave_Cliente"))

AJids_t2 <- id_masked %>% 
  anti_join(tabla2,by=c("CustomerId"="Llave_Cliente"))

ids_t3 <- id_masked %>% 
  inner_join(t3Full,by=c("LoanId"="llaveprimaria")) %>% 
  group_by(LoanId) %>% 
  mutate(n=max(row_number())) %>% 
  ungroup()

AJids_t3 <- id_masked %>% 
  anti_join(t3Full,by=c("LoanId"="llaveprimaria"))




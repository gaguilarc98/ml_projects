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

####____LECTURA DE TABLAS____####
setwd("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/")
table1_part1 <- fread('D:/!bso/accion/deliver_final/deliver_tabla_1/TABLA1_2017_2023_parte1.csv',
                      encoding = "UTF-8",fill = T,sep=";") %>% 
  remove_empty("cols") %>% 
  rename(Cod_Solicitud=V1,Llave_Cuenta=V2,Decision_Cred=V3,Fecha_Decision=V4)
# table1_part2 <- fread('D:/!bso/accion/deliver_final/deliver_tabla_1/TABLA1_2017_2023_parte2.csv',
#                       encoding = "UTF-8",fill = T,sep=";")

table2_part1 <- fread('D:/!bso/accion/deliver_final/deliver_tabla_2/TABLA2_2017_2023_parte1.csv',
                      encoding = "UTF-8",fill = T,sep=";") %>% 
  remove_empty("cols") %>% 
  rename(Llave_Cuenta=V1, Fecha_Crea=V2, Fecha_1Cred=V3, Genero= V4, Fecha_nacimiento=V5,
         Lugar_nacimiento=V6, Estado_Civil=V7, Nivel_Escolaridad=V8, Num_Hijos=V9,
         Num_Personas_Cargo=V10, Tenencia_Hogar=V11, Tenencia_Local=V12)
# table2_part2 <- fread('D:/!bso/accion/deliver_final/deliver_tabla_2/TABLA2_2017_2023_parte2.csv',
#                       encoding = "UTF-8",fill = T,sep=";")
sapply(table1_part1, function(x){length(which(is.na(x)))})
sapply(table2_part1, function(x){length(which(is.na(x)))})

years <- 2017:2023
tabla3List <- list()
for (i in 1:length(years)) {
  print(years[i])
  table3 <- readxl::read_xlsx(paste0('D:/!bso/accion/deliver_final/deliver_tabla_3/Tabla3_',years[i],'.xlsx')) %>% 
    mutate(year=years[i])
  tabla3List[[i]] <- table3
}
tabla3Full <- rbindlist(tabla3List)

sapply(tabla3Full, function(x){length(which(is.na(x)))})
#En tabla 3 se encuentra la información de cada operación y cuenta cliente por
#Cada solicitud del cliente.
#En tabla 1 se encuentra la información de la cuenta cliente por cada solicitud.
#En tabla 2 se encuentra la información genérica de cada cliente.
#Unir tabla 3 con 1 por codigo de solicitud y llave_cuenta y luego unir tabla 2 por cuenta

tablaJoin3_1 <- tabla3Full %>% 
  left_join(table1_part1,by=c("Cod_Solicitud","Llave_Cuenta"))
  
sapply(tablaJoin3_1, function(x){length(which(is.na(x)))})

tablaJoin3_1_2 <- tablaJoin3_1 %>% 
  left_join(table2_part1,by="Llave_Cuenta")

sapply(tablaJoin3_1_2, function(x){length(which(is.na(x)))})
################################################################################
#Verificación de unicidad de la combinación Cod_Solicitud y llaveprimaria
x <- tablaJoin3_1_2 %>% 
  group_by(Cod_Solicitud,llaveprimaria) %>% 
  mutate(m=max(row_number())) %>% 
  ungroup() %>% 
  dplyr::filter(m>1)

y <- tablaJoin3_1_2 %>% 
  group_by(Cod_Solicitud,llaveprimaria) %>% 
  mutate(m=max(row_number())) %>% 
  ungroup() %>% 
  dplyr::filter(m<=1)
################################################################################
tablaJoin3_1_2clean <- tablaJoin3_1_2 %>% 
  group_by(Cod_Solicitud,llaveprimaria) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() 
####___LECTURA DE ETAPA 1____####
month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2020:2023)
mon <- c("ene. ","feb. ","mar. ","abr. ","may. ","jun. ","jul.","ago. ","sep. ","oct. ","nov. ","dic. ")
mycsv <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))
myutf <- as.vector(sapply(year, function(x){paste0(x,mes)}))
monDate <- as.vector(sapply(year, function(x){paste0(mon,x)})) 
report_Etapa1_312 <- data.frame(fbase = as.character(NULL),
                      N_KEY = as.numeric(),
                      N_MISS_PRE_2017 = as.numeric(),
                      N_MISS_POST_2017 = as.numeric(),
                      N_MISS_DES = as.numeric())
report_Buro_312 <- data.frame(fbase = as.character(NULL),
                              N_KEY_OPS_ONLY = as.numeric(),
                              N_NOT_FOUND = as.numeric(),
                              N_MISS_PRE_2017 = as.numeric(),
                              N_MISS_POST_2017 = as.numeric(),
                              N_MISS_DES = as.numeric(),
                              N_MISS_CTA = as.numeric())

Cuentas <- tablaJoin3_1_2clean %>% 
  select(Llave_Cuenta) %>% 
  distinct_all() %>% 
  mutate(countercta=1)
Ops <- tablaJoin3_1_2clean %>% 
  select(Llave_Cuenta,Llave_Operacion,llaveprimaria) %>% 
  distinct_all() %>% 
  mutate(counterop=1)

for (i in 1:length(mycsv)) {
  tryCatch({
    print(mycsv[[i]])
    stage1 <- fread(paste0('D:/!bso/accion/deliver_final/deliver_Etapa_1_csv/',
                                    mycsv[i],'.csv'),encoding = "UTF-8",sep=",") %>% 
      select(llaveprimaria=LLAVEPRIMARIA,fdes=Fecha.de.desembolso) %>% 
      mutate(pre2017 = ifelse(year(fdes)<2017,1,0)) %>% 
      mutate(cosechaM = as.yearmon(fdes)) %>% 
      mutate(samemonth = ifelse(as.yearmon(fdes)==as.yearmon(monDate[i]),1,0))
    
    s1 <- n_distinct(stage1$llaveprimaria)
    stage1 <- stage1 %>% 
      left_join(Ops,by="llaveprimaria")
    
    report1 <- data.frame(fbase = myutf[i],
                       N_KEY = s1,
                       N_MISS_PRE_2017 = length(which(is.na(stage1$counterop[stage1$pre2017==1]))),
                       N_MISS_POST_2017 = length(which(is.na(stage1$counterop[stage1$pre2017==0]))),
                       N_MISS_DES = length(which(is.na(stage1$counterop[stage1$samemonth==1]))))
    
    buro <- fread(paste0('D:/!bso/accion/deliver_final/deliver_Buro_tabla_4_csv/BSO_masked_buro_',
                         myutf[i],'.csv'),encoding = "UTF-8",sep=",") %>% 
      rename(llaveprimaria=LLAVEPRIMARIA, Llave_Cuenta=MASCARA_CUENTA, Llave_Operacion=MASCARA_OPERACION)
    
    buro_ops_only <- buro %>% 
      dplyr::filter(!is.na(llaveprimaria))
    
    b1 <- n_distinct(buro$Llave_Cuenta)#Contador de cuentas distintas
    bop <- n_distinct(buro_ops_only$llaveprimaria) #Contador de filas con cuenta cliente y operacion
    
    buro_ops_only <- buro_ops_only %>% 
      left_join(stage1,by = "llaveprimaria")
    
    buro_cta_only <- buro %>% 
      group_by(Llave_Cuenta) %>% 
      dplyr::filter(row_number()==1) %>% 
      ungroup() %>% 
      left_join(Cuentas,by="Llave_Cuenta")
    
    report2 <- data.frame(fbase=myutf[i],
                          N_KEY_OPS_ONLY = bop,
                          N_NOT_FOUND = length(which(is.na(buro_ops_only$pre2017))),
                          N_MISS_PRE_2017 = length(which(is.na(stage1$counterop[stage1$pre2017==1]))),
                          N_MISS_POST_2017 = length(which(is.na(stage1$counterop[stage1$pre2017==0]))),
                          N_MISS_DES = length(which(is.na(stage1$counterop[stage1$samemonth==1]))),
                          N_MISS_CTA = length(which(is.na(buro_cta_only$countercta))))
    
    report_Etapa1_312 <- report_Etapa1_312 %>% bind_rows(report1)
    report_Buro_312 <- report_Buro_312 %>% bind_rows(report2)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

reportes <- list(report_Etapa1_312 = report_Etapa1_312,report_Buro_312 = report_Buro_312)
write.xlsx(reportes,"D:/!bso/accion/Reporte_Intgridad.xlsx")

####____INTEGRIDAD 2DA ENTREGA____####
setwd("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2")
month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2018:2019)

myxlsx <- as.vector(sapply(year, function(x){paste0('/',x,'/',mes,'_Etapa1_',month,x)}))
mydest <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))

repList <- list()
# file_list <- list.files(path='//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/rds/')
myxlsx <- "/04_Etapa1_abril2023"
mydest <- "04_Etapa1_abril2023"
for (i in 1:length(myxlsx)) {
  tryCatch({
    print(myxlsx[[i]])
    # keys <- readxl::read_excel(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2',
    #                                 myxlsx[i],'.xlsx'),sheet = "MS81479brllaves") %>% 
    #   select(CTACLIENTE=`Cuenta Cliente`, OPERACION = Operacion, MASCARA_CUENTA,
    #          MASCARA_OPERACION, LLAVEPRIMARIA) 
    bdc <- readxl::read_excel(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/Abril_2daEntrega',
                                    myxlsx[i],'.xlsx'),sheet = "MS81479brbase") 
    bdg <- readxl::read_excel(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/Abril_2daEntrega',
                                    myxlsx[i],'.xlsx'),sheet = "MS81479brgarantias")
    fwrite(bdc, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/all/',
                       mydest[i],'.csv'),row.names = F)
    fwrite(bdg, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/all/',
                       mydest[i],'_garantias.csv'),row.names = F)
    # bdcAux <- bdc %>% 
    #   select(FECHA = `Fecha de Corte`, LLAVEPRIMARIA, ESTADO, MODULO)
    # OBSERVATIONS <- bdc %>% 
    #   summarise(OBSERVACIONES = n(),N_SIN_LLAVEPRIMARIA = length(which(is.na(LLAVEPRIMARIA))))
    # 
    # REPETIDOS <- bdc %>% 
    #   dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
    #   group_by(LLAVEPRIMARIA) %>% 
    #   dplyr::filter(max(row_number())>1) %>% 
    #   ungroup() %>% 
    #   summarise(N_LLAVE_REPETIDA = n_distinct(LLAVEPRIMARIA))
    # 
    # Cleankeys <- keys %>% 
    #   dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
    #   distinct_all()
    # 
    # MATCHES <- bdc %>% 
    #   dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
    #   left_join(Cleankeys,by="LLAVEPRIMARIA") %>% 
    #   summarise(N_SIN_MATCH = length(which(is.na(OPERACION))))
    # 
    # ESTADOS <- bdc %>% 
    #   dplyr::filter(is.na(LLAVEPRIMARIA)) %>% 
    #   group_by(FECHA, ESTADO) %>% 
    #   mutate(ESTADO = paste0("SIN_LLAVEPRIMARIA_", ESTADO)) %>% 
    #   summarise(Operaciones = n()) %>%
    #   ungroup() %>% 
    #   pivot_wider(names_from = ESTADO, values_from = Operaciones)
    # 
    # REPORTE <- OBSERVATIONS %>% 
    #   bind_cols(REPETIDOS,MATCHES,ESTADOS) %>% 
    #   relocate(FECHA)
    # 
    # repList[[i]] <- REPORTE
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
repFull <- rbindlist(repList)

write_xlsx(repFull,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/ReporteIntegridad_202305.xlsx'))

Diccionario <- data.frame(VARIABLE = names(repFull),
                          DETALLE  = c("Fecha de base","Cantidad de observaciones","Cantidad de observaciones sin llave primaria",
                                       "Cantidad de llaves primarias repetidas","Cantidad de observaciones con llave primaria que no es encuentran en lista de llaves",
                                       "Cantidad de Observaciones"))
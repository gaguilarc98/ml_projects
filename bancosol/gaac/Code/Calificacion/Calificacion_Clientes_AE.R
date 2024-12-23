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
library(fastDummies)
library(openxlsx)
library(sqldf)
# require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FIJANDO TEMPORALIDAD DE REVISION____####
myRev <- "May2023"
####____LECTURA DE BASE BANTOTAL____####
bdcBSO <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myRev,'.rds')) %>%
  dplyr::filter(!MODULO %in% c(29,131)) %>% 
  select(CTACLIENTE, OPERACION, CI, CALIFICACION, MODULO, TIPO_OPER, ESTADO, DIASMORA) 
####____TRATAMIENTO DE INFOCRED____#####
####METODOLOGIA ANTERIOR
# * Eliminar castigos, líneas de crédito, boletas de garantía y castigos de la base de
# * Bantotal y agrupar por CI para obtener la peor calificación
# * Obtener la peor calificación en el SF de deudores titulares de INFOCRED agrupando por CI
# * Unir la base de BANTOTAL con la de INFOCRED por CI.
####PROBLEMAS IDENTIFICADOS
# * La unión por CI no es íntegra, ya que existen variaciones en los CI de Infocred y Bantotal
####SOLUCIÓN METODOLÓGICA
# * Eliminar líneas de crédito, boletas de garantía de la base de Bantotal
# * Seleccionar CTACLIENTE, OPERACION, CI, CALIFICACION
# * En Infocred añadir una columna de peor calificación en SF para cada CI
#   conservando la desagregación por OPERACION
# * Unir la base de BANTOTAL con la de INFOCRED por CTACLIENTE y OPERACION
# * Consolidar para cada CTACLIENTE la peor calificación en BSO y en SF.

infoCred <- readRDS(paste0('D:/!bso/califClientes/process/comp_',myRev,'.rds'))
infoClean <- infoCred %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(CALIF_SBEF = ifelse(esBSO==0, CALIFICACION, '_')) %>% 
  mutate(CALIF_SBEF = max(CALIF_SBEF)) %>% 
  ungroup() %>% 
  dplyr::filter(SIGLA=="BSO") %>% 
  select(CI, CTACLIENTE, OPERACION, CALIFICACION, CALIF_SBEF) 
  
infoCred_Bantotal <- bdcBSO %>% 
  inner_join(infoClean, by=c("CTACLIENTE","OPERACION"), suffix=c("_BSO","_SF")) %>% 
  group_by(CI_SF) %>% 
  mutate(NCTAS = n_distinct(CTACLIENTE)) %>% 
  ungroup() %>% 
  arrange(desc(NCTAS), CI_SF)

write_xlsx(infoCred_Bantotal, paste0("D:/!bso/requests/BD_CARTERA_INFO_CAL_",myRev,".xlsx"))
#CHECK DE INTEGRIDAD DE CALIFICACION ENTRE BANTOTAL Y LO REPORTADO EN INFOCRED
length(which(infoCred_Bantotal$CALIFICACION_BSO!=infoCred_Bantotal$CALIFICACION_SF))
Check <- infoCred_Bantotal %>% 
  dplyr::filter(CALIFICACION_SF!=CALIFICACION_BSO)
#Si observamos para Mayo 2023 exiten 17 inconsistencias, todas provienen de cartera
#migrada de FSL, a los cuales se tuvo que cambiar manualmente la califcación a "A"

####____TRATAMIENTO FINAL PARA EXPORTAR____####
infoCred_Bantotal_Exp <- infoCred_Bantotal %>% 
  group_by(CI_SF) %>% 
  summarise(CTACLIENTE = max(CTACLIENTE),
            CALIFICACION_BSO = max(CALIFICACION_BSO),
            SBEF_CALIFICACION_SF = max(CALIF_SBEF))

fwrite(infoCred_Bantotal_Exp, paste0("D:/!bso/requests/BD_CARTERA_INFO_CAL",myRev,".csv"),
       quote = F,sep = '|',row.names = F)
#CLIENTES CAPTURADOS CON METODOLOGÍA ACTUAL
n_distinct(infoCred_Bantotal$CI_BSO)
n_distinct(infoCred_Bantotal$CI_SF)
n_distinct(infoCred_Bantotal$CTACLIENTE)
#CLIENTES CAPTURADO CON METODOLOGIA ANTERIOR
length(which(unique(bdcBSO$CI) %in% unique(infoClean$CI)))
  
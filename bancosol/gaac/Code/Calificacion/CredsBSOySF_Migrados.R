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
####____LECTURA DE BASE DE MKT____####
migMKT <- read_xlsx("D:/!bso/shared/opsMigradasSep2023.xlsx")

####____CARGA DE BASES____####

shortcred <- "Ago2023"

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortcred,'.rds'))

infoCIS <- infoCheck %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  
  dplyr::filter(esBSO == 1) %>% 
  select(CI_inf = CI, CTACLIENTE, OPERACION)

bdc_raw  <- readRDS("D:/!bso/girCartera/rds/ec_Sep2023.rds")
bdc  <- bdc_raw %>% 
  left_join(infoCIS, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(NOMBRE = str_trim(paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT))) %>% 
  mutate(CI_inf = ifelse(is.na(CI_inf), CI, CI_inf)) %>% 
  mutate(NDOC = str_replace(CI_inf, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"),1,0)) %>% 
  mutate(FECHA = as.Date(monDate, frac=1)) %>% 
  select(FECHA, CTACLIENTE,OPERACION,MODULO,TIPO_OPER,AGENCIA,NOMBRE_AGENCIA,NOMBRE,
         CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA, esFSL,NDOC,CI_inf)
sum(bdc$esFSL)
bdcFSL <- bdc %>% 
  select(CTACLIENTE, OPERACION, esFSL)

MultCreds <- bdc %>% 
  mutate(noFSL = 1-esFSL) %>% 
  group_by(NDOC) %>% 
  mutate(tieneFSL = max(esFSL)) %>%
  mutate(tieneNOFSL = max(noFSL)) %>%
  # dplyr::filter(max(row_number())>1) %>% 
  dplyr::filter(tieneFSL==1 & tieneNOFSL==1) %>% 
  arrange(NDOC) %>% 
  select(-tieneFSL, -tieneNOFSL)

infoClientes <- infoCheck %>% 
  # dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
  mutate(esBSO = ifelse(SIGLA=='BSO', 1, 0)) %>%
  mutate(noesBSO = ifelse(SIGLA!='BSO', 1, 0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  left_join(bdcFSL, by=c("CTACLIENTE", "OPERACION")) %>% 
  replace_na(list(esFSL=0)) %>% 
  group_by(CI) %>%
  mutate(tieneFSL = max(esFSL)) %>% 
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(PEOR_CALIF_SF = max(CALIFICACION[esBSO==0])) %>% 
  mutate(PEOR_CALIF_BSO = max(CALIFICACION[esBSO==1])) %>% 
  dplyr::filter(tieneFSL ==1) %>% #esBSO==0
  dplyr::filter(!(SIGLA=="BSO" & esFSL==0)) %>% 
  ungroup() %>% 
  mutate(FECHA = as.Date("2023-08-31")) %>% 
  select(FECHA, CTACLIENTE, OPERACION, CI, REGULADO, NOMBRE, TIPO_OBLIGADO, SIGLA, ENTIDAD, 
         TIPO_CREDITO, CALIFICACION, ESTADO, DiasMora, FECHAINICIO, FECHAVTO,
         MonedaOrigen, MontoOriginal, starts_with("saldo"), starts_with("PEOR"),
         Periodo_Pago, HISTORICO, esFSL)

Base <- list(CredsBSO_Migrada = MultCreds, CredsSF_Migrada = infoClientes)

write_xlsx(Base, "D:/!bso/requests/CreditosCarteraMigrada_Sep2023.xlsx")

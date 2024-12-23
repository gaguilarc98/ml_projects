####____CARGA DE PAQUETES Y LIBRERIAS____####
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
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

####____MUESTRAS____####
cic <- readRDS("D:/!bso/CIC/rds/cic_Sep2023.rds") %>% 
  mutate(Edad = floor(as.numeric(FechaCorte-FechaNacimiento)/365)) %>%
  group_by(CTACLIENTE) %>% 
  summarise(Edad=max(Edad)) %>% 
  ungroup()

dfAux <- readRDS('D:/!bso/features/Clientes_Ene15Sep23.rds') %>% 
  group_by(CTACLIENTE) %>% 
  arrange(fdes) %>% 
  mutate(Ciclo = row_number()) %>% 
  mutate(esMontoMax = ifelse(MONTOUS == max(MONTOUS),1,0)) %>% 
  mutate(esMontoMin = ifelse(MONTOUS == min(MONTOUS),1,0)) %>% 
  mutate(MONTO_MAXIMO_USD = max(MONTOUS),
         MONTO_MINIMO_USD = min(MONTOUS),
         Fecha_Monto_Max = max(fdes[esMontoMax==1]),
         Fecha_Monto_Min = min(fdes[esMontoMin==1])) %>% 
  ungroup() %>% 
  select(CTACLIENTE, OPERACION, MONTO_MAXIMO_USD, MONTO_MINIMO_USD, Fecha_Monto_Max,
         Fecha_Monto_Min, Ciclo)

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Sep2023.rds") %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA, DIASMORA) %>% 
  left_join(dfAux, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(cic, by = "CTACLIENTE")

bdcCriterios <- bdc %>% 
  dplyr::filter(FDESEMBOLSO>=as.Date("2021-09-01")) %>% 
  dplyr::filter(day(FDESEMBOLSO)>=24) %>% 
  dplyr::filter(Ciclo %in% c(1)) %>% 
  dplyr::filter(is.na(Edad) | (Edad>=25 & Edad<=35))

write_xlsx(bdcCriterios, "D:/!bso/girCartera/samples/Pob_Sep2023.xlsx")

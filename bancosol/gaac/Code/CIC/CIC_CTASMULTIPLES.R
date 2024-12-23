####____CARGA DE PAQUETES____####
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
library(tseries)
library(scales)
library(openxlsx)
library(janitor)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____LECTURA DE CUENTAS RECLASIFICADAS____####
anexo <- read_xlsx("D:/Files/Shared/Anexo2_Cuentas_reclasificadas.xlsx") %>% 
  select(-Nro) %>% 
  mutate(CORREGIDO=1)
####____LECTURA DE CIC____####
cicJun <- readRDS("D:/!bso/CIC/rds/cic_Jun2023.rds")
cicJul <- readRDS("D:/!bso/CIC/rds/cic_Jul2023.rds")
cicAgo <- readRDS("D:/!bso/CIC/rds/cic_Ago2023.rds")
table(cicJun$TipoCancelacion)
table(cicJul$TipoCancelacion)

cicleftout <- anexo %>% 
  anti_join(cicJul, by=c("CTACLIENTE","OPERACION"))

cicCorregido2 <- cicJul %>% 
  left_join(anexo, by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(CORREGIDO=0)) %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
  group_by(NDOC) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  arrange(CI) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, CI, NDOC, TipoCancelacion,
         CodEnvioOrigen, CORREGIDO) %>% 
  mutate(TieneCorregido = max(CORREGIDO))

cicJunShort <- cicJun %>% 
  select(CTACLIENTE, OPERACION, TipoCancelacion, CodEnvioOrigen, SaldoBruto)

cicCorregido <- cicJul %>% 
  left_join(anexo, by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(CORREGIDO=0)) %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
  group_by(NDOC) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  arrange(NDOC) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, CI, NDOC, Nombre,
         TipoCancelacion, CodEnvioOrigen, SaldoBruto, CORREGIDO) %>% 
  mutate(TieneCorregido = max(CORREGIDO)) %>% 
  mutate(TieneAmbos = ifelse(min(CORREGIDO)==0 & max(CORREGIDO)==1, 1 ,0)) %>% 
  mutate(AunSinCorregir = ifelse(n_distinct(CTACLIENTE[is.na(TipoCancelacion)])>1 , 1, 0)) %>% 
  dplyr::filter(CI!="NA") %>% 
  left_join(cicJunShort, by = c("CTACLIENTE", "OPERACION"), suffix=c("_Jul","_Jun"))
  
table(cicCorregido$TipoCancelacion, useNA = "always")

write_xlsx(cicCorregido,"D:/!bso/validaciones/ArregloCIC_Jul2023.xlsx")

####____CIC SEPTIEMBRE____####
cicSep <- readRDS("D:/!bso/CIC/rds/cic_Sep2023.rds") %>% 
  mutate(Solved = ifelse(OPERACION %in% anexo$OPERACION, 1, 0)) %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
  group_by(NDOC) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  dplyr::filter(CI!="NA") %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, CI, NDOC, Nombre,
         TipoCancelacion, CodEnvioOrigen, SaldoBruto, Solved) %>% 
  mutate(TieneCorregido = max(Solved)) %>% 
  mutate(TieneAmbos = ifelse(min(Solved)==0 & max(Solved)==1, 1, 0)) %>% 
  arrange(desc(TieneAmbos), NDOC) 

write_xlsx(cicSep, "D:/!bso/validaciones/CTASMULTIPLES_Sep2023.xlsx")

####____BANTOTAL____####
bdcSep <- readRDS("D:/!bso/girCartera/rds/ec_Sep2023.rds") %>% 
  mutate(Solved = ifelse(OPERACION %in% anexo$OPERACION, 1, 0)) %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
  group_by(NDOC) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  dplyr::filter(CI!="NA") %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
  select(monDate, FDESEMBOLSO, CTACLIENTE, OPERACION, CI, NDOC, NOMBRE_TIT,
         esFSL, saldous, Solved) %>% 
  mutate(TieneCorregido = max(Solved)) %>% 
  mutate(TieneAmbos = ifelse(min(Solved)==0 & max(Solved)==1, 1, 0)) %>% 
  arrange(desc(TieneAmbos), NDOC) 
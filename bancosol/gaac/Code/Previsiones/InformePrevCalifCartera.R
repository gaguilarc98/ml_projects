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
library(stringr) # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____PREVISION POR CALIFICACION DE CARTERA____####
my <- "Sep2023"
bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",my,".rds"))
cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",my,".rds"))

bdcCalif <- bdc %>% 
  mutate(MIGRADA = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
  mutate(SALDO_BS = saldous*6.86) %>% 
  mutate(PREV_BS = previus*6.86) %>% 
  select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, MIGRADA, FDESEMBOLSO, DIASMORA,
         CALIFICACION, ESTADO, CTA_CONTABLE=ctaCont, SALDO_USD=saldous, PREV_USD = previus, 
         SALDO_BS, PREV_BS)

cicCalif <- cic %>% 
  mutate(Saldo_USD = SaldoBruto+SaldoContingente) %>% 
  mutate(Saldo_BS = Saldo_USD*6.86) %>% 
  mutate(PrevEspecifica_BS = PrevEspecifica*6.86) %>% 
  mutate(PrevCiclica_BS = PrevCiclica*6.86) %>% 
  select(CTACLIENTE, OPERACION, TipoCancelacion, TipoProducto, Calificacion, 
         CuentaContable = CuentaContable.y, 
         ESTADO, Saldo_USD, Saldo_BS, PrevEspecifica_USD=PrevEspecifica,
         PrevEspecifica_BS, PrevCiclica_USD=PrevCiclica, PrevCiclica_BS)

Bases <- list(BANTOTAL = bdcCalif, CIC = cicCalif)
write_xlsx(Bases, paste0("D:/!bso/previsiones/Prev_CalifCartera_",my,".xlsx"))

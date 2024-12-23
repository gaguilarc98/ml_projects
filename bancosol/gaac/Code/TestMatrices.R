####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
library(dplyr)
library(stringr)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(openxlsx)
library(scales)
library(janitor)
options("encoding" = "UTF-8")
options(scipen = 999)#Para prevenir que se muestren resultados en notación científica

####____MATRICES DE TRANSICION____####
setwd("D:/!bso/girCartera/rds/")
oldMonth <- readRDS("ec_Mar2023.rds")
newMonth <- readRDS("ec_Abr2023.rds")

newMonth <- newMonth %>% 
  filter(MODULO!=131) %>% 
  mutate(CALIFICACION = ifelse(ESTADO=="CASTIGADA",'S',CALIFICACION)) %>% 
  select(CTACLIENTE,OPERACION,CALIFICACION)

Trans <- oldMonth %>% 
  mutate(CALIFICACION = ifelse(ESTADO=="CASTIGADA",'S',CALIFICACION)) %>% 
  inner_join(newMonth, by=c("CTACLIENTE","OPERACION"),suffix=c("_old","_new")) %>% 
  mutate(saldous = ifelse(saldoCast>0,saldoCast,saldous)) %>% 
  select(CTACLIENTE, OPERACION, saldous, CALIFICACION_old, CALIFICACION_new)

Cancel <- oldMonth %>% 
  anti_join(newMonth,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(saldous = ifelse(saldoCast>0,saldoCast,saldous)) %>% 
  select(CTACLIENTE,OPERACION,saldous,CALIFICACION_old = CALIFICACION) %>% 
  mutate(CALIFICACION_new = 'Z')

All <- Trans %>% 
  bind_rows(Cancel) %>%
  group_by(CALIFICACION_old, CALIFICACION_new) %>% 
  summarise(Saldo=sum(saldous),Nops=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = CALIFICACION_new,values_from = c(Saldo,Nops),
              values_fill = 0)

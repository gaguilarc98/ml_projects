#-------------------------
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")

################################################################################
tabs<- readRDS('C:/!bso/genero/output/tablaGIRGenero.rds')

tabSucursal<-tabs %>% 
  dplyr::filter(Fecha>='2022-11-30') %>% 
  dplyr::filter(GENERO!='J') %>% 
  select(Sucursal, GENERO, Fecha, Cartera_Bruta, PaR_30_Bruta, PaR_1_Bruta) %>%
  group_by(Sucursal, Fecha, GENERO) %>% 
  summarise(Mora1d=sum(PaR_1_Bruta)/sum(Cartera_Bruta)*100) %>% 
  pivot_wider(names_from = Fecha, values_from = Mora1d) %>% 
  mutate(`Var Ac`=`2023-02-28`-`2022-12-31`) %>% 
  
  tabCartera<-tabs %>%
  dplyr::filter(Fecha>='2021-12-31') %>% 
  dplyr::filter(GENERO!='J') %>% 
  select(Sucursal, GENERO, Fecha, Cartera_Bruta, PaR_30_Bruta, PaR_1_Bruta) %>% 
  ungroup() %>% 
  group_by(GENERO, Fecha) %>%
  mutate(pctCartera=sum(Cartera_Bruta)) %>% 
  ungroup() %>% 
  mutate(pctCartera = pctCartera/Cartera_Bruta)
 
  

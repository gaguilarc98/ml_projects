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

#===========================================================
bdaily <- readRDS('C:/!bso/vipCartera/ddaily_raw.rds')

dailyMMop <- bdaily %>% 
  dplyr::filter(year >= 2022) %>% 
  select(DIASMORA, OPERACION, CTACLIENTE, dayDate) %>% 
  mutate(monDate = as.yearmon(dayDate)) %>%
  dplyr::filter(monDate >= 'oct. 2022') %>% 
  group_by(CTACLIENTE, OPERACION, monDate) %>%
  select(-dayDate) %>% 
  summarise_all(max, na.rm = T) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(maxMM = max(DIASMORA, na.rm = T))
# %>% 
#   dplyr::filter(maxMM > 0)
write_rds(dailyMMop, 'C:/!bso/vipCartera/ttf_im_all.rds')  

dailyMMop <- readRDS('C:/!bso/vipCartera/ttf_im_all.rds')

repList_IM <- dailyMMop %>%
  ungroup() %>% 
  dplyr::filter(monDate > 'sep. 2022') %>% 
  # dplyr::filter(DIASMORA <= 31) %>%
  # dplyr::filter(maxMM <= 31) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(counter_ = ifelse(DIASMORA > 5, 1, 0),
         counter= sum(counter_),
         counterLast_ = ifelse(DIASMORA > 5 & monDate > 'dic. 2022', 1, 0),
         counterLast3 = sum(counterLast_)) %>% 
  # dplyr::filter(counter == 6) %>%
  mutate(sdIM = sd(DIASMORA),
         avgIM = mean(DIASMORA),
         cvIM = sdIM/avgIM*100) %>% 
  dplyr::filter(monDate == 'mar. 2023')

saveRDS(repList_IM, 'C:/!bso/ttf/reprogList_IM_mar2023_all.rds')
#==============================================================================
# current descriptives
repList_crit <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% 
  select(CTACLIENTE, OPERACION, saldous, CALIFICACION,
         previus, MODULO, ESTADO, ctaCont, sucursal, tipoCred,
         MONEDA, MONTO) %>%
  mutate(montous = ifelse(MONEDA == 0, MONTO/6.86, MONTO)) %>% 
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous > 1000 & montous <= 3000 ~'b. 1k-3k',
                            montous > 3000 & montous <= 5000 ~'c. 3k-5k',
                            montous > 5000 & montous <= 8000 ~'d. 5k-8k',
                            montous > 8000 & montous <= 10000 ~'e. 8k-10k',
                            montous > 10000 & montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k')) %>%
  mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                              sucursal == '10' ~ 'El Alto',
                              sucursal == '2' ~ 'La Paz',
                              sucursal == '3' ~ 'Cochabamba',
                              sucursal == '4' ~ 'Oruro',
                              sucursal == '5' ~ 'Potosí',
                              sucursal == '6' ~ 'Tarija',
                              sucursal == '7' ~ 'Santa Cruz',
                              sucursal == '8' ~ 'Beni',
                              sucursal == '9' ~ 'Pando',)) %>% 
  dplyr::filter(CALIFICACION > 'B' & ctaCont < '135') %>% 
  mutate(pct_saldoRestante = saldous/montous*100)
  
saveRDS(repList_crit, 'C:/!bso/ttf/reprogList_crit_mar2023.rds')

#==============================================================================
# p/Fassil
dailyMMop <- readRDS('C:/!bso/vipCartera/ttf_im.rds')

repList_IM <- bdaily %>%
  ungroup() %>% 
  mutate(monDate  =as.yearmon(dayDate)) %>% 
  dplyr::filter(monDate > 'nov. 2022') %>% 
  select(CTACLIENTE, OPERACION, DIASMORA, monDate) %>% 
  dplyr::rename(max_mora_IM = DIASMORA)
write_rds(repList_IM, 'C:/!bso/ttf/moraIM_pFassil.rds')

#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(xlsx)
library(dplyr)
#library(foreign)
#library(reshape)
#library(reshape2)
#library(stringr)
library(lubridate)
#library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
#library(hrbrthemes)
#library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
library(plotly)
####____MORA POR HORA____####
diaria <- fread('D:/!bso/girCartera/BaseCartera_20220129.txt',
                encoding = 'Latin-1', fill = T)
dia0 <- diaria %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(activoAgo = 1) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  dplyr::filter(DIASMORA>0) %>% 
  select(OPERACION,saldous) %>% 
  rename(Operacion=OPERACION)

Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv')
P2 <- Pagos %>%
  dplyr::filter(FechaPago>=as.Date('2022-01-30') & FechaPago<=as.Date('2022-01-31')) %>%
  select(Operacion, CapitalPagado, FechaPago, HoraPago) %>%
  group_by(Operacion) %>%
  summarise(HoraPago = max(HoraPago), FechaPago = max(FechaPago),
            CapitalPagado = sum(CapitalPagado)) %>%
  left_join(dia0, by='Operacion')

diaPago <- dia0 %>%
  left_join(P2, by='Operacion') %>%
  dplyr::filter(!is.na(HoraPago)) %>%
  mutate(hora=as.POSIXct(paste(FechaPago,HoraPago),format="%Y-%m-%d %H:%M:%S"))
par0Pagox <- sum(diaPago$saldous.x)
par0Pagoy <- sum(diaPago$saldous.y)

#___________________________________________________________
#____PaR0 para los últimos días del mes para cada mes 012022 - 092022
#####_____MENSUAL____####
fecha <- c('20220129','20220223','20220329','20220428','20220529',
           '20220628','20220729','20220829','20220928')
dias_base <- paste0('BaseCartera_',fecha)
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv')
i <- 1

par0pago <- vector(mode = "numeric",length = length(fecha))
for (i in 1:length(dias_base)) {
  dia <- fread(paste0('D:/!bso/girCartera/',dias_base[i],'.txt'),
               encoding = 'Latin-1', fill = T)
  diad <- dia %>% 
    dplyr::filter(MODULO != 131) %>%
    dplyr::filter(ESTADO != 'CASTIGADA') %>%
    mutate(activoAgo = 1) %>%
    mutate(fdes = dmy(FDESEMBOLSO)) %>%
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    dplyr::filter(DIASMORA>0) %>% 
    select(OPERACION,saldous) %>% 
    rename(Operacion=OPERACION)
  fechad <- as.Date(fecha[i],format="%Y%m%d")
  P <- Pagos %>%
    dplyr::filter(FechaPago>fechad & month(FechaPago)==month(fechad)) %>% 
    select(Operacion, CapitalPagado, FechaPago, HoraPago) %>%
    group_by(Operacion) %>%
    summarise(HoraPago = max(HoraPago), FechaPago = max(FechaPago),
              CapitalPagado = sum(CapitalPagado)) %>%
    inner_join(diad,by='Operacion') %>% 
    mutate(hora=as.POSIXct(paste(FechaPago,HoraPago),format="%Y-%m-%d %H:%M:%S"))
  par0pago[i] <- sum(na.omit(P$saldous))
}

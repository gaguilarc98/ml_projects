#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
#library(xlsx)
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
#library(forecast)
library(quantmod)
#library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
library(tseries)
#library(xtable)
#library(openxlsx)
#library(hrbrthemes)
#library(viridis)
#library(scales)
#library(janitor)
#library(RColorBrewer)
#library(paletteer)
#library(plotly)
library(ggplot2)
####____MORA POR HORA____####
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraHastaOct2022.csv')
P <- Pagos %>%
  group_by(Operacion) %>%
  summarise(HoraPago = max(HoraPago), FechaPago = max(FechaPago),
            CapitalPagado = sum(CapitalPagado)) %>%
  ungroup() %>% 
  group_by(FechaPago) %>%
  select(Operacion) %>% 
  summarise(n=n())

P <- P %>% 
  mutate(mes=month(FechaPago))

ggplot(P,aes(x=FechaPago,y=n,color=factor(mes)))+
  geom_line()

#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(tseries)
library(ggplot2)
options(scipen = 999)
paleta <- colorRampPalette(c("blue2","slateblue4","slateblue3","violetred3",
                             "red3","tan2","yellow3","yellow2"),bias=1.5)
#_______________________________________________________________________________
####____OBTENIENDO LAS OPERACIONES EJEMPLO____####
join <- read_xlsx("D:/!bso/mph/BaseCarteraSep2022_join7.xlsx",sheet = 'Sheet1')
j <- join %>% 
  dplyr::filter(Operacion_Riesgosa==1) %>% 
  select(Operacion)

set.seed(14112022)
Ops_ejemplo <- sample(j$Operacion,size = 3)
####____OBTENIENDO SUS PAGOS____####
lastday <- data.frame(dia=seq.Date(as.Date("2016-01-01"),as.Date("2022-12-31"),by="1 day"))
lastday <- data.frame(dia=seq.Date(as.Date("2016-01-01"),as.Date("2022-12-31"),by="1 day")) %>% 
  mutate(year=as.yearmon(dia)) %>% 
  group_by(year) %>% 
  summarise(maxdia=max(dia))

lastday <- ultimosdias %>% 
  group_by(month(dia)) %>% 
  summarise(maxdia=max(dia)) %>% 
  ungroup() %>%
  mutate(mes=month(maxdia)) %>% 
  select(maxdia,mes)

Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv',
               encoding = "Latin-1",fill=T) %>% 
  dplyr::filter(month(FechaPago)!=10)
ListaPagos <- NULL
for (i in 1:length(Ops_ejemplo)) {
  Pagos_Op <- Pagos %>% 
    dplyr::filter(Operacion==Ops_ejemplo[i]) %>% 
    select(Operacion,FechaPago,HoraPago,FechaPrevistaPago) %>% 
    mutate(dia=day(FechaPago),mes=month(FechaPago)) %>% 
    left_join()
    dplyr::filter(dia >= day(lastday[which(month(lastday)==month(FechaPago))]))
  ListaPagos <- ListaPagos %>% 
    bind_rows(Pagos_Op)
}
fverif <- unique(c(as.Date(ListaPagos$FechaPago)-1,as.Date(ListaPagos$FechaPago),as.Date(ListaPagos$FechaPago)+1))
fverif <- sort(fverif)
fverif
write_xlsx(ListaPagos,path = "D:/!bso/mph/PagosUR_ejemplos.xlsx")
#_______________________________________________________________________________
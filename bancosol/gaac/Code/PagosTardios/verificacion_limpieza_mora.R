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
library(openxlsx)
#library(hrbrthemes)
#library(viridis)
#library(scales)
#library(janitor)
#library(RColorBrewer)
#library(paletteer)
#library(plotly)
library(ggplot2)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)

####____OBTENIENDO LAS OPERACIONES EJEMPLO____####
join <- read_xlsx("D:/!bso/mph/BaseCarteraOct2022_join.xlsx",sheet = 'Sheet1')
jj <- join %>% 
  dplyr::filter(Instancias_UR>=4) %>% 
  select(Operacion)
####____PARA OBTENER UNA MUESTRA____####
set.seed(14112022)
Ops_ejemplo <- sample(jj$Operacion,size = 3)
####____PARA OBTENER TODAS LAS OPERACIONES____####
#Ops_ejemplo <- jj$Operacion
####____OBTENIENDO SUS PAGOS____####
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEne2022_Oct2022.csv')
P2full <- Pagos %>% 
  mutate(mesPago = month(FechaPago)) %>% 
  mutate(dayPago = day(FechaPago)) %>% 
  mutate(yearPago = year(FechaPago)) %>% 
  mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
  dplyr::filter(yearPago == 2022) %>% 
  group_by(Operacion, mesPago, yearPago) %>% 
  summarise(HoraPago = max(HoraPago), Hora_UltDia = max(Hora_UltDia),
            FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago),
            CapitalPagado=sum(CapitalPagado)) %>% 
  ungroup() %>% 
  mutate(FechaPago=as.Date(paste0(yearPago)))

# P2full2 <- Pagos %>% 
#   mutate(mesPago = month(FechaPago)) %>% 
#   mutate(dayPago = day(FechaPago)) %>% 
#   mutate(yearPago = year(FechaPago)) %>% 
#   #mutate(Hora = hour(as.POSIXct(HoraPago,format="%H:%M:%S")))
#   select(Operacion, CapitalPagado, FechaPago, HoraPago, mesPago, dayPago, 
#          yearPago, FechaPrevistaPago) %>% 
#   group_by(Operacion, mesPago, yearPago) %>% 
#   mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
#   summarise(HoraPago = max(HoraPago), Hora_UltDia = max(Hora_UltDia),
#             FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago)) %>% 
#   ungroup() %>% 
#   dplyr::filter(yearPago == 2022) %>% 
#   group_by(Operacion) %>% 
#   mutate(apps = max(row_number())) %>% 
#   ungroup()

lastday <- data.frame(dia=seq.Date(as.Date("2022-01-01"),as.Date("2022-12-31"),by="1 day")) %>%
  mutate(mesano=as.yearmon(dia)) %>%
  group_by(mesano) %>%
  summarise(maxdia=max(dia)) %>% 
  ungroup()

ListaPagos <- NULL
for (i in 1:length(Ops_ejemplo)) {
  Pagos_Op <-P2full %>% 
    dplyr::filter(Operacion==Ops_ejemplo[i]) %>%
    mutate(mesano=as.yearmon(FechaPago)) %>% 
    left_join(lastday, by='mesano') %>% 
    dplyr::filter(FechaPago==maxdia & as.numeric(substr(HoraPago,1,2))>12) %>% 
    select(Operacion,FechaPago,HoraPago,FechaPrevistaPago)
  ListaPagos <- ListaPagos %>% 
    bind_rows(Pagos_Op)
}
fverifprev <- unique(c(as.Date(ListaPagos$FechaPago)-1))
fveriflast <- unique(c(as.Date(ListaPagos$FechaPago)))
# fverif <- sort(fverif)
# write_xlsx(ListaPagos,path = "D:/!bso/mph/PagosUR_ejemplos.xlsx")
#_______________________________________________________________________________
####___MONITOREO DE DIAS MORA____####

ListaPagos <- ListaPagos %>% 
  mutate(FechaPago = as.Date(ListaPagos$FechaPago))
ejemplos <- data.frame(Operacion=as.numeric(),
                       Fecha=as.Date(character()),
                       DIASMORA=as.numeric(),
                       FechaPago=as.Date(as.numeric()),
                       HoraPago=character(),
                       FechaPrevistaPago=as.Date(as.numeric()))

i <- 1
for (i in 1:length(fverifprev)){
  fecha_verif <- str_replace(str_replace(as.character(fverifprev[i]),"-",""),"-","")
  tryCatch({
    ListaDia <- ListaPagos %>% 
      dplyr::filter(FechaPago==fverifprev[i]+1)
    baseDia <- fread(paste0('D:/!bso/girCartera/BaseCartera_',fecha_verif,'.txt'),
                     encoding = 'Latin-1', fill = T) %>% 
      dplyr::filter(OPERACION %in% ListaDia$Operacion) %>% 
      select(OPERACION,DIASMORA) %>% 
      rename(Operacion=OPERACION) %>% 
      left_join(ListaDia,by="Operacion") %>% 
      mutate(Fecha=fverifprev[i]) %>% 
      relocate(Operacion,Fecha,DIASMORA,FechaPago,HoraPago)
    ejemplos <- ejemplos %>% bind_rows(baseDia)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
for (i in 1:length(fveriflast)){
  fecha_verif <- str_replace(str_replace(as.character(fveriflast[i]),"-",""),"-","")
  tryCatch({
    ListaDia <- ListaPagos %>% 
      dplyr::filter(FechaPago==fveriflast[i])
    baseDia <- fread(paste0('D:/!bso/girCartera/BaseCartera_',fecha_verif,'.txt'),
                     encoding = 'Latin-1', fill = T) %>% 
      dplyr::filter(OPERACION %in% ListaDia$Operacion) %>% 
      select(OPERACION,DIASMORA) %>% 
      rename(Operacion=OPERACION) %>% 
      left_join(ListaDia,by="Operacion") %>% 
      mutate(Fecha=fveriflast[i]) %>% 
      relocate(Operacion,Fecha,DIASMORA,FechaPago,HoraPago)
    ejemplos <- ejemplos %>% bind_rows(baseDia)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
ejemplos <- ejemplos %>% 
  arrange(Operacion,Fecha)

for (i in 1:nrow(ListaPagos)) {
  j <- which(fverif==(ListaPagos$FechaPago[i]-1))
  fecha_verif <- str_replace(str_replace(as.character(fverif[j]),"-",""),"-","")
  tryCatch({
    dia <- fread(paste0('D:/!bso/girCartera/BaseCartera_',fecha_verif,'.txt'),
                       encoding = 'Latin-1', fill = T) %>% 
      dplyr::filter(OPERACION == ListaPagos$Operacion[i]) %>% 
      select(OPERACION,DIASMORA)
    verif <- data.frame(Fecha=fverif[j],
                        Operacion=dia$OPERACION,
                        DIASMORA=dia$DIASMORA,
                        FechaPago=ListaPagos$FechaPago[i],
                        HoraPago=ListaPagos$HoraPago[i]) 
    ejemplos <- ejemplos %>% bind_rows(verif)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  l <- which(fverif==(ListaPagos$FechaPago[i]))
  fecha_verif <- str_replace(str_replace(as.character(fverif[l]),"-",""),"-","")
  tryCatch({
    dia <- fread(paste0('D:/!bso/girCartera/BaseCartera_',fecha_verif,'.txt'),
                 encoding = 'Latin-1', fill = T) %>% 
      dplyr::filter(OPERACION == ListaPagos$Operacion[i]) %>% 
      select(OPERACION,DIASMORA)
    verif <- data.frame(Fecha=fverif[j],
                        Operacion=dia$OPERACION,
                        DIASMORA=dia$DIASMORA,
                        FechaPago=ListaPagos$FechaPago[i],
                        HoraPago=ListaPagos$HoraPago[i]) 
    ejemplos <- ejemplos %>% bind_rows(verif)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  # k <- which(fverif==(ListaPagos$FechaPago[i]+1))
  # fecha_verif <- str_replace(str_replace(as.character(fverif[k]),"-",""),"-","")
  # tryCatch({
  #   dia <- fread(paste0('D:/!bso/girCartera/BaseCartera_',fecha_verif,'.txt'),
  #                encoding = 'Latin-1', fill = T) %>%
  #     dplyr::filter(OPERACION == ListaPagos$Operacion[i]) %>%
  #     select(OPERACION,DIASMORA)
  #   verif <- data.frame(Fecha=fverif[j],
  #                       Operacion=dia$OPERACION,
  #                       DIASMORA=dia$DIASMORA,
  #                       FechaPago=ListaPagos$FechaPago[i],
  #                       HoraPago=ListaPagos$HoraPago[i]) 
  #   ejemplos <- ejemplos %>% bind_rows(verif)
  # }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

ejemplos <- ejemplos %>% 
  arrange(Operacion,Fecha)

write_xlsx(ejemplos,path = "D:/!bso/mph/PagosUR_verif.xlsx")
#_______________________________________________________________________________
####____VERIFICACION MANUAL DE DIASMORA Y OPERACION____####
fverif <- '20220210'
op <- 3464493
dias_base <- paste0('BaseCartera_',fverif)

dia <- fread(paste0('D:/!bso/girCartera/',dias_base,'.txt'),
             encoding = 'Latin-1', fill = T) %>% 
  dplyr::filter(OPERACION == op) %>% 
  select(OPERACION,DIASMORA)
####____VERIFICACION MANUAL PARA UNA OPERACION EN CARTERA MENSUAL____####
op <- 3026572
dias_base <- paste0('BaseCartera_',fverif)
dia <- fread(paste0('D:/!bso/girCartera/',dias_base,'.txt'),
             encoding = 'Latin-1', fill = T) %>% 
  dplyr::filter(OPERACION == op) %>% 
  select(OPERACION,DIASMORA,SALDO,FDESEMBOLSO)
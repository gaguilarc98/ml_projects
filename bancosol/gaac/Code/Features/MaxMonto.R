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
library(stringr)    # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ggplot2)
library(fastDummies)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____HISTORIAL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
clist <- list()
# agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
# file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
bdcList <- list()
i <- 1
for (i in 1:(length(myrds)-1)) {
  tryCatch({
    k <- i+1
    print(myrds[i])
    print(myrds[k])
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                          myrds[i],'.rds')) %>% 
      select(CTACLIENTE,OPERACION, CI, GENERO, MONTO, MONEDA, fdes, monDate, fbase,
             CALIFICACION, montous, DIASMORA,AGENCIA,NOMBRE_ASESOR) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
      select(-MONTO,-MONEDA)
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                          myrds[k],'.rds')) %>% 
      select(CTACLIENTE,OPERACION, CI, GENERO, MONTO, MONEDA, fdes, monDate, fbase,
             CALIFICACION, montous, DIASMORA,AGENCIA,NOMBRE_ASESOR) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
      select(-MONTO,-MONEDA)
    
    OpsNuevas <- df2$OPERACION[!(df2$OPERACION %in% df1$OPERACION)]
    # OpsAntiguas <- df2$OPERACION[(df2$OPERACION %in% df1$OPERACION)]
    OpsCanceladas <- df1$OPERACION[!(df1$OPERACION %in% df2$OPERACION)]
    
    dfNuevas <- df2 %>% 
      dplyr::filter(OPERACION %in% OpsNuevas) %>% 
      mutate(moraAcum = DIASMORA) %>% 
      mutate(moraMax = DIASMORA) %>% 
      mutate(peorCalif = CALIFICACION)
    if(i==1){
      dfCancel <- df1 %>% 
        dplyr::filter(OPERACION %in% OpsCanceladas) %>% 
        mutate(moraAcum = DIASMORA) %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION)
    }else{
      dfCancel <- dfJoin %>% 
        dplyr::filter(OPERACION %in% OpsCanceladas)
    }
    dfJoin <- df1 %>% 
      select(-MONTOUS,-monDate,-fbase,-fdes,-montous,-GENERO) %>% 
      inner_join(select(df2,-AGENCIA,-NOMBRE_ASESOR),by=c("OPERACION","CTACLIENTE","CI")) %>% 
      group_by(OPERACION,CTACLIENTE,CI) %>% 
      mutate(moraAcum = sum(DIASMORA.x,DIASMORA.y,na.rm = T)) %>% 
      mutate(moraMax = max(DIASMORA.x,DIASMORA.y,na.rm = T)) %>% 
      mutate(peorCalif = max(CALIFICACION.x,CALIFICACION.y,na.rm = T)) %>% 
      ungroup() %>% 
      rename(CALIFICACION = CALIFICACION.y, 
             DIASMORA = DIASMORA.y) %>% 
      select(-DIASMORA.x,-CALIFICACION.x)
    if(i==1){
      dfTotal <- dfNuevas %>% 
        bind_rows(dfCancel) %>% 
        bind_rows(dfJoin)
    }else{
      dfTemp <- dfNuevas %>% 
        bind_rows(dfCancel) %>% 
        bind_rows(dfJoin)
      OpsAntiguas <- dfTotal$OPERACION[(dfTotal$OPERACION %in% dfTemp$OPERACION)]
      dfTotal <- dfTotal %>% 
        dplyr::filter(!(OPERACION %in% OpsAntiguas)) %>% 
        bind_rows(dfTemp)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

write_rds(dfTotal,'D:/!bso/vipCartera/Clientes_Ene15Dic23.rds')

dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Mar23.rds')

x <- dfTotal %>% 
  group_by(year(fdes)) %>% 
  summarise(monto=sum(MONTOUS),nOps=n_distinct(OPERACION))
dfTot
mutate(eom = as.Date(monDate, frac = 1)) %>%  
  mutate(loanDays = as.integer(eom - fdes))

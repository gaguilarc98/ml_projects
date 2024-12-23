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
library(scales)
library(janitor)
library(ggplot2)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FEATURES____####
3044093

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
clist <- list()
agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
bdcList <- list()
i <- 1
for (i in 1:(length(file_list)-1)) {
  tryCatch({
    k <- i+1
    print(myrds[i])
    print(myrds[k])
    df1 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',
                          myrds[i],'.rds')) %>% 
      select(CTACLIENTE,OPERACION, CI, GENERO, MONTO, MONEDA, fdes, monDate,
             CALIFICACION, DIASMORA) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
      select(-MONTO,-MONEDA) %>% 
      mutate(fcierre = as.Date(monDate, frac = 1)) %>%  
      mutate(loanDays = as.integer(fcierre - fdes))
    df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',
                          myrds[k],'.rds')) %>% 
      select(CTACLIENTE,OPERACION, CI, GENERO, MONTO, MONEDA, fdes, monDate,
             CALIFICACION, DIASMORA) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
      select(-MONTO,-MONEDA) %>% 
      mutate(fcierre = as.Date(monDate, frac = 1)) %>%  
      mutate(loanDays = as.integer(fcierre - fdes))
    
    OpsNuevas <- df2$OPERACION[!(df2$OPERACION %in% df1$OPERACION)]
    # OpsAntiguas <- df2$OPERACION[(df2$OPERACION %in% df1$OPERACION)]
    OpsCanceladas <- df1$OPERACION[!(df1$OPERACION %in% df2$OPERACION)]
    
    dfNuevas <- df2 %>% 
      dplyr::filter(OPERACION %in% OpsNuevas) %>% 
      mutate(moraMax = DIASMORA) %>% 
      mutate(peorCalif = CALIFICACION)
    if(i==1){
      dfCancel <- df1 %>% 
        dplyr::filter(OPERACION %in% OpsCanceladas) %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION)
    }else{
      dfCancel <- dfJoin %>% 
        dplyr::filter(OPERACION %in% OpsCanceladas)
    }
    dfJoin<- df1 %>% 
      select(-MONTOUS,-monDate,-fdes,-GENERO,-CI,-loanDays,-fcierre) %>% 
      inner_join(df2,by=c("OPERACION","CTACLIENTE")) %>% 
      group_by(OPERACION,CTACLIENTE) %>% 
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
write_rds(dfTotal,'D:/!bso/vipCartera/Operaciones_Ene15Feb23.rds')

n_distinct(dfTotal$OPERACION)
x <- dfTotal %>% 
  group_by(CI) %>% 
  mutate(cis=n_distinct(CTACLIENTE)) %>% 
  ungroup() %>% 
  dplyr::filter(cis>1)
#Hay 46 carnets que tienen más 2 o más ctaclientes

bdcFull <- readRDS('D:/!bso/vipCartera/Operaciones_Ene15Feb23.rds') %>% 
  group_by(CTACLIENTE) %>% 
  arrange(monDate) %>% 
  mutate(CI = CI[row_number()==1]) %>%
  ungroup() %>% 
  dplyr::filter(!is.na(CTACLIENTE)) %>% 
  group_by(CI,CTACLIENTE) %>% 
  mutate(totalMonto = sum(MONTOUS, na.rm = T)) %>% 
  mutate(maxMonto = max(MONTOUS, na.rm = T)) %>% # check measure
  mutate(minMonto = min(MONTOUS, na.rm = T)) %>% # check measure
  mutate(montoRat = maxMonto/minMonto) %>% # CHECK MEASURE
  mutate(moraAcum = sum(moraMax, na.rm = T)) %>% # CHECK MEASURE
  mutate(moraMax = max(moraMax, na.rm = T)) %>% # CHECK MEASURE
  mutate(totalLoanDays = sum(loanDays ,na.rm = T)) %>% 
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% # check measure
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0)) %>% # check measure
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% # check measure
  mutate(totalNops = sum(nops)) %>%# check measure
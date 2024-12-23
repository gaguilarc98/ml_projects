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
# require(XLConnect)
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
####____FEATURES____####
####____HISTORIAL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
myfecha <- as.vector(sapply(year,function(x){paste0(x,month)}))
clist <- list()
# agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
# file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
bdcList <- list()
i <- 2
for (i in 1:(length(myrds))) {
  tryCatch({
    print(myrds[i])
    bdc <-  readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                           myrds[i],'.rds')) %>% 
      mutate(saldous = ifelse(saldoCast>0,saldoCast,saldous)) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
      select(CTACLIENTE, OPERACION, OPERACION_ORI_REF, ctaCont, fdes, monDate, 
             MONTOUS, saldous, previus, intus, DIASMORA, CALIFICACION) 
    bdcList[[i]] <- bdc
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
bdcFull <- rbindlist(bdcList)
saveRDS(bdcFull, "D:/!bso/features/Historial_Operaciones.rds")

####____ADD A MONTH TO HISTORIAL____####
bdcFull <- readRDS("D:/!bso/features/Historial_Operaciones.rds")
bdcFull <- bdcFull %>% 
  dplyr::filter(monDate!="Sep. 2023") %>% 
  # dplyr::filter(monDate!="May. 2023") %>% 
  glimpse()
myrds <- c("Sep2023")

bdc <-  readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds,'.rds')) %>% 
  mutate(saldous = ifelse(saldoCast>0,saldoCast,saldous)) %>% 
  mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
  select(CTACLIENTE, OPERACION, OPERACION_ORI_REF, ctaCont, fdes, monDate, 
         MONTOUS, saldous, previus, intus, DIASMORA, CALIFICACION) 
tail(bdcFull %>% 
       group_by(monDate) %>% 
       summarise(nOps=n()))
bdcFull <- bdcFull %>% 
  bind_rows(bdc)

saveRDS(bdcFull, "D:/!bso/features/Historial_Operaciones.rds")

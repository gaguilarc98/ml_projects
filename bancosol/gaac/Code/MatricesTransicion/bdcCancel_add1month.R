####____CARGA DE PAQUETES y FUNCIONES____####
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
library(scales)
library(stringr)
library(forcats)
library(tseries)
library(openxlsx)
library(scales)
library(janitor)
library(ggplot2)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)

cases <- function(quant,levs,values,default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____ADDING A MONTH TO BDC CANCEL____####
dfCancel <- readRDS("D:/!bso/Consultas/dfCancelEne17Dic22.rds")
dfCancel <- dfCancel %>% 
  dplyr::filter(mCancel<'dic. 2022')
print(dfCancel %>% 
        dplyr::filter(mCancel > 'dic. 2021') %>% 
        group_by(mCancel) %>% 
        summarise(saldo=sum(saldous),nOps=n()))

bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds') %>% 
  dplyr::filter(monDate<'dic. 2022')
print(bdcCancel %>% 
        ungroup() %>% 
        dplyr::filter(monDate>'dic. 2021') %>% 
        select(-cm1,-cmt) %>% 
        pivot_wider(names_from = monDate,values_from = one))

myrds <- c('Nov2022','Dic2022') #Colocar nombre MonthYYYY del mes previo y mes actual
tryCatch({
  print(myrds[1])
  print(myrds[2])
  
  df1 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', myrds[1], '.rds')) #se abre mes anterior
  
  df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', myrds[2], '.rds')) #se abre mes posterior
  
  #Se obtienen los cancelados como aquellos que están en el cierre del mes anterior pero no el posterior
  df3 <- df1[!(df1$OPERACION %in% df2$OPERACION),] %>% 
    select(OPERACION, CI, CTACLIENTE, saldous, CALIFICACION, ESTADO,
           tipoCred, montous, sucursal) %>% 
    dplyr::filter(CALIFICACION %in% c("A","B","C","D","E","F","S")) %>% 
    mutate(rangom = ifelse(montous > 20000, '20k+', 'under20k')) %>% 
    mutate(mCancel = as.yearmon(paste0(substr(myrds[2],1,3),". ",substr(myrds[2],4,7)))) %>% 
    mutate(mSearch = as.yearmon(paste0(substr(myrds[1],1,3),". ",substr(myrds[1],4,7))))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

dfCancel <- dfCancel %>% 
  bind_rows(df3)
write_rds(dfCancel,"D:/!bso/Consultas/dfCancelEne17Dic22.rds")

df3 <- df3 %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = mCancel) %>%
  mutate(cmt='Z') %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate) %>%
  group_by(cm1, cmt, trans, monDate) %>% 
  tally() %>% 
  dplyr::rename(one = n) %>% 
  glimpse()

bdcCancel <- bdcCancel %>% 
  bind_rows(df3)
write_rds(dfCancelSum, 'D:/!bso/transMat/matCancel.rds')

####____ADDING A MONTH TO BDC FULL____####
bdcFull <- readRDS('D:/!bso/transMat/bdcFull.rds')
bdcFull <- bdcFull %>%  
  dplyr::filter(monDate<'dic. 2022')
print(bdcFull %>% 
        dplyr::filter(monDate>'dic. 2021') %>% 
        group_by(monDate) %>% 
        summarise(saldo=sum(saldous),nOps=n()))
myrds <- 'Dic2022' #Colocar MonthYYYY del mes actual
print(myrds)
bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', myrds, '.rds')) %>% 
  select(OPERACION, CI, CTACLIENTE, saldous, CALIFICACION, ESTADO, DIASMORA, 
         montous, saldous, previus, saldoCast, tipoCred, SECTOR_CARTERA) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  mutate(monDate = as.yearmon(paste0(substr(myrds,1,3),". ",substr(myrds,4,7)))) %>% 
  mutate(saldous = ifelse(CALIFICACION == 'S', saldoCast, saldous)) %>% 
  select(-saldoCast)

bdcFull <- bdcFull %>% 
  bind_rows(bdc)
write_rds(bdcFull,'D:/!bso/transMat/bdcFull.rds')
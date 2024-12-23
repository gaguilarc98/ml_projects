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
library(ca)
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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____READING RDS____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2017,2018,2019,2020,2021,2022)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)}))
myrds <- myrds[-c(1:11)]
bdcList <- list()
for (i in 1:length(myrds)) {
  print(myrds[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', myrds[i], '.rds'))
  if(substr(myrds[i],4,7) %in% c('2017','2018','2019','2020')){
    bdcNC <- bdc %>% 
      mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>%
      mutate(saldoDifFranz =  0) %>%
      mutate(saldoDif =  0) %>%
      mutate(mybase = as.yearmon(paste0(substr(myrds[i],1,3),". ",substr(myrds[i],4,7)))) %>% 
      select(mybase,CALIFICACION,saldous,previus,par30,saldoCast,saldoReprog,saldoRef,
             saldoDif,saldoDifFranz,saldoMora)
    bdcList[[i]] <- bdcNC
  }else{
    bdcNC <- bdc %>% 
      mutate(mybase = as.yearmon(paste0(substr(myrds[i],1,3),". ",substr(myrds[i],4,7)))) %>% 
      select(mybase,CALIFICACION,saldous,previus,par30,saldoCast,saldoReprog,saldoRef,
                    saldoDif,saldoDifFranz,saldoMora)
    bdcList[[i]] <- bdcNC
  }
}
Consolidado <- rbindlist(bdcList)

Consolidado <- bind_rows(bdcList)

lastDay <- data.frame(dia=seq.Date(as.Date("2017-12-01"),as.Date("2022-12-31"),by="1 day")) %>% 
  mutate(mybase=as.yearmon(dia)) %>% 
  group_by(mybase) %>% 
  arrange(desc(dia)) %>% 
  dplyr::filter(row_number()==1)

Consolidado <- Consolidado %>% 
  left_join(lastDay,by="mybase") %>% 
  select(-mybase) %>% 
  rename(mybase=dia)

write_rds(Consolidado,'D:/!bso/girCartera/bdcConsolidadoDic17Dic22.rds')
Consolidado <- readRDS('D:/!bso/girCartera/bdcConsolidadoDic17Dic22.rds')


Check1 <- Consolidado %>% 
  select(mybase,saldoReprog,saldoCast,saldoRef,saldoDif,saldoDifFranz) %>% 
  group_by(mybase) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  rename(`CIERRE`=mybase,
         `Saldo reprogramado`=saldoReprog,
         `Saldo castigado`=saldoCast,
         `Saldo refinanciado`=saldoRef,
         `Saldo diferido`=saldoDif,
         `Saldo diferido ASFI`=saldoDifFranz)

Check2 <- Consolidado %>% 
  select(mybase,par30,saldoCast,saldous,saldoMora) %>% 
  group_by(mybase) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(IMora30 =saldoMora/saldous,
         IMora30Cast=(saldoMora+saldoCast)/saldous,
         mora30=par30/saldous,
         mora30Cast=(par30+saldoCast)/saldous) %>% 
  rename(`CIERRE`=mybase,
         `Saldo en mora`=saldoMora,
         `Saldo castigado`=saldoCast,
         `Saldo (USD)`=saldous,
         `Índice de mora`=mora30,
         `Índice de mora más catigo`=mora30Cast,
         `Mora`=IMora30,
         `Mora más castigo`=IMora30Cast)
  
  
Check3 <- Consolidado %>% 
  dplyr::filter(CALIFICACION %in% c('C','D','E')) %>% 
  mutate(saldoC=ifelse(CALIFICACION=="C",saldous,0)) %>% 
  mutate(saldoD=ifelse(CALIFICACION=="D",saldous,0)) %>% 
  mutate(saldoE=ifelse(CALIFICACION=="E",saldous,0)) %>% 
  mutate(previC=ifelse(CALIFICACION=="C",previus,0)) %>% 
  mutate(previD=ifelse(CALIFICACION=="D",previus,0)) %>% 
  mutate(previE=ifelse(CALIFICACION=="E",previus,0)) %>% 
  select(mybase,saldoC,previC,saldoD,previD,saldoE,previE) %>% 
  group_by(mybase) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  rename(`CIERRE`=mybase,
         `SALDO C (USD)`=saldoD,
         `SALDO D (USD)`=saldoD,
         `SALDO E (USD)`=saldoD,
         `PREVISIÓN C (USD)`=previC,
         `PREVISIÓN D (USD)`=previD,
         `PREVISIÓN E (USD)`=previE)

Check4 <- Consolidado %>% 
  dplyr::filter(CALIFICACION %in% c('C','D','E')) %>% 
  dplyr::filter(month(mybase)==12) %>%
  dplyr::filter(year(mybase)>=2019) %>%
  mutate(saldoC=ifelse(CALIFICACION=="C",saldous,0)) %>% 
  mutate(saldoD=ifelse(CALIFICACION=="D",saldous,0)) %>% 
  mutate(saldoE=ifelse(CALIFICACION=="E",saldous,0)) %>% 
  mutate(previC=ifelse(CALIFICACION=="C",previus,0)) %>% 
  mutate(previD=ifelse(CALIFICACION=="D",previus,0)) %>% 
  mutate(previE=ifelse(CALIFICACION=="E",previus,0)) %>% 
  select(mybase,saldoC,previC,saldoD,previD,saldoE,previE) %>% 
  group_by(mybase) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  rename(`CIERRE`=mybase,
         `SALDO C (USD)`=saldoD,
         `SALDO D (USD)`=saldoD,
         `SALDO E (USD)`=saldoD,
         `PREVISIÓN C (USD)`=previC,
         `PREVISIÓN D (USD)`=previD,
         `PREVISIÓN E (USD)`=previE)

excel <- list(Check1 = Check1, Check2 = Check2,
              Check3 = Check3, Check4 = Check4)
write.xlsx(excel,"D:/!bso/girCartera/bdcConsolidado.xlsx")

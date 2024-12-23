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
####____CREATING AMORTIZADOS____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2017,2018,2019,2020,2021,2022)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)}))
amortList <- list()
for (i in 1:(length(myrds)-1)) {
  tryCatch({
    print(i)
    print(myrds[i])
    k <- i+1
    df1 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[i],'.rds')) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      select(OPERACION,CTACLIENTE,saldous,montous) %>% 
      rename(susPrevAmor=saldous)
    df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[k],'.rds')) %>% 
      select(OPERACION,CTACLIENTE,saldous) %>% 
      rename(susAmor=saldous)
    df3 <- df2 %>% 
      inner_join(df1,by=c("OPERACION","CTACLIENTE")) %>% 
      dplyr::filter(susPrevAmor/montous>0.5) %>% #Filtro de saldo previo a la amort/monto >0.5
      dplyr::filter(susAmor/susPrevAmor<0.5) %>% #Filtro saldo amort/saldo prev amort <0.5
      mutate(myAmor = as.yearmon(paste0(substr(myrds[k],1,3),". ",substr(myrds[k],4,7)))) %>% 
      mutate(myPrevAmor = as.yearmon(paste0(substr(myrds[i],1,3),". ",substr(myrds[i],4,7))))
    amortList[[i]] <- df3
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

amorFull <- bind_rows(amortList) %>% 
  arrange(OPERACION,CTACLIENTE,myAmor) %>% 
  glimpse()
write_rds(amorFull,'D:/!bso/Consultas/amortFullFeb17Dic22.rds')

####____ADD A MONTH TO AMORTIZADOS____####
amorFull <- readRDS("D:/!bso/Consultas/amortFullEne17Dic22.rds")
myrds <- c('Dic2022','Ene2023') #Colocar nombre MonthYYYY del mes previo y mes actual
i <- 1
tryCatch({
  print(i)
  print(myrds[1])
  k <- i+1
  df1 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[1],'.rds')) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    select(OPERACION,CTACLIENTE,saldous,montous) %>% 
    rename(susPrevAmor=saldous)
  df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[2],'.rds')) %>% 
    select(OPERACION,CTACLIENTE,saldous) %>% 
    rename(susAmor=saldous)
  df3 <- df2 %>% 
    inner_join(df1,by=c("OPERACION","CTACLIENTE")) %>% 
    dplyr::filter(susPrevAmor/montous>0.5) %>% #Filtro de saldo previo a la amort/monto >0.5
    dplyr::filter(susAmor/susPrevAmor<0.5) %>% #Filtro saldo amort/saldo prev amort <0.5
    mutate(myAmor = as.yearmon(paste0(substr(myrds[2],1,3),". ",substr(myrds[2],4,7)))) %>% 
    mutate(myPrevAmor = as.yearmon(paste0(substr(myrds[1],1,3),". ",substr(myrds[1],4,7))))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

amorFull <- amorFull %>% 
  bind_rows(df3)
write_rds(amorFull,'D:/!bso/Consultas/amortFullEne17Ene23.rds')

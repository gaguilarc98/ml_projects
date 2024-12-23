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
library(openxlsx)
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
####____DAN AYUDA____####
ops <- read_excel('D:/!bso/bases/excel/secuencias.xlsx')

count_ops <- ops %>% 
  group_by(`OPERACIÓN`) %>% 
  mutate(NSecuencias=n()) %>% 
  ungroup() %>% 
  rename(OPERACION=`OPERACIÓN`)


mes <- c('Dic','Nov','Oct','Sep','Ago','Jul','Jun','May','Abr','Mar','Feb','Ene')
year <- c(2022,2021,2020,2019)
myrds <- as.vector(sapply(year, function(x){paste0(mes,x)}))

flag <- 0
i <- 1
while (flag==0) {
  print(myrds[i])
  if(i==1){
    bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[i],'.rds')) %>% 
      select(OPERACION,FDESEMBOLSO)
    count_ops <- count_ops %>% 
      left_join(bdc,by="OPERACION")
    if(length(which(is.na(count_ops$FDESEMBOLSO)))==0){ flag <- 1}
    i <- i+1
  }else{
    bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[i],'.rds')) %>% 
      select(OPERACION,FDESEMBOLSO) %>% 
      rename(FDESEMBOLSO2=FDESEMBOLSO)
    count_ops <- count_ops %>% 
      left_join(bdc,by="OPERACION") %>% 
      mutate(FDESEMBOLSO=ifelse(is.na(FDESEMBOLSO),FDESEMBOLSO2,FDESEMBOLSO)) %>% 
      select(-FDESEMBOLSO2)
    if(length(which(is.na(count_ops$FDESEMBOLSO)))==0){ flag <- 1}
    i <- i+1
  }
}


length(which(is.na(count_ops$FDESEMBOLSO)))

count_ops <- count_ops %>% 
  group_by(OPERACION) %>% 
  dplyr::filter(row_number()==1)
write.xlsx(count_ops,'D:/!bso/secuencias_v2.xlsx')

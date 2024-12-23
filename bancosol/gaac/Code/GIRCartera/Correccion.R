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
library(openxlsx)
library(ca)
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
####___GETTING OPERATIONS THAT NEED TO BE CORRECTED____####
corregir <- read_excel('D:/!bso/girCartera/correccion/corregir.xlsx')
corregir <- corregir %>%
  dplyr::filter(!is.na(Dummy)) %>% 
  select(Cta,Op) %>% 
  mutate(Fix = 1) %>% 
  rename(OPERACION=Op,
         CTACLIENTE=Cta)
####____LECTURA DE RDS____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022)
myrds <- sapply(year,function(x) paste0(mes,x))
myrds <- myrds[-c(1:5)]
i <- 1
bdcfixList <- list()
for (i in 1:length(myrds)) {
  print(myrds[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[i],'.rds'))
  print(table(bdc$ESTADO))
  bdcfixed <- bdc %>% 
    left_join(corregir, by=c("OPERACION","CTACLIENTE")) %>% 
    mutate(Fix=ifelse(is.na(Fix),0,Fix)) %>% 
    #mutate(ESTADO=ifelse(Fix==1,"CASTIGADA",ESTADO)) %>% 
    dplyr::filter(Fix==1) %>% 
    select(CI,OPERACION,CTACLIENTE,ESTADO,fbase,CALIFICACION,MONTO,SALDO,saldous)
  bdcfixList[[i]] <- bdcfixed 
  print(table(bdcfixed$ESTADO))
}
names(bdcfixList) <- myrds
write.xlsx(bdcfixList,'D:/!bso/girCartera/correccion/0622_1222_Ops_fixed.xlsx')


xx <- bdc[bdc$OPERACION %in% corregir$OPERACION,]

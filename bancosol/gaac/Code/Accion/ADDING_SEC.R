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
####____CLIENTES TOTALES____####
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Abr23_v6.rds')
MasterKeyListFull <- readRDS("D:/!bso/accion/MasterKeyList.rds")
MasterKeyList <- readRDS("D:/!bso/accion/MasterKeyList_Abril2023.rds") %>% 
  distinct(MASCARA_CUENTA,CTACLIENTE)
####____ADDING SEQUENCE____####
month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
# year <- c(2018:2019)
year <- c(2023)
mon <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mycsv <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))
myfecha <- as.vector(sapply(year,function(x){paste0(mon,". ",x)})) #lista de meses-años para abrir

length(which(!stage1$MASCARA_CUENTA %in% MasterKeyList$MASCARA_CUENTA))
length(which(!stage1$MASCARA_CUENTA %in% dfAux$MASCARA_CUENTA))
i <- 4
j <- 1
for (i in 1:length(mycsv)) {
  tryCatch({
    print(mycsv[[i]])
    stage1 <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/Entrega_2_23may/deliver_Etapa_1_csv/',
                           mycsv[i],'.csv'),encoding = "UTF-8",sep=",") %>% 
      mutate(LLAVEPRIMARIA2 = LLAVEPRIMARIA) %>% 
      separate_wider_delim(LLAVEPRIMARIA2, delim="-",names = c("MASCARA_CUENTA","MASCARA_OPERACION"),too_few = "align_start",too_many = "drop") %>% 
      mutate(MASCARA_CUENTA=as.numeric(MASCARA_CUENTA)) %>% 
      dplyr::filter(ESTADO!="CASTIGADA") %>% 
      dplyr::filter(MODULO!=131)
    
    dfAux <- dfTotal %>% 
      mutate(mydes = as.yearmon(fdes)) %>% 
      dplyr::filter(mydes <= as.yearmon(myfecha[i])) %>% 
      # mutate(fueRefin = ifelse((!is.na(FechaRefin) & as.Date(FechaRefin) <= as.Date("2015-01-01")),0,fueRefin)) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(N_OPNOREFIN = length(which(fueRefin==0)), N_OPREFIN = length(which(fueRefin==1)), N_TOTAL = N_OPNOREFIN+N_OPREFIN) %>% 
      left_join(MasterKeyList,by=c("CTACLIENTE")) %>% 
      select(MASCARA_CUENTA,N_OPNOREFIN,N_OPREFIN, N_TOTAL) %>% 
      dplyr::filter(!is.na(MASCARA_CUENTA))
    
    print(paste(mycsv[i],"NOTFOUND: ",length(which(!stage1$MASCARA_CUENTA %in% dfAux$MASCARA_CUENTA))))
    stage_wsec <- stage1 %>% 
      # select(-N_OPNOREFIN, -N_OPREFIN) %>% 
      left_join(dfAux,by="MASCARA_CUENTA") %>% 
      select(-MASCARA_CUENTA,-MASCARA_OPERACION) %>% 
      relocate(LLAVEPRIMARIA, .after = `Fecha de Corte`)
    
    fwrite(stage_wsec,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/Entrega_2_23may/deliver_Etapa_1_csv/',
                  mycsv[i],'.csv'),row.names = F,sep=",")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


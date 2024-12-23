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
library(ggrepel)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____UNMASKING AND ADDING SEQUENCE SINCE 2015____####
MasterKeyList <- readRDS('D:/!bso/accion/MasterKeyList.rds') %>% 
  select(-MASCARA_CUENTA, -MASCARA_OPERACION)
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15May23_v2.rds')
moraFull <- readRDS("D:/!bso/accion/moraIntraMes_Ene2018May2023.rds") %>% 
  mutate(`Fecha de Corte`= as.Date(monDate, frac=1)) %>% 
  select(`Fecha de Corte`, CTACLIENTE, OPERACION, `Máxima DPD`=maximaDPD)

month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2018:2023)
myfile <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))

myfile <- c('04_Etapa1_abril2023')
i <- 1
for (i in 1:length(myfile)) {
  tryCatch({
    print(myfile[i])
    # stage1 <- readxl::read_excel(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                                     myfile[i],'.xlsx'),sheet='MS81479brbase')
    # garantias1 <- readxl::read_excel(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                                         myfile[i],'.xlsx'),sheet='MS81479brgarantias')
    # fwrite(stage1, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                       myfile[i],'.csv'),row.names = F,sep=",")
    # fwrite(garantias1, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                           myfile[i],'_garantías.csv'),row.names = F,sep=",")
    
    stage1 <- readxl::read_excel(paste0("D:/!bso/accion/entrega/", myfile[i],'.xlsx'))
    # dplyr::filter(ESTADO!="CASTIGADA") %>% 
    stage1 <- stage1 %>% 
      mutate(`Fecha de Corte` = as.Date(`Fecha de Corte`)) %>% 
      dplyr::filter(MODULO!=131) %>% 
      left_join(MasterKeyList,by="LLAVEPRIMARIA") %>% 
      select(-`Máxima DPD`) %>% 
      left_join(moraFull,by=c("Fecha de Corte","CTACLIENTE","OPERACION"))
    
    # stageMora <- stage1 %>% 
    #   dplyr::filter(MODULO!=131) %>% 
    #   mutate(`Fecha de Corte` = as.IDate(`Fecha de Corte`)) %>% 
    #   left_join(moraFull, by=c("Fecha de Corte","LLAVEPRIMARIA"))
    
    month_base <- str_to_title(substr(myfile[i],11,13))
    year_base <- substr(myfile[i],nchar(myfile[i])-3,nchar(myfile[i]))
    myfecha <- paste0(month_base,'. ',year_base)
    
    dfAux <- dfTotal %>% 
      mutate(mydes = as.yearmon(fdes)) %>% 
      dplyr::filter(mydes <= as.yearmon(myfecha)) %>% 
      # mutate(fueRefin = ifelse((!is.na(FechaRefin) & as.Date(FechaRefin) <= as.Date("2015-01-01")),0,fueRefin)) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(N_OPNOREFIN = length(which(fueRefin==0)), N_OPREFIN = length(which(fueRefin==1)), N_TOTAL = N_OPNOREFIN+N_OPREFIN) %>% 
      select(CTACLIENTE, N_OPNOREFIN, N_OPREFIN, N_TOTAL) 
    
    stage_wseq <- stage1 %>% 
      # select(-N_OPNOREFIN, -N_OPREFIN) %>% 
      left_join(dfAux,by="CTACLIENTE") %>% 
      select(-CTACLIENTE,-OPERACION) %>% 
      relocate(LLAVEPRIMARIA, .after = `Fecha de Corte`)
    
    print(paste(myfile[i],"NOTFOUND: ",length(which(is.na(stage_wseq$N_TOTAL))) ))
    # fwrite(stage_wseq,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                          myfile[i],'.csv'), row.names = F, sep=",")
    
    # write_xlsx(informe, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                            myfile[i],'.xlsx'))
    fwrite(stage_wseq, paste0('C:/accion/entrega/',
                                  myfile[i],'.csv'),row.names = F,sep="|")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

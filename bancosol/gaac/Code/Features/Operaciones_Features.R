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
year <- c(2022,2023)
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
# agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
# file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')

i <- 102
for (i in 1:103) {
  tryCatch({
    
    print(myrds[i])
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        mutate(NOMBRE = str_trim(paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT))) %>%
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        select(CTACLIENTE, OPERACION, CI, NOMBRE, GENERO, MONTOUS, fdes, CAEDEC_DEST, CIU, tipoCred,
               Sector_Actividad, Sector_Destino, PLAZODIAS, Sucursal, AGENCIA, ASESOR, NOMBRE_ASESOR,
               monDate, CALIFICACION, DIASMORA, OPERACION_ORI_REF, ctaCont) 
      dfTotal <- df1 %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION) %>% 
        mutate(fueRefin = if_else(OPERACION_ORI_REF!=0,1,0)) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0)) %>%
        mutate(fueCast = if_else(ctaCont %in% c('865'),1,0)) %>% 
        mutate(NReprog = fueReprog) %>% 
        mutate(fdesLast = fdes) %>% 
        mutate(FechaRefin = if_else(fueRefin==1,fdes, as.Date(NA))) %>% 
        mutate(FechaReprog = if_else(fueReprog==1,fdes, as.Date(NA))) %>% 
        mutate(FechaCast = if_else(fueCast==1,monDate-1/12, as.yearmon(NA))) 
    }else{
      df2 <- df1
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        mutate(NOMBRE = paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT)) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        select(CTACLIENTE, OPERACION, CI, NOMBRE, GENERO, MONTOUS, fdes, CAEDEC_DEST, CIU, tipoCred,
               Sector_Actividad, Sector_Destino, PLAZODIAS, Sucursal, AGENCIA, ASESOR, NOMBRE_ASESOR,
               monDate, CALIFICACION, DIASMORA, OPERACION_ORI_REF, ctaCont) 
      dfNew <- df1 %>% 
        anti_join(dfTotal,by=c("CTACLIENTE","OPERACION")) %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION) %>% 
        mutate(fueRefin = if_else(OPERACION_ORI_REF!=0,1,0)) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0)) %>%
        mutate(fueCast = if_else(ctaCont %in% c('865'),1,0)) %>% 
        mutate(NReprog = fueReprog) %>% 
        mutate(fdesLast = fdes) %>% 
        mutate(FechaRefin = if_else(fueRefin==1,fdes,as.Date(NA))) %>% 
        mutate(FechaReprog = if_else(fueReprog==1,fdes,as.Date(NA))) %>% 
        mutate(FechaCast = if_else(fueCast==1,monDate-1/12,as.yearmon(NA))) 
      dfUpdate <- df1 %>% 
        select(-CI,-NOMBRE,-GENERO,-MONTOUS,-AGENCIA,-ASESOR,-NOMBRE_ASESOR, -Sector_Actividad,
               -Sector_Destino, -PLAZODIAS, -Sucursal)
      dfOld <- dfTotal %>% 
        inner_join(dfUpdate,by=c("CTACLIENTE","OPERACION")) %>% 
        mutate(DIASMORA.y = if_else(is.na(DIASMORA.y),DIASMORA.x,DIASMORA.y)) %>% 
        mutate(CALIFICACION.y = if_else(is.na(CALIFICACION.y),CALIFICACION.x,CALIFICACION.y)) %>% 
        group_by(CTACLIENTE,OPERACION) %>% 
        mutate(moraMax = max(moraMax,DIASMORA.y,na.rm = T)) %>% 
        mutate(peorCalif = max(CALIFICACION.x,CALIFICACION.y,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(cambio = case_when(fdesLast==fdes.y~0,
                                  fdesLast!=fdes.y & ctaCont.y %in% c('135','136','137')~1,
                                  fdesLast!=fdes.y & !(ctaCont.y %in% c('135','136','137'))~2, 
                                  TRUE~0)) %>% #1 means Reprog 2 means Refin, 0 is no change in fdes
        mutate(FechaReprog = if_else(cambio==1, fdes.y, FechaReprog)) %>% 
        mutate(fueReprog = if_else(fueReprog==0 & cambio==1,1,fueReprog)) %>% 
        mutate(NReprog = if_else(cambio==1, NReprog+1, NReprog)) %>% 
        mutate(FechaRefin = if_else(cambio==2, fdes.y, FechaRefin)) %>% 
        mutate(fueRefin = if_else(fueRefin==0 & cambio==2,1,fueRefin)) %>% 
        mutate(OPERACION_ORI_REF = if_else(cambio==2 & OPERACION_ORI_REF.x==0,OPERACION_ORI_REF.y,OPERACION_ORI_REF.x)) %>% 
        mutate(FechaCast = if_else(fueCast==0 & ctaCont.y =='865',monDate.y,FechaCast)) %>%
        mutate(fueCast = if_else(fueCast==0 & ctaCont.y =='865',1,fueCast)) %>%
        mutate(fdesLast = fdes.y) %>% 
        mutate(CAEDEC_DEST = ifelse(is.na(CAEDEC_DEST.x),CAEDEC_DEST.y, CAEDEC_DEST.x)) %>% 
        mutate(CIU = ifelse(is.na(CIU.x),CIU.y, CIU.x)) %>%
        mutate(tipoCred = ifelse(is.na(tipoCred.x),tipoCred.y, tipoCred.x)) %>%
        #Si una op refin es reprogramada luego, se elimina su op_ori_ref por eso nos quedamos con la primera op_ori_ref
        select(CTACLIENTE, OPERACION, CI, NOMBRE, GENERO, MONTOUS, CAEDEC_DEST, CIU, tipoCred,
               Sector_Actividad, Sector_Destino, PLAZODIAS, Sucursal, AGENCIA, ASESOR, NOMBRE_ASESOR,
               moraMax,peorCalif,fueRefin,FechaRefin,fueReprog,FechaReprog,NReprog,fueCast,FechaCast,fdesLast,
               CALIFICACION = CALIFICACION.y,
               DIASMORA = DIASMORA.y,
               ctaCont = ctaCont.y,
               OPERACION_ORI_REF,
               monDate = monDate.y,
               fdes = fdes.x)
      dfCancel <- dfTotal %>% 
        anti_join(df1,by=c("CTACLIENTE","OPERACION"))
      
      dfTotal <- dfCancel %>% 
        bind_rows(dfOld) %>% 
        bind_rows(dfNew)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

n_distinct(dfTotal$ASESOR)
n_distinct(dfTotal$NOMBRE_ASESOR)

x <- dfTotal %>% 
  group_by(ASESOR) %>% 
  mutate(len = n_distinct(NOMBRE_ASESOR)) %>%
  ungroup() %>% 
  dplyr::filter(len>1)
dfTotal_fixed <- dfTotal %>% 
  group_by(ASESOR) %>% 
  mutate(NOMBRE_ASESOR = max(NOMBRE_ASESOR[which(nchar(NOMBRE_ASESOR)==max(nchar(NOMBRE_ASESOR)))],na.rm = T)) %>% 
  ungroup()

y <- dfTotal_fixed %>% 
  group_by(NOMBRE_ASESOR) %>% 
  mutate(len=n_distinct(ASESOR)) %>% 
  ungroup() %>% 
  dplyr::filter(len>1)
saveRDS(dfTotal_fixed,'D:/!bso/features/Operaciones_Ene15Jul23.rds')

dfTotal <- readRDS('D:/!bso/features/Operaciones_Ene15Jul23.rds')
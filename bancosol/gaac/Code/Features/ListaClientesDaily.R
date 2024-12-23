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
####____CONSOLIDADOS MENSUALES_____####
year <- c(2021:2023)
month <- str_pad(1:12,2,side="left",pad="0")
my <- as.vector(sapply(year, function(x){paste0(x,month)}))
my <- my[-c(1:which(my=="202105"),which(my=="202305"):length(my))]
dia <- str_pad(1:31,2,side="left",pad="0")
i <- 1
j <- 1
for (i in 1:length(my)) {
  bdcList <- list()
  for (j in 1:length(dia)) {
    k <- 1
    tryCatch({
      print(paste(my[i],dia[j]))
      bdc <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_',
                            my[i],dia[j],'.rds')) %>% 
        mutate(saldous = ifelse(saldoCast>0,saldoCast,saldous)) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        mutate(dayDate = as.Date(dayDate)) %>%
        select(CTACLIENTE, OPERACION, OPERACION_ORI_REF, ctaCont, fdes, dayDate, 
               MONTOUS, saldous, previus, intus, DIASMORA, CALIFICACION) 
      bdcList[[k]] <- bdc
      k <- k+1
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  bdcFull <- rbindlist(bdcList)
  saveRDS(bdcFull, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_',
                          my[i],'.rds'))
}

####____DAILY DELINQUENCY DATA LOOP____####
#CI, OPERACION, CTACLIENTE,maxMoraIM_cl,maxMoraIM_op
mm <- "202303"
year <- c(2022:2023)
month <- str_pad(1:12,2,side="left",pad="0")
my <- as.vector(sapply(year, function(x){paste0(x,month)}))
my <- my[-c(which(my==mm):length(my))]
for (i in 1:length(my)) {
  if(i==1){
    daily <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_",
                            my[i],".rds")) %>% 
      select(CTACLIENTE, DIASMORA) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
      ungroup()
  }
  else{
    dAux <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_",
                           my[i],".rds")) %>% 
      select(CTACLIENTE, DIASMORA) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
      ungroup()
    daily <- daily %>% 
      full_join(daily,by=c("CTACLIENTE"),suffix=c("_old","_new")) %>% 
      mutate(maxMoraIM_cl = ifelse(!is.na(maxMoraIM_cl_old) | !is.na(maxMoraIM_cl_new), 
                                   max(maxMoraIM_cl_old,maxMoraIM_cl_new, na.rm = T),0)) %>% 
      select(-maxMoraIM_cl_old, -maxMoraIM_cl_new)
  }
}

VIPJoin_DailyVal <- VIPJoin %>% 
  left_join(daily, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  glimpse()
####____LECTURA DE BDC DIARIA EN LOOP____####
####____HISTORIAL____####
myrds <- list.files("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/")
myrds <- sort(myrds)
myrds <- myrds[-c(1:2,468:length(myrds))] # Hasta el 30 de abril
#1a version es desde el 01 ene 2022 hasta el 30 abr 2023
i <- 5
for (i in 1:(length(myrds))) {
  tryCatch({
    print(myrds[i])
    if(i==1){
      df1 <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/',
                            myrds[i])) %>% 
        mutate(NOMBRE = paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT)) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        mutate(dayDate = as.Date(dayDate)) %>% 
        select(CTACLIENTE,OPERACION,CI,NOMBRE,GENERO,MONTOUS,MONEDA,fdes,AGENCIA,ASESOR,NOMBRE_ASESOR,
               dayDate,CALIFICACION,DIASMORA,OPERACION_ORI_REF,ctaCont)
      dfTotal <- df1 %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION) %>%
        mutate(tuvoMora = if_else(!is.na(DIASMORA) & DIASMORA>0,1,0)) %>% 
        mutate(FechaMora1 = if_else(tuvoMora==1,dayDate,NA)) %>% 
        mutate(FechaMoraMax = dayDate) %>% 
        mutate(FechaPeorCalif = dayDate)
    }else{
      df2 <- df1
      df1 <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/',
                            myrds[i])) %>% 
        mutate(NOMBRE = paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT)) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        select(CTACLIENTE,OPERACION,CI,NOMBRE,GENERO,MONTOUS,MONEDA,fdes,AGENCIA,ASESOR,NOMBRE_ASESOR,
               dayDate,CALIFICACION,DIASMORA,OPERACION_ORI_REF,ctaCont)
      dfNew <- df1 %>% 
        anti_join(dfTotal,by=c("CTACLIENTE","OPERACION")) %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION) %>%
        mutate(tuvoMora = if_else(!is.na(DIASMORA) & DIASMORA>0,1,0)) %>% 
        mutate(FechaMora1 = if_else(tuvoMora==1,dayDate,NA)) %>% 
        mutate(FechaMoraMax = dayDate) %>% 
        mutate(FechaPeorCalif = dayDate)
      dfUpdate <- df1 %>% 
        select(-CI,-NOMBRE,-GENERO,-MONTOUS,-MONEDA,-AGENCIA,-ASESOR,-NOMBRE_ASESOR)
      dfOld <- dfTotal %>% 
        inner_join(dfUpdate,by=c("CTACLIENTE","OPERACION")) %>% 
        group_by(CTACLIENTE,OPERACION) %>% 
        mutate(moraMax = max(DIASMORA.x,DIASMORA.y,na.rm = T)) %>% 
        mutate(peorCalif = max(CALIFICACION.x,CALIFICACION.y,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(cambio = case_when(tuvoMora==1~0,
                                  tuvoMora==0 & !is.na(DIASMORA.y) & DIASMORA.y>0~1,
                                  TRUE~0)) %>% 
        mutate(tuvoMora = if_else(tuvoMora==0 & cambio==1,1,tuvoMora)) %>% 
        mutate(FechaMora1 = if_else(cambio==1,dayDate.y, FechaMora1)) %>% 
        mutate(FechaMoraMax = if_else(!is.na(DIASMORA.x) & !is.na(DIASMORA.y) & DIASMORA.y > DIASMORA.x, dayDate.y, FechaMoraMax)) %>% 
        mutate(FechaPeorCalif = if_else(!is.na(CALIFICACION.x) & !is.na(CALIFICACION.y) & CALIFICACION.y>CALIFICACION.x,dayDate.y,FechaPeorCalif)) %>% 
        mutate(DIASMORA.y = ifelse(is.na(DIASMORA.y),DIASMORA.x,DIASMORA.y)) %>% 
        mutate(CALIFICACION.y = ifelse(is.na(CALIFICACION.y),CALIFICACION.x,CALIFICACION.y)) %>% 
        # mutate(FechaRefin = ifelse(fueRefin==0 & OPERACION_ORI_REF.y!=0,as.character(fdes.y),FechaRefin)) %>% 
        # mutate(FechaReprog = ifelse(fueReprog==0 & ctaCont.y %in% c('135','136','137'),as.character(fdes.y),FechaReprog)) %>% 
        # mutate(FechaCast = ifelse(fueCast==0 & (ctaCont.y =='865'),as.character(monDate.y),FechaCast)) %>% 
        # mutate(fueReprog = ifelse(fueReprog==0 & ctaCont.y %in% c('135','136','137'),1,fueReprog)) %>% 
        #Si una op refin es reprogramada luego, se elimina su op_ori_ref por eso nos quedamos con la primera op_ori_ref
        select(CTACLIENTE,OPERACION,CI,NOMBRE,GENERO,MONTOUS,MONEDA,AGENCIA,ASESOR,NOMBRE_ASESOR,
               moraMax,peorCalif, tuvoMora,FechaMora1, FechaMoraMax, FechaPeorCalif,
               CALIFICACION = CALIFICACION.y,
               DIASMORA = DIASMORA.y,
               ctaCont = ctaCont.y,
               OPERACION_ORI_REF = OPERACION_ORI_REF.x,
               dayDate = dayDate.y,
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
  ungroup() %>% 
  distinct(CTACLIENTE,OPERACION,.keep_all = TRUE)

y <- dfTotal_fixed %>% 
  group_by(NOMBRE_ASESOR) %>% 
  mutate(len=n_distinct(ASESOR)) %>% 
  ungroup() %>% 
  dplyr::filter(len>1)
saveRDS(dfTotal_fixed,'D:/!bso/features/ClientesDaily_Ene22Abr23.rds')


dfTotal <- readRDS('D:/!bso/features/ClientesDaily_Ene22Abr23.rds')

####

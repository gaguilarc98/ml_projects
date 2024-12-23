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
library(sqldf)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
################################################################################
bdcDic <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds')

myutf <- c('202208','202209','202210','202211','202212','202301')
mybdc <- c("Ago2022","Sep2022","Oct2022","Nov2022","Dic2022","Ene2023")
infoList <- list()
bsoList <- list()
for(i in 1:length(mybdc)){
  tryCatch({
    bdcBSO_full <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                                mybdc[i],'.txt'),encoding = 'Latin-1', fill = T) %>% 
      mutate(fbase = mybdc[i]) %>%
      mutate(mon = substr(fbase,1,3)) %>%
      mutate(year = substr(fbase,4,7)) %>%
      mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
      select(-fbase) %>%
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                   ctaCont == '134'~saldous,
                                   ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
      # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
      select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO,Fecha) %>%
      mutate(par0 = ifelse(DIASMORA > 0, saldous, 0))
    
    infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO',myutf[i],'_utf8.txt'), encoding = 'UTF-8', fill = T)
    infoCheck <- infoRaw %>%
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
      select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
             `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
             MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
             saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
      separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
      mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
      mutate(saldo = saldo/6.86) %>%
      mutate(saldoMora = saldoMora/6.86) %>%
      rename(saldoMoraInfo=saldoMora) %>% 
      mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7))))
    
    infoCleanA <- infoCheck %>% 
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
      mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
      mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
      group_by(CI) %>%
      dplyr::filter(max(row_number())>1) %>%
      mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
             califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
      mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
             califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
      mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
             califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
      mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMMCNoBSO = sum(saldoMMC*noesBSO,na.rm=T)) %>% 
      mutate(entidadSF = ifelse(`SBEF CALIFICACION` == califSF_2, `SIGLA SBEF`, '_'),
             entidadSF_2 = max(entidadSF[which(entidadSF!='BSO')], na.rm = T)) %>%  # Max entidad en SF
      dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
      select(-califBSO,-califSF,-`SBEF CALIFICACION`) %>% 
      ungroup() %>% 
      dplyr::filter(califSF_2!="_")
    
    infoJoin <- infoCleanA %>% 
      rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoList[[i]] <- infoJoin
    
    bdcBSO_full <- bdcBSO_full %>% 
      dplyr::filter(MODULO!=131) %>%
      dplyr::filter(ESTADO!="CASTIGADA") %>%
      group_by(Fecha) %>% 
      summarise(SaldoBSO = sum(saldous),ClientesBSO=n_distinct(CI),SaldoMoraBSO=sum(saldoMora),
                par0BSO=sum(par0)) %>% 
      ungroup()
    bsoList[[i]] <- bdcBSO_full
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoFull <- bind_rows(infoList)
bsoFull <- bind_rows(bsoList)

write_rds(infoFull,'D:/!bso/califClientes/infoFull.rds')
infoFull <- readRDS('D:/!bso/califClientes/infoFull.rds')
infoFull %>% 
  group_by(Fecha) %>% 
  summarise(nNAs = length(which(is.na(ESTADO))))

bsoList <- list()
for (i in 1:length(mybdc)) {
  bdcBSO_full <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                              mybdc[i],'.txt'),encoding = 'Latin-1', fill = T) %>% 
    dplyr::filter(MODULO!=131) %>%
    dplyr::filter(ESTADO!="CASTIGADA") %>%
    mutate(fbase = mybdc[i]) %>%
    mutate(mon = substr(fbase,1,3)) %>%
    mutate(year = substr(fbase,4,7)) %>%
    mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
    mutate(ctaCont = substr(RUBRO,1,3)) %>% 
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
    mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                 ctaCont == '134'~saldous,
                                 ctaCont == '136'~saldous,
                                 ctaCont == '137'~saldous,
                                 TRUE ~ 0)) %>% 
    mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>% 
    group_by(Fecha) %>% 
    summarise(SaldoBSO = sum(saldous),ClientesBSO=n_distinct(CI),SaldoMoraBSO=sum(saldoMora),
              par0BSO=sum(par0)) %>% 
    ungroup()
  bsoList[[i]] <- bdcBSO_full
}
bsoFull <- bind_rows(bsoList)


infoFull %>% group_by(Fecha) %>% summarise(saldo=sum(saldo),nOps=n_distinct(CI),n = n())
infoPerf <- infoFull %>% 
  # dplyr::filter(MODULO!=131) %>%
  # dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(Fecha,CI,GENERO,entidadSF_2) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  left_join(bsoFull,by="Fecha") %>%
  mutate(Fecha=as.Date(Fecha)) %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor_comp=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

write.xlsx(infoPerf,'D:/!bso/califClientes/infoPerfAgo22Ene23_v2.xlsx')

################################################################################
####____ENTIDAD COMPARTIDA____####
myutf <- c('202208','202209','202210','202211','202212','202301')
mybdc <- c("Ago2022","Sep2022","Oct2022","Nov2022","Dic2022","Ene2023")
myutf <- c('202212')
mybdc <- c("Dic2022")
infoList <- list()
for(i in 1:length(mybdc)){
  tryCatch({
    bdcBSO_full <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                                mybdc[i],'.txt'),encoding = 'Latin-1', fill = T) %>% 
      mutate(fbase = mybdc[i]) %>%
      mutate(mon = substr(fbase,1,3)) %>%
      mutate(year = substr(fbase,4,7)) %>%
      mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
      select(-fbase) %>%
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                   ctaCont == '134'~saldous,
                                   ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
      # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
      select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO,Fecha) %>%
      mutate(par0 = ifelse(DIASMORA > 0, saldous, 0))
    
    infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO',myutf[i],'_utf8.txt'), encoding = 'UTF-8', fill = T)
    infoCheck <- infoRaw %>%
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
      select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
             `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
             MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
             saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
      separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
      mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
      mutate(saldo = saldo/6.86) %>%
      mutate(saldoMora = saldoMora/6.86) %>%
      rename(saldoMoraInfo=saldoMora) %>% 
      mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7))))
    
    infoCleanA <- infoCheck %>% 
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
      mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
      mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
      group_by(CI) %>%
      dplyr::filter(max(row_number())>1) %>%
      mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
             califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
      mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
             califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
      mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMMCNoBSO = sum(saldoMMC*noesBSO,na.rm=T)) %>% 
      mutate(entidadSF = ifelse(`SBEF CALIFICACION` == califSF_2, `SIGLA SBEF`, '_'),
             entidadSF_2 = max(entidadSF[which(entidadSF!='BSO')], na.rm = T)) %>%  # Max entidad en SF
      dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
      select(-califBSO,-califSF,-entidadSF) %>% 
      ungroup() %>% 
      dplyr::filter(califSF_2!="_")
    
    infoJoin <- infoCleanA %>% 
      rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoList[[i]] <- infoJoin
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoFull <- bind_rows(infoList)


infoPerf <- infoJoin %>% 
  # dplyr::filter(MODULO!=131) %>%
  # dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(Fecha,CI,GENERO,entidadSF_2) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  left_join(bsoFull,by="Fecha") %>%
  mutate(Fecha=as.Date(Fecha)) %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor_comp=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

write.xlsx(infoPerf,'D:/!bso/califClientes/infoPerfDic22.xlsx')
################################################################################
####____VAINAS INNECESARIAS____####



bdcBSO_full <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraDic2022.txt',
                     encoding = 'Latin-1', fill = T) %>% 
  mutate(fbase = 'Dic2022') %>%
  mutate(mon = substr(fbase,1,3)) %>%
  mutate(year = substr(fbase,4,7)) %>%
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>%
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  mutate(Fecha = as.yearmon(dayDate)) %>%
  select(-fbase) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                               ctaCont == '134'~saldous,
                               ctaCont == '136'~saldous,
                               ctaCont == '137'~saldous,
                               TRUE ~ 0)) %>% 
  # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
  select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO) %>%
  mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>%
  glimpse()
################################################################################
infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO202212_utf8.txt'), encoding = 'UTF-8', fill = T)
infoCheck <- infoRaw %>%
  mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
         saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
         saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
         saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
         `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
         saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
  separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
  mutate(CTACLIENTE=as.numeric(CTACLIENTE),
         OPERACION=as.numeric(OPERACION)) %>% 
  rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
  mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
  mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
  mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
  mutate(saldo = saldo/6.86) %>%
  mutate(saldoMora = saldoMora/6.86) %>%
  rename(saldoMoraInfo=saldoMora) %>% 
  mutate(fbase= '202212') %>%
  mutate(Mes=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d")))

infoCleanA <- infoCheck %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
  mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
  group_by(CI) %>%
  dplyr::filter(max(row_number())>1) %>%
  # rename(Calif = `SBEF CALIFICACION`) %>% 
  # mutate(Calif = ifelse(is.na(Calif),'_',Calif)) %>% 
  # mutate(PeorCalifBSOG = max(Calif[esBSO==1])) %>%
  # mutate(PeorCalifSFG = max(`SBEF CALIFICACION`[which(`SIGLA SBEF`=='BSO')],na.rm = T)) %>%
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
  # dplyr::filter(califBSO_2 != '_') %>%
  # select(-califBSO) %>%
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
  dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
  select(-califBSO,-califSF,-`SBEF CALIFICACION`) %>% 
  ungroup() %>% 
  dplyr::filter(califSF_2!="_")

bsoFull <- bdcBSO_full %>% 
  # mutate(CI_2 = gsub("[^0-9.-]", "", CI)) %>%
  # mutate(CI_2 = gsub("[^[:alnum:] ]", "", CI_2)) %>%
  # mutate(CI_2 = as.numeric(CI_2)) %>%
  mutate(CI_2 = str_extract(CI,pattern = "[0-9]+")) %>% 
  mutate(CI_2 = as.numeric(CI_2)) %>% 
  # select(CI,CI_2,GENERO) %>% 
  distinct_all() %>% 
  glimpse()

infoJoin <- infoCleanA %>% 
  select(-CI) %>% 
  left_join(bsoFull,by=c("CTACLIENTE","OPERACION")) 

bdcBSO_tab <- bsoFull %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(ESTADO!="CASTIGADA") %>%
  summarise(Saldo=sum(saldous),Clientes=n_distinct(CI))

infoPerfFiltro <- infoJoin %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(CI,GENERO) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2)) %>% 
  ungroup() %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(SaldoBSO=bdcBSO_tab$Saldo,
         ClientesBSO=bdcBSO_tab$Clientes)

infoPerf <- infoJoin %>% 
  group_by(CI,GENERO) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2)) %>% 
  ungroup() %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(SaldoBSO=bdcBSO_tab$Saldo,
         ClientesBSO=bdcBSO_tab$Clientes)

infos <- list(infoPerfFiltro=infoPerfFiltro,infoPerf=infoPerf)

write.xlsx(infos,'D:/!bso/califClientes/infoPerf2.xlsx')

################################################################################
bdcEne <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Ene2023.rds')
x <- bdcEne %>% 
  dplyr::filter(cosechaM==monDate) %>% 
  dplyr::filter(OPERACION_ORI_REF==0) %>% 
  summarise(saldo=sum(saldous),nOps=n())

bdcFeb <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Feb2023.rds')
y <- bdcFeb %>% 
  dplyr::filter(cosechaM==monDate) %>% 
  dplyr::filter(OPERACION_ORI_REF==0) %>% 
  summarise(saldo=sum(saldous),nOps=n())
bind_rows(x,y) %>% 
  summarise(saldo=sum(saldo),nOps=sum(nOps),prom=saldo/nOps)

#####____ANTIGUO CON AHMED____####

################################################################################
####____ENTIDAD COMPARTIDA____####
myutf <- c('202208','202209','202210','202211','202212','202301')
mybdc <- c("Ago2022","Sep2022","Oct2022","Nov2022","Dic2022","Ene2023")
myutf <- c('202212')
mybdc <- c("Dic2022")
infoList <- list()
for(i in 1:length(mybdc)){
  tryCatch({
    bdcBSO_full <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                                mybdc[i],'.txt'),encoding = 'Latin-1', fill = T) %>% 
      mutate(fbase = mybdc[i]) %>%
      mutate(mon = substr(fbase,1,3)) %>%
      mutate(year = substr(fbase,4,7)) %>%
      mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
      select(-fbase) %>%
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                   ctaCont == '134'~saldous,
                                   ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
      # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
      select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO,Fecha) %>%
      mutate(par0 = ifelse(DIASMORA > 0, saldous, 0))
    
    infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO',myutf[i],'_utf8.txt'), encoding = 'UTF-8', fill = T)
    infoCheck <- infoRaw %>%
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
      select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
             `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
             MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
             saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
      separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
      mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
      mutate(saldo = saldo/6.86) %>%
      mutate(saldoMora = saldoMora/6.86) %>%
      rename(saldoMoraInfo=saldoMora) %>% 
      mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7))))
    
    infoCleanA <- infoCheck %>% 
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
      mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
      mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
      group_by(CI) %>%
      dplyr::filter(max(row_number())>1) %>%
      mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
             califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
      mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
             califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
      mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMMCNoBSO = sum(saldoMMC*noesBSO,na.rm=T)) %>% 
      mutate(entidadSF = ifelse(`SBEF CALIFICACION` == califSF_2, `SIGLA SBEF`, '_'),
             entidadSF_2 = max(entidadSF[which(entidadSF!='BSO')], na.rm = T)) %>%  # Max entidad en SF
      dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
      select(-califBSO,-califSF,-entidadSF) %>% 
      ungroup() %>% 
      dplyr::filter(califSF_2!="_")
    
    infoJoin <- infoCleanA %>% 
      rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoList[[i]] <- infoJoin
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoFull <- bind_rows(infoList)


infoPerf <- infoJoin %>% 
  # dplyr::filter(MODULO!=131) %>%
  # dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(Fecha,CI,GENERO,entidadSF_2) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  left_join(bsoFull,by="Fecha") %>%
  mutate(Fecha=as.Date(Fecha)) %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor_comp=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

write.xlsx(infoPerf,'D:/!bso/califClientes/infoPerfDic22.xlsx')


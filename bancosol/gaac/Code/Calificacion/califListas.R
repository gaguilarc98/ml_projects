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
####____LOADING INFOCRED DATABASES____####
myutf <- c('202211','202212','202301','202302')
mybdc <- c("Nov2022","Dic2022","Ene2023","Feb2023")
infoCompList <- list()
infoExcList <- list()
i <- 4
for(i in 1:length(mybdc)){
  tryCatch({
    bdcBSO_full <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                                mybdc[i],'.txt'),encoding = 'Latin-1', fill = T) %>% 
      dplyr::filter(MODULO!="131") %>% 
      mutate(fbase = mybdc[i]) %>%
      mutate(mon = substr(fbase,1,3)) %>%
      mutate(year = substr(fbase,4,7)) %>%
      mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
      select(-fbase) %>%
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(subCtaCont = substr(RUBRO,1,5)) %>% 
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
      mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                   ctaCont == '134'~saldous,
                                   ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
      mutate(saldoVig = case_when(ctaCont == '131'~saldous,
                                   ctaCont == '135'~saldous,
                                   TRUE ~ 0)) %>% 
      mutate(saldoCast = ifelse(ESTADO=="CASTIGADA",saldous,0)) %>% 
      mutate(saldous = ifelse(ESTADO=="CASTIGADA",0,saldous)) %>% 
      mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>% 
      select(Fecha,CTACLIENTE,OPERACION,CI,GENERO,ESTADO,MODULO,CalifBSO=CALIFICACION,
             montous,saldous,saldoMora,saldoVig,saldoCast)
      
    
    infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO',myutf[i],'_utf8.txt'), encoding = 'UTF-8', fill = T)
    infoCheck <- infoRaw %>%
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% #Filtro de deudor
      mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      rowwise() %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldoInfo = sum(`SBEF VIGENTE`,`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
             saldoMoraInfo = sum(`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T)) %>%
      ungroup() %>% 
      select(Fecha,NumeroOp, CI, `TIPO OBLIGADO SBEF`, SIGLA=`SIGLA SBEF`,ENTIDAD=`ENTIDAD SBEF`, 
             CALIFICACION = `SBEF CALIFICACION`,MonedaOrigen, MontoOriginal, saldoInfo, 
             saldoMoraInfo, saldoVigInfo = `SBEF VIGENTE`, saldoCastInfo = `SBEF CASTIGADO`) %>%
      separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #Sin importar el valor de moneda
      mutate(saldoInfo = saldoInfo/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(saldoMoraInfo = saldoMoraInfo/6.86) %>% 
      mutate(saldoVigInfo = as.numeric(saldoVigInfo)/6.86) %>%
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) 
    
    infoComp <- infoCheck %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_",CALIFICACION)) %>% 
      group_by(CI,esBSO) %>% 
      mutate(maxMonto = ifelse(MontoOriginal==max(MontoOriginal),1,0)) %>% 
      mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
      ungroup() %>% 
      group_by(CI) %>% 
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(entidadMaxSF = ifelse(maxMonto==1 & esBSO==0,ENTIDAD,'_')) %>% 
      mutate(siglaMaxSF = ifelse(maxMonto==1 & esBSO==0,SIGLA,'_')) %>% 
      mutate(CalifMaxSF = ifelse(maxMonto==1 & esBSO==0,CALIFICACION,'_')) %>% 
      mutate(entidadPeorSF = ifelse(peorCalif==1 & esBSO==0,ENTIDAD,'_')) %>% 
      mutate(siglaPeorSF = ifelse(peorCalif==1 & esBSO==0,SIGLA,'_')) %>% 
      mutate(CalifPeorSF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
      mutate(across(entidadMaxSF:CalifPeorSF,~max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
      mutate(saldoNoBSO = sum(saldoInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoCastNoBSO = sum(saldoCastInfo*noesBSO,na.rm=T)) %>% 
      mutate(montoNoBSO = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
      ungroup() %>% 
      dplyr::filter(CalifMaxSF!="_" & SIGLA=="BSO")
    
    infoJoin <- infoComp %>% 
      rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoCompList[[i]] <- infoJoin
    
    infoAntiJoin <- bdcBSO_full %>% 
      anti_join(infoJoin,by=c("CTACLIENTE","OPERACION","Fecha"))
    infoExcList[[i]] <- infoAntiJoin
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoComp <- bind_rows(infoCompList)
infoExc <- bind_rows(infoExcList)

Comp <- infoComp %>% 
  group_by(Fecha,CI_info,CTACLIENTE,GENERO) %>% 
  mutate(maxSaldo = ifelse(saldous==max(saldous),1,0)) %>% 
  mutate(ESTADO = ifelse(maxSaldo==1,ESTADO,'_')) %>% 
  mutate(CalifBSO = ifelse(maxSaldo==1,CalifBSO,'_')) %>% 
  mutate(MODULO = ifelse(maxSaldo==1,MODULO,-1)) %>% 
  mutate(across(c(ESTADO,MODULO,CalifBSO),~max(.x,na.rm = T))) %>% 
  summarise(across(c(ESTADO, MODULO, CalifBSO, entidadMaxSF, siglaMaxSF, CalifMaxSF,entidadPeorSF,
                     siglaPeorSF, CalifPeorSF, saldoNoBSO, saldoMoraNoBSO,saldoCastNoBSO,montoNoBSO),~max(.x)),
            across(c(saldous,montous,saldoMora, saldoVig, saldoCast),~sum(.x)))

Exc <- infoExc %>% 
  group_by(Fecha, CI,CTACLIENTE, GENERO) %>% 
  mutate(maxSaldo = ifelse(saldous==max(saldous),1,0)) %>% 
  mutate(ESTADO = ifelse(maxSaldo==1,ESTADO,'_')) %>% 
  mutate(CalifBSO = ifelse(maxSaldo==1,CalifBSO,'_')) %>% 
  mutate(MODULO = ifelse(maxSaldo==1,MODULO,-1)) %>% 
  summarise(across(c(ESTADO, MODULO, CalifBSO),~max(.x)),
            across(c(saldous,montous,saldoMora, saldoVig, saldoCast),~sum(.x)))
  
lista <- list(Compartidos = Comp,Exclusivos = Exc)
write.xlsx(lista,'D:/!bso/califClientes/Marketing_v2.xlsx')

Comp %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldous+saldoCast),n=n_distinct(CI_info))
Exc %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldous))

infoFull %>% 
  group_by(Fecha) %>% 
  summarise(nNAs = length(which(is.na(ESTADO))), saldo=sum(saldo,na.rm = T),
            n=n())


x <- infoJoin %>% dplyr::filter(MODULO!=131) %>% 
  dplyr::filter(ESTADO!="CASTIGADA")
y <- infoAntiJoin %>% dplyr::filter(MODULO!=131) %>% 
  dplyr::filter(ESTADO!="CASTIGADA")

####____IFC MUJERES____####
myutf <- c('202212')
mybdc <- c("Dic2022")
infoCompList <- list()
infoExcList <- list()
for(i in 1:length(mybdc)){
  tryCatch({
    bdcBSO_full <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                                mybdc[i],'.txt'),encoding = 'Latin-1', fill = T) %>% 
      dplyr::filter(MODULO!="131") %>% 
      mutate(fbase = mybdc[i]) %>%
      mutate(mon = substr(fbase,1,3)) %>%
      mutate(year = substr(fbase,4,7)) %>%
      mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
      select(-fbase) %>%
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(subCtaCont = substr(RUBRO,1,5)) %>% 
      mutate(fdes=dmy(FDESEMBOLSO)) %>% 
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
      mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                   ctaCont == '134'~saldous,
                                   ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
      mutate(saldoVig = case_when(ctaCont == '131'~saldous,
                                  ctaCont == '135'~saldous,
                                  TRUE ~ 0)) %>% 
      mutate(saldoCast = ifelse(ESTADO=="CASTIGADA",saldous,0)) %>% 
      mutate(saldous = ifelse(ESTADO=="CASTIGADA",0,saldous)) %>% 
      mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>% 
      mutate(ESTADO = case_when(ESTADO=="VIGENTE"~"1. VIGENTE",
                                ESTADO=="OP VENCIDA"~"2. VENCIDA",
                                ESTADO=="SUSPENSO"~"3. SUSPENSO",
                                ESTADO=="JUDICIAL"~"4. JUDICIAL",
                                ESTADO=="CASTIGADA"~"5. CASTIGADA",)) %>% 
      select(Fecha,CTACLIENTE,OPERACION,CI,GENERO,ESTADO,MODULO,CalifBSO=CALIFICACION, fdes,
             montous,saldous,saldoMora,saldoVig,saldoCast, TIPO_CREDITO)
    
    
    infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO',myutf[i],'_utf8.txt'), encoding = 'UTF-8', fill = T)
    infoCheck <- infoRaw %>%
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% #Filtro de deudor
      mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      rowwise() %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldoInfo = sum(`SBEF VIGENTE`,`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
             saldoMoraInfo = sum(`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
             saldoTotInfo = sum(`SBEF VIGENTE`,`SBEF VENCIDO`,`SBEF EJECUCION`,`SBEF CASTIGADO`,na.rm = T)) %>%
      ungroup() %>% 
      mutate(ESTADOInfo = case_when(`SBEF VIGENTE`>0 ~ '1. VIGENTE',
                                `SBEF VENCIDO`>0 ~ '2. VENCIDA',
                                `SBEF EJECUCION`>0 ~ '4. JUDICIAL',
                                `SBEF CASTIGADO`>0 ~ '5. CASTIGADA',
                                `SBEF CONTINGENTE`>0 ~ '0. CONTINGENTE')) %>% 
      select(Fecha,NumeroOp, CI, `TIPO OBLIGADO SBEF`, SIGLA=`SIGLA SBEF`,ENTIDAD=`ENTIDAD SBEF`, 
             CALIFICACION = `SBEF CALIFICACION`,MonedaOrigen, MontoOriginal, saldoInfo, saldoTotInfo,
             saldoMoraInfo, saldoVigInfo = `SBEF VIGENTE`, saldoCastInfo = `SBEF CASTIGADO`,
             saldoVenInfo = `SBEF VENCIDO`, ESTADOInfo, TipoCredInfo =`TIPO CREDITO SBEF`) %>%
      separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #Sin importar el valor de moneda
      mutate(saldoInfo = saldoInfo/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(saldoMoraInfo = saldoMoraInfo/6.86) %>% 
      mutate(saldoVigInfo = as.numeric(saldoVigInfo)/6.86) %>%
      mutate(saldoVenInfo = saldoVenInfo/6.86) %>% 
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) 
    
    infoComp <- infoCheck %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_",CALIFICACION)) %>% 
      group_by(CI,esBSO) %>% 
      mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
      mutate(peorSaldo = ifelse(saldoTotInfo==max(saldoTotInfo),1,0)) %>% 
      ungroup() %>% 
      group_by(CI) %>% 
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(entidadPeorSF = ifelse(peorCalif==1 & esBSO==0,ENTIDAD,'_')) %>% 
      mutate(siglaPeorSF = ifelse(peorCalif==1 & esBSO==0,SIGLA,'_')) %>% 
      mutate(ESTADOPeorSF = ifelse(peorCalif==1 & esBSO==0,ESTADOInfo,'_')) %>% 
      mutate(TipoCredSaldoMax = ifelse(peorSaldo==1 & esBSO==0,TipoCredInfo,'_')) %>% 
      mutate(CalifPeorSF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
      mutate(across(entidadPeorSF:CalifPeorSF, ~ max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
      mutate(saldoNoBSO = sum(saldoInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoVenNoBSO = sum(saldoVenInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoCastNoBSO = sum(saldoCastInfo*noesBSO,na.rm=T)) %>% 
      mutate(montoNoBSO = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
      ungroup() %>% 
      dplyr::filter(CalifPeorSF!="_" & SIGLA=="BSO")
    
    infoJoin <- infoComp %>% 
      rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoCompList[[i]] <- infoJoin
    
    infoAntiJoin <- bdcBSO_full %>% 
      anti_join(infoJoin,by=c("CTACLIENTE","OPERACION","Fecha"))
    infoExcList[[i]] <- infoAntiJoin
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoComp <- bind_rows(infoCompList)
infoExc <- bind_rows(infoExcList)

Comp <- infoComp %>% 
  group_by(Fecha,CI,CTACLIENTE,GENERO) %>% 
  mutate(maxSaldo = ifelse(saldous==max(saldous),1,0)) %>% 
  mutate(peorCalif = ifelse(CalifBSO==max(CalifBSO),1,0)) %>% 
  mutate(ESTADO = ifelse(peorCalif==1,ESTADO,'_')) %>% 
  mutate(CalifBSO = ifelse(peorCalif==1,CalifBSO,'_')) %>% 
  # mutate(MODULO = ifelse(peorCalif==1,MODULO,-1)) %>% 
  mutate(TIPO_CREDITO = ifelse(maxSaldo==1,TIPO_CREDITO,'_')) %>% 
  mutate(across(c(ESTADOInfo,CalifBSO,TipoCredInfo),~max(.x,na.rm = T))) %>% 
  # dplyr::filter(montous>5000) %>%  #Filtro a nivel de operación y luego agrupar
  mutate(OP_MAYOR_5000 = ifelse(montous>5000,1,0)) %>% 
  mutate(OPERACIONES = 1) %>% 
  #El filtro funciona dentro de group_by si ungrupo se queda sin filas se elimina
  summarise(across(c(ESTADO, TIPO_CREDITO, CalifBSO, entidadPeorSF,
                     siglaPeorSF, CalifPeorSF, ESTADOPeorSF, TipoCredSaldoMax,
                     saldoNoBSO, saldoMoraNoBSO,saldoVenNoBSO,saldoCastNoBSO,montoNoBSO),~max(.x)),
            across(c(saldous,montous,saldoMora, saldoVig, saldoCast,OP_MAYOR_5000,OPERACIONES),~sum(.x)),
            FDESEMBOLSO=min(fdes)) %>% 
  ungroup()

Exc <- infoExc %>% 
  group_by(Fecha,CI,CTACLIENTE,GENERO) %>% 
  mutate(maxSaldo = ifelse(saldous==max(saldous),1,0)) %>% 
  mutate(peorCalif = ifelse(CalifBSO==max(CalifBSO),1,0)) %>% 
  mutate(ESTADO = ifelse(peorCalif==1,ESTADO,'_')) %>% 
  mutate(CalifBSO = ifelse(peorCalif==1,CalifBSO,'_')) %>% 
  # mutate(MODULO = ifelse(peorCalif==1,MODULO,-1)) %>% 
  mutate(TIPO_CREDITO = ifelse(maxSaldo==1,TIPO_CREDITO,'_')) %>% 
  mutate(across(c(ESTADO,CalifBSO,TIPO_CREDITO),~max(.x,na.rm = T))) %>% 
  # dplyr::filter(montous>5000) %>% #Filtro a nivel de operación y luego agrupar
  mutate(OP_MAYOR_5000 = ifelse(montous>5000,1,0)) %>% 
  mutate(OPERACIONES = 1) %>% 
  summarise(across(c(ESTADO, TIPO_CREDITO, CalifBSO),~max(.x)),
            across(c(saldous,montous,saldoMora, saldoVig, saldoCast,OP_MAYOR_5000,OPERACIONES),~sum(.x)),
            FDESEMBOLSO=min(fdes)) %>% 
  ungroup()

set.seed(12345)
Clientes <- Comp %>%
  bind_rows(Exc) %>% 
  mutate(IDMask = sample(1e6:5e6,size = n(),replace = F))

IDMask <- Clientes %>% 
  select(IDMask,CTACLIENTE,CI)

Clientes <- Clientes %>% 
  select(-CTACLIENTE,-CI)
write.xlsx(Clientes,'D:/!bso/califClientes/IFC_Dic2022.xlsx')
write.xlsx(IDMask,'D:/!bso/califClientes/IDMask_Dic2022.xlsx')

####____UNMASKING____####
Clientes <- read.xlsx('D:/!bso/califClientes/IFC_Dic2022.xlsx',sheet = "Lista")
ID <- read.xlsx('D:/!bso/califClientes/IDMask_Dic2022.xlsx')
Clientes <- Clientes %>% 
  left_join(ID,by="IDMask") %>% 
  select(-CI,-IDMask) %>% 
  relocate(CTACLIENTE)

Clientes <- Clientes %>% 
  mutate(FECHA = as.yearmon(FECHA)) %>% 
  mutate(FDESEMBOLSO = as.Date(FDESEMBOLSO,origin="1899-12-30"))
write.xlsx(Clientes, 'D:/!bso/califClientes/IFC_Dic2022_v2.xlsx')

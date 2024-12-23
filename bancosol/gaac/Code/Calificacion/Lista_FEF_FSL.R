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
mybdc <- c("Nov2022","Dic2022","Ene2023","Mar2023")
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
      mutate(ESTADO = case_when(ESTADO=="VIGENTE"~"1. VIGENTE",
                                ESTADO=="OP VENCIDA"~"2. VENCIDA",
                                ESTADO=="SUSPENSO"~"3. SUSPENSO",
                                ESTADO=="JUDICIAL"~"4. JUDICIAL",
                                ESTADO=="CASTIGADA"~"5. CASTIGADA",)) %>% 
      select(Fecha,CTACLIENTE,OPERACION,CI,GENERO,ESTADO,MODULO,CalifBSO=CALIFICACION, AGENCIA,
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
      mutate(ESTADOInfo = case_when(`SBEF VIGENTE`>0 ~ '1. VIGENTE',
                                    `SBEF VENCIDO`>0 ~ '2. VENCIDA',
                                    `SBEF EJECUCION`>0 ~ '4. JUDICIAL',
                                    `SBEF CASTIGADO`>0 ~ '5. CASTIGADA',
                                    `SBEF CONTINGENTE`>0 ~ '0. CONTINGENTE')) %>% 
      select(Fecha,NumeroOp, CI, `TIPO OBLIGADO SBEF`, SIGLA=`SIGLA SBEF`,ENTIDAD=`ENTIDAD SBEF`, 
             CALIFICACION = `SBEF CALIFICACION`,MonedaOrigen, MontoOriginal, saldoInfo, 
             saldoMoraInfo, saldoVigInfo = `SBEF VIGENTE`, saldoCastInfo = `SBEF CASTIGADO`,
             saldoContingente = `SBEF CONTINGENTE`, ESTADOInfo) %>%
      separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      # mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
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
      # mutate(maxMonto = ifelse(MontoOriginal==max(MontoOriginal),1,0)) %>% 
      mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
      ungroup() %>% 
      group_by(CI) %>% 
      mutate(Tiene_FEF=ifelse(saldoInfo>0 & SIGLA=="FEF",1,0),
             Tiene_FEF=max(Tiene_FEF)) %>% 
      mutate(Tiene_FSL=ifelse(saldoInfo>0 & SIGLA=="FSL",1,0),
             Tiene_FSL=max(Tiene_FSL)) %>% 
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      dplyr::filter(Tiene_FEF>0 | Tiene_FSL>0) %>% 
      mutate(siglaPeorSF = ifelse(peorCalif==1 & esBSO==0,SIGLA,'_')) %>% 
      mutate(CalifPeorSF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
      mutate(ESTADOPeorSF = ifelse(peorCalif==1 & esBSO==0,ESTADOInfo,'_')) %>% 
      mutate(across(siglaPeorSF:ESTADOPeorSF, ~ max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
      mutate(esFEF = ifelse(saldoInfo>0 & SIGLA=="FEF",1,0)) %>% 
      mutate(esFSL = ifelse(saldoInfo>0 & SIGLA=="FSL",1,0)) %>% 
      mutate(N_CRED_FEF = sum(esFEF)) %>% 
      mutate(N_CRED_FSL = sum(esFSL)) %>% 
      mutate(Saldo_FEF = sum(saldoInfo*esFEF,na.rm = T)) %>% 
      mutate(Saldo_FSL = sum(saldoInfo*esFSL,na.rm = T)) %>% 
      mutate(Max_Saldo_FEF = max(saldoInfo*esFEF,na.rm = T)) %>% 
      mutate(Max_Saldo_FSL = max(saldoInfo*esFSL,na.rm = T)) %>% 
      # mutate(saldoMora_FEF = sum(saldoMoraInfo*esFEF,na.rm = T)) %>% 
      # mutate(saldoMora_FSL = sum(saldoMoraInfo*esFSL,na.rm = T)) %>% 
      # mutate(saldoCont_FEF = sum(saldoContingente*esFEF,na.rm = T)) %>% 
      # mutate(saldoCont_FSL = sum(saldoContingente*esFSL,na.rm = T)) %>% 
      # mutate(saldoCast_FEF = sum(saldoCastInfo*esFEF,na.rm = T)) %>% 
      # mutate(saldoCast_FSL = sum(saldoCastInfo*esFSL,na.rm = T)) %>% 
      mutate(Monto_FEF = sum(MontoOriginal*esFEF,na.rm=T)) %>% 
      mutate(Monto_FSL = sum(MontoOriginal*esFSL,na.rm=T)) %>% 
      ungroup() %>% 
      dplyr::filter(SIGLA=="BSO")
    infoJoin <- infoComp %>% 
      rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoCompList[[i]] <- infoJoin
    
    infoAntiJoin <- bdcBSO_full %>% 
      anti_join(infoJoin,by=c("CTACLIENTE","OPERACION","Fecha"))
    infoExcList[[i]] <- infoAntiJoin
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

Comp <- infoJoin %>% 
  dplyr::filter(!is.na(GENERO)) %>% 
  group_by(Fecha,CI_info,CTACLIENTE,GENERO) %>% 
  mutate(maxSaldo = ifelse(saldous==max(saldous),1,0)) %>% 
  mutate(ESTADO = ifelse(maxSaldo==1,ESTADO,'_')) %>% 
  mutate(CalifBSO = ifelse(maxSaldo==1,CalifBSO,'_')) %>% 
  mutate(MODULO = ifelse(maxSaldo==1,MODULO,-1)) %>% 
  mutate(across(c(ESTADO,MODULO,CalifBSO),~max(.x,na.rm = T))) %>% 
  summarise(across(c(AGENCIA,ESTADO, MODULO, CalifBSO, CalifPeorSF, ESTADOPeorSF,Tiene_FEF, 
                     Tiene_FSL,N_CRED_FEF,N_CRED_FSL,Saldo_FEF,Saldo_FSL,Max_Saldo_FEF,
                     Max_Saldo_FSL,Monto_FEF,Monto_FSL),~max(.x)),
            across(c(saldous,montous,saldoMora, saldoVig, saldoCast),~sum(.x))) %>% 
  dplyr::filter(ESTADO!="5. CASTIGADA") %>% 
  mutate(cred_prom_FEF = ifelse(N_CRED_FEF!=0,Saldo_FEF/N_CRED_FEF,0),
         cred_prom_FSL = ifelse(N_CRED_FSL!=0,Saldo_FSL/N_CRED_FSL,0)) %>% 
  mutate(rango_ok = ifelse(Saldo_FEF>=20000 | Saldo_FSL>=20000,1,0)) %>% 
  mutate(rango_max = ifelse(Max_Saldo_FEF>=20000 | Max_Saldo_FSL >=20000,1,0))


VIPPreAP <- read_xlsx("D:/!bso/vipCartera/Leads_PreAprobados_CompraCartera_Marzo2023.xlsx",sheet="PreAprobados")
VIPCompra <- read_xlsx("D:/!bso/vipCartera/Leads_PreAprobados_CompraCartera_Marzo2023.xlsx",sheet="CompraCartera")

lastCierre <- fread("D:/!bso/mph/Oreports/lastCierrreUR_Mar2023.csv",
                    encoding = "UTF-8",sep=",",fill=T) %>% 
  replace_na(list(Instancias_UR=0)) %>% 
  group_by(CTACLIENTE) %>% 
  summarise(maxTardios = max(Instancias_UR,na.rm=T))

codAge <- read_xlsx("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
infoPotencial <- Comp %>% 
  left_join(codAge,by="AGENCIA") %>% 
  select(-Regional,-AGENCIA,-NOMBRE_AGENCIA) %>% 
  relocate(Sucursal,.after = Fecha) %>% 
  dplyr::filter(!is.na(GENERO)) %>% 
  mutate(Banco = case_when(Tiene_FSL==1 & Tiene_FEF ==1~"Ambos",
                           Tiene_FSL==1 & Tiene_FEF ==0~"Fassil",
                           Tiene_FSL==0 & Tiene_FEF ==1~"Ecofuturo",)) %>% 
  mutate(EnLista = ifelse(CTACLIENTE %in% VIPCompra$CTACLIENTE,1,0)) %>% 
  mutate(Fecha=as.Date(Fecha,frac=1)) %>% 
  left_join(lastCierre,by="CTACLIENTE")

write_xlsx(infoPotencial,"D:/!bso/vipCartera/Universo_Compartido_FEF_FSL_VIP_v5.xlsx")

####____ADDING MORA INTRA MES____####
lastFSL <- read_xlsx("D:/!bso/vipCartera/Universo_Compartido_FEF_FSL_VIP_v6_2.xlsx",sheet = "Lista")
moraIntraMes <- readRDS("D:/!bso/vipCartera/moraIM_pFassil.rds") %>% 
  group_by(CTACLIENTE,OPERACION) %>% 
  arrange(desc(monDate)) %>% 
  dplyr::filter(row_number() <= 4) %>% 
  group_by(CTACLIENTE) %>% 
  summarise(Max_Mora_IM = max(max_mora_IM), Prom_Mora_IM = mean(Max_Mora_IM))

lastFSL_Mora <- lastFSL %>% 
  left_join(moraIntraMes, by="CTACLIENTE") %>% 
  mutate(Fecha=as.Date(Fecha))

write_xlsx(lastFSL_Mora,"D:/!bso/vipCartera/Universo_Compartido_FEF_FSL_VIP_v7.xlsx")
####____SOLICITUD DE SERGIO____####
x <- infoRaw %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  dplyr::filter(`SIGLA SBEF`=="FSL")

write.xlsx(x,"D:/!bso/listaFSL.xlsx")
####____COMPARTIDOS CON FSL____####
year <- c("2018","2019","2020","2021","2022","2023")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
myutf <- as.vector(sapply(year, function(x){paste0(x,mes)}))
mybdc <- as.vector(sapply(year, function(x){paste0(month,x)}))
i <- 1
for(i in 1:length(mybdc)){
  tryCatch({
    print(myutf[i])
    infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO',myutf[i],'_utf8.txt'), encoding = 'UTF-8', fill = T)
    infoCheck <- infoRaw %>%
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% #Filtro de deudor
      dplyr::filter(`SIGLA SBEF` =="FSL") %>% 
      mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      rowwise() %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldoInfo = sum(`SBEF VIGENTE`,`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
             saldoMoraInfo = sum(`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T)) %>%
      ungroup() %>% 
      mutate(ESTADOInfo = case_when(`SBEF VIGENTE`>0 ~ '1. VIGENTE',
                                    `SBEF VENCIDO`>0 ~ '2. VENCIDA',
                                    `SBEF EJECUCION`>0 ~ '4. JUDICIAL',
                                    `SBEF CASTIGADO`>0 ~ '5. CASTIGADA',
                                    `SBEF CONTINGENTE`>0 ~ '0. CONTINGENTE')) %>% 
      select(Fecha,NumeroOp, CI, `TIPO OBLIGADO SBEF`, SIGLA=`SIGLA SBEF`,ENTIDAD=`ENTIDAD SBEF`, 
             CALIFICACION = `SBEF CALIFICACION`,MonedaOrigen, MontoOriginal, saldoInfo, 
             saldoMoraInfo, saldoVigInfo = `SBEF VIGENTE`, saldoCastInfo = `SBEF CASTIGADO`,
             saldoContingente = `SBEF CONTINGENTE`, ESTADOInfo) %>%
      separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      # mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #Sin importar el valor de moneda
      mutate(saldoInfo = saldoInfo/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(saldoMoraInfo = saldoMoraInfo/6.86) %>% 
      mutate(saldoVigInfo = as.numeric(saldoVigInfo)/6.86) %>%
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) 
    write.xlsx(infoCheck,"D:/!bso/calif.xlsx")
      
    infoCheck <- infoRaw %>%
      mutate(Fecha=as.yearmon(ymd(paste0(substr(myutf[i],1,4),'/',substr(myutf[i],5,8), '/01')))) %>% 
      rowwise() %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldo = sum(`SBEF VIGENTE`,`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
             saldoMora = sum(`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T)) %>%
      ungroup() %>% 
      mutate(ESTADO = case_when(`SBEF VIGENTE`>0 ~ '1. VIGENTE',
                                `SBEF VENCIDO`>0 ~ '2. VENCIDA',
                                `SBEF EJECUCION`>0 ~ '4. JUDICIAL',
                                `SBEF CASTIGADO`>0 ~ '5. CASTIGADA',
                                `SBEF CONTINGENTE`>0 ~ '0. CONTINGENTE')) %>% 
      select(NDOC = `NRO DOCUMENTO`, EXT, CI, NOMBRE = `NOMBRE COMPLETO`,TIPO_OBLIGADO = `TIPO OBLIGADO SBEF`, 
             HISTORICO, DiasMora, SIGLA = `SIGLA SBEF`,ENTIDAD = `ENTIDAD SBEF`, 
             CALIFICACION = `SBEF CALIFICACION`, FechaInicio = `FECHA INICIO OPERACION`,
             MonedaOrigen, MontoOriginal, saldo, saldoMora, saldoVig = `SBEF VIGENTE`, 
             saldoCast = `SBEF CASTIGADO`, saldoCont = `SBEF CONTINGENTE`, NumeroOp, ESTADO, 
             TIPO_CREDITO = `TIPO CREDITO SBEF`) %>%
      separate_wider_delim(NumeroOp, names = c("CTACLIENTE","OPERACION"), delim="-",
                           too_few = 'align_start', too_many = 'drop') %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      mutate(across(MontoOriginal:saldoCont,~as.numeric(.x)/6.86)) %>% 
      mutate(HISTORICO = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
      mutate(histLast16 = substr(HISTORICO, 1, 16)) %>% 
      mutate(histLast16 = ifelse(is.na(histLast16), '0', histLast16)) %>% 
      mutate(histLast1 = substr(HISTORICO, 1, 1)) %>% 
      mutate(histLast1 = ifelse(is.na(histLast1), '0', histLast1)) %>% 
      mutate(badCredit = ifelse((str_detect(histLast16, '2') | str_detect(histLast16, '3') |
                                   str_detect(histLast16, '4')),1,0)) %>% # CHECK MEASURE
      mutate(worstCredit = ifelse(str_detect(histLast16, '4'),1,0)) %>% # CHECK MEASURE
      mutate(N_VIGENTE = str_count(histLast16,'1'),
             N_VENCIDO = str_count(histLast16,'2'),
             N_EJECUCION = str_count(histLast16,'3'),
             N_CASTIGADO = str_count(histLast16,'4')) %>% 
      ungroup() %>% 
      dplyr::filter(!(is.na(HISTORICO) & is.na(SIGLA) & is.na(TIPO_OBLIGADO)))
    
    saveRDS(infoCheck,paste0('D:/!bso/califClientes/process/comp_',mybdc[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
################################################################################
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Mar2023.rds") %>% 
  select(CI,ESTADO) %>% 
  distinct_all()
write.xlsx(bdc,"D:/!bso/bdc_CI_ESTADO.xlsx")
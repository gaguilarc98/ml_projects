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
# require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FUNCIONES____####
prevision <- function(x){
  x %>% 
    mutate(prev=case_when(califPot=='A' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==1 & MONEDA == 'MN'~0,
                          califPot=='B' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==1 & MONEDA == 'MN'~0.025,
                          califPot=='A' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==2 & MONEDA == 'MN'~0.0025,
                          califPot=='B' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==2 & MONEDA == 'MN'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'MN'~0.0025,
                          califPot=='B' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'MN'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'MN'~0.03,
                          califPot=='B' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'MN'~0.065,
                          califPot=='A' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'MN'~0.0025,
                          califPot=='B' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'MN'~0.05,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'MN'~0.015,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'MN'~0.03,
                          califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & MONEDA == 'MN'~0.065,
                          califPot=='C' ~ 0.20,
                          califPot=='D' ~ 0.50,
                          califPot=='E' ~ 0.80,
                          califPot=='F' ~ 1,
                          califPot=='A' & tipoCred %in% c('Micro','PyMe') & CONTINGENTE==0 & MONEDA == 'ME'~0.025,
                          califPot=='A' & tipoCred %in% c('Micro','PyMe') & CONTINGENTE==1 & MONEDA == 'ME'~0.01,
                          califPot=='B' & tipoCred %in% c('Micro','PyMe') & MONEDA == 'ME'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.025,
                          califPot=='B' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.07,
                          califPot=='B' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.12,
                          califPot=='A' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.025,
                          califPot=='B' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.05,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.05,
                          califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.08,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.07,
                          califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.12,
                          califPot=='S' ~ 1,
                          califPot=='Z' ~ 0,)) %>% 
    mutate(previusNew=saldous*prev)
}
####____CONVERTING TXT TO RDS FOR BETTER STORAGE____####
year <- c("2018","2019","2020","2021","2022","2023")
myutf <- "202308"

# infoRaw <- fread(paste0('D://SCRIPTS R//Clientes compartidos//BSO',myutf,'_utf8.txt'), encoding = 'UTF-8', fill = T)
# infoRaw <- fread(paste0('D:/SCRIPTS R/Clientes compartidos/BSO',myutf,'.txt'), encoding = 'UTF-8', fill = T)
infoRaw <- fread(paste0('D:/SCRIPTS R/Clientes compartidos/BSO',myutf,'.txt'), encoding = 'Latin-1', fill = T)
saveRDS(infoRaw,paste0("D:/SCRIPTS R/Clientes compartidos/BSO",myutf,".rds"))
####____PROCESS INFO_CRED____####
year <- c("2018","2019","2020","2021","2022","2023")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
myutf <- as.vector(sapply(year, function(x){paste0(x,mes)}))
mybdc <- as.vector(sapply(year, function(x){paste0(month,x)}))
i <- 69
for(i in 1:length(mybdc)){
  tryCatch({
    
    print(myutf[i])
    infoRaw <- readRDS(paste0("D:/SCRIPTS R/Clientes compartidos/BSO",myutf[i],".rds")) %>% 
      dplyr::filter(nchar(`NOMBRE COMPLETO`)<100)
    # infoRaw <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/rds/BSO',myutf[i],'.rds')) %>% 
    #   dplyr::filter(nchar(`NOMBRE COMPLETO`)<100)
    #CHECK DE INTEGRIDAD
    rep <- length(which(infoRaw$`ENTIDAD FR`!="" & infoRaw$`ENTIDAD SBEF`!=""))
    print(paste0("NRO SBEF Y FR: ", rep))
    if(rep>0){
      break
    }
    infoCheck <- infoRaw %>%
      mutate(Fecha=as.yearmon(ymd(paste0(substr(myutf[i],1,4),'/',substr(myutf[i],5,8), '/01')))) %>% 
      mutate(REGULADO = case_when(!is.na(`SIGLA SBEF`) & `SIGLA SBEF`!="" ~ "SBEF",
                                  !is.na(`SIGLA FR`) & `SIGLA FR`!="" ~ "FR",
                                  TRUE ~ "")) %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
      rowwise() %>% 
      mutate(saldo = ifelse(REGULADO=="SBEF",sum(`SBEF VIGENTE`,`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
                            sum(`FR VIGENTE`,`FR VENCIDO`,`FR EJECUCION`,na.rm = T))) %>% 
      mutate(saldoMora = ifelse(REGULADO=="SBEF",sum(`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
                                sum(`FR VENCIDO`,`FR EJECUCION`,na.rm = T))) %>% 
      ungroup() %>% 
      mutate(saldoVig = ifelse(REGULADO=="SBEF", `SBEF VIGENTE`, `FR VIGENTE`)) %>% 
      mutate(saldoVen = ifelse(REGULADO=="SBEF", `SBEF VENCIDO`, `FR VENCIDO`)) %>% 
      mutate(saldoEje = ifelse(REGULADO=="SBEF", `SBEF EJECUCION`, `FR EJECUCION`)) %>% 
      mutate(saldoCast = ifelse(REGULADO=="SBEF", `SBEF CASTIGADO`, `FR CASTIGADO`)) %>% 
      mutate(ESTADO = case_when(saldoCast>0 ~ '4. CASTIGADA',
                                saldoEje>0 ~ '3. EJECUCION',
                                saldoVen>0 ~ '2. VENCIDA',
                                saldoVig>0 ~ '1. VIGENTE',
                                `SBEF CONTINGENTE`>0 ~ '0. CONTINGENTE',)) %>% 
      mutate(TIPO_OBLIGADO = ifelse(REGULADO=="SBEF", `TIPO OBLIGADO SBEF`, `TIPO OBLIGADO FR`)) %>%
      mutate(SIGLA = ifelse(REGULADO=="SBEF", `SIGLA SBEF`, `SIGLA FR`)) %>%
      mutate(ENTIDAD = ifelse(REGULADO=="SBEF", `ENTIDAD SBEF`, `ENTIDAD FR`)) %>%
      mutate(TIPO_CREDITO = ifelse(REGULADO=="SBEF", `TIPO CREDITO SBEF`, `TIPO CREDITO FR`)) %>%
      mutate(CALIFICACION = ifelse(REGULADO=="SBEF", `SBEF CALIFICACION`, `FR CALIFICACION`)) %>%
      mutate(ENTIDAD = str_replace_all(ENTIDAD, '\\.|,|"', '')) %>% 
      mutate(TIPO_CREDITO = ifelse(nchar(TIPO_CREDITO)<2,TIPO_CREDITO,substr(TIPO_CREDITO,1,2))) %>% 
      select(CI, EXT, NOMBRE = `NOMBRE COMPLETO`, DEPARTAMENTO, 
             REGULADO, TIPO_OBLIGADO, SIGLA, ENTIDAD, TIPO_CREDITO, CALIFICACION, 
             FECHAINICIO = `FECHA INICIO OPERACION`, FECHAVTO, ESTADO, NumeroOp, 
             DiasMora, MonedaOrigen, MontoOriginal, saldo, saldoMora, saldoVig, saldoCast, 
             saldoCont = `SBEF CONTINGENTE`, Periodo_Pago = `PERIODO PAGO`, HISTORICO) %>%
      mutate(NOP_ORIGINAL = NumeroOp) %>% 
      separate_wider_delim(NumeroOp, names = c("CTACLIENTE","OPERACION"), delim="-",
                           too_few = 'align_start', too_many = 'merge') %>% 
      mutate(CTACLIENTE = ifelse(SIGLA=="BSO",as.numeric(CTACLIENTE),0),
             OPERACION = ifelse(SIGLA=="BSO",as.numeric(OPERACION),0)) %>% 
      mutate(across(MontoOriginal:saldoCont,~as.numeric(.x)/6.86)) %>% 
      mutate(esBSO = ifelse(SIGLA=="BSO",1,0)) %>% 
      mutate(esNoBSO = ifelse(SIGLA!="BSO",1,0)) %>% 
      mutate(esDeudorNoBSO = ifelse(str_detect(TIPO_OBLIGADO,'A - ') & SIGLA!="BSO",1,0)) %>% 
      mutate(esDeudorBSO = ifelse(str_detect(TIPO_OBLIGADO,'A - ') & SIGLA=="BSO",1,0)) %>% 
      mutate(esCodeudorBSO = ifelse(str_detect(TIPO_OBLIGADO,'B - ') & SIGLA=="BSO",1,0)) %>% 
      mutate(esGaranteBSO = ifelse(!str_detect(TIPO_OBLIGADO,'A - ') & !str_detect(TIPO_OBLIGADO,'B - ') & SIGLA=="BSO",1,0)) %>% 
      group_by(CI) %>% 
      dplyr::filter(sum(esBSO)>0) %>% #New ADDITION 14/06/23 TO FILTER CLIENTS THAT HAVE NO BSO OPERATION IN THE BASE
      arrange(desc(esDeudorBSO), desc(esCodeudorBSO), desc(esBSO), desc(saldo)) %>% #Para ordenar por esBSO
      mutate(across(esBSO:esGaranteBSO,~sum(.x))) %>% 
      mutate(CTACLIENTE_BSO = CTACLIENTE[row_number()==1]) %>% 
      ungroup() %>% 
      mutate(HISTORICO = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
      mutate(histLast16 = substr(HISTORICO, 1, 16)) %>% 
      mutate(histLast16 = ifelse(is.na(histLast16), '0', histLast16)) %>% 
      # mutate(histLast1 = substr(HISTORICO, 1, 1)) %>% 
      # mutate(histLast1 = ifelse(is.na(histLast1), '0', histLast1)) %>% 
      # mutate(badCredit = ifelse((str_detect(histLast16, '2') | str_detect(histLast16, '3') |
      #                              str_detect(histLast16, '4')),1,0)) %>% # CHECK MEASURE
      # mutate(worstCredit = ifelse(str_detect(histLast16, '4'),1,0)) %>% # CHECK MEASURE
      # mutate(N_VIGENTE = str_count(histLast16,'1'),
      #        N_VENCIDO = str_count(histLast16,'2'),
      #        N_EJECUCION = str_count(histLast16,'3'),
      #        N_CASTIGADO = str_count(histLast16,'4')) %>%
      ungroup() 
    
    l <- length(which(is.na(infoCheck$CTACLIENTE_BSO)))
    print(paste0("OBSERVATIONS DROPPED: ", l))
    
    infoCheck <- infoCheck %>% 
      dplyr::filter(!(is.na(CTACLIENTE_BSO)))
    
    saveRDS(infoCheck,paste0('D:/SCRIPTS R/Clientes compartidos/comp_',mybdc[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

####Muestra en  EXCEL
comp <- readRDS("D:/!bso/califClientes/process/comp_Ene2021.rds")
write_xlsx(infoRaw,"D:/!bso/califClientes/BSO202101.xlsx")
write_xlsx(comp,"D:/!bso/califClientes/comp_Ene2021.xlsx")
####____CHECK SALDO MONEDA FOR A SINGLE MONTH____####
bdc <- readRDS('//VFSNALSRV/Bases_Riesgos/MIS_SGNRFAR/analitica/rdsGAR/ec_Ago2023.rds')
infoClean <- readRDS(paste0('//VFSNALSRV/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/process/comp_','Ago2023','.rds')) %>%
# infoClean <- readRDS(paste0('D:/SCRIPTS R/Clientes compartidos/comp_','Ago2023','.rds')) %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  dplyr::filter(SIGLA =="BSO") %>% 
  left_join(select(bdc,CTACLIENTE,OPERACION,MODULO,saldous,saldoMora,saldoCast),
            by=c("CTACLIENTE","OPERACION"),suffix=c("_info","_bdc")) %>%
  # dplyr::filter(saldo!=saldous) %>% 
  # mutate(dif=abs(saldo-saldous)) %>% 
  # dplyr::filter(dif<2) %>% 
  glimpse()
sum(infoClean$saldo)+sum(infoClean$saldoCont)
sum(infoClean$saldous,na.rm = T)
sum(infoClean$saldoMora_info)
sum(infoClean$saldoMora_bdc)
sum(infoClean$saldoCast_bdc)
sum(infoClean$saldoCast_info)

####____WORST CALIF____####
year <- c("2018","2019","2020","2021","2022","2023")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mybdc <- as.vector(sapply(year, function(x){paste0(month,x)}))

mybdc <- c("Dic2022","Jul2023","Ago2023")
# mybdc <- c("Nov2022")
infoList <- list()

for(i in 1:length(mybdc)){
  tryCatch({
    print(mybdc[i])
    bdcBSO_full <- readRDS(paste0('//VFSNALSRV/Bases_Riesgos/MIS_SGNRFAR/analitica/rdsGAR/ec_',mybdc[i],'.rds')) %>%
      mutate(Fecha = as.yearmon(monDate)) %>%
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(ESTADO = case_when(ESTADO=="VIGENTE"~"1. VIGENTE",
                                ESTADO=="OP VENCIDA"~"2. VENCIDA",
                                ESTADO=="SUSPENSO"~"3. SUSPENSO",
                                ESTADO=="JUDICIAL"~"4. JUDICIAL",
                                ESTADO=="CASTIGADA"~"5. CASTIGADA",)) %>% 
      mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
      mutate(SEC_PROD = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                                     "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada",
                                                     "8.Otra Prod.No Controlada"),1,2)) %>% 
      mutate(MONEDA = ifelse(MONEDA==0,"MN","ME")) %>% 
      mutate(esFSL = ifelse(MODULO == 118 | str_detect(TIPO_OPER,'MIGR'),1,0)) %>% 
      # select(Fecha,CTACLIENTE,OPERACION,CI,GENERO,ESTADO, MODULO, Sucursal, DIASMORA,
      #        MONTOUS, previus, saldous, saldoMora, saldoCast, CONTINGENTE, SEC_PROD,
      #        MONEDA, TIPO_CRED = TIPO_CREDITO, tipoCred, fdes)
      select(Fecha,CTACLIENTE,OPERACION,CI,GENERO,ESTADO, MODULO, Sucursal, DIASMORA,
             MONTOUS, previus, saldous, saldoMora, saldoCast, CONTINGENTE, SEC_PROD,
             MONEDA, TIPO_CRED = TIPO_CREDITO, tipoCred, fdes, TIPO_OPER, esFSL)
    
    infoCheck <- readRDS(paste0('D:/SCRIPTS R/Clientes compartidos/comp_',mybdc[i],'.rds'))

    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      mutate(Fecha = as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>%
      group_by(CI) %>%
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      ungroup() %>% 
      group_by(CI,esBSO) %>% 
      arrange(desc(CALIFICACION), desc(saldo)) %>% 
      mutate(peorCalif = CALIFICACION[row_number() == 1]) %>% 
      mutate(siglaPeor = SIGLA[row_number() == 1]) %>% 
      mutate(ESTADOPeor = ESTADO[row_number() == 1]) %>% 
      ungroup() %>% 
      group_by(CI) %>% 
      mutate(siglaPeorSF = max(siglaPeor[esBSO == 0])) %>% 
      mutate(califPeorSF = max(peorCalif[esBSO == 0])) %>% 
      mutate(califPeorBSO = max(peorCalif[esBSO == 1])) %>% 
      mutate(ESTADOPeorSF = max(ESTADOPeor[esBSO == 0])) %>% 
      mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMora*noesBSO,na.rm=T)) %>% 
      mutate(montoNoBSO = sum(MontoOriginal*noesBSO,na.rm=T)) %>%
      ungroup() %>% 
      dplyr::filter(califPeorSF!="_" & SIGLA=="BSO")
    
    bdcSummary <- bdcBSO_full %>% 
      dplyr::filter(MODULO!=131, MODULO!=29) %>%
      dplyr::filter(!str_detect(ESTADO,"CASTIGADA")) %>%
      group_by(Fecha) %>% 
      summarise(SaldoTotalBSO = sum(saldous),ClientesBSO=n_distinct(CI)) %>% 
      ungroup()
    
    infoJoin <- infoClean %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha"),
                suffix=c("_info","_bdc")) %>%
      # inner_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha"),
      #                     suffix=c("_info","_bdc")) %>% ###
      left_join(bdcSummary, by="Fecha") 
    
    infoList[[i]] <- infoJoin
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoFull <- bind_rows(infoList)

####____PEOR CALIFICACION____####
lastmonth <- "Ago. 2023"
shortmonth <- str_replace(lastmonth,". ","")

infoPerf <- infoFull %>% 
  group_by(CI_info) %>% 
  mutate(GENERO = max(GENERO)) %>% 
  ungroup() %>% 
  group_by(Fecha,CI_info,GENERO) %>% 
  summarise(across(c(Sucursal,ESTADO_bdc,siglaPeorSF:montoNoBSO,SaldoTotalBSO,ClientesBSO, esFSL),~max(.x)),
            across(c(saldo,saldous,saldoMora_bdc,saldoCast_bdc),~sum(.x))) %>%
  mutate(Cliente = 1.0) %>% 
  mutate(PeorEnSF=ifelse(califPeorBSO < califPeorSF,1,0)) %>% 
  mutate(MejorEnSF=ifelse(califPeorBSO > califPeorSF,1,0)) %>% 
  mutate(igualEnSF=ifelse(califPeorBSO==califPeorSF,1,0)) %>% 
  mutate(saldoPeor=PeorEnSF*saldo) %>% 
  ungroup()

infoPerf %>% dplyr::filter(PeorEnSF == 1 & esFSL == 1) %>% summarise(saldo = sum(saldo), sp = sum(saldoPeor)) 

x <- infoPerf %>% 
  filter(Fecha == "Jul. 2023") %>% 
  group_by(CI_info) %>% 
  filter(max(row_number())>1) %>% 
  mutate(GENERO = max(GENERO))
  
  
check <- infoPerf %>% #Check de cuadre con meses anteriores
  group_by(Fecha) %>% 
  summarise(Clientes = sum(Cliente),saldoBSO=sum(saldous),Cast=sum(saldoCast_bdc),
            cliente_distint = n_distinct(CI_info),
            SaldoTotBSO = saldoBSO+Cast,ClientesTot=max(ClientesBSO),
            SaldoTot=max(SaldoTotalBSO),
            Clientes_perc=Clientes/max(ClientesBSO),
            SaldoBSO_perc = saldoBSO/max(SaldoTotalBSO),Peores=sum(PeorEnSF),
            SaldoPeor = sum(saldoPeor))

infoPerf %>% #Check peor entidad
  dplyr::filter(Fecha==lastmonth) %>% 
  group_by(siglaPeorSF) %>% 
  summarise(Clientes=sum(Cliente),ClientesPeores=sum(PeorEnSF),SaldoBSO=sum(saldo),SaldoPeor=sum(saldoPeor)) %>% 
  arrange(desc(SaldoBSO))

infoPerf %>% #Check por peor Calif
  dplyr::filter(Fecha==lastmonth, PeorEnSF==1) %>% 
  group_by(califPeorSF,califPeorBSO) %>% 
  summarise(Saldo=sum(saldoPeor)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = califPeorSF, values_from = Saldo) %>% 
  adorn_totals("col")

# write_rds(infoFull,'D:/!bso/califClientes/infoFull.rds')
# infoFull <- readRDS('D:/!bso/califClientes/infoFull.rds')

####____PREVISION POR CONTAGIO____####
lastCierre <- fread(paste0("D:/SCRIPTS R/Pagos tardios/lastCierrreUR_",shortmonth,".txt"),
                    encoding = "UTF-8",sep="|",fill=T) %>% 
  select(CTACLIENTE=Cuenta, OPERACION =Operacion,Instancias_UR,Ultimo_mes,Prevision_USD,CALIFICACION)

#Check de integridad para ver si las calificaciones coinciden, el resultado debe ser cero
length(which(infoFull$califPeorBSO!=infoFull$CALIFICACION))

infoPrev <- infoFull %>% 
  dplyr::filter(Fecha==lastmonth) %>% 
  rename(califPot = califPeorSF, FDES=fdes) %>% 
  prevision() %>% 
  dplyr::filter(califPot>califPeorBSO) %>% #califPot>CALIFICACION 
  select(CTACLIENTE,OPERACION,saldous,previus,previusNew,califPeorBSO,califPot, esFSL) %>% 
  full_join(lastCierre,by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(Instancias_UR=0,Ultimo_mes=0)) %>% 
  mutate(RangoPagosTardios = case_when(Instancias_UR<6~as.character(Instancias_UR),
                                       Instancias_UR>=6~"6+",)) %>% 
  mutate(previusNew = ifelse(califPot<=califPeorBSO,0,previusNew)) %>% 
  mutate(difPrev = previusNew-previus)

#Check de integridad para ver si las calificaciones coinciden, el resultado debe ser cero
length(which(infoFull$califPeorBSO!=infoFull$CALIFICACION))
#Check de integridad para ver si el número de filas coinciden antes y después del join
nrow(lastCierre)==nrow(infoPrev)

infoPrev %>% #Check de integridad de prevision adicional
  group_by(califPot,CALIFICACION) %>% 
  summarise(difPrev=sum(previusNew)) %>% 
  pivot_wider(names_from = califPot,values_from = difPrev) %>% 
  adorn_totals('row')
  
infoPrev %>% #Check de Pagos tardíos
  dplyr::filter(califPot!="A") %>% 
  group_by(califPot,RangoPagosTardios) %>% 
  summarise(Saldo=sum(saldous)) %>% 
  ungroup() %>%
  pivot_wider(names_from = RangoPagosTardios,values_from = Saldo) %>% 
  adorn_totals(c("col","row"))

infoPerf <- infoPerf %>% 
  mutate(Fecha = as.Date(Fecha,frac=1))
infoPrev <- infoPrev 
Informe <- list(Perf = infoPerf, Prev = infoPrev) 

write_xlsx(Informe, paste0("D:/SCRIPTS R/Clientes compartidos/lastComp_",shortmonth,"_nuevo2.xlsx"))

####____CALIFREADING____#### 
lastCierre <- fread(paste0("D:/!bso/mph/Oreports/lastCierrreUR_",shortmonth,".csv"),
                    encoding = "UTF-8",sep=",",fill=T) %>% 
  select(CTACLIENTE=Cuenta, OPERACION =Operacion, Sucursal, Agencia, Asesor)
Informe <- read_xlsx(paste0("D:/!bso/califClientes/output/lastComp_",shortmonth,"_v2.xlsx"), sheet = "Prev")

Informe_wSucursal <- Informe %>% 
  left_join(lastCierre,by=c("CTACLIENTE","OPERACION"))

write_xlsx(Informe_wSucursal,"D:/!bso/mph/condonados/PeorCalif_wSucursalAbr2023.xlsx")
####____ADDING CONDONACIONES____####
Informe_wSucursal_select <- infoClean %>% 
  select(CTACLIENTE, califPeorSF) %>% 
  group_by(CTACLIENTE) %>% 
  summarise(PeorCalificacion = max(califPeorSF,na.rm = T))

tabla <- read.xlsx("D:/!bso/mph/condonados/TardioCond_May2023_v2.xlsx",sheet = "Datos")

condTardioPeorCalif <- tabla %>% 
  left_join(Informe_wSucursal_select, by=c("Cuenta"="CTACLIENTE"))

write_xlsx(condTardioPeorCalif,"D:/!bso/mph/condonados/TardioCond_May2023_v3.xlsx")  
####_____COMPLETAR CON PAGOS TARDIOS____####
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

####____COMPARTIDOS Y EXCLUSIVOS MKT____####
mybdc <- "Mar2023"

print(mybdc)
bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc,'.rds')) %>%
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(FECHA = as.Date(monDate,frac=1)) %>%
  mutate(MONTO_USD = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(ESTADO = case_when(ESTADO %in% c("VIGENTE","SUSPENSO")~"1. VIGENTE",
                            ESTADO=="OP VENCIDA"~"2. VENCIDA",
                            ESTADO=="JUDICIAL"~"3. EJECUCION",
                            ESTADO=="CASTIGADA"~"4. CASTIGADA",)) %>% 
  mutate(SALDO_VIG = ifelse(ctaCont %in% c('131','135'),saldous,0)) %>% 
  select(FECHA, CTACLIENTE, OPERACION, CI, GENERO, ESTADO, MODULO, CALIFICACION,
         MONTO_USD, SALDO_USD = saldous, SALDO_MORA = saldoMora, SALDO_VIG,
         SALDO_CAST = saldoCast) %>% 
  # group_by(FECHA, CTACLIENTE) %>% 
  # summarise(across(c(CI, GENERO, ESTADO, CALIFICACION),~max(.x)),
  #           across(c(MONTO_USD, SALDO_USD, SALDO_MORA, SALDO_VIG, SALDO_CAST),~sum(.x))) %>% 
  glimpse()
  

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mybdc,'.rds'))

infoClean <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI,esBSO) %>% 
  mutate(maxMonto = ifelse(MontoOriginal==max(MontoOriginal),1,0)) %>% 
  mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(ENTIDAD_MAX_SF = ifelse(maxMonto==1 & esBSO==0,ENTIDAD,'_')) %>% 
  mutate(SIGLA_MAX_SF = ifelse(maxMonto==1 & esBSO==0,SIGLA,'_')) %>% 
  mutate(CALIF_MAX_SF = ifelse(maxMonto==1 & esBSO==0,CALIFICACION,'_')) %>% 
  mutate(ENTIDAD_PEOR_SF = ifelse(peorCalif==1 & esBSO==0,ENTIDAD,'_')) %>% 
  mutate(SIGLA_PEOR_SF = ifelse(peorCalif==1 & esBSO==0,SIGLA,'_')) %>% 
  mutate(CALIF_PEOR_SF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
  mutate(across(ENTIDAD_MAX_SF:CALIF_PEOR_SF,~max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
  mutate(SALDO_USD = sum(saldo*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_VIG = sum(saldoVig*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_MORA = sum(saldoMora*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_CAST = sum(saldoCast*noesBSO,na.rm=T)) %>% 
  mutate(MONTO_USD = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::filter(CALIF_MAX_SF!="_" & SIGLA=="BSO") %>% 
  select(CI, CTACLIENTE, OPERACION, ENTIDAD_MAX_SF, SIGLA_MAX_SF, CALIF_MAX_SF, 
         ENTIDAD_PEOR_SF, SIGLA_PEOR_SF, CALIF_PEOR_SF, SALDO_USD, SALDO_VIG, SALDO_MORA, 
         SALDO_CAST, MONTO_USD)


infoJoin <- bdcBSO_full %>% 
  inner_join(select(infoClean,-CI), by = c("CTACLIENTE","OPERACION"), 
            suffix=c("_BSO","_SF"))
infoAntiJoin <- bdcBSO_full %>% 
  anti_join(infoClean,by=c("CTACLIENTE","OPERACION"))


Comp <- infoJoin %>% 
  group_by(FECHA, CTACLIENTE, CI, GENERO) %>% 
  mutate(maxSaldo = ifelse(SALDO_USD_BSO==max(SALDO_USD_BSO),1,0)) %>% 
  mutate(ESTADO = ifelse(maxSaldo==1,ESTADO,'_')) %>% 
  mutate(CALIFICACION_BSO = ifelse(maxSaldo==1,CALIFICACION,'_')) %>% 
  mutate(MODULO = ifelse(maxSaldo==1,MODULO,-1)) %>% 
  mutate(across(c(ESTADO,MODULO,CALIFICACION_BSO),~max(.x,na.rm = T))) %>% 
  summarise(across(c(ESTADO, MODULO, CALIFICACION, ENTIDAD_MAX_SF, SIGLA_MAX_SF, CALIF_MAX_SF,ENTIDAD_PEOR_SF,
                     SIGLA_PEOR_SF, CALIF_PEOR_SF, SALDO_USD_SF, SALDO_MORA_SF,SALDO_CAST_SF,MONTO_USD_SF),~max(.x)),
            across(c(SALDO_USD_BSO,MONTO_USD_BSO,SALDO_MORA_BSO, SALDO_VIG, SALDO_CAST_BSO),~sum(.x)))

Exc <- infoAntiJoin %>% 
  group_by(FECHA, CI,CTACLIENTE, GENERO) %>% 
  mutate(maxSaldo = ifelse(SALDO_USD==max(SALDO_USD),1,0)) %>% 
  mutate(ESTADO = ifelse(maxSaldo==1,ESTADO,'_')) %>% 
  mutate(CALIFICACION = ifelse(maxSaldo==1,CALIFICACION,'_')) %>% 
  mutate(MODULO = ifelse(maxSaldo==1,MODULO,-1)) %>% 
  summarise(across(c(ESTADO, MODULO, CALIFICACION),~max(.x)),
            across(c(SALDO_USD,MONTO_USD,SALDO_MORA, SALDO_VIG, SALDO_CAST),~sum(.x)))

n_distinct(bdcBSO_full$CTACLIENTE)== nrow(Exc)+nrow(Comp)
lista <- list(Compartidos = Comp,Exclusivos = Exc)
write.xlsx(lista,'D:/!bso/califClientes/Compartidos_Exclusivos_Abr2023.xlsx')

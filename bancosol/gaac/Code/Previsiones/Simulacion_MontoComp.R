####____CARGA DE PAQUETES Y FUNCIONES____####
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
library(stringr) # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ca)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FUNCIONES____####
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

#La primera es por norma Artículo 20, sección 10
#califPot=='A' & tipoCred!="Consumo"& FDES >= as.Date("2021-08-02") & FDES <= as.Date("2022-07-29") & MONEDA == "MN"~0,
#Solo aplica si nunca cambio de calificación A, de lo contrario aplica lo normal
prevision <- function(x, CALIF,MONTO){
  x %>% 
    mutate(prev=case_when({{CALIF}}=='A' & tipoCred %in% c('Micro','PyMe','Vivienda') & MONEDA=='MN' & Excepcion==1 ~ 0,
                          {{CALIF}}=='A' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==1 & MONEDA == 'MN'~0,
                          {{CALIF}}=='B' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==1 & MONEDA == 'MN'~0.025,
                          {{CALIF}}=='A' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==2 & MONEDA == 'MN'~0.0025,
                          {{CALIF}}=='B' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==2 & MONEDA == 'MN'~0.05,
                          {{CALIF}}=='A' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'MN'~0.0025,
                          {{CALIF}}=='B' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'MN'~0.05,
                          {{CALIF}}=='A' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'MN'~0.03,
                          {{CALIF}}=='B' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'MN'~0.065,
                          {{CALIF}}=='A' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'MN'~0.0025,
                          {{CALIF}}=='B' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'MN'~0.05,
                          {{CALIF}}=='A' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'MN'~0.015,
                          {{CALIF}}=='A' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'MN'~0.03,
                          {{CALIF}}=='B' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & MONEDA == 'MN'~0.065,
                          {{CALIF}}=='C' ~ 0.20,
                          {{CALIF}}=='D' ~ 0.50,
                          {{CALIF}}=='E' ~ 0.80,
                          {{CALIF}}=='F' ~ 1,
                          {{CALIF}}=='A' & tipoCred %in% c('Micro','PyMe') & CONTINGENTE==0 & MONEDA == 'ME'~0.025,
                          {{CALIF}}=='A' & tipoCred %in% c('Micro','PyMe') & CONTINGENTE==1 & MONEDA == 'ME'~0.01,
                          {{CALIF}}=='B' & tipoCred %in% c('Micro','PyMe') & MONEDA == 'ME'~0.05,
                          {{CALIF}}=='A' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.025,
                          {{CALIF}}=='B' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.05,
                          {{CALIF}}=='A' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.07,
                          {{CALIF}}=='B' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.12,
                          {{CALIF}}=='A' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.025,
                          {{CALIF}}=='B' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.05,
                          {{CALIF}}=='A' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.05,
                          {{CALIF}}=='B' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.08,
                          {{CALIF}}=='A' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.07,
                          {{CALIF}}=='B' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.12,
                          {{CALIF}}=='S' ~ 1,
                          {{CALIF}}=='Z' ~ 0,)) %>% 
    mutate(previusNew={{MONTO}}*prev)
}
cuts <- function(quant,levs,values,lowest = 0){
  new <- rep(NA,length(quant))
  levs <- c(lowest,levs)
  n <- length(values)
  for (i in 1:n) {
    new[which(quant>levs[i] & quant<levs[i+1])] <- values[i]
  }
  return(new) 
}
####____REPRODUCING PREVISION FOR PREVIOUS YEAR____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- 2018
myrds <- as.vector(sapply(year, function(x){paste0(mes,x)}))
prevList <- list()
for (i in 1:length(myrds)) {
  print(myrds[i])
  bdcRDS <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
    dplyr::filter(MODULO != 131) %>%
    dplyr::filter(ESTADO != 'CASTIGADA') %>% 
    select(monDate,saldous, previus, ctaCont, PRODUCTIVO, CALIFICACION,
           TIPO_CRED = TIPO_CREDITO, FDES=fdes,tipoCred, MONEDA, caedec3dC,MODULO,
           CTACLIENTE, OPERACION) %>% 
    mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
    mutate(SEC_PROD = ifelse(str_detect(caedec3dC,"Productivo"),2,1)) %>% 
    mutate(MONEDA = ifelse(MONEDA==0,"MN","ME"))
  Prev <- bdcRDS %>% 
    rename(califPot=CALIFICACION) %>% 
    prevision() %>% 
    group_by(monDate) %>% 
    summarise(PrevReal=sum(previus,na.rm = T),PrevCalc=sum(previusNew,na.rm = T))
  prevList[[i]] <- Prev
}
prevFull <- bind_rows(prevList) %>% 
  mutate(dif_rel = (PrevCalc-PrevReal)/PrevReal)

ggplot(prevFull,aes(x=monDate)) +
  geom_line(aes(y=PrevCalc),size=1.5,color="red")+
  geom_line(aes(y=PrevReal),size=1.5,color="blue")+
  theme_light()
####____PREV BASED ON A SINGLE MONTH wCIC INFO____####
mes <- "Jul2023"
cicRDS <- readRDS(paste0('D:/!bso/CIC/rds/cic_',mes,'.rds')) %>% 
  dplyr::filter(is.na(TipoCancelacion)) %>% 
  dplyr::filter(!(CuentaContable.y=='865' & SaldoCastigado==0)) %>% 
  select(CTACLIENTE, OPERACION, starts_with("Garantia"))
#REPRODUCCION MONTO COMPUTABLE CIC
# cicTest <- cicRDS %>% 
#   mutate(saldous = SaldoBruto+SaldoContingente) %>% 
#   mutate(MontoComp = case_when(GarantiaEntidadRed100>0 & GarantiaEntidadRed100<saldous ~ saldous-1*GarantiaEntidadRed100, 
#                                GarantiaEntidadRed100>0 & GarantiaEntidadRed100>=saldous ~ saldous-1*saldous,
#                                GarantiaEntidadRed55>0 & GarantiaEntidadRed55<saldous ~ saldous-0.55*GarantiaEntidadRed55, 
#                                GarantiaEntidadRed55>0 & GarantiaEntidadRed55>=saldous ~ saldous-0.55*saldous,
#                                GarantiaEntidadRed50>0 & GarantiaEntidadRed50<saldous ~ saldous-0.5*GarantiaEntidadRed50, 
#                                GarantiaEntidadRed50>0 & GarantiaEntidadRed50>=saldous ~ saldous-0.5*saldous,
#                                TRUE~saldous))
# x <- cicTest %>% 
#   dplyr::filter(abs(MontoComp-MontoComputable)>0.1)
# sum(cicTest$MontoComp)
# sum(cicTest$MontoComputable)
#AÑADIR GARANTIAS DEL ÚLTIMO MES AL MES ACTUAL
mesNew <- "Ago2023"
bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mesNew,'.rds')) %>% 
  left_join(cicRDS, c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(GarantiaEntidadRed0=0,GarantiaEntidadRed50=0,
                  GarantiaEntidadRed55=0,GarantiaEntidadRed100=0)) %>% 
  mutate(MontoComp = case_when(GarantiaEntidadRed100>0 & GarantiaEntidadRed100<saldous ~ saldous-1*GarantiaEntidadRed100, 
                               GarantiaEntidadRed100>0 & GarantiaEntidadRed100>=saldous ~ saldous-1*saldous,
                               GarantiaEntidadRed55>0 & GarantiaEntidadRed55<saldous ~ saldous-0.55*GarantiaEntidadRed55, 
                               GarantiaEntidadRed55>0 & GarantiaEntidadRed55>=saldous ~ saldous-0.55*saldous,
                               GarantiaEntidadRed50>0 & GarantiaEntidadRed50<saldous ~ saldous-0.5*GarantiaEntidadRed50, 
                               GarantiaEntidadRed50>0 & GarantiaEntidadRed50>=saldous ~ saldous-0.5*saldous,
                               TRUE~saldous))
sum(bdc$MontoComp)

bdcRDS <- bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  select(CTACLIENTE, OPERACION, MontoComp, starts_with("Garantia"), saldous, previus, 
         ctaCont, CALIFICACION, TIPO_CRED = TIPO_CREDITO, FDES=fdes, tipoCred, 
         MONEDA, SECTOR_CARTERA) %>% 
  # select(saldous, previus, ctaCont, PRODUCTIVO, CALIFICACION,
  #        TIPO_CRED = TIPO_CREDITO, FDES=fdes,tipoCred, MONEDA, caedec3dC,MODULO,
  #        CTACLIENTE, OPERACION, CAEDEC_DEST, OBJETO_CRED,DESC_OBJCRED,SECTOR_CARTERA,
  #        divCaedecC,divCaedecD,grupoCaedecC,grupoCaedecD, CIU) %>% 
  mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
  mutate(SEC_PROD = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                                 "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada",
                                                 "8.Otra Prod.No Controlada"),1,2)) %>% 
  mutate(MONEDA = ifelse(MONEDA==0,"MN","ME")) %>% 
  mutate(Excepcion=0)

x <- bdcRDS %>% 
  rename(califPot = CALIFICACION) %>% 
  prevision(CALIF = califPot,MONTO=MontoComp)

y <- x %>% 
  dplyr::filter(abs(previus-previusNew)>0.01) %>% 
  mutate(Excepcion = ifelse(FDES>=as.Date("2021-08-02") & FDES<=as.Date("2022-07-29")
                            & califPot=="A" & previus==0, 1, 0)) %>% 
  select(CTACLIENTE, OPERACION, Excepcion)
table(y$Excepcion) #ir variando la diferencia absoluta hasta estabilizar el conteo de 1's

bdcRDS <- bdcRDS %>% 
  select(-Excepcion) %>% 
  left_join(y, by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(Excepcion=0))

x <- bdcRDS %>% 
  rename(califPot = CALIFICACION) %>% 
  prevision(CALIF = califPot,MONTO=MontoComp)
sum(x$previus)
sum(x$previusNew)
####____CARTERA AGOSTO____####
mylong <- "Ago. 2023"
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Ago23.rds")
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Ago2023.rds')
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"), ~sum(.x))) %>% 
  ungroup()
pt_grouped <- ptFull %>% 
  dplyr::filter(myPago>(as.yearmon(mylong)-1) & myPago<=mylong) %>% 
  group_by(Operacion) %>% 
  mutate(Ult12MesesPagoTardio = n()) %>% 
  ungroup() %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(CantPT12Meses = max(Ult12MesesPagoTardio)) %>% 
  ungroup()
cond_grouped <- cond_clean %>% 
  dplyr::filter(myCond>(as.yearmon(mylong)-1) & myCond<=mylong) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(CantCond12Meses = n()) %>% 
  ungroup()

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mes,'.rds'))
infoClean <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI,esBSO) %>% 
  mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(califPeorSF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
  mutate(califPeorBSO = ifelse(peorCalif==1 & esBSO==1,CALIFICACION,'_')) %>% 
  mutate(across(califPeorSF:califPeorBSO, ~ max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
  ungroup() %>% 
  dplyr::filter(califPeorSF!="_" & SIGLA=="BSO") %>% 
  select(CTACLIENTE, OPERACION, califPeorSF)

bdcRDS <- bdcRDS %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  mutate(NXN = case_when(CantPT12Meses>=12 & CantCond12Meses>=12 ~ '12x12',
                         CantPT12Meses>=11 & CantCond12Meses>=11 ~ '11x11',
                         CantPT12Meses>=10 & CantCond12Meses>=10 ~ '10x10',
                         CantPT12Meses>=9 & CantCond12Meses>=9 ~ '09x09',
                         CantPT12Meses>=8 & CantCond12Meses>=8 ~ '08x08',
                         CantPT12Meses>=7 & CantCond12Meses>=7 ~ '07x07',
                         CantPT12Meses>=6 & CantCond12Meses>=6 ~ '06x06',
                         CantPT12Meses>=5 & CantCond12Meses>=5 ~ '05x05',
                         CantPT12Meses>=4 & CantCond12Meses>=4 ~ '04x04',
                         CantPT12Meses>=3 & CantCond12Meses>=3 ~ '03x03',
                         CantPT12Meses>=2 & CantCond12Meses>=2 ~ '02x02',
                         CantPT12Meses>=1 & CantCond12Meses>=1 ~ '01x01',)) %>% 
  mutate(CalifSapo = case_when(NXN %in% c('10x10','11x11','12x12') ~ 'F',
                               NXN %in% c('08x08','09x09') ~ 'E',
                               NXN %in% c('06x06','07x07') ~ 'D',
                               NXN %in% c('04x04','05x05') ~ 'C',
                               NXN %in% c('02x02','03x03') ~ 'B',
                               NXN %in% c('01x01') ~ 'A', TRUE~'_')) %>% 
  left_join(infoClean, by=c("CTACLIENTE","OPERACION"), suffix = c("_BSO","_SF")) %>% 
  mutate(CalifSapo = ifelse(!is.na(CalifSapo) & CalifSapo>CALIFICACION, 
                               CalifSapo, CALIFICACION)) %>% 
  mutate(califPeorSF = ifelse(!is.na(califPeorSF) & califPeorSF>CALIFICACION, 
                               califPeorSF, CALIFICACION)) %>% 
  prevision(CALIF = CalifSapo, MONTO = MontoComp) %>% 
  rename(previusBC=previusNew) %>% 
  prevision(CALIF = califPeorSF, MONTO=MontoComp) %>% 
  rename(previusSF=previusNew)
####____SIMULACION MONTECARLO____####
calificaciones <- c("A","B","C","D","E","F","S","Z")

arch <- c("Sup95IM4","avg16","BC3_avg4","BC_avg16","SF2_avg4","SF2_avg16")
for (j in 1:length(arch)) {
  tmCum <- fread(paste0("D:/!bso/previsiones/prev_",mesNew,"/tmCum_",arch[j],".csv"),encoding = "UTF-8",fill = T) %>% 
    mutate(across(everything(),~as.numeric(.x)))
  tmCum <- data.matrix(tmCum)
  
  bdcSimList <- list()
  for (i in 1:1) {
    print(i)
    set.seed(i)
    bdcSim <- bdcRDS %>% 
      mutate(aleatorio = runif(n(),0,100)) %>% 
      mutate(califPot = case_when(CALIFICACION=="A"~cuts(aleatorio,tmCum[1,1:8],calificaciones),
                                  CALIFICACION=="B"~cuts(aleatorio,tmCum[2,1:8],calificaciones),
                                  CALIFICACION=="C"~cuts(aleatorio,tmCum[3,1:8],calificaciones),
                                  CALIFICACION=="D"~cuts(aleatorio,tmCum[4,1:8],calificaciones),
                                  CALIFICACION=="E"~cuts(aleatorio,tmCum[5,1:8],calificaciones),
                                  CALIFICACION=="F"~cuts(aleatorio,tmCum[6,1:8],calificaciones),
                                  CALIFICACION=="S"~cuts(aleatorio,tmCum[7,1:8],calificaciones),
                                  CALIFICACION=="Z"~cuts(aleatorio,tmCum[8,1:8],calificaciones),)) %>% 
      group_by(CTACLIENTE) %>% 
      mutate(califPot = max(califPot)) %>% 
      ungroup() %>% 
      prevision(CALIF = califPot, MONTO = MontoComp) %>% 
      mutate(difPrev = previusNew-previus) %>% 
      select(CALIFICACION, califPot, previus, previusNew, difPrev) %>% 
      group_by(CALIFICACION,califPot) %>% 
      summarise(across(everything(),~sum(.x)),
                nops=n()) %>% 
      ungroup() %>% 
      mutate(NSim = i) %>% relocate(NSim)
    
    print(sum(bdcSim$difPrev))
    bdcSimList[[i]] <- bdcSim
  }
  bdcSimFull <- rbindlist(bdcSimList)
  write.csv(bdcSimFull,paste0("D:/!bso/previsiones/prev_",mesNew,"/dpprev_",arch[j],".csv"), row.names = F, quote = F)
}

Prev <- bdcRDS %>% 
  rename(califPot=CALIFICACION) %>% 
  prevision() %>% 
  # dplyr::filter(round(previusNew+0.01)<round(previus)) %>% 
  glimpse()
####____SIMULACION MONTECARLO BC____####
calificaciones <- c("A","B","C","D","E","F","S","Z")

arch <- c("avg4","avg16")
for (j in 1:length(arch)) {
  tmCum <- fread(paste0("D:/!bso/previsiones/prev_",mesNew,"/tmCum_",arch[j],".csv"),encoding = "UTF-8",fill = T) %>% 
    mutate(across(everything(),~as.numeric(.x)))
  tmCum <- data.matrix(tmCum)
  
  bdcSimList <- list()
  for (i in 1:500) {
    print(i)
    set.seed(i)
    bdcSim <- bdcRDS %>% 
      mutate(aleatorio = runif(n(),0,100)) %>% 
      mutate(califPot = case_when(CalifSapo=="A"~cuts(aleatorio,tmCum[1,1:8],calificaciones),
                                  CalifSapo=="B"~cuts(aleatorio,tmCum[2,1:8],calificaciones),
                                  CalifSapo=="C"~cuts(aleatorio,tmCum[3,1:8],calificaciones),
                                  CalifSapo=="D"~cuts(aleatorio,tmCum[4,1:8],calificaciones),
                                  CalifSapo=="E"~cuts(aleatorio,tmCum[5,1:8],calificaciones),
                                  CalifSapo=="F"~cuts(aleatorio,tmCum[6,1:8],calificaciones),
                                  CalifSapo=="S"~cuts(aleatorio,tmCum[7,1:8],calificaciones),
                                  CalifSapo=="Z"~cuts(aleatorio,tmCum[8,1:8],calificaciones),)) %>% 
      group_by(CTACLIENTE) %>% 
      mutate(califPot = max(califPot)) %>% 
      ungroup() %>% 
      prevision(CALIF = califPot, MONTO = MontoComp) %>% 
      mutate(difPrev = previusNew-previus) %>% 
      mutate(difPrevBC = previusNew-previusBC) %>% 
      select(CALIFICACION, califPot, previus, previusBC, previusNew, difPrev, difPrevBC) %>% 
      group_by(CALIFICACION,califPot) %>% 
      summarise(across(everything(),~sum(.x)),
                nops=n()) %>% 
      ungroup() %>% 
      mutate(NSim = i) %>% relocate(NSim)
    
    print(sum(bdcSim$difPrev))
    bdcSimList[[i]] <- bdcSim
  }
  bdcSimFull <- rbindlist(bdcSimList)
  write.csv(bdcSimFull,paste0("D:/!bso/previsiones/prev_",mesNew,"/dpprev_BC",arch[j],".csv"), row.names = F, quote = F)
}

####____SIMULACION MONTECARLO SF____####
calificaciones <- c("A","B","C","D","E","F","S","Z")

arch <- c("avg4","avg16")
for (j in 1:length(arch)) {
  tmCum <- fread(paste0("D:/!bso/previsiones/prev_",mesNew,"/tmCum_",arch[j],".csv"),encoding = "UTF-8",fill = T) %>% 
    mutate(across(everything(),~as.numeric(.x)))
  tmCum <- data.matrix(tmCum)
  
  bdcSimList <- list()
  for (i in 1:500) {
    print(i)
    set.seed(i)
    bdcSim <- bdcRDS %>% 
      mutate(aleatorio = runif(n(),0,100)) %>% 
      mutate(califPot = case_when(califPeorSF=="A"~cuts(aleatorio,tmCum[1,1:8],calificaciones),
                                  califPeorSF=="B"~cuts(aleatorio,tmCum[2,1:8],calificaciones),
                                  califPeorSF=="C"~cuts(aleatorio,tmCum[3,1:8],calificaciones),
                                  califPeorSF=="D"~cuts(aleatorio,tmCum[4,1:8],calificaciones),
                                  califPeorSF=="E"~cuts(aleatorio,tmCum[5,1:8],calificaciones),
                                  califPeorSF=="F"~cuts(aleatorio,tmCum[6,1:8],calificaciones),
                                  califPeorSF=="S"~cuts(aleatorio,tmCum[7,1:8],calificaciones),
                                  califPeorSF=="Z"~cuts(aleatorio,tmCum[8,1:8],calificaciones),)) %>% 
      group_by(CTACLIENTE) %>% 
      mutate(califPot = max(califPot)) %>% 
      ungroup() %>% 
      prevision(CALIF = califPot, MONTO = MontoComp) %>% 
      mutate(difPrev = previusNew-previus) %>% 
      mutate(difPrevSF = previusNew-previusSF) %>% 
      select(CALIFICACION, califPot, previus, previusSF, previusNew, difPrev, difPrevSF) %>% 
      group_by(CALIFICACION,califPot) %>% 
      summarise(across(everything(),~sum(.x)),
                nops=n()) %>% 
      ungroup() %>% 
      mutate(NSim = i) %>% relocate(NSim)
    
    print(sum(bdcSim$difPrev))
    bdcSimList[[i]] <- bdcSim
  }
  bdcSimFull <- rbindlist(bdcSimList)
  write.csv(bdcSimFull,paste0("D:/!bso/previsiones/prev_",mesNew,"/dpprev_SF",arch[j],".csv"), row.names = F, quote = F)
}

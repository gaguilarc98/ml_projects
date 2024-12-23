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
####____PREV BASED ON A SINGLE MONTH____####
mes <- "Mar2023"
bdcRDS <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mes,'.rds')) %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  select(saldous, previus, ctaCont, PRODUCTIVO, CALIFICACION,
         TIPO_CRED = TIPO_CREDITO, FDES=fdes,tipoCred, MONEDA,
         CTACLIENTE, OPERACION, SECTOR_CARTERA) %>% 
  # select(saldous, previus, ctaCont, PRODUCTIVO, CALIFICACION,
  #        TIPO_CRED = TIPO_CREDITO, FDES=fdes,tipoCred, MONEDA, caedec3dC,MODULO,
  #        CTACLIENTE, OPERACION, CAEDEC_DEST, OBJETO_CRED,DESC_OBJCRED,SECTOR_CARTERA,
  #        divCaedecC,divCaedecD,grupoCaedecC,grupoCaedecD, CIU) %>% 
  mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
  mutate(SEC_PROD = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                                 "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada",
                                                 "8.Otra Prod.No Controlada"),1,2)) %>% 
  mutate(MONEDA = ifelse(MONEDA==0,"MN","ME"))

####____SIMULACION MONTECARLO____####
calificaciones <- c("A","B","C","D","E","F","S","Z")

arch <- c("avg12","avg12_sens_ab","avg12_sens_ba","P5P95","P10P90","P25P75")
for (j in 1:length(arch)) {
  tmCum <- fread(paste0("D:/!bso/transMat/prev",mes,"/tm23_",arch[j],".csv"),encoding = "UTF-8",fill = T) %>% 
    mutate(across(everything(),~as.numeric(.x)))
  tmCum <- data.matrix(tmCum)
  
  bdcSimList <- list()
  for (i in 1:200) {
    print(i)
    set.seed(i)
    bdcSim <- bdcRDS %>% 
      mutate(aleatorio = runif(n(),0,100)) %>% 
      mutate(califPot = case_when(CALIFICACION=="A"~cuts(aleatorio,tmCum[1,1:8],calificaciones),
                                  CALIFICACION=="B"~cuts(aleatorio,tmCum[1,9:16],calificaciones),
                                  CALIFICACION=="C"~cuts(aleatorio,tmCum[1,17:24],calificaciones),
                                  CALIFICACION=="D"~cuts(aleatorio,tmCum[1,25:32],calificaciones),
                                  CALIFICACION=="E"~cuts(aleatorio,tmCum[1,33:40],calificaciones),
                                  CALIFICACION=="F"~cuts(aleatorio,tmCum[1,41:48],calificaciones),
                                  CALIFICACION=="S"~cuts(aleatorio,tmCum[1,49:56],calificaciones),
                                  CALIFICACION=="Z"~cuts(aleatorio,tmCum[1,57:64],calificaciones),)) %>% 
      group_by(OPERACION) %>% 
      mutate(califPot = max(califPot)) %>% 
      ungroup() %>% 
      prevision() %>% 
      mutate(difPrev = previusNew-previus) %>% 
      select(CALIFICACION,califPot,previus,previusNew,difPrev) %>% 
      group_by(CALIFICACION,califPot) %>% 
      summarise(across(everything(),~sum(.x)),
                nops=n()) %>% 
      ungroup()
    
    print(sum(bdcSim$previusNew))
    bdcSimList[[i]] <- bdcSim
  }
  bdcSimFull <- rbindlist(bdcSimList)
  write.csv(bdcSimFull,paste0("D:/!bso/transMat/prev",mes,"/dpprev_",arch[j],".csv"),row.names = F)
}

Prev <- bdcRDS %>% 
  rename(califPot=CALIFICACION) %>% 
  prevision() %>% 
  # dplyr::filter(round(previusNew+0.01)<round(previus)) %>% 
  glimpse()

#####____REPRODUCING ASFI____####
baseDepurada <- readRDS('D:/!bso/bases/rds/REPORTE_ASFI_DIC2022_DEP.rds') %>% 
  dplyr::filter(TIPO_REL %in% c("1A","4A","5A","6A","7A")) %>% 
  dplyr::filter(ESTADO %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía")) %>% 
  separate(OPERACION,into=c("CTACLIENTE","OPERACION"),sep="-") %>% 
  mutate(CTACLIENTE = as.numeric(CTACLIENTE)) %>% 
  mutate(OPERACION = as.numeric(OPERACION))
baseASFI <- baseDepurada %>% 
  mutate(CART_NODIF=substr(CTACONT_NODIF,1,3),
         CART_DIF=substr(CTACONT_DIF,1,3)) %>% 
  mutate(CONTINGENTE=ifelse(CART_NODIF %in% c('700','600','519','988','978') |
                              CART_DIF %in% c('700','600','519','988','978'),1,0))

bdcRDSJoin <- bdcRDS %>% 
  left_join(select(baseDepurada,CTACLIENTE,OPERACION,PROD_ASFI=SEC_PROD),by=c("CTACLIENTE","OPERACION"))

Prods <- bdcRDSJoin %>% 
  dplyr::filter(PROD_ASFI==1) 
NoProds <- bdcRDSJoin %>% 
  dplyr::filter(PROD_ASFI==2) 

unique(NoProds$CAEDEC_DEST) %in% unique(Prods$CAEDEC_DEST)
unique(NoProds$OBJETO_CRED) %in% unique(Prods$OBJETO_CRED)
unique(NoProds$DESC_OBJCRED) %in% unique(Prods$DESC_OBJCRED)
unique(NoProds$SECTOR_CARTERA) %in% unique(Prods$SECTOR_CARTERA)
unique(NoProds$divCaedecC) %in% unique(Prods$divCaedecC)
unique(NoProds$divCaedecD) %in% unique(Prods$divCaedecD)
unique(NoProds$grupoCaedecC) %in% unique(Prods$grupoCaedecC)
unique(NoProds$grupoCaedecD) %in% unique(Prods$grupoCaedecD)

Prev <- bdcRDSJoin %>% 
  rename(l=SEC_PROD, SEC_PROD = PROD_ASFI, califPot=CALIFICACION) %>% 
  prevision() %>% 
  dplyr::filter(round(previusNew)<round(previus))

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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
################################################################################
N_Campos <- readxl::read_excel("D:/!bso/bases/excel/CodCaedecDestSector.xls", sheet = "subclase") %>% 
  select(CAEDEC_DEST, SECTOR) %>% 
  mutate(CAEDEC_DEST = as.character(CAEDEC_DEST))
####____READING BASE DE CARTERA____####
mes <- "Dic2022"
bdcRDS <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mes,'.rds')) %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  select(saldous, previus, ctaCont, PRODUCTIVO, CALIFICACION,
         TIPO_CRED = TIPO_CREDITO, FDES=fdes,tipoCred, MONEDA,
         CTACLIENTE, OPERACION, SECTOR_CARTERA, CAEDEC_DEST) %>% 
  mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
  mutate(SEC_PROD = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                                 "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada",
                                                 "8.Otra Prod.No Controlada"),1,2)) %>% 
  mutate(MONEDA = ifelse(MONEDA==0,"MN","ME")) %>% 
  left_join(N_Campos,by="CAEDEC_DEST")
#EL CAMPO SECTOR DE N_Campos NO REPRODUCE LA VARIABLE SECTOR PRODUCTIVO DE LA ASFI
####____BASE ASFI____####
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
####____READING INFOFULL____####
infoFull <- readRDS('D:/!bso/califClientes/infoFull.rds')
infoPrev <- infoFull %>% 
  dplyr::filter(Fecha=="ene. 2023") %>% 
  dplyr::filter(califSF_2>califBSO_2) #Filtrar aquellos ciya calif en sistema es peor
sort(unique(bdcRDS$RUBRO)) #CARTERA CONTINGENTE ES CTA CONTABLE: 610, 620, 640
# infoPrev <- infoPrev %>% 
#   select(-saldous,-previus) %>% 
#   left_join(bdcRDS,by=c("CTACLIENTE","OPERACION")) %>% 
#   select(saldous, previus, califBSO_2, califSF_2, ctaCont, PRODUCTIVO,
#          TIPO_CREDITO, tipoCred, MONEDA, MonedaOrigen, caedec3dC, fdes) %>% 
#   mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
#   mutate(SEC_PROD = ifelse(str_detect("Productivo",caedec3dC),2,1)) %>% 
#   mutate(MONEDA=MonedaOrigen) %>% 
#   rename(TIPO_CRED = TIPO_CREDITO,
#          FDES = fdes,
#          califPot = califSF_2)

infoPrev <- infoPrev %>% 
  select(CTACLIENTE, OPERACION, califBSO_2, califSF_2, MonedaOrigen) %>%
  mutate(Perf=1)
infoPrev <- bdcRDS %>% 
  select(saldous, previus, ctaCont, PRODUCTIVO, CALIFICACION,
         TIPO_CREDITO, tipoCred, MONEDA, caedec3dC, fdes,
         CTACLIENTE, OPERACION) %>% 
  left_join(infoPrev,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
  mutate(SEC_PROD = ifelse(str_detect("Productivo",caedec3dC),2,1)) %>% 
  mutate(MONEDA=MonedaOrigen) %>% 
  rename(TIPO_CRED = TIPO_CREDITO,
         FDES = fdes,
         califPot = califSF_2) %>% 
  mutate(Perf=ifelse(is.na(Perf),0,Perf))

PrevNew <- infoPrev %>% 
  mutate(prev=case_when(califPot=='A' & tipoCred=='Micro' & SEC_PROD==1 & MONEDA == 'MN'~0,
                        califPot=='B' & tipoCred=='Micro' & SEC_PROD==1 & MONEDA == 'MN'~0.025,
                        califPot=='A' & tipoCred=='Micro' & SEC_PROD==2 & MONEDA == 'MN'~0.0025,
                        califPot=='B' & tipoCred=='Micro' & SEC_PROD==2 & MONEDA == 'MN'~0.05,
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
                        califPot=='A' & tipoCred=='Micro' & CONTINGENTE==0 & MONEDA == 'ME'~0.025,
                        califPot=='B' & tipoCred=='Micro' & CONTINGENTE==1 & MONEDA == 'ME'~0.01,
                        califPot=='B' & tipoCred=='Micro' & MONEDA == 'ME'~0.05,
                        califPot=='A' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.025,
                        califPot=='B' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.05,
                        califPot=='A' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.07,
                        califPot=='B' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.12,
                        califPot=='A' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.025,
                        califPot=='B' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.05,
                        califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.05,
                        califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.08,
                        califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.07,
                        califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.12,))

Prev <- PrevNew %>%   
  mutate(previusOld=ifelse(Perf==1,previus,0)) %>% 
  mutate(previusNew=saldous*prev)

Prev %>% 
  summarise(prev=sum(previus,na.rm=T),
            prevOld=sum(previusOld,na.rm=T),
            prevNew=sum(previusNew,na.rm = T))
Prev %>% 
  group_by(CALIFICACION) %>% 
  summarise(prev=sum(previusNew,na.rm = T))

write.xlsx(Prev,'D:/!bso/Previsiones/PrevCalc_Dic22.xlsx')
################################################################################
####____CRUCE CON PAGOS TARDIOS____####
infoPrev <- infoFull %>% 
  dplyr::filter(Fecha=="ene. 2023") %>% 
  dplyr::filter(califSF_2>califBSO_2) %>%  #Filtrar aquellos ciya calif en sistema es peor 
  select(califSF_2, califBSO_2, CTACLIENTE, OPERACION, saldo)

lastCierre <- fread("D:/!bso/mph/Oreports/lastCierrreUR_Ene2023.csv",
                    encoding = "UTF-8",sep=",",fill=T) %>% 
  select(Operacion, CTACLIENTE, Instancias_AR, Instancias_UR, Ultimo_mes) %>% 
  rename(OPERACION = Operacion)

infoPerf <- Prev %>% 
  left_join(lastCierre,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(Instancias_UR =ifelse(is.na(Instancias_UR),0,Instancias_UR)) %>% 
  mutate(Instancias_AR =ifelse(is.na(Instancias_AR),0,Instancias_AR)) %>% 
  mutate(Ultimo_mes =ifelse(is.na(Ultimo_mes),0,Ultimo_mes)) %>% 
  # group_by(califSF_2,Instancias_UR) %>%
  # summarise(saldo=sum(saldo,na.rm = T)) %>%
  # ungroup() %>%
  # pivot_wider(names_from = Instancias_UR,values_from = saldo) %>%
  glimpse()
  
write.xlsx(infoPerf,"D:/!bso/Previsiones/peorTardiosEne23_v2.xlsx")

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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____CREATING RDS FOR BASE ASFI____####
baseASFI <- read.xlsx('D:/!bso/bases/excel/REPORTE_ASFI_DIC2022.xlsx')
write_rds(baseASFI,'D:/!bso/bases/rds/REPORTE_ASFI_DIC2022.rds')
####____READING BASE ASFI____####
baseASFI <- readRDS('D:/!bso/bases/rds/REPORTE_ASFI_DIC2022.rds')

baseDepurada <- baseASFI %>% 
  mutate(tipoCred = case_when(substr(`29_TIPO_CRED`,1,1)=='M'~'Micro',
                              substr(`29_TIPO_CRED`,1,1)=='H'~'Vivienda',
                              substr(`29_TIPO_CRED`,1,1)=='N'~'Consumo',
                              substr(`29_TIPO_CRED`,1,1)=='P'~'PyMe',)) %>% 
  mutate(FDES=as.Date(`28_FECHA_OP`,"%d-%m-%Y")) %>% 
  rename(CI=`08_IDENTIFICACION`, MONEDA=`31_MONEDA`,
         TIPO_REL=`07_COD_TIPO_RELACION`,
         CTACONT_NODIF=`76_CTA_CONTABLE_NO_DIFERIDA`,
         CTACONT_DIF=`78_CTA_CONTABLE_DIFERIDA`,
         SEC_PROD=`100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`,
         TIPO_CRED=`29_TIPO_CRED`, califASFI=`92_CALIF_ENT`,
         OPERACION=`27_NRO_OPERACION`,
         ESTADO=`88_ESTADO`,
         NRO_LINEA=`25_NRO_LINEA`,
         COD_TIPO_LINEA= `22_COD_TIPO_LINEA`) %>% 
  mutate(SALDOBS=as.numeric(`80_SALDO_TOTAL_BS`)) %>% 
  select(OPERACION,CI,califASFI,FDES,MONEDA,SEC_PROD,tipoCred,TIPO_CRED, TIPO_REL,
         CTACONT_DIF,ESTADO,CTACONT_NODIF,NRO_LINEA,COD_TIPO_LINEA,SALDOBS)
write_rds(baseDepurada,'D:/!bso/bases/rds/REPORTE_ASFI_DIC2022_DEP.rds')

baseDepurada <- readRDS('D:/!bso/bases/rds/REPORTE_ASFI_DIC2022_DEP.rds') %>% 
  dplyr::filter(TIPO_REL %in% c("1A","4A","5A","6A","7A")) %>% 
  dplyr::filter(ESTADO %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía"))

bdcRDS <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds') %>% 
  rename(Operacion=OPERACION) %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  select(Operacion,CI,CTACLIENTE,cosechaY)  
####____READING INFOPERF____####
infoPerf <- fread('D:/!bso/bases/csv/infoPerf_SF_dic2022.csv',
                  encoding = 'UTF-8',sep=',',fill = T)

infoPerf <- fread('D:/!bso/bases/csv/infoPerf_SF_dic2022.csv',
                  encoding = 'UTF-8',sep=',',fill = T) %>% 
  dplyr::filter(fbase==202212)
  
infoPerf_ASFI <- infoPerf %>% 
  inner_join(baseDepurada,by="CI") %>% 
  mutate(CART_NODIF=substr(CTACONT_NODIF,1,3),
         CART_DIF=substr(CTACONT_DIF,1,3)) %>% 
  mutate(CONTINGENTE=ifelse(CART_NODIF %in% c('700','600','519','988','978') |
                              CART_DIF %in% c('700','600','519','988','978'),1,0))

# infoPerf_ASFI <- infoPerf %>% 
#   inner_join(bdcRDS,by="CI") 
  
infoPerf1XCI <- infoPerf %>%
  group_by(CI) %>%
  arrange(desc(fbase)) %>%
  dplyr::filter(row_number()==1) %>%
  ungroup()

infoPerf_ASFI <- infoPerf1XCI %>% 
  left_join(baseDepurada,by="CI") 
  

infoPerf_ASFI <- infoPerf1XCI %>% 
  inner_join(baseDepurada,by="CI") %>% 
  mutate(CART_NODIF=substr(CTACONT_NODIF,1,3),
         CART_DIF=substr(CTACONT_DIF,1,3)) %>% 
  mutate(CONTINGENTE=ifelse(CART_NODIF %in% c('700','600','519','988','978') |
                              CART_DIF %in% c('700','600','519','988','978'),1,0))

table(infoPerf_ASFI$TIPO_CRED,useNA = 'ifany')
table(infoPerf_ASFI$califPot,infoPerf_ASFI$califASFI,useNA = 'ifany')

infoPerf_ASFI <- infoPerf_ASFI %>% 
  mutate(califPot=ifelse(califASFI>califPot & !is.na(califASFI),califASFI,califPot)) %>% 
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
infoPerf_ASFIPrev <- infoPerf_ASFI %>%   
  mutate(previusNew=saldous*prev,
         previusNewBs=SALDOBS*prev)

write_rds(infoPerf_ASFIPrev,'D:/!bso/Previsiones/infoPerf_ASFIPrev.rds')
write.xlsx(infoPerf_ASFIPrev,'D:/!bso/Previsiones/infoPerf_ASFIPrev.xlsx')

infoPerf_ASFIPrev %>% 
  summarise(prevOld=sum(previus,na.rm=T),prevNew=sum(previusNew,na.rm = T),
            prevNewBs=sum(previusNewBs,na.rm=T))

####____WRITING PREVISION____####
dummyPrev <- infoPerf_ASFIPrev %>% 
  group_by(CI) %>% 
  summarise(califPot=max(califPot),saldous=sum(saldous),previusNew=sum(previusNew)) %>% 
  select(CI,califPot,previusNew)

write_rds(dummyPrev,'D:/!bso/bases/rds/dummyPrev_Dic22.rds')
####____EMPECEMOS DE NUEVO____####
baseDepurada <- readRDS('D:/!bso/bases/rds/REPORTE_ASFI_DIC2022_DEP.rds') %>% 
  # dplyr::filter(TIPO_REL %in% c("1A","4A","5A","6A","7A")) %>% 
  # dplyr::filter(ESTADO %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía")) %>% 
  glimpse()

infoPerf <- fread('D:/!bso/bases/csv/infoPerf_SF_dic2022.csv',
                  encoding = 'UTF-8',sep=',',fill = T) %>% 
  dplyr::filter(fbase==202212)

infoPerf_ASFI <- infoPerf %>% 
  inner_join(baseDepurada,by="CI") %>% 
  mutate(CART_NODIF=substr(CTACONT_NODIF,1,3),
         CART_DIF=substr(CTACONT_DIF,1,3)) %>% 
  mutate(CONTINGENTE=ifelse(CART_NODIF %in% c('700','600','519','988','978') |
                              CART_DIF %in% c('700','600','519','988','978'),1,0))

infoPerf_ASFI <- infoPerf_ASFI %>% 
  mutate(califPot=ifelse(califASFI>califPot & !is.na(califASFI),califASFI,califPot)) %>% 
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
infoPerf_ASFIPrev <- infoPerf_ASFI %>%   
  mutate(previusNew=saldous*prev,
         previusNewBs=SALDOBS*prev)

infoPerf_ASFIPrev %>% 
  summarise(prevOld=sum(previus,na.rm=T),prevNew=sum(previusNew,na.rm = T),
            prevNewBs=sum(previusNewBs,na.rm=T))

infoPerf_ASFIPrev %>% 
  dplyr::filter(!(TIPO_REL %in% c("1A","4A","5A","6A","7A"))) %>% 
  dplyr::filter((ESTADO %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía"))) %>% 
  summarise(nRows = n(),prevNoDeudor = sum(previusNew,na.rm = T))

################################################################################
####___VERIFICACION DICIEMBRE____####
bdcRDS <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds') %>% 
  rename(Operacion=OPERACION) %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA')
infoPrev <- fread('D:/!bso/bases/csv/infoPerf_SF_dic2022.csv',
                  encoding = 'UTF-8',sep=',',fill = T) %>% 
  dplyr::filter(fbase==202212) %>% 
  dplyr::filter(CALIFICACION < califPot)

infoPrev <- infoPrev %>% 
  select(CI, califPot) %>%
  mutate(Perf=1)
infoPrev <- bdcRDS %>% 
  select(saldous, previus, ctaCont, PRODUCTIVO, CALIFICACION,
         TIPO_CREDITO, tipoCred, MONEDA, caedec3dC, fdes,
         CTACLIENTE,CI) %>% 
  left_join(infoPrev,by=c("CI")) %>% 
  mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
  mutate(SEC_PROD = ifelse(str_detect("Productivo",caedec3dC),2,1)) %>% 
  mutate(MONEDA=ifelse(MONEDA==0,"MN","ME")) %>% 
  rename(TIPO_CRED = TIPO_CREDITO,
         FDES = fdes) %>% 
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

sum(infoPrev$Perf,na.rm = T) #Para observar número de joins en Prev

y <- baseDepurada %>% 
  separate_wider_delim(OPERACION,names=c("CTACLIENTE","OPERACION"),delim='-') %>% 
  mutate(CTACLIENTE =as.numeric(CTACLIENTE)) %>% 
  mutate(OPERACION = as.numeric(OPERACION))

z <- y %>% 
  select(CTACLIENTE,OPERACION,TIPO_CRED_ASFI=TIPO_CRED) %>%
  inner_join(select(x,CTACLIENTE,OPERACION,TIPO_CRED_BANTOTAL=TIPO_CREDITO),by=c("OPERACION","CTACLIENTE"))
write.xlsx(z,"D:/!bso/PYME_P9_ASFI_BANTOTAL_DIC23.xlsx")

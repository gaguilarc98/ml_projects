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
####____CON INFOPERF RAW____####
lastCierre <- fread("D:/!bso/mph/Oreports/lastCierrreUR_Dic2022.csv",
                    encoding="UTF-8",fill=T,sep=",") %>% 
  select(CI, cosechaY, Instancias_UR, Instancias_AR, Saldo2M, Saldo3M, Saldo4M, Saldo5M,
         Saldo6M,Saldo_USD, Ultimo_mes) %>% 
  rename(saldoPT=Saldo_USD)

infoPerf <- fread('D:/!bso/bases/csv/infoPerf_SF_dic2022.csv',
                  encoding = 'UTF-8',sep=',',fill = T) 
infoPerf %>% 
  group_by(fbase) %>% 
  summarise(saldo=sum(saldous,na.rm = T))

infoPerf <- infoPerf %>% 
  dplyr::filter(fbase==202212) 

infoPerf2 <- infoPerf %>% 
  group_by(CI) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  arrange(CI,pos)

tardioPerf <- infoPerf %>% 
  left_join(lastCierre,by=c("CI")) %>% 
  replace_na(list(Instancias_AR=0,Instancias_UR=0)) %>% 
  group_by(CI) %>% 
  arrange(desc(Instancias_UR)) %>% 
  dplyr::filter(row_number()==1)

write.xlsx(tardioPerf,'D:/!bso/Previsiones/prevtardios.xlsx')
####____PREVISION____####
baseDepurada <- readRDS('D:/!bso/bases/rds/REPORTE_ASFI_DIC2022_DEP.rds') %>% 
  dplyr::filter(TIPO_REL %in% c("1A","4A","5A","6A","7A")) %>% 
  dplyr::filter(ESTADO %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía")) %>% 
  glimpse()

infoPerf_ASFI <- infoPerf %>% 
  inner_join(baseDepurada,by="CI") %>% 
  mutate(CART_NODIF=substr(CTACONT_NODIF,1,3),
         CART_DIF=substr(CTACONT_DIF,1,3)) %>% 
  mutate(CONTINGENTE=ifelse(CART_NODIF %in% c('700','600','519','988','978') |
                              CART_DIF %in% c('700','600','519','988','978'),1,0))

infoPerf1XCI <- infoPerf %>%
  group_by(CI) %>%
  arrange(desc(fbase)) %>%
  dplyr::filter(row_number()==1) %>%
  ungroup() %>% 
  inner_join(baseDepurada,by="CI")
  
  
infoPerf_ASFI <- infoPerf1XCI %>% 
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

write_rds(infoPerf_ASFIPrev,'D:/!bso/Previsiones/infoPerf_ASFIPrev.rds')
write.xlsx(infoPerf_ASFIPrev,'D:/!bso/Previsiones/infoPerf_ASFIPrev.xlsx')

infoPerf_ASFIPrev %>% 
  summarise(prevOld=sum(previus,na.rm=T),prevNew=sum(previusNew,na.rm = T),
            prevNewBs=sum(previusNewBs,na.rm=T))

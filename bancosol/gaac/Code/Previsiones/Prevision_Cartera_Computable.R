####____CARGA DE PAQUETES____####
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
library(tseries)
library(scales)
library(openxlsx)
library(janitor)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####___FUNCION DE CALCULO DE PREVISION____####
prevision <- function(x,califPot,MONEDA,tipoCred,TIPO_CRED,SEC_PROD,CONTINGENTE,FDES,Excepcion){
  x %>% 
    mutate(prev=case_when({{Excepcion}}==1 ~ 0,
                          {{califPot}}=='A' & {{tipoCred}} %in% c('Micro','PyMe') & {{SEC_PROD}}==1 & {{MONEDA}} == 'MN'~0,
                          {{califPot}}=='B' & {{tipoCred}} %in% c('Micro','PyMe') & {{SEC_PROD}}==1 & {{MONEDA}} == 'MN'~0.025,
                          {{califPot}}=='A' & {{tipoCred}} %in% c('Micro','PyMe') & {{SEC_PROD}}==2 & {{MONEDA}} == 'MN'~0.0025,
                          {{califPot}}=='B' & {{tipoCred}} %in% c('Micro','PyMe') & {{SEC_PROD}}==2 & {{MONEDA}} == 'MN'~0.05,
                          {{califPot}}=='A' & {{TIPO_CRED}} %in% c('H0','H3','H4') & {{MONEDA}} == 'MN'~0.0025,
                          {{califPot}}=='B' & {{TIPO_CRED}} %in% c('H0','H3','H4') & {{MONEDA}} == 'MN'~0.05,
                          {{califPot}}=='A' & {{TIPO_CRED}} %in% c('H1','H2') & {{MONEDA}} == 'MN'~0.03,
                          {{califPot}}=='B' & {{TIPO_CRED}} %in% c('H1','H2') & {{MONEDA}} == 'MN'~0.065,
                          {{califPot}}=='A' & {{tipoCred}}=='Consumo' & {{FDES}} < as.Date("2009-12-17") & {{MONEDA}} == 'MN'~0.0025,
                          {{califPot}}=='B' & {{tipoCred}}=='Consumo' & {{FDES}} < as.Date("2009-12-17") & {{MONEDA}} == 'MN'~0.05,
                          {{califPot}}=='A' & {{tipoCred}}=='Consumo' & {{FDES}} >= as.Date("2009-12-17") & {{FDES}} <= as.Date("2010-12-16") & {{MONEDA}} == 'MN'~0.015,
                          {{califPot}}=='A' & {{tipoCred}}=='Consumo' & {{FDES}} >= as.Date("2010-12-17") & {{MONEDA}} == 'MN'~0.03,
                          {{califPot}}=='B' & {{tipoCred}}=='Consumo' & {{FDES}} >= as.Date("2009-12-17") & {{MONEDA}} == 'MN'~0.065,
                          {{califPot}}=='C' ~ 0.20,
                          {{califPot}}=='D' ~ 0.50,
                          {{califPot}}=='E' ~ 0.80,
                          {{califPot}}=='F' ~ 1,
                          {{califPot}}=='A' & {{tipoCred}} %in% c('Micro','PyMe') & {{CONTINGENTE}}==0 & {{MONEDA}} == 'ME'~0.025,
                          {{califPot}}=='A' & {{tipoCred}} %in% c('Micro','PyMe') & {{CONTINGENTE}}==1 & {{MONEDA}} == 'ME'~0.01,
                          {{califPot}}=='B' & {{tipoCred}} %in% c('Micro','PyMe') & {{MONEDA}} == 'ME'~0.05,
                          {{califPot}}=='A' & {{TIPO_CRED}} %in% c('H0','H3','H4') & {{MONEDA}} == 'ME'~0.025,
                          {{califPot}}=='B' & {{TIPO_CRED}} %in% c('H0','H3','H4') & {{MONEDA}} == 'ME'~0.05,
                          {{califPot}}=='A' & {{TIPO_CRED}} %in% c('H1','H2') & {{MONEDA}} == 'ME'~0.07,
                          {{califPot}}=='B' & {{TIPO_CRED}} %in% c('H1','H2') & {{MONEDA}} == 'ME'~0.12,
                          {{califPot}}=='A' & {{tipoCred}}=='Consumo' & {{FDES}} < as.Date("2009-12-17") & {{MONEDA}} == 'ME'~0.025,
                          {{califPot}}=='B' & {{tipoCred}}=='Consumo' & {{FDES}} < as.Date("2009-12-17") & {{MONEDA}} == 'ME'~0.05,
                          {{califPot}}=='A' & {{tipoCred}}=='Consumo' & {{FDES}} >= as.Date("2009-12-17") & {{FDES}} <= as.Date("2010-12-16") & {{MONEDA}} == 'ME'~0.05,
                          {{califPot}}=='B' & {{tipoCred}}=='Consumo' & {{FDES}} >= as.Date("2009-12-17") & {{FDES}} <= as.Date("2010-12-16") & {{MONEDA}} == 'ME'~0.08,
                          {{califPot}}=='A' & {{tipoCred}}=='Consumo' & {{FDES}} >= as.Date("2010-12-17") & {{MONEDA}} == 'ME'~0.07,
                          {{califPot}}=='B' & {{tipoCred}}=='Consumo' & {{FDES}} >= as.Date("2010-12-17") & {{MONEDA}} == 'ME'~0.12,
                          {{califPot}}=='S' ~ 1,
                          {{califPot}}=='Z' ~ 0,))
}
####____MAXIMA CALIF____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022,2023)
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
myrds <- myrds[-c(1:which(myrds=="Jul2021"),which(myrds=="Abr2023"):length(myrds))]
i <- 2
for (i in 1:(length(myrds))) {
  tryCatch({
    print(myrds[i])
    if(i<=1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        select(monDate,CTACLIENTE,OPERACION,fdes,ctaCont, OPERACION_ORI_REF,
               TIPO_CREDITO,tipoCred, CALIFICACION) %>% 
        mutate(Check = if_else(fdes>=as.Date("2021-08-02") & fdes<=as.Date("2022-07-29") &
                                 tipoCred %in% c('Micro','PyMe','Vivienda') & 
                                 ctaCont=='131' & OPERACION_ORI_REF==0,1,0))
        
      dfTotal <- df1 %>% 
        mutate(peorCalif = CALIFICACION) %>% 
        mutate(fdesLast = fdes) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0))
    }else{
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        select(monDate,CTACLIENTE,OPERACION,fdes,ctaCont, OPERACION_ORI_REF,
               TIPO_CREDITO,tipoCred, CALIFICACION) %>% 
        mutate(Check = if_else(fdes>=as.Date("2021-08-02") & fdes<=as.Date("2022-07-29") &
                                 tipoCred %in% c('Micro','PyMe','Vivienda') & 
                                 ctaCont=='131' & OPERACION_ORI_REF==0,1,0))
      dfNew <- df1 %>% 
        anti_join(dfTotal,by=c("CTACLIENTE","OPERACION")) %>% 
        mutate(peorCalif = CALIFICACION) %>% 
        mutate(fdesLast = fdes) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0))
      dfUpdate <- df1 %>% 
        select(-Check,-TIPO_CREDITO,-tipoCred)
      dfOld <- dfTotal %>% 
        inner_join(dfUpdate,by=c("CTACLIENTE","OPERACION")) %>% 
        group_by(CTACLIENTE,OPERACION) %>% 
        mutate(peorCalif = max(CALIFICACION.x,CALIFICACION.y,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(cambio = case_when(fdesLast==fdes.y~0,
                                  fdesLast!=fdes.y & ctaCont.y %in% c('135','136','137')~1,
                                  fdesLast!=fdes.y & !(ctaCont.y %in% c('135','136','137'))~2, 
                                  TRUE~0)) %>% #1 means Reprog 2 means Refin, 0 is no change in fdes
        mutate(fueReprog = if_else(fueReprog==0 & cambio==1,1,fueReprog)) %>% 
        mutate(OPERACION_ORI_REF = if_else(cambio==2 & OPERACION_ORI_REF.x==0,OPERACION_ORI_REF.y,OPERACION_ORI_REF.x)) %>% 
        mutate(fdesLast = fdes.y) %>% 
        select(monDate = monDate.y,CTACLIENTE,OPERACION,CALIFICACION = CALIFICACION.y,
               OPERACION_ORI_REF,fdes = fdes.x,ctaCont = ctaCont.x,
               peorCalif,fueReprog,fdesLast, Check, TIPO_CREDITO, tipoCred)
        
      dfCancel <- dfTotal %>% 
        anti_join(df1,by=c("CTACLIENTE","OPERACION"))
      
      dfTotal <- dfCancel %>% 
        bind_rows(dfOld) %>% 
        bind_rows(dfNew)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

saveRDS(dfTotal, "D:/!bso/Previsiones/PeorCalif_DispTrans_Ago2021Mar2023.rds")

####____APLICANDO EXCEPCIONES____####
cActividadEconomica <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT004") %>% 
  select(CodActividadEconomica = caecn, GrupoActEcon = cgaec) %>% 
  glimpse()
dfTotal <- readRDS("D:/!bso/Previsiones/PeorCalif_DispTrans_Ago2021Mar2023.rds")
Except <- dfTotal %>% 
  select(CTACLIENTE, OPERACION, Check, fdes, OPERACION_ORI_REF, ctaCont,fueReprog,peorCalif) %>% 
  mutate(Check = if_else(Check==1 & peorCalif>'A',2,Check))

cods <- c(55101,55102,55103,55201,60100,60212,60222,61200,62101,60221,71110,71120,
          63041,63042, 92320,92330,72200,73101,73102,73200,92110,92141,31600,31700,
          51508,52592,34400,34500,50103)
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Mar2023.rds")
bdcPrev <- bdc %>% 
  dplyr::filter(!MODULO %in% c(131)) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  select(saldous, previus, ctaCont, CALIFICACION,
         TIPO_CRED = TIPO_CREDITO, FDES=fdes,tipoCred, MONEDA,
         CTACLIENTE, OPERACION, SECTOR_CARTERA, CAEDEC_DEST) %>% 
  mutate(CAEDEC_DEST = as.numeric(CAEDEC_DEST)) %>% 
  left_join(cActividadEconomica, by=c("CAEDEC_DEST"="CodActividadEconomica")) %>% 
  mutate(PRODUC = case_when(!(tipoCred %in% c('Micro','PyMe')) ~ 2,
                            GrupoActEcon %in% c('A','B','C','D','E','F','G') ~ 1,
                            CAEDEC_DEST %in% cods ~ 1,
                            TRUE~2)) %>% 
  mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para filtrar cartera contingente
  mutate(SEC_PROD = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                                 "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada",
                                                 "8.Otra Prod.No Controlada"),1,2)) %>% 
  mutate(MONEDA = ifelse(MONEDA==0,"MN","ME")) %>% 
  left_join(Except,by=c("CTACLIENTE","OPERACION")) %>% 
  prevision(califPot = CALIFICACION,MONEDA,tipoCred,TIPO_CRED,PRODUC,CONTINGENTE,FDES,Check) %>% 
  mutate(previusNew = saldous*prev)
  
x <- bdcPrev %>% 
  dplyr::filter(abs(previus-previusNew)>1)

####____CARTERA COMPUTABLE CIC____####


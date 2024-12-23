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

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Feb2023.rds")
cic <- readRDS("D:/!bso/CIC/rds/cic_Feb2023.rds")

bdcProd <- bdc %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  select(monDate, CTACLIENTE, OPERACION, ctaCont, SECTOR_CARTERA, MONEDA,
         MODULO, TIPO_OPER, CAEDEC_DEST, DESC_OBJCRED, FDESEMBOLSO, tipoCred,
         grupoCaedecD)
cicProd <- cic %>% 
  select(OPERACION, CTACLIENTE, CredProdNoProd, CodActEconDest, ActEconDest,
         GrupoActEconDest, ObjetoCredito)

bdcic <- bdcProd %>% 
  left_join(cicProd, by=c("CTACLIENTE","OPERACION"))

table(bdcic$SECTOR_CARTERA, bdcic$CredProdNoProd)

####____REPRODUCING SectorCartera____####
codProd <- read_excel("D:/!bso/bases/excel/CodSecProd.xlsx",sheet='RPT004') %>% 
  rename(CAEDEC = caecn, GrupoActEcon = cgaec) %>% 
  glimpse()
#AL PARECER NO SE ADMITEN ME EN SECTOR TURISMO
#En DIC hay en G. Construccion lo que hace pensar que solo se acepta de A-G

bdcRep <- bdcic %>% 
  mutate(CAEDEC_DEST = as.integer(CAEDEC_DEST)) %>% 
  left_join(codProd,by=c("CAEDEC_DEST"="CAEDEC")) %>% 
  mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD')) %>% 
  mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') 
                             & grupoCaedecD %in% c('A','B','C','D','E','F','G'), 'S', 'N')) %>% #PARA PROD GRUPOS A-G
  mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'TURISMO' 
                             & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23")))
                             & str_detect(DESC_OBJCRED, "INVERSION"), 'S', PROD_NORMA)) %>% #SECTOR TURISMO
  mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'PRODUCCION INTELECTUAL' 
                             & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>% #PRODUCCION INTELECTUAL
  mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'H & E' 
                             & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>% 
  mutate(PROD_BASE = ifelse(SECTOR_CARTERA %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada',
                                                  '3.C2.Sector Turismo','4.C3.Prod Intelectual',
                                                  '5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                                  '8.Otra Prod.No Controlada'),'S','N'))

table(bdcRep$CredProdNoProd, bdcRep$PROD_NORMA)
table(bdcRep$CredProdNoProd, bdcRep$PROD_BASE)

v <- bdcRep %>% 
  dplyr::filter(PROD_BASE!=CredProdNoProd)
w <- bdcRep %>% 
  dplyr::filter(PROD_NORMA!=CredProdNoProd)
x <- bdcRep %>% 
  dplyr::filter(PROD_NORMA!=PROD_BASE)
y <- bdcRep %>% 
  dplyr::filter(SECTOR_CARTERA=="3.C2.Sector Turismo")


table(bdcRep$Sector, bdcRep$MONEDA)
table(bdcRep$Sector, bdcRep$PROD_BASE)
table(bdcRep$CredProdNoProd, bdcRep$PROD_NORMA)


####____LOOP FOR CRED PROD NO PROD____####
codProd <- read_excel("D:/!bso/bases/excel/CodSecProd.xlsx",sheet='RPT004') %>% 
  rename(CAEDEC = caecn, GrupoActEcon = cgaec) %>% 
  glimpse()

month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022:2023)

myrds <- as.vector(sapply(year,function(x){paste0(month,x)}))
myrds <- myrds[-c(which(myrds=="Ago2023"):length(myrds))]

prodList <- list()
i <- 19
for (i in 1:length(myrds)) {
  print(myrds[i])
 
  bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",myrds[i],".rds"))
  cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",myrds[i],".rds")) %>% 
    # mutate(CredProdNoProd = ifelse(CredProdNoProd=="1",'S','N')) %>% 
    mutate(CredProdNoProd = case_when(CredProdNoProd %in% c("1","S")~'S',
                                      CredProdNoProd %in% c("2","N")~'N',
                                      CredProdNoProd =='0'~'S')) %>% 
    ungroup()
  
  bdcProd <- bdc %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
    select(monDate, CTACLIENTE, OPERACION, ctaCont, SECTOR_CARTERA, MONEDA,
           saldous, MODULO, TIPO_OPER, CAEDEC_DEST, DESC_OBJCRED, FDESEMBOLSO, tipoCred,
           grupoCaedecD,)
  cicProd <- cic %>% 
    select(OPERACION, CTACLIENTE, CredProdNoProd, CodActEconDest, ActEconDest,
           GrupoActEconDest, ObjetoCredito, SaldoBruto)
  
  bdcic <- bdcProd %>% 
    left_join(cicProd, by=c("CTACLIENTE","OPERACION"))
  
  bdcRep <- bdcic %>% 
    mutate(CAEDEC_DEST = as.integer(CAEDEC_DEST)) %>% 
    left_join(codProd,by=c("CAEDEC_DEST"="CAEDEC")) %>% 
    mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD')) %>% 
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') 
                               & grupoCaedecD %in% c('A','B','C','D','E','F','G'), 'S', 'N')) %>% #PARA PROD GRUPOS A-G
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'TURISMO' 
                               & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23")))
                               & str_detect(DESC_OBJCRED, "INVERSION"), 'S', PROD_NORMA)) %>% #SECTOR TURISMO
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'PRODUCCION INTELECTUAL' 
                               & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>% #PRODUCCION INTELECTUAL
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'H & E' 
                               & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>% 
    mutate(PROD_BASE = ifelse(SECTOR_CARTERA %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada',
                                                    '3.C2.Sector Turismo','4.C3.Prod Intelectual',
                                                    '5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                                    '8.Otra Prod.No Controlada'),'S','N'))
  wtf <- bdcRep %>% 
    dplyr::filter(PROD_NORMA!=CredProdNoProd) %>% 
    mutate(monDate=as.Date(monDate,frac=1)) %>% 
    rename(FechaCorte=monDate)

  prodList[[i]] <- wtf
}


names(prodList) <- myrds
write_xlsx(prodList,"D:/!bso/girCartera/validacion/SectorProdNoProd_Ene2022Jul2023.xlsx")

####____FIXING VALUES OF CredProdNoProd____####
#According to the norm these have to be either 1 (Productivo) or 2(No Productivo)
codProd <- read_excel("D:/!bso/bases/excel/CodSecProd.xlsx",sheet='RPT004') %>% 
  rename(CAEDEC = caecn, GrupoActEcon = cgaec) %>% 
  select(CAEDEC, Sector) %>% 
  glimpse()

month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018:2023)
myrds <- as.vector(sapply(year,function(x){paste0(month,x)}))
myrds <- myrds[-c(which(myrds=="Ago2023"):length(myrds))]

i <- 60
for (i in 1:length(myrds)) {
  print(myrds[i])
  
  bdc2 <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",myrds[i],".rds")) %>% 
    mutate(CAEDEC_DEST=as.integer(CAEDEC_DEST)) %>%
    # select(CTACLIENTE, OPERACION, AGENCIA, Sucursal) %>%
    left_join(codProd,by=c("CAEDEC_DEST"="CAEDEC")) %>%
    mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD')) %>%
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe')
                               & grupoCaedecD %in% c('A','B','C','D','E','F','G'), 'S', 'N')) %>% #PARA PROD GRUPOS A-G
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'TURISMO'
                               & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23")))
                               & str_detect(DESC_OBJCRED, "INVERSION"), 'S', PROD_NORMA)) %>% #SECTOR TURISMO
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'PRODUCCION INTELECTUAL'
                               & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>% #PRODUCCION INTELECTUAL
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'H & E'
                               & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>%
    mutate(PROD_BASE = ifelse(SECTOR_CARTERA %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada',
                                                    '3.C2.Sector Turismo','4.C3.Prod Intelectual',
                                                    '5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                                    '8.Otra Prod.No Controlada'),'S','N')) %>%
    select(CTACLIENTE, OPERACION, FDESEMBOLSO, MONEDA, tipoCred, grupoCaedecD, Sector,
           DESC_OBJCRED, PROD_NORMA, PROD_BASE, SECTOR_CARTERA, ctaCont) %>% 
    ungroup() 
  
  cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",myrds[i],".rds")) %>% 
    left_join(bdc, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(codProd,by=c("CodActEconDest"="CAEDEC")) %>% 
    mutate(ProdRNSF = ifelse(TipoCredito %in% c('Micro','Pyme') 
                               & GrupoActEconDest %in% c('A','B','C','D','E','F','G'), 'S', 'N')) %>% #PARA PROD GRUPOS A-G
    mutate(ProdRNSF = ifelse(TipoCredito %in% c('Micro','Pyme') & Sector == 'TURISMO' 
                               & (Moneda=="MN" | (Moneda=="ME" & FechaInicio<=as.Date("2013-12-23")))
                               & str_detect(ObjetoCredito, "INVERSION"), 'S', ProdRNSF)) %>% #SECTOR TURISMO
    mutate(ProdRNSF = ifelse(TipoCredito %in% c('Micro','Pyme') & Sector == 'PRODUCCION INTELECTUAL' 
                               & (Moneda=="MN" | (Moneda=="ME" & FechaInicio<=as.Date("2013-12-23"))), 'S', ProdRNSF)) %>% #PRODUCCION INTELECTUAL
    mutate(ProdRNSF = ifelse(TipoCredito %in% c('Micro','Pyme') & Sector == 'H & E' 
                               & (Moneda=="MN" | (Moneda=="ME" & FechaInicio<=as.Date("2013-12-23"))), 'S', ProdRNSF)) %>% 
    ungroup()
  
  x <- cic %>% 
    dplyr::filter(ProdRNSF!=CredProdNoProd) %>% 
    select(CTACLIENTE, OPERACION, Moneda, TipoCredito, GrupoActEconDest, Sector, FechaInicio,
           ObjetoCredito, CredProdNoProd, ProdRNSF)
}
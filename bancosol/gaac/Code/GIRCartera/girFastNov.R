#-------------------------
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#################################
N_Campos <- readxl::read_excel("C:/!bso/girCartera/Clasificacion_Sector_Economico_Modificado.xls", sheet = "subclase") %>% 
  select(CAEDEC_DEST, cat)
ventamm <- readxl::read_excel("C:/!bso/girCartera/ventamm2.xlsx") 

bdc <- fread('C:/!bso/bases/BaseCarteraNov2022.txt',  encoding = 'Latin-1', fill = T, sep='|') 

################################################################################
####VERIFICACIÓN BASES####
bdc1 <- fread('C:/!bso/bases/BaseCarteraNov2022.txt',  encoding = 'Latin-1', fill = T, sep='|')  %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0),
         par30 = ifelse(DIASMORA >30, saldous, 0)) %>% 
  select(saldous, par1, par30) %>%
  summarise_all(sum,na.rm=T) %>% 
  mutate(par0rel=par1/saldous*100) %>% 
  mutate(par30rel=par30/saldous*100) %>% 
  glimpse()

bdc2 <- fread('C:/!bso/BaseCartera_20221130.txt',  encoding = 'Latin-1', fill = T, sep='|') %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0),
         par30 = ifelse(DIASMORA >30, saldous, 0)) %>% 
  select(saldous, par1, par30) %>%
  summarise_all(sum,na.rm=T) %>% 
  mutate(par0rel=par1/saldous*100) %>% 
  mutate(par30rel=par30/saldous*100) %>% 
  glimpse()

####ParticipaciondeCartera####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(categ = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
                           SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
                             SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
                             SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
                             SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
                             SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
                           SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
                             SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
                             SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%

  select(saldous, categ) %>% 
  group_by(categ) %>% 
  summarise_all(sum) %>% 
  mutate(pct=saldous/sum(saldous, na.rm=T)*100) %>% 
  glimpse()

####TasaPromedioPonderada####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(categ = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
                           SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
                             SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
                             SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
                             SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
                             SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
                           SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
                             SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
                             SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%
  mutate(intus = saldous * TASAACT/100) %>% 
  select(saldous, intus, categ) %>% 
  group_by(categ) %>% 
  summarise_all(sum) %>% 
  adorn_totals('row') %>% 
  mutate(tppa=intus/saldous*100) %>% 
  glimpse()
####PromedioDesembolsado####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
  mutate(categ = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
                           SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
                             SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
                             SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
                             SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
                             SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
                           SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
                             SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
                             SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(month(fdes)==11 & year(fdes)==2022) %>%
  mutate(ops=1) %>% 
  select(montous, categ, ops) %>% 
  group_by(categ) %>% 
  summarise_all(sum) %>% 
  adorn_totals('row') %>% 
  mutate(prom=montous/ops) %>% 
  glimpse()

####MORAS####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIGADA'), saldous, 0)) %>% 
  mutate(saldous=ifelse(str_detect(ESTADO,'CASTIG'), NA, saldous)) %>% 
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0),
         par30 = ifelse(DIASMORA >30, saldous, 0)) %>% 
  select(saldous, par1, par30, saldoCast) %>%
  summarise_all(sum,na.rm=T) %>% 
  mutate(par0rel=par1/saldous*100) %>% 
  mutate(par30rel=par30/saldous*100) %>% 
  mutate(mmc=(par30+saldoCast)/saldous*100) %>% 
  glimpse()

####
# RRRRRRRRREEEEEEEEEVVVVVVVVIIISARRRRRRRRRR
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(categ = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
                           SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
                             SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
                             SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
                             SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
                             SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
                           SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
                             SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
                             SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
  select(saldous, par1, categ) %>% 
  group_by(categ) %>% 
  summarise_all(sum,na.rm=T) %>% 
  #dplyr::filter(!is.na(categ)) %>% 
  adorn_totals('row') %>% 
  mutate(par0rel=par1/saldous*100) %>% 
  glimpse()

####TipoCredito####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>%
  mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
                              substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
                              substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
                              substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
  
  select(saldous, tipoCred, par1) %>% 
  group_by(tipoCred) %>% 
  summarise_all(sum) %>% 
  mutate(pct=saldous/sum(saldous, na.rm=T)*100) %>% 
  mutate(moras= par1/saldous*100) %>% 
  adorn_totals('row') %>% 
  glimpse()

####SUCURSAL####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal)) %>%
  mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                              sucursal == '10' ~ 'El Alto',
                              sucursal == '2' ~ 'La Paz',
                              sucursal == '3' ~ 'Cochabamba',
                              sucursal == '4' ~ 'Oruro',
                              sucursal == '5' ~ 'Potosí',
                              sucursal == '6' ~ 'Tarija',
                              sucursal == '7' ~ 'Santa Cruz',
                              sucursal == '8' ~ 'Beni',
                              sucursal == '9' ~ 'Pando',)) %>% 
  select(saldous, Sucursal) %>% 
  group_by(Sucursal) %>% 
  summarise_all(sum) %>% 
  mutate(pct=saldous/sum(saldous, na.rm=T)*100) %>% 
  adorn_totals('row') %>% 
  glimpse()

####SECTORACTUVIDAD####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(divCaedecC = substr(CIU,1,2)) %>% 
  mutate(grupoCaedecC = case_when(divCaedecC == '01'~'A',
                                  divCaedecC == '02'~'B',
                                  divCaedecC == '03'~'B',
                                  divCaedecC == '05'~'B',
                                  divCaedecC == '11'~'C',
                                  divCaedecC == '10'~'D',
                                  divCaedecC == '11'~'D',
                                  divCaedecC == '12'~'D',
                                  divCaedecC == '13'~'D',
                                  divCaedecC == '14'~'D',
                                  divCaedecC == '15'~'E',
                                  divCaedecC == '16'~'E',
                                  divCaedecC == '17'~'E',
                                  divCaedecC == '18'~'E',
                                  divCaedecC == '19'~'E',
                                  divCaedecC == '20'~'E',
                                  divCaedecC == '21'~'E',
                                  divCaedecC == '22'~'E',
                                  divCaedecC == '23'~'E',
                                  divCaedecC == '24'~'E',
                                  divCaedecC == '25'~'E',
                                  divCaedecC == '26'~'E',
                                  divCaedecC == '27'~'E',
                                  divCaedecC == '28'~'E',
                                  divCaedecC == '29'~'E',
                                  divCaedecC == '30'~'E',
                                  divCaedecC == '31'~'E',
                                  divCaedecC == '32'~'E',
                                  divCaedecC == '33'~'E',
                                  divCaedecC == '34'~'E',
                                  divCaedecC == '35'~'E',
                                  divCaedecC == '36'~'E',
                                  divCaedecC == '37'~'E',
                                  divCaedecC == '40'~'F',
                                  divCaedecC == '41'~'F',
                                  divCaedecC == '45'~'G',
                                  divCaedecC == '50'~'H',
                                  divCaedecC == '51'~'H',
                                  divCaedecC == '52'~'H',
                                  divCaedecC == '55'~'I',
                                  divCaedecC == '60'~'J',
                                  divCaedecC == '61'~'J',
                                  divCaedecC == '62'~'J',
                                  divCaedecC == '63'~'J',
                                  divCaedecC == '64'~'J',
                                  divCaedecC == '65'~'K',
                                  divCaedecC == '66'~'K',
                                  divCaedecC == '67'~'K',
                                  divCaedecC == '70'~'L',
                                  divCaedecC == '71'~'L',
                                  divCaedecC == '72'~'L',
                                  divCaedecC == '73'~'L',
                                  divCaedecC == '74'~'L',
                                  divCaedecC == '75'~'M',
                                  divCaedecC == '80'~'N',
                                  divCaedecC == '85'~'O',
                                  divCaedecC == '90'~'O',
                                  divCaedecC == '91'~'O',
                                  divCaedecC == '92'~'O',
                                  divCaedecC == '93'~'O',
                                  divCaedecC == '95'~'P',
                                  divCaedecC == '98'~'Q',
                                  divCaedecC == '99'~'Z',)) %>% 
  mutate(labGrupoC = case_when(grupoCaedecC == 'A'~ 'A. Agricola',
                               grupoCaedecC == 'B'~ 'B. Caza, Pesca',
                               grupoCaedecC == 'C'~ 'C. Ext. Gas y Pet.',
                               grupoCaedecC == 'D'~ 'D. Ext. Minerales',
                               grupoCaedecC == 'E'~ 'E. Ind. y Manu.',
                               grupoCaedecC == 'F'~ 'F. Dist. EE y agua',
                               grupoCaedecC == 'G'~ 'G. Construcción',
                               grupoCaedecC == 'H'~ 'H. Comercio',
                               grupoCaedecC == 'I'~ 'I. Hoteles',
                               grupoCaedecC == 'J'~ 'J. Transporte',
                               grupoCaedecC == 'K'~ 'K. Inter. Fin.',
                               grupoCaedecC == 'L'~ 'L. Serv. Inmob.',
                               grupoCaedecC == 'M'~ 'M. Adm. Pública',
                               grupoCaedecC == 'N'~ 'N. Educación',
                               grupoCaedecC == 'O'~ 'O. Serv. Hosp + Otros',
                               grupoCaedecC == 'P'~ 'P. Serv. Doméstico',
                               grupoCaedecC == 'Q'~ 'Q. Org. Extraterritoriales',
                               grupoCaedecC == 'Z'~ 'Z. Jubilados, Est. y AC',
                               TRUE ~ 'Otros')) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |  
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |  
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      # sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      # sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                      TRUE ~ labGrupoC)) %>% 
  mutate(Sector_Actividad = ifelse(is.na(Sector_Actividad),'Otros', Sector_Actividad)) %>% 
  select(saldous, Sector_Actividad) %>% 
  group_by(Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(pct=saldous/sum(saldous, na.rm=T)*100) %>% 
  adorn_totals('row') %>% 
  glimpse()

####Reprog####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(saldoReprog = case_when(ctaCont == '135'~saldous,
                                 ctaCont == '136'~saldous,
                                 ctaCont == '137'~saldous,
                                 TRUE ~ 0)) %>% 
  mutate(saldoRepPaR0 = case_when(ctaCont == '136'~saldous,
                                  ctaCont == '137'~saldous,
                                  TRUE ~ 0)) %>% 
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
  select(saldous, saldoReprog, saldoRepPaR0) %>%
  summarise_all(sum,na.rm=T) %>% 
  mutate(par0rel=saldoRepPaR0/saldoReprog*100,
         pctReprog = saldoReprog/saldous*100) %>% 
  glimpse()

####MoraSucursal####
gph<-bdc %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(saldoReprog = case_when(ctaCont == '135'~saldous,
                                 ctaCont == '136'~saldous,
                                 ctaCont == '137'~saldous,
                                 TRUE ~ 0)) %>% 
  mutate(saldoRepPaR0 = case_when(ctaCont == '136'~saldous,
                                  ctaCont == '137'~saldous,
                                  TRUE ~ 0)) %>% 
  mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal)) %>%
  mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                              sucursal == '10' ~ 'El Alto',
                              sucursal == '2' ~ 'La Paz',
                              sucursal == '3' ~ 'Cochabamba',
                              sucursal == '4' ~ 'Oruro',
                              sucursal == '5' ~ 'Potosí',
                              sucursal == '6' ~ 'Tarija',
                              sucursal == '7' ~ 'Santa Cruz',
                              sucursal == '8' ~ 'Beni',
                              sucursal == '9' ~ 'Pando',)) %>% 
  select(saldous, par1, Sucursal, saldoReprog, saldoRepPaR0) %>%
  group_by(Sucursal) %>% 
  summarise_all(sum,na.rm=T) %>% 
  mutate(pctSucursal = saldous/sum(saldous)*100) %>% 
  adorn_totals('row') %>% 
  mutate(par0Rel = par1/saldous*100) %>% 
  mutate(morareprog = saldoRepPaR0/saldoReprog*100)
  glimpse()
  
  ##############################################################################
 ####Rango####
  #Con Saldous
  
   gph<-bdc %>% 
    dplyr::filter(MODULO != 131) %>%
    dplyr::filter(ESTADO != 'CASTIGADA') %>%
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
    mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
    select(saldous, rango) %>% 
    group_by(rango) %>% 
    summarise_all(sum) %>% 
    mutate(pct=saldous/sum(saldous)*100) %>% 
    glimpse()
    
################################################################################
####MoraDiferida####
    
gph<-bdc %>% 
      dplyr::filter(MODULO != 131) %>%
      dplyr::filter(ESTADO != 'CASTIGADA') %>%
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(SALDO_CAPITAL_DIFERIDO = 0) %>%
      mutate(SALDO_INT_CAPITAL_DIFERIDO =  0) %>%
      mutate(saldousdifFranz =  0) %>%
      mutate(saldousdif =  0) %>%
      mutate(saldoRefMora = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 0,
                                   saldous, 0)) %>% 
      mutate(saldoRefMora30 = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 30,
                                     saldous, 0)) %>% 
      mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>% 
      mutate(saldousdifMora = 0) %>% 
      mutate(saldousdifMora30 = 0) %>% 
      mutate(saldousdifFranzMora = 0) %>% 
      mutate(saldousdifFranzMora30 = 0) %>% 
      mutate(saldoReprogDif = ifelse(MODULO == 121 & saldousdif > 0 ,
                                     saldoReprog, 0)) %>% 
      mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldousdif >0 ,
                                    saldoReprog, 0)) %>% 
      mutate(saldoReprogDifMora = 0) %>% 
      mutate(saldoReprogDifMora30 =  0) %>% 
      mutate(saldoRefinDifMora = 0) %>% 
      mutate(saldoRefinDifMora30 = 0) %>% 
      select(starts_with('saldo')) %>% 
      summarise_all(sum)
    
    
################################################################################
################################################################################
bd_1 <- readRDS('C:/!bso/girCartera/rds/ec_Oct2022.rds')
bd_2 <- readRDS('C:/!bso/girCartera/rds/ec_Nov2022.rds')

dif_1 <- bd_1 %>% 
  select(monDate, saldous, MODULO, REFINANCIAMIENTO_GENUINO, DIASMORA,
         SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO, MONEDA, 
         starts_with('RUBRO'), OPERACION, CTACLIENTE, ESTADO) %>% 
  mutate(SALDO_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_CAPITAL_DIFERIDO), 0, SALDO_CAPITAL_DIFERIDO)) %>%
  mutate(SALDO_INT_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_INT_CAPITAL_DIFERIDO), 0, SALDO_INT_CAPITAL_DIFERIDO)) %>% 
  mutate(saldousdifFranz = ifelse((SALDO_CAPITAL_DIFERIDO + SALDO_INT_CAPITAL_DIFERIDO) > 0, 
                                  saldous, 0)) %>% 
  mutate(rubDif = substr(as.character(RUBRO_CAPITAL_DIFERIDO),1, 3)) %>% 
  mutate(saldousdif = case_when(rubDif == '131' | rubDif == '133' |
                                  rubDif == '134' | rubDif == '135' |
                                  rubDif == '136' | rubDif == '137' ~ SALDO_CAPITAL_DIFERIDO, TRUE~0)) %>% 
  mutate(saldousdif = ifelse(MONEDA == 0, saldousdif/6.86, saldousdif)) %>% 
  mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>% 
  ungroup() %>% 
  dplyr::filter(saldousdif > 0) %>% 
  select(OPERACION, CTACLIENTE, saldous, ESTADO, monDate, saldousdif, saldoRef)

dif_2 <- bd_2 %>% 
  # mutate(date = as.Date('2022-11-30')) %>% 
  # mutate(monDate = as.yearmon(date)) %>% 
  select(monDate, saldous, MODULO, REFINANCIAMIENTO_GENUINO, DIASMORA,
         SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO, MONEDA, 
         starts_with('RUBRO'), OPERACION, CTACLIENTE, ESTADO) %>% 
  mutate(SALDO_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_CAPITAL_DIFERIDO), 0, SALDO_CAPITAL_DIFERIDO)) %>%
  mutate(SALDO_INT_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_INT_CAPITAL_DIFERIDO), 0, SALDO_INT_CAPITAL_DIFERIDO)) %>% 
  mutate(saldousdifFranz = ifelse((SALDO_CAPITAL_DIFERIDO + SALDO_INT_CAPITAL_DIFERIDO) > 0, 
                                  saldous, 0)) %>% 
  mutate(rubDif = substr(as.character(RUBRO_CAPITAL_DIFERIDO),1, 3)) %>% 
  mutate(saldousdif = case_when(rubDif == '131' | rubDif == '133' |
                                  rubDif == '134' | rubDif == '135' |
                                  rubDif == '136' | rubDif == '137' ~ SALDO_CAPITAL_DIFERIDO, TRUE~0)) %>% 
  mutate(saldousdif = ifelse(MONEDA == 0, saldousdif/6.86, saldousdif)) %>% 
  mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>% 
  ungroup() %>% 
  dplyr::filter(saldousdif > 0) %>% 
  select(OPERACION, CTACLIENTE, saldous, ESTADO, monDate, saldousdif, saldoRef)

bdcFinal <- dif_1 %>% 
  bind_rows(dif_2) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  arrange(CTACLIENTE, OPERACION, dayDate) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(pos = row_number()) %>% 
  mutate(posTot = sum(pos)) %>% 
  ungroup() %>% 
  group_by(dayDate) %>% 
  mutate(saldoTot = sum(saldousdif)) %>% 
  ungroup() %>% 
  mutate(difTot =  saldoTot - dplyr::lag(saldoTot)) %>%
  group_by(OPERACION) %>% 
  mutate(difSD = saldousdif - dplyr::lag(saldousdif)) %>% 
  mutate(newRef = ifelse(saldoRef > dplyr::lag(saldoRef), saldoRef - dplyr::lag(saldoRef), 0)) %>% 
  ungroup() %>% 
  mutate(amort = sum(difSD, na.rm = T)) %>% 
  mutate(refin = sum(newRef, na.rm = T)) %>% 
  mutate(cancel = ifelse(posTot == 1, sum(saldousdif), 0))
sum(bdcFinal$difSD, na.rm = T)

write.xlsx(bdcFinal, 'C:/!bso/girCartera/SolucionesRefin_Nov2022.xlsx')
bdcFinal %>% 
  dplyr::filter(newRef > 0)
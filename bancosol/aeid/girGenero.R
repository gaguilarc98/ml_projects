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


################################################################################
#############################################################################
# Appending
long_list<-c()

long_list <- c('ec_Dic2015.rds', 'ec_Dic2016.rds', 'ec_Dic2017.rds',
               'ec_Dic2018.rds', 'ec_Dic2019.rds', 'ec_Dic2020.rds')

bdcList_1<- list()

for(i in 1:length(long_list)) {
  print(long_list[i])
  bdcNC <- readRDS(paste0('C:/!bso/girCartera/rds/', long_list[i])) 
  sumBDC_full <- bdcNC %>%
    mutate(montoHist = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(rangom = case_when(montoHist <= 1000 ~'a. Hasta 1k',
                              montoHist > 1000 & montoHist <= 3000 ~'b. 1k-3k',
                              montoHist > 3000 & montoHist <= 5000 ~'c. 3k-5k',
                              montoHist > 5000 & montoHist <= 8000 ~'d. 5k-8k',
                              montoHist > 8000 & montoHist <= 10000 ~'e. 8k-10k',
                              montoHist > 10000 & montoHist <= 20000 ~'f. 10k-20k',
                              montoHist > 20000 ~'g. mayor20k')) %>% 
    select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
           saldous, montous, previus, intus, categ, opDes, saldoReprog,
           saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
           saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
           tipoCred, CAEDEC_DEST, CIU, GENERO, OPERACION_ORI_REF) %>% 
    mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                        labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoD == 'M. Adm. Pública' |  
                                        labGrupoD ==  'N. Educación' ~'Asalariados',
                                      labGrupoD == 'P. Serv. Doméstico' |  
                                        labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      # sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      # sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                      TRUE ~ labGrupoD)) %>% 
    mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
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
    dplyr::rename(saldoRepPaR30 = saldoRepPaR0) %>% 
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
    select(-REFINANCIAMIENTO_GENUINO, -DIASMORA, -MONEDA, -MODULO,
           -starts_with('SALDO', ignore.case = F), -CAEDEC_DEST,
           -labGrupoD, -labGrupoC, -CIU) %>% 
    mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
    mutate(categPar30=case_when(saldoRefMora30 >0 ~ 'Par30Refin',
                                saldoRepPaR30 >0~ 'Par30Reprog',
                                saldousdif>0~'par30dif'
                               )) %>%
    mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
    mutate(categPar1=case_when(saldoRefMora >0 ~ 'Par1Refin',
                               saldoRepMora >0~ 'Par1Reprog',
                               saldousdif>0~'par1dif'
                               )) %>% 
    mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
    mutate(OPERACION_ORI_REF=as.integer(OPERACION_ORI_REF)) %>% 
    mutate(categPar30_2=case_when(OPERACION_ORI_REF>0 ~ 'Par30Refin2',
                                  saldoRepPaR30 >0~ 'Par30Reprog2',
                                  TRUE~'Normal')) %>%
    mutate(categPar1_2=case_when(OPERACION_ORI_REF>0 ~ 'Par1Refin2',
                                 saldoRepMora >0~ 'Par1Reprog2',
                                 TRUE~'Normal')) %>%   
    ungroup() %>% 
    select(-OPERACION_ORI_REF) %>% 
    group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
             Sector_Destino, Sector_Actividad, GENERO, categPar1, categPar30, categPar1_2,
             categPar30_2) %>%
    summarise_all(sum)
  bdcList_1[[i]] <- sumBDC_full
}

bdcFinal_1 <- bind_rows(bdcList_1) %>%
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  dplyr::filter(dayDate >= '2014-12-31')
bdcFinalExp_1 <- bdcFinal_1 %>% 
  ungroup() %>% 
  select(-monDate) %>% 
  dplyr::rename(Sector_Cartera = categ,
                Sucursal = sucursal,
                Rango_Desembolso = rangom,
                Rango_Saldo_Actual = rangos,
                Cartera_Bruta = saldous,
                Monto_Desembolsado = montous,
                Previsión_USD = previus,
                Interés_Anual = intus,
                Operaciones_Desembolsadas = opDes,
                Operaciones_Totales = opTot,
                Cartera_Reprogramada = saldoReprog,
                PaR_30_Bruta = saldoMora,
                PaR_30_Reprogramada = saldoRepPaR30,
                Cartera_Castigada = saldoCast,
                PaR_1_Bruta = par1,
                Cartera_Reprogramada_Vigente = saldoRepVig,
                PaR_1_Reprogramada = saldoRepMora,
                Cartera_Diferida_ASFI = saldousdif,
                Cartera_Diferida_RC = saldousdifFranz,
                PaR_1_Refinanciada = saldoRefMora,
                PaR_30_Refinanciada = saldoRefMora30,
                Cartera_Refinanciada = saldoRef,
                PaR_1_Diferida_ASFI = saldousdifMora,
                PaR_30_Diferida_ASFI = saldousdifMora30 ,
                PaR_1_Diferida_RC = saldousdifFranzMora,
                PaR_30_Diferida_RC = saldousdifFranzMora30 ,
                Fecha = dayDate,
                Tipo_Credito = tipoCred) %>%
  mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
                              Sucursal == '10' ~ 'El Alto',
                              Sucursal == '2' ~ 'La Paz',
                              Sucursal == '3' ~ 'Cochabamba',
                              Sucursal == '4' ~ 'Oruro',
                              Sucursal == '5' ~ 'Potosí',
                              Sucursal == '6' ~ 'Tarija',
                              Sucursal == '7' ~ 'Santa Cruz',
                              Sucursal == '8' ~ 'Beni',
                              Sucursal == '9' ~ 'Pando',)) %>% 
  select(-starts_with('saldo'))%>% 
  glimpse()

write_rds(bdcFinalExp_1, 'C:/!bso/genero/output/añospasados.rds')
#############################################################################
ap<-readRDS('C:/!bso//genero/output/añospasados.rds')
# After dec-2020
gc()
bdcList <- list()
short_list <- c('ec_Dic2021.rds','ec_Dic2022.rds', 'ec_Ene2023.rds',
                'ec_Feb2023.rds', 'ec_Mar2023.rds')


for(i in 1:length(short_list)) {
  bdcNC <- readRDS(paste0('C:/!bso/girCartera/rds/', short_list[i]))
  sumBDC_full <- bdcNC %>%
    mutate(montoHist = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(rangom = case_when(montoHist <= 1000 ~'a. Hasta 1k',
                              montoHist > 1000 & montoHist <= 3000 ~'b. 1k-3k',
                              montoHist > 3000 & montoHist <= 5000 ~'c. 3k-5k',
                              montoHist > 5000 & montoHist <= 8000 ~'d. 5k-8k',
                              montoHist > 8000 & montoHist <= 10000 ~'e. 8k-10k',
                              montoHist > 10000 & montoHist <= 20000 ~'f. 10k-20k',
                              montoHist > 20000 ~'g. mayor20k')) %>% 
    select(monDate, categ, sucursal, rangos, rangom, labGrupoC, labGrupoD,
           saldous, montous, previus, intus, categ, opDes, saldoReprog,
           saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
           saldoRepVig, saldoRepMora, MODULO, REFINANCIAMIENTO_GENUINO, DIASMORA,
           SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO, MONEDA,
           RUBRO_CAPITAL_DIFERIDO, tipoCred, labGrupoC, labGrupoD, sectCart, 
           CAEDEC_DEST, CIU, GENERO, OPERACION_ORI_REF) %>%
    
    mutate(opTot = ifelse(saldoCast > 0, 0,opTot)) %>% 
    mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                        labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoD == 'M. Adm. Pública' |  
                                        labGrupoD ==  'N. Educación' ~'Asalariados',
                                      labGrupoD == 'P. Serv. Doméstico' |  
                                        labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      # sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      # sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                      TRUE ~ labGrupoD)) %>% 
    mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
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
    dplyr::rename(saldoRepPaR30 = saldoRepPaR0) %>%
    mutate(SALDO_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_CAPITAL_DIFERIDO), 0, SALDO_CAPITAL_DIFERIDO)) %>%
    mutate(SALDO_INT_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_INT_CAPITAL_DIFERIDO), 0, SALDO_INT_CAPITAL_DIFERIDO)) %>%
    mutate(saldousdifFranz = ifelse((SALDO_CAPITAL_DIFERIDO + SALDO_INT_CAPITAL_DIFERIDO) > 0,
                                    saldous, 0)) %>%
    mutate(rubDif = substr(RUBRO_CAPITAL_DIFERIDO,1,3))%>% 
    mutate(saldousdif = ifelse((rubDif == '131' | rubDif == '133' |
                                  rubDif == '134' | rubDif == '135' |
                                  rubDif == '136' | rubDif == '137') & MONEDA == 0 , 
                               as.numeric(SALDO_CAPITAL_DIFERIDO)/6.86, 
                               0)) %>% 
    mutate(saldousdif = ifelse((rubDif == '131' | rubDif == '133' |
                                  rubDif == '134' | rubDif == '135' |
                                  rubDif == '136' | rubDif == '137') & MONEDA != 0 , 
                               as.numeric(SALDO_CAPITAL_DIFERIDO), 
                               saldousdif)) %>% 
    select(-rubDif) %>% 
    mutate(saldoRefMora = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 0,
                                 saldous, 0)) %>%
    mutate(saldoRefMora30 = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 30,
                                   saldous, 0)) %>%
    mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>%
    mutate(saldousdifMora = ifelse(DIASMORA > 0, saldousdif, 0)) %>%
    mutate(saldousdifMora30 = ifelse(DIASMORA > 30, saldousdif, 0)) %>%
    mutate(saldousdifFranzMora = ifelse(DIASMORA > 0, saldousdifFranz, 0)) %>%
    mutate(saldousdifFranzMora30 = ifelse(DIASMORA > 30, saldousdifFranz, 0)) %>%
    mutate(saldoReprogDif = ifelse(MODULO == 121 & saldousdif >0 ,
                                   saldoReprog, 0)) %>%
    mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldousdif >0 ,
                                  saldoReprog, 0)) %>%
    mutate(saldoReprogDifMora = ifelse(MODULO == 121 & saldousdif > 0 & DIASMORA > 0,
                                       saldoReprog, 0)) %>%
    mutate(saldoReprogDifMora30 = ifelse(MODULO == 121 & saldousdif > 0 & DIASMORA > 30,
                                         saldoReprog, 0)) %>%
    mutate(saldoRefinDifMora = ifelse(REFINANCIAMIENTO_GENUINO != '-' &
                                        saldousdif >0 & DIASMORA > 0 ,
                                      saldoReprog, 0)) %>%
    mutate(saldoRefinDifMora30 = ifelse(REFINANCIAMIENTO_GENUINO != '-' &
                                          saldousdif >0 & DIASMORA > 30 ,
                                        saldoReprog, 0)) %>%
    select(-REFINANCIAMIENTO_GENUINO, -DIASMORA, -MONEDA, -MODULO,
           -starts_with('SALDO', ignore.case = F), -CAEDEC_DEST, -sectCart, -CIU, 
           -labGrupoC, -labGrupoD) %>%
    ungroup() %>%
    mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
    mutate(categPar30=case_when(saldoRefMora30 >0 ~ 'Par30Refin',
                                saldoRepPaR30 >0~ 'Par30Reprog',
                                saldousdif>0~'par30dif')) %>%
    mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
    mutate(categPar1=case_when(saldoRefMora >0 ~ 'Par1Refin',
                               saldoRepMora >0~ 'Par1Reprog',
                               saldousdif>0~'par1dif')) %>% 
    mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
    select(-OPERACION_ORI_REF) %>% 
    group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
             Sector_Destino, Sector_Actividad, GENERO, categPar1, categPar30) %>%
    summarise_all(sum)
  bdcList[[i]] <- sumBDC_full
}

bdcFinal <- bind_rows(bdcList) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) 
bdcFinalExp <- bdcFinal %>% 
  ungroup() %>% 
  select(-monDate) %>% 
  dplyr::rename(Sector_Cartera = categ,
                Sucursal = sucursal,
                Rango_Desembolso = rangom,
                Rango_Saldo_Actual = rangos,
                Cartera_Bruta = saldous,
                Monto_Desembolsado = montous,
                Previsión_USD = previus,
                Interés_Anual = intus,
                Operaciones_Desembolsadas = opDes,
                Operaciones_Totales = opTot,
                Cartera_Reprogramada = saldoReprog,
                PaR_30_Bruta = saldoMora,
                PaR_30_Reprogramada = saldoRepPaR30,
                Cartera_Castigada = saldoCast,
                PaR_1_Bruta = par1,
                Cartera_Reprogramada_Vigente = saldoRepVig,
                PaR_1_Reprogramada = saldoRepMora,
                Cartera_Diferida_ASFI = saldousdif,
                Cartera_Diferida_RC = saldousdifFranz,
                PaR_1_Refinanciada = saldoRefMora,
                PaR_30_Refinanciada = saldoRefMora30,
                Cartera_Refinanciada = saldoRef,
                PaR_1_Diferida_ASFI = saldousdifMora,
                PaR_30_Diferida_ASFI = saldousdifMora30 ,
                PaR_1_Diferida_RC = saldousdifFranzMora,
                PaR_30_Diferida_RC = saldousdifFranzMora30 ,
                Fecha = dayDate,
                Tipo_Credito = tipoCred) %>%
  mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
                              Sucursal == '10' ~ 'El Alto',
                              Sucursal == '2' ~ 'La Paz',
                              Sucursal == '3' ~ 'Cochabamba',
                              Sucursal == '4' ~ 'Oruro',
                              Sucursal == '5' ~ 'Potosí',
                              Sucursal == '6' ~ 'Tarija',
                              Sucursal == '7' ~ 'Santa Cruz',
                              Sucursal == '8' ~ 'Beni',
                              Sucursal == '9' ~ 'Pando',)) %>% 
  select(-starts_with('saldo'), -starts_with('RUBRO')) %>% 
  
  glimpse()

################################################################################
###############################################################################
#saving
bdcFinalJoin<- bdcFinalExp_1 %>% 
  bind_rows(bdcFinalExp) %>% 
  mutate(GENERO=ifelse(GENERO!='F' & GENERO!='M', 'J', GENERO)) %>% 
  mutate(Cartera_Bruta_Normal=Cartera_Bruta-Cartera_Diferida_ASFI-Cartera_Reprogramada-
           Cartera_Refinanciada,
         Par_30_Normal=PaR_30_Bruta-PaR_30_Reprogramada - PaR_30_Diferida_ASFI-PaR_30_Refinanciada,
         Par_1_Normal=PaR_1_Bruta-PaR_1_Reprogramada - PaR_1_Diferida_ASFI- PaR_1_Refinanciada)


write.xlsx(bdcFinalJoin, 'C:/!bso/genero/output/tablaGIRgeneroMar23_vrango.xlsx', rowNames = F)
write.csv(bdcFinalJoin, 'C:/!bso/genero/output/tablaGIRDiciembres.csv', row.names = F)
write_rds(bdcFinalJoin, 'C:/!bso/genero/output/tablaGIRGenero.rds')

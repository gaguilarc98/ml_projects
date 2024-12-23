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
#############################################################################
df <- readRDS('C:/!bso/girCartera/rds/ec_Ago2018.rds') %>% 
  glimpse()
#############################################################################
# Appending
gc()
bdcList_1 <- list()
file_list <-  list.files(path='C:/!bso/girCartera/rds/')
flist <- file_list
for(i in 1:length(file_list)) {
  if(str_detect(file_list[i], '2021') | str_detect(file_list[i], '2022')) {
    flist <- flist[-c(i)]
  }
}

for(i in 1:length(flist)) {
  bdcNC <- readRDS(paste0('C:/!bso/girCartera/rds/', file_list[i])) 
  sumBDC_full <- bdcNC %>% 
    select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
           saldous, montous, previus, intus, categ, opDes, saldoReprog,
           saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
           saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
           tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
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
           -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
    ungroup() %>% 
    group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
             Sector_Destino, Sector_Actividad) %>% 
    summarise_all(sum)
  bdcList_1[[i]] <- sumBDC_full
}

bdcFinal_1 <- bind_rows(bdcList_1) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  dplyr::filter(dayDate >= '2015-12-31')
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
  #mutate(PaR_1_Diferida_ASFI =0,
         #PaR_30_Diferida_RC =0,
         #Cartera_Diferida_RC=0,
         #Cartera_Diferida_ASFI=0,) %>% 
  glimpse()

bdcAgg_1 <- bdcFinalExp_1 %>%
  ungroup() %>%
  select(-Sucursal, -Rango_Desembolso, -Rango_Saldo_Actual, -Sector_Cartera, 
         -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  group_by(Fecha) %>%
  summarise_all(sum)


################################################################################
#############################################################################
# After dec-2020

gc()

bdcList <- list()
#file_list <-  list.files(path='D:/!bso/girCartera/rds/')
short_list <- c('ec_Ene2021.rds', 'ec_Feb2021.rds', 'ec_Mar2021.rds',
                'ec_Abr2021.rds', 'ec_May2021.rds', 'ec_Jun2021.rds',
                'ec_Jul2021.rds', 'ec_Ago2021.rds', 'ec_Sep2021.rds',
                'ec_Oct2021.rds', 'ec_Nov2021.rds', 'ec_Dic2021.rds',
                'ec_Ene2022.rds', 'ec_Feb2022.rds', 'ec_Mar2022.rds',
                'ec_Abr2022.rds', 'ec_May2022.rds', 'ec_Jun2022.rds',
                'ec_Jul2022.rds', 'ec_Ago2022.rds', 'ec_Sep2022.rds')

for(i in 1:length(short_list)) {
  bdcNC <- readRDS(paste0('C:/!bso/girCartera/rds/', short_list[i]))
  sumBDC_full <- bdcNC %>%
    select(monDate, categ, sucursal, rangos, rangom, labGrupoC, labGrupoD,
           saldous, montous, previus, intus, categ, opDes, saldoReprog,
           saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
           saldoRepVig, saldoRepMora, MODULO, REFINANCIAMIENTO_GENUINO, DIASMORA,
           SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO, MONEDA,
           RUBRO_CAPITAL_DIFERIDO, tipoCred, labGrupoC, labGrupoD, sectCart, 
           CAEDEC_DEST, CIU) %>%
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
    group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
             Sector_Destino, Sector_Actividad) %>%
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

bdcAgg_2 <- bdcFinalExp %>%
  ungroup() %>%
  select(-Sucursal, -Rango_Desembolso, -Rango_Saldo_Actual, -Sector_Cartera,
         -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  group_by(Fecha) %>%
  summarise_all(sum, na.rm = T)
   
bdcAgg_3 <- bdcAgg_1 %>%
bind_rows(bdcAgg_2) %>%
  dplyr::filter(!(year(Fecha) >= 2021 & Cartera_Diferida_ASFI == 0))


bdcExp <- bdcFinalExp_1 %>% 
  dplyr::filter(Fecha <= '2020-12-31') %>%  
  bind_rows(bdcFinalExp) 

# %>% 
#  
#   dplyr::distinct(Fecha,Sector_Cartera, Fecha, Rango_Desembolso, Sucursal,
#                   Grupo_CAEDEC_ac, Grupo_CAEDEC_dc, .keep_all = T)

###################################################################
###############################################################################
###############################################################################
# Adding missing months
# 2016: 9 y 10
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Sep2016.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC1 <- bdcExp %>% 
  bind_rows(bdcFill)
# Octubre 2016
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Oct2016.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC2 <- bdcExpC1 %>% 
  bind_rows(bdcFill)

# 2017: 9 y 10

bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Sep2017.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |  
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |  
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>% 
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |  
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |  
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC3 <- bdcExpC2 %>% 
  bind_rows(bdcFill)

# Checks and figs
bdcAgg <- bdcExpC15 %>%
  ungroup() %>%
  select(-Sucursal, -Rango_Desembolso, -Sector_Cartera,
         -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  group_by(Fecha) %>%
  summarise_all(sum, na.rm = T)

ggplot(bdcAgg, aes(x = Fecha, y = Cartera_Bruta)) + 
  geom_line(size = 1.25) + theme_minimal() 
###############################################################################
# 2017: 10

bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Oct2017.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |  
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |  
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>% 
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |  
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |  
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC4 <- bdcExpC3 %>% 
  bind_rows(bdcFill)

## 2018: 9 y 10
#Septiembre
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Sep2018.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |  
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |  
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>% 
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |  
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |  
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC5 <- bdcExpC4 %>% 
  bind_rows(bdcFill)

#OCtubre
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Oct2018.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |  
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |  
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>% 
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |  
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |  
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC6 <- bdcExpC5 %>% 
  bind_rows(bdcFill)

##2019: 9, 10 y 11
# Septiembre
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Sep2019.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |  
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |  
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>% 
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |  
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |  
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC7 <- bdcExpC6 %>% 
  bind_rows(bdcFill)

#Octubre

bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Oct2019.rds')
bdcFill <- bdcNC %>% 
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |  
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |  
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>% 
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |  
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |  
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
  ungroup() %>% 
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad) %>% 
  summarise_all(sum) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
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
  select(-starts_with('saldo')) %>% 
  glimpse()

bdcExpC8 <- bdcExpC7 %>% 
  bind_rows(bdcFill)

##2020: 9 10 y 11
# #Septiembre
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Sep2020.rds')
bdcFill <- bdcNC %>%
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD,
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>%
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' |
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>%
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>%
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' |
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>%
  ungroup() %>%
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred,
           Sector_Destino, Sector_Actividad) %>%
  summarise_all(sum) %>%
  mutate(dayDate = as.Date(monDate, frac = 1)) %>%
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
  select(-starts_with('saldo')) %>%
  glimpse()

bdcExpC9 <- bdcExpC8 %>%
  bind_rows(bdcFill)
#Octubre
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Oct2020.rds')
bdcFill <- bdcNC %>%
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD,
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>%
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' |
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>%
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>%
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' |
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>%
  ungroup() %>%
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred,
           Sector_Destino, Sector_Actividad) %>%
  summarise_all(sum) %>%
  mutate(dayDate = as.Date(monDate, frac = 1)) %>%
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
  select(-starts_with('saldo')) %>%
  glimpse()

bdcExpC10 <- bdcExpC9 %>%
  bind_rows(bdcFill)

# # ##2020
# #Noviembre
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Nov2020.rds')
bdcFill <- bdcNC %>%
  select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD,
         tipoCred, sectCart, CAEDEC_DEST, CIU) %>%
  mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' |
                                      labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
                                    labGrupoD == 'M. Adm. Pública' |
                                      labGrupoD ==  'N. Educación' ~'Asalariados',
                                    labGrupoD == 'P. Serv. Doméstico' |
                                      labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
                                    sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                    sectCart == 'Venta al por menor' ~ 'Venta por menor',
                                    TRUE ~ labGrupoD)) %>%
  mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>%
  mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' |
                                        labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
                                      labGrupoC == 'M. Adm. Pública' |
                                        labGrupoC ==  'N. Educación' ~'Asalariados',
                                      labGrupoC == 'P. Serv. Doméstico' |
                                        labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
                                      sectCart == 'Venta al por mayor' ~'Venta por mayor',
                                      sectCart == 'Venta al por menor' ~ 'Venta por menor',
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
         -sectCart, -labGrupoD, -labGrupoC, -CIU) %>%
  ungroup() %>%
  group_by(monDate, categ, sucursal, rangos, rangom, tipoCred,
           Sector_Destino, Sector_Actividad) %>%
  summarise_all(sum) %>%
  mutate(dayDate = as.Date(monDate, frac = 1)) %>%
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
  select(-starts_with('saldo')) %>%
  glimpse()

bdcExpC11 <- bdcExpC10 %>%
  bind_rows(bdcFill)

# ##Mayo 2016
# bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_May2016.rds')
# bdcFill <- bdcNC %>% 
#   select(monDate, categ, sucursal, rango, REFINANCIAMIENTO_GENUINO,
#          saldous, montous, previus, intus, categ, opDes, saldoReprog,
#          saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
#          saldoRepVig, saldoRepMora, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
#          tipoCred, sectCart, CAEDEC_DEST, CIU) %>% 
#   mutate(Sector_Destino = case_when(labGrupoD== 'A. Agricola' | 
#                                       labGrupoD == 'B. Caza, Pesca' ~'Agropecuario',
#                                     labGrupoD == 'M. Adm. Pública' |  
#                                       labGrupoD ==  'N. Educación' ~'Asalariados',
#                                     labGrupoD == 'P. Serv. Doméstico' |  
#                                       labGrupoD ==  'Z. Jubilados, Est. y AC'~'Otros',
#                                     sectCart == 'Venta al por mayor' ~'Venta por mayor',
#                                     sectCart == 'Venta al por menor' ~ 'Venta por menor',
#                                     TRUE ~ labGrupoD)) %>% 
#   mutate(Sector_Destino = ifelse(is.na(Sector_Destino),'Otros', Sector_Destino)) %>% 
#   mutate(Sector_Actividad = case_when(labGrupoC== 'A. Agricola' | 
#                                         labGrupoC == 'B. Caza, Pesca' ~'Agropecuario',
#                                       labGrupoC == 'M. Adm. Pública' |  
#                                         labGrupoC ==  'N. Educación' ~'Asalariados',
#                                       labGrupoC == 'P. Serv. Doméstico' |  
#                                         labGrupoC ==  'Z. Jubilados, Est. y AC'~'Otros',
#                                       sectCart == 'Venta al por mayor' ~'Venta por mayor',
#                                       sectCart == 'Venta al por menor' ~ 'Venta por menor',
#                                       TRUE ~ labGrupoC)) %>% 
#   mutate(Sector_Actividad = ifelse(is.na(Sector_Actividad),'Otros', Sector_Actividad)) %>% 
#   dplyr::rename(saldoRepPaR30 = saldoRepPaR0) %>% 
#   mutate(SALDO_CAPITAL_DIFERIDO = 0) %>%
#   mutate(SALDO_INT_CAPITAL_DIFERIDO =  0) %>%
#   mutate(saldousdifFranz =  0) %>%
#   mutate(saldousdif =  0) %>%
#   mutate(saldoRefMora = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 0,
#                                saldous, 0)) %>% 
#   mutate(saldoRefMora30 = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 30,
#                                  saldous, 0)) %>% 
#   mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>% 
#   mutate(saldousdifMora = 0) %>% 
#   mutate(saldousdifMora30 = 0) %>% 
#   mutate(saldousdifFranzMora = 0) %>% 
#   mutate(saldousdifFranzMora30 = 0) %>% 
#   mutate(saldoReprogDif = ifelse(MODULO == 121 & saldousdif > 0 ,
#                                  saldoReprog, 0)) %>% 
#   mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldousdif >0 ,
#                                 saldoReprog, 0)) %>% 
#   mutate(saldoReprogDifMora = 0) %>% 
#   mutate(saldoReprogDifMora30 =  0) %>% 
#   mutate(saldoRefinDifMora = 0) %>% 
#   mutate(saldoRefinDifMora30 = 0) %>% 
#   select(-REFINANCIAMIENTO_GENUINO, -DIASMORA, -MONEDA, -MODULO,
#          -starts_with('SALDO', ignore.case = F), -CAEDEC_DEST, 
#          -sectCart, -labGrupoD, -labGrupoC, -CIU) %>% 
#   ungroup() %>% 
#   group_by(monDate, categ, sucursal, rango, tipoCred, 
#            Sector_Destino, Sector_Actividad) %>% 
#   summarise_all(sum) %>% 
#   mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
#   ungroup() %>% 
#   select(-monDate) %>% 
#   dplyr::rename(Sector_Cartera = categ,
#                 Sucursal = sucursal,
#                 Rango_Desembolso = rango,
#                 Cartera_Bruta = saldous,
#                 Monto_Desembolsado = montous,
#                 Previsión_USD = previus,
#                 Interés_Anual = intus,
#                 Operaciones_Desembolsadas = opDes,
#                 Operaciones_Totales = opTot,
#                 Cartera_Reprogramada = saldoReprog,
#                 PaR_30_Bruta = saldoMora,
#                 PaR_30_Reprogramada = saldoRepPaR30,
#                 Cartera_Castigada = saldoCast,
#                 PaR_1_Bruta = par1,
#                 Cartera_Reprogramada_Vigente = saldoRepVig,
#                 PaR_1_Reprogramada = saldoRepMora,
#                 Cartera_Diferida_ASFI = saldousdif,
#                 Cartera_Diferida_RC = saldousdifFranz,
#                 PaR_1_Refinanciada = saldoRefMora,
#                 PaR_30_Refinanciada = saldoRefMora30,
#                 Cartera_Refinanciada = saldoRef,
#                 PaR_1_Diferida_ASFI = saldousdifMora,
#                 PaR_30_Diferida_ASFI = saldousdifMora30 ,
#                 PaR_1_Diferida_RC = saldousdifFranzMora,
#                 PaR_30_Diferida_RC = saldousdifFranzMora30 ,
#                 Fecha = dayDate,
#                 Tipo_Credito = tipoCred) %>%
#   mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
#                               Sucursal == '10' ~ 'El Alto',
#                               Sucursal == '2' ~ 'La Paz',
#                               Sucursal == '3' ~ 'Cochabamba',
#                               Sucursal == '4' ~ 'Oruro',
#                               Sucursal == '5' ~ 'Potosí',
#                               Sucursal == '6' ~ 'Tarija',
#                               Sucursal == '7' ~ 'Santa Cruz',
#                               Sucursal == '8' ~ 'Beni',
#                               Sucursal == '9' ~ 'Pando',)) %>% 
#   select(-starts_with('saldo')) %>% 
#   glimpse()
# 
# bdcExpC16 <- bdcExpC15 %>% 
#   bind_rows(bdcFill)
###############################################################################
###############################################################################
# Checks and figs
bdcAgg <- bdcExpC11 %>%
  ungroup() %>%
  select(-Sucursal, -Rango_Desembolso, -Rango_Saldo_Actual, -Sector_Cartera,
         -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  group_by(Fecha) %>%
  summarise_all(sum, na.rm = T)
write.xlsx(bdcAgg, 'C:/!bso/girCartera/girFull_AGG.xlsx', rowNames = F)

ggplot(bdcAgg, aes(x = Fecha, y = Cartera_Bruta)) + 
  geom_line(size = 1.25) + theme_minimal() 

write.xlsx(bdcExpC11, 'C:/!bso/girCartera/tablaGIR.xlsx', rowNames = F)
write.csv(bdcExpC11, 'C:/!bso/girCartera/tablaGIR.csv', row.names = F)

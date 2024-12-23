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
library(data.table)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")

#############################################################################
girold <- fread('C:/!bso/girCartera/GIRCorrects/output/tablaGIR2023.csv')
#############################################################################

gc()
bdcNC <- readRDS('C:/!bso/girCartera/rds/ec_Mar2023.rds') %>% 
  glimpse()
girNew <- bdcNC %>%
  mutate(montoHist = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
  mutate(rangom = case_when(montoHist < 500 ~'1. menor500USD',
                            montoHist > 500 & montoHist <= 1000 ~'2. 500-1k',
                            montoHist > 1000 & montoHist <= 5000 ~'3. 1k-5k',
                            montoHist > 5000 & montoHist <= 10000 ~'4. 5k-10k',
                            montoHist > 10000 & montoHist <= 15000 ~'5. 10k-15k',
                            montoHist > 15000 & montoHist <= 20000 ~'6. 15k-20k',
                            montoHist > 20000 ~'7. mayor20k')) %>%
  mutate(rangos = case_when(saldous < 500 ~'1. menor500USD',
                            saldous > 500 & saldous <= 1000 ~'2. 500-1k',
                            saldous > 1000 & saldous <= 5000 ~'3. 1k-5k',
                            saldous > 5000 & saldous <= 10000 ~'4. 5k-10k',
                            saldous > 10000 & saldous <= 15000 ~'5. 10k-15k',
                            saldous > 15000 & saldous <= 20000 ~'6. 15k-20k',
                            saldous > 20000 ~'7. mayor20k')) %>% 
  select(monDate, categ, sucursal, rangos, rangom, labGrupoC, labGrupoD,
         saldous, montous, previus, intus, categ, opDes, saldoReprog,
         saldoMora, saldoRepPaR0, saldoCast, par1, opTot, saldoRepPaR0,
         saldoRepVig, saldoRepMora, MODULO, REFINANCIAMIENTO_GENUINO, DIASMORA,
         SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO, MONEDA,
         RUBRO_CAPITAL_DIFERIDO, tipoCred, labGrupoC, labGrupoD, sectCart, 
         CAEDEC_DEST, CIU, GENERO, OPERACION_ORI_REF, rangos) %>%
  
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
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  mutate(Fecha = as.IDate(Fecha)) %>% 
  select(-OPERACION_ORI_REF, -monDate) %>% 
  group_by(Fecha, categ, sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad, GENERO, categPar1, categPar30) %>%
  summarise_all(sum)


bdcFinalExp <- girNew %>% 
  ungroup() %>% 
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

###############################################################################
# Adding New months
bdcExp <- girold %>% 
  bind_rows(bdcFinalExp) %>% 
  mutate(GENERO=ifelse(GENERO!='F' & GENERO!='M', 'J', GENERO)) %>% 
  mutate(Cartera_Bruta_Normal=Cartera_Bruta-Cartera_Diferida_ASFI-Cartera_Reprogramada-
           Cartera_Refinanciada,
         Par_30_Normal=PaR_30_Bruta-PaR_30_Reprogramada - PaR_30_Diferida_ASFI-PaR_30_Refinanciada,
         Par_1_Normal=PaR_1_Bruta-PaR_1_Reprogramada - PaR_1_Diferida_ASFI- PaR_1_Refinanciada)

###############################################################################
write.xlsx(bdcExp, 'C:/!bso/girCartera/GIRCorrects/output/tablaGIR2023_vf.xlsx', rowNames = F)
write.csv(bdcExp, 'C:/!bso/girCartera/GIRCorrects/output/tablaGIR2023.csv')

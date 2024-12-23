library(pmdplyr)
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
library(readstata13)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(ggplot2)
library(plotly)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen=999)
#-------------------------------------------------------------------------------
pathin = "C:/!CIDRE/BaseDeCartera/prueba/"
pathout = "C:/!CIDRE/BaseDeCartera/csvMonthly/bases_proc"
asfiUR <-  read.csv('C:/!CIDRE/Planning/clasifUR_ASFI.csv')
####################
file.names <- dir(pathin, pattern ="base202110") 
file.names
t <- Sys.time()
dlist <- list()
for(i in 1:length(file.names)){
  print(i)
  fn <- file.names[i]
  fn <- sub('.csv', '', fn)
  year <- sub('base', '', fn)
  bdc <- read_csv(paste0(pathin, file.names[i]))
  bdc$year <- year
  bdc <- bdc %>% 
    dplyr::filter(!str_detect(estadocredito, "CANCEL") & !str_detect(estadocredito, 'CASTIG')) %>%     
    dplyr::filter(!str_detect(productocrediticio, "BDP") & !str_detect(productocrediticio, 'COR')) %>% 
    mutate(saldo = as.numeric(saldo)) %>% 
    mutate(montodesembolsado = as.numeric(montodesembolsado)) %>%
    mutate(saldo = ifelse(str_detect(agencia, 'MONTEAGUDO') & str_detect(financiador, 'FONDECO'), saldo*0.9, saldo)) %>% 
    #mutate(Dfechaultimodesembolso = lubridate::dmy(fechaultimodesembolso)) %>%
    mutate(municipio = ifelse(str_detect(municipio, 'LA PAZ (MURILLO)'), 'LA PAZ',
                              ifelse(str_detect(municipio, 'ENTRERIOS'), 'ENTRE RIOS',
                                     ifelse(str_detect(municipio, 'EUCXALIPTUS'), 'EUCALIPTUS',
                                            ifelse(str_detect(municipio, 'PUERTO CARABUCO'), 'LA PAZ',
                                                   ifelse(str_detect(municipio, 'AYOPAYA (VILLA INDEPENDENCIA)'), 'INDEPENDENCIA',
                                                          ifelse(str_detect(municipio, 'PUERTO GONZALO MORENO'), 'PUERTO GONZALES MORENO',
                                                                 ifelse(str_detect(municipio, 'JUAN JOSE PEREZ(CHARAZANI)'), 'CHARAZANI',
                                                                        ifelse(str_detect(municipio, 'SALINAS DE GARCI MENDOZA'), 'SALINAS DE GARCIA MENDOZA',
                                                                               ifelse(str_detect(municipio, 'SAN PEDRO DE BUENA VISTA'), 'S.P. DE BUENA VISTA',
                                                                                      ifelse(str_detect(municipio, 'SAN PEDRO DE CURAHUARA'), 'SAN PEDRO CUARAHUARA',
                                                                                             ifelse(str_detect(municipio, 'SANTIAGO DE HUARI'), 'HUARI',
                                                                                                    ifelse(str_detect(municipio, 'SANTUARIO DE QUILLACAS'), 'QUILLACAS',
                                                                                                           ifelse(str_detect(municipio, 'TOCO'), 'TOKO',
                                                                                                                  ifelse(str_detect(municipio, 'TOTORA') & str_detect(departamento, 'ORURO'), 'SAN PEDRO DE TOTORA',
                                                                                                                         ifelse(str_detect(municipio, 'ANCORAINES'), 'ANCORAIMES',
                                                                                                                                ifelse(str_detect(municipio, 'ASCENCIO DE GUARAYOS'), 'ASCENCION DE GUARAYOS',
                                                                                                                                       ifelse(str_detect(municipio, 'SAAVEDRA')& str_detect(departamento, 'SANTA CRUZ'), 'GRAL. SAAVEDRA',municipio)))))))))))))))))) %>% 
    left_join(asfiUR, by = c('departamento', 'municipio')) %>% 
    mutate(sectorruralurbano = ifelse(!is.na(clasifAsfiUR), clasifAsfiUR, sectorruralurbano)) %>% 
    mutate(sectorruralurbano = ifelse(sectorruralurbano == 'U', 'URBANO', 
                                      ifelse(sectorruralurbano == 'R', 'RURAL', sectorruralurbano))) %>%
    mutate(credFid = 'RRPP') %>% 
    mutate(credFid = ifelse(str_detect(productocrediticio, 'BDP') |str_detect(productocrediticio, 'FID'), 
                            'FID', credFid)) %>% 
    mutate(codGarantia = substr(tipogarantiaprincipal, 1, 3 )) %>%
    mutate(codGarantia = ifelse(codGarantia == "NO ", "IPN", codGarantia)) %>% 
    mutate(Garantia = ifelse(codGarantia == 'HI1'  | str_detect(codGarantia, 'HO1') |
                               str_detect(codGarantia, 'HR1')  | str_detect(codGarantia, 'HT1'), 'Hipotecaria de vivienda',
                             ifelse(str_detect(codGarantia, 'HI2')  | str_detect(codGarantia, 'HO2') |
                                      str_detect(codGarantia, 'HR2')  | str_detect(codGarantia, 'HT2') |
                                      str_detect(codGarantia, 'HN')   | str_detect(codGarantia, 'HV'), 'Otras hipotecas',
                                    ifelse(str_detect(codGarantia, 'P03')  | str_detect(codGarantia, 'P04') |
                                             str_detect(codGarantia, 'PD3')  | str_detect(codGarantia, 'PD4'), 'Prend. Maquinaria', 
                                           ifelse(str_detect(codGarantia, 'IPJ')  | str_detect(codGarantia, 'IPN')  | 
                                                    str_detect(tipogarantiaprincipal, 'IPN'), 'Personal',       
                                                  ifelse(str_detect(codGarantia, 'OT1')  | str_detect(codGarantia, 'P09'), 'Semovientes y Docs. en custodia',       
                                                         ifelse(str_detect(codGarantia, 'P05')  | str_detect(codGarantia, 'P02') |
                                                                  str_detect(codGarantia, 'OT9')  | str_detect(codGarantia, 'IPQ') |
                                                                  str_detect(codGarantia, 'P01')  | str_detect(codGarantia, 'P06'), 'Quirografaria y otras prendas',      
                                                                'Sin Registro'))))))) %>% 
    mutate(Destino = ifelse(str_detect(productocrediticio, 'AGRO'), 'Agropecuario' ,
                            ifelse(str_detect(productocrediticio, 'VBI'), 'Productivo',
                                   ifelse(str_detect(productocrediticio, 'VIVIENDA'), 'Vivienda',
                                          ifelse(str_detect(productocrediticio, 'NO PRODUCTIVO') & str_detect(grupocaedeccredito, 'H,') | str_detect(productocrediticio, 'COMERCIO') | str_detect(productocrediticio, 'NO PRODUCTIVO') & str_detect(grupocaedeccredito, 'H-'), 'Comercio',
                                                 ifelse(str_detect(productocrediticio, 'NO PRODUCTIVO') & !str_detect(grupocaedeccredito, 'H,') | str_detect(productocrediticio, 'SERV'), 'Servicios',
                                                        ifelse(str_detect(productocrediticio, 'CONSUMO'), 'Consumo', 'Productivo'))))))) %>%
    mutate(Rango_Desembolso = ifelse(montodesembolsado <= 35000 & tipomoneda == 'Bs.', '1. Menor a 5K',
                                     ifelse(montodesembolsado <= 5102  & tipomoneda != 'Bs.', '1. Menor a 5K', 
                                            ifelse(montodesembolsado > 35000  & montodesembolsado <= 70000  & tipomoneda == 'Bs.', '2. 5K - 10K',
                                                   ifelse(montodesembolsado > 5102   & montodesembolsado <= 10204  & tipomoneda != 'Bs.', '2. 5K - 10K',
                                                          ifelse(montodesembolsado > 70000  & montodesembolsado <= 140000 & tipomoneda == 'Bs.', '3. 10K - 20K',
                                                                 ifelse(montodesembolsado > 10204  & montodesembolsado <= 20408  & tipomoneda != 'Bs.', '3. 10K - 20K',
                                                                        ifelse(montodesembolsado > 140000 & montodesembolsado <= 350000 & tipomoneda == 'Bs.', '4. 20K - 50K',
                                                                               ifelse(montodesembolsado > 20408  & montodesembolsado <= 51020  & tipomoneda != 'Bs.', '4. 20K - 50K',
                                                                                      ifelse(montodesembolsado > 350000 & montodesembolsado <= 700000 & tipomoneda == 'Bs.', '5. 50K - 100K',
                                                                                             ifelse(montodesembolsado > 51020  & montodesembolsado <= 102040 & tipomoneda != 'Bs.', '5. 50K - 100K',
                                                                                                    ifelse(montodesembolsado > 700000 & tipomoneda == 'Bs.', '6. 100K+',
                                                                                                           ifelse(montodesembolsado > 102040 & tipomoneda != 'Bs.', '6. 100K+', 'NA'))))))))))))) %>%
    mutate(agencia = ifelse(str_detect(agencia, 'POTOSI'), 'REG. POTOSI',
                            ifelse(str_detect(agencia, 'SUCRE'), 'REG. CHUQSACA',
                                   ifelse(str_detect(agencia, 'SAN IGNACIO'), 'SAN IGNACIO',
                                          ifelse(str_detect(agencia, 'SATELITE'), 'AG. SATELITE NORTE',
                                                 ifelse(str_detect(agencia, 'STA CRUZ'), 'REG. STA CRUZ',
                                                        ifelse(str_detect(agencia, 'REG. SANTA CRUZ'), 'REG. STA CRUZ', agencia))))))) %>%
    mutate(agencia = gsub('SUC. ', 'REG. ', agencia)) %>%
    mutate(saldo = ifelse(str_detect(tipomoneda, 'Bs'), saldo/6.86, saldo)) %>%
    mutate(montodesembolsado = ifelse(str_detect(tipomoneda, 'Bs'), montodesembolsado/6.86, montodesembolsado)) %>%
    mutate(saldoMora = ifelse(!str_detect(estadocredito, 'VIGENTE'), saldo, NA)) %>%
    mutate(saldoMora_PAR0 = ifelse(diasenmora > 0, saldo, NA)) %>%
    mutate(saldoVigente = ifelse(str_detect(estadocredito, 'VIGENTE'), saldo, NA)) %>%
    mutate(saldoReprog = ifelse(str_detect(estadocredito, 'REPROG'), saldo, NA)) %>%
    mutate(saldoReprogMora = ifelse(str_detect(estadocredito, 'VENCIDO-REPROG') | str_detect(estadocredito, 'EJECUCION-REPROG'), saldo, NA)) %>%
    # mutate(previsionmonto = ifelse(str_detect(tipomoneda, 'Bs'), previsionmonto/6.86, previsionmonto)) %>%
    mutate(montoacapitalproxpago = ifelse(str_detect(tipomoneda, 'Bs'), montoacapitalproxpago/6.86, montoacapitalproxpago)) %>%
    mutate(ultpagoacapital = ifelse(is.character(ultpagoacapital), as.numeric(ultpagoacapital), ultpagoacapital)) %>% 
    mutate(ultpagoacapital = ifelse(str_detect(tipomoneda, 'Bs'), ultpagoacapital/6.86, ultpagoacapital)) %>%
    # mutate(fechaproxpago  = as.Date(fechaproxpago, format = "%d/%m/%Y")) %>%
    # mutate(fechadeclaraciondelcliente  = as.Date(fechadeclaraciondelcliente, format = "%d/%m/%Y")) %>%
    mutate(Dfechaultimodesembolso  = as.Date(fechaultimodesembolso, format = "%d/%m/%Y")) %>%
    # mutate(fechaprimerdesembolso  = as.Date(fechaprimerdesembolso, format = "%d/%m/%Y")) %>%
    # mutate(fechaultpagocapital  = as.Date(fechaultpagocapital, format = "%d/%m/%Y")) %>%
    # mutate(mondes = as.yearmon(fechaultimodesembolso)) %>%
    # mutate(monrec = as.yearmon(fechaultpagocapital)) %>%
    mutate(Dfechadeclaraciondelcliente = as.Date(fechadeclaraciondelcliente, format = "%d/%m/%Y")) %>%
    mutate(Dfechadeclaraciondelcliente2 = as.Date(fechadeclaraciondelcliente, format = "%d/%m/%Y")) %>%
    mutate(yeardec = year(Dfechadeclaraciondelcliente)) %>% 
    mutate(tc = 6.86) %>% 
    mutate(totalpasivos = as.numeric(totalpasivos)) %>% 
    mutate(totalpasivos = ifelse(is.na(totalpasivos), 0,totalpasivos)) %>%
    mutate(totalactivos = ifelse(is.character(totalactivos), as.numeric(totalactivos), totalactivos)) %>% 
    mutate(totalactivos = ifelse(!str_detect(moneda, 'Bs'), totalactivos*tc, totalactivos),
           totalpasivos = ifelse(!str_detect(moneda, 'Bs'), totalpasivos*tc, totalpasivos),
           patrimonio = ifelse(!str_detect(moneda, 'Bs'), patrimonio*tc, patrimonio),
           ingresofijo = ifelse(!str_detect(moneda, 'Bs'), ingresofijo*tc, ingresofijo),
           ingresoporventas = ifelse(!str_detect(moneda, 'Bs'), ingresoporventas*tc, ingresoporventas),
           ingresovariable = ifelse(!str_detect(moneda, 'Bs'), ingresovariable*tc, ingresovariable),
           gastosfijos = ifelse(!str_detect(moneda, 'Bs'), gastosfijos*tc, gastosfijos),
           gastosvariables = ifelse(!str_detect(moneda, 'Bs'), gastosvariables*tc, gastosvariables)) %>% 
    mutate(ingresofijo = replace(ingresofijo, is.na(ingresofijo), 0)) %>%
    mutate(ingresoporventas = replace(ingresoporventas, is.na(ingresoporventas), 0)) %>%
    mutate(gastosfijos = replace(gastosfijos, is.na(gastosfijos), 0)) %>%
    mutate(gastosvariables = replace(gastosvariables, is.na(gastosvariables), 0)) %>%
    # mutate(ingresofijo = case_when(ingresofijo=='NA' & ingresoporventas!='NA' ~  0, TRUE~ingresofijo)) %>%
    # mutate(ingresoporventas = case_when(ingresoporventas=='NA' & ingresofijo!='NA' ~  0, TRUE~ingresoporventas)) %>%
    # mutate(gastosfijos = case_when(gastosfijos=='NA' & gastosvariables!='NA' ~  0, TRUE~gastosfijos)) %>%
    # mutate(gastosvariables = case_when(gastosvariables=='NA' & gastosfijos!='NA' ~  0, TRUE~gastosvariables)) %>%
    # case_when(ingresofijo!='NA' & ingresoporventas=='NA' ~ ingresoporventas==0) %>%
    # case_when(gastosfijos=='NA' & gastosvariables!='NA' ~ gastosfijos==0) %>%
    # case_when(gastosfijos!='NA' & gastosvariables=='NA' ~ gastosvariables==0) %>%
    mutate(ytot = ingresofijo + ingresoporventas) %>% 
    mutate(etot = gastosfijos + gastosvariables) %>%  
    mutate(previsionmonto = ifelse(str_detect(tipomoneda, 'Bs'), previsionmonto/6.86, previsionmonto)) %>%
    mutate(Garantia = ifelse(is.na(Garantia), 'personal', Garantia)) %>%
    mutate(califPotVivienda = ifelse(diasenmora <= 30 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'A',
                                     ifelse(diasenmora >= 31 & diasenmora <= 90 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'B',
                                            ifelse(diasenmora >= 91 & diasenmora <= 180 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'C',
                                                   ifelse(diasenmora >= 181 & diasenmora <= 270 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'D',
                                                          ifelse(diasenmora >= 271 & diasenmora <= 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'E',
                                                                 ifelse(diasenmora > 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'F', 'NA'))))))) %>%
    mutate(califPot = ifelse(diasenmora <= 30 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'A',
                             ifelse(diasenmora >= 31 & diasenmora <= 90 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'B',
                                    ifelse(diasenmora >= 91 & diasenmora <= 180 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'C',
                                           ifelse(diasenmora >= 181 & diasenmora <= 270 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'D',
                                                  ifelse(diasenmora >= 271 & diasenmora <= 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'E',
                                                         ifelse(diasenmora > 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'F',
                                                                ifelse(diasenmora <= 20 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'A',
                                                                       ifelse(diasenmora >= 21 & diasenmora <= 30 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'B',
                                                                              ifelse(diasenmora >= 31 & diasenmora <= 56 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'C',
                                                                                     ifelse(diasenmora >= 57 & diasenmora <= 76 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'D',
                                                                                            ifelse(diasenmora >= 77 & diasenmora <= 90 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'E',
                                                                                                   ifelse(diasenmora > 90 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'F',
                                                                                                          ifelse(diasenmora <=5 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'A',
                                                                                                                 ifelse(diasenmora >=6 & diasenmora <=30 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'B',
                                                                                                                        ifelse(diasenmora >=31 & diasenmora <=55 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'C',
                                                                                                                               ifelse(diasenmora >=56 & diasenmora <=75 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'D',
                                                                                                                                      ifelse(diasenmora >=76 & diasenmora <=90 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'E',
                                                                                                                                             ifelse(diasenmora >90 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'F', 'NA'))))))))))))))))))) %>%
    mutate(contagio = ifelse(cantidadcreditos > 1, 1, 0)) %>%
    group_by(nrocliente) %>%
    mutate(contagio = mean(contagio)) %>%
    mutate(maxCalif = max(califPot)) %>%
    ungroup() %>%
    #% prevision
    mutate(pctPreviPot = ifelse(maxCalif == 'A' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0,
                                ifelse(maxCalif == 'B' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.025,
                                       ifelse(maxCalif == 'C' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.2,
                                              ifelse(maxCalif == 'D' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.5,
                                                     ifelse(maxCalif == 'E' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.8,
                                                            ifelse(maxCalif == 'F' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 1,
                                                                   ifelse(maxCalif == 'A' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.0025,
                                                                          ifelse(maxCalif == 'B' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.05,
                                                                                 ifelse(maxCalif == 'C' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                        ifelse(maxCalif == 'D' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                               ifelse(maxCalif == 'E' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                      ifelse(maxCalif == 'F' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 1,
                                                                                                             ifelse(maxCalif == 'A' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.0025,
                                                                                                                    ifelse(maxCalif == 'B' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.05,
                                                                                                                           ifelse(maxCalif == 'C' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                                                                  ifelse(maxCalif == 'D' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                                                                         ifelse(maxCalif == 'E' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                                                                ifelse(maxCalif == 'F' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 1,
                                                                                                                                                       ifelse(maxCalif == 'A' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.025,
                                                                                                                                                              ifelse(maxCalif == 'B' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.05,
                                                                                                                                                                     ifelse(maxCalif == 'C' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                                                                                                            ifelse(maxCalif == 'D' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                                                                                                                   ifelse(maxCalif == 'E' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                                                                                                          ifelse(maxCalif == 'F' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 1,
                                                                                                                                                                                                 ifelse(maxCalif == 'A' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.025,
                                                                                                                                                                                                        ifelse(maxCalif == 'B' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.05,
                                                                                                                                                                                                               ifelse(maxCalif == 'C' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                                                                                                                                                      ifelse(maxCalif == 'D' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                                                                                                                                                             ifelse(maxCalif == 'E' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                                                                                                                                                    ifelse(maxCalif == 'F' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 1, 'NA'))))))))))))))))))))))))))))))) %>%
    mutate(M = ifelse(saldo < valorgarantiashipotecariasus, saldo, valorgarantiashipotecariasus)) %>%
    mutate(califPotCode = ifelse(califPot == 'A', 1,
                                 ifelse(califPot == 'B', 2,
                                        ifelse(califPot == 'C', 3,
                                               ifelse(califPot == 'D', 4,
                                                      ifelse(califPot == 'E', 5,
                                                             ifelse(califPot == 'F', 6, 'NA'))))))) %>%
    mutate(pctPreviPot = as.numeric(pctPreviPot)) %>%
    #mutate(previsionPot = saldo * pctPreviPot) %>%
    mutate(previsionPot = ifelse(diasenmora <=0, previsionmonto,
                                 ifelse(Garantia != 'Hipotecaria de vivienda'  & diasenmora >= 1, saldo * pctPreviPot,
                                        ifelse(Garantia == 'Hipotecaria de vivienda' & diasenmora >= 1, pctPreviPot * (saldo - 0.5*M), previsionmonto)))) %>%
    mutate(Destino2 = ifelse(Destino == 'Agropecuario', 'Agro',
                          ifelse(Destino == 'Productivo' | Destino == 'Vivienda', 'Prod',
                              ifelse(Destino == 'Comercio', 'Com',
                                  ifelse(Destino == 'Servicios', 'Serv', NA))))) %>%
    mutate(Rango2 = ifelse(Rango_Desembolso == '2. 5K - 10K' | Rango_Desembolso == '1. Menor a 5K', 'u10k',
                        ifelse(Rango_Desembolso == '3. 10K - 20K' | Rango_Desembolso == '4. 20K - 50K', 'f10kt50k',
                           ifelse(Rango_Desembolso == '5. 50K - 100K' | Rango_Desembolso == '6. 100K+', 'o50k', NA)))) %>%
    dplyr::filter(!is.na(Destino2)) %>%
    mutate(panel = paste0(Destino2, '_', Rango2)) %>%
    mutate(tasadeinteresvariable = ifelse(is.character(tasadeinteresvariable), as.numeric(tasadeinteresvariable), tasadeinteresvariable)) %>% 
    mutate(tasadeinteresvariable = ifelse(tasadeinteresvariable > 100, 21.24, tasadeinteresvariable))

  data_1 <- bdc %>%
    select(year, panel, saldo, tasadeinteresvariable, previsionPot, previsionmonto, diasenmora) %>%
    mutate(year = lubridate::ymd(year)) %>%
    mutate(interes = (saldo*tasadeinteresvariable)/100) %>%
    mutate(interes = ifelse(diasenmora > 0, 0, interes)) %>%
    mutate(int30 =  (saldo*tasadeinteresvariable)/100) %>%
    mutate(int30 = ifelse(diasenmora > 30, 0, int30)) %>%
    mutate(interes = as.numeric(interes)) %>%
    mutate(int30 = as.numeric(int30)) %>%
    select(year, panel, saldo, interes, previsionPot, previsionmonto, int30) %>%
    dplyr::rename(sal = saldo) %>%
    dplyr::rename(int00 = interes) %>%
    dplyr::rename(pre = previsionmonto) %>%
    dplyr::rename(pot = previsionPot) %>%
    group_by(year, panel) %>%
    summarise_all(sum) 
    pivot_wider(names_from = panel, values_from = c(sal, int00, int30, pre, pot))

  data_1 <- as.data.frame(data_1)

  preflist <- c('sal', 'int00', 'int30', 'pre', 'pot')
  new <- sapply(preflist, function(xx) rowSums(data_1[,grep(xx, names(data_1)), drop=FALSE]))
  new <- as.data.frame(t(new))
  colnames(new) <- paste0(colnames(new), 'Total')
  data_final <- cbind(data_1, new)

  ranglist_sal <- c('sal_Agro_f10kt50k', 'sal_Agro_u10k', 'sal_Agro_o50k', 'sal_Com_f10kt50k', 'sal_Com_u10k', 'sal_Com_o50k', 'sal_Prod_f10kt50k', 'sal_Prod_u10k', 'sal_Prod_o50k', 'sal_Serv_f10kt50k', 'sal_Serv_u10k', 'sal_Serv_o50k')

  new_2 <- sapply(ranglist_sal, function(xx) data_1[,grep(xx, names(data_1))]/data_final$salTotal)
  new_2 <- as.data.frame(t(new_2))
  colnames(new_2) <- paste0('W_', colnames(new_2))
  data_final <- cbind(data_final, new_2)

  ranglist_int00 <- c('int00_Agro_f10kt50k', 'int00_Agro_u10k', 'int00_Agro_o50k', 'int00_Com_f10kt50k', 'int00_Com_u10k', 'int00_Com_o50k', 'int00_Prod_f10kt50k', 'int00_Prod_u10k', 'int00_Prod_o50k', 'int00_Serv_f10kt50k', 'int00_Serv_u10k', 'int00_Serv_o50k')

  new_3 <- sapply(ranglist_int00, function(xx) data_1[,grep(xx, names(data_1))]/data_final$int00Total)
  new_3 <- as.data.frame(t(new_3))
  colnames(new_3) <- paste0('W_', colnames(new_3))
  data_final <- cbind(data_final, new_3)

  ranglist_int30 <- c('int30_Agro_f10kt50k', 'int30_Agro_u10k', 'int30_Agro_o50k', 'int30_Com_f10kt50k', 'int30_Com_u10k', 'int30_Com_o50k', 'int30_Prod_f10kt50k', 'int30_Prod_u10k', 'int30_Prod_o50k', 'int30_Serv_f10kt50k', 'int30_Serv_u10k', 'int30_Serv_o50k')

  new_4 <- sapply(ranglist_int30, function(xx) data_1[,grep(xx, names(data_1))]/data_final$int30Total)
  new_4 <- as.data.frame(t(new_4))
  colnames(new_4) <- paste0('W_', colnames(new_4))
  data_final <- cbind(data_final, new_4)

  ranglist_pre <- c('pre_Agro_f10kt50k', 'pre_Agro_u10k', 'pre_Agro_o50k', 'pre_Com_f10kt50k', 'pre_Com_u10k', 'pre_Com_o50k', 'pre_Prod_f10kt50k', 'pre_Prod_u10k', 'pre_Prod_o50k', 'pre_Serv_f10kt50k', 'pre_Serv_u10k', 'pre_Serv_o50k')

  new_5 <- sapply(ranglist_pre, function(xx) data_1[,grep(xx, names(data_1))]/data_final$preTotal)
  new_5 <- as.data.frame(t(new_5))
  colnames(new_5) <- paste0('W_', colnames(new_5))
  data_final <- cbind(data_final, new_5)

  ranglist_pot <- c('pot_Agro_f10kt50k', 'pot_Agro_u10k', 'pot_Agro_o50k', 'pot_Com_f10kt50k', 'pot_Com_u10k', 'pot_Com_o50k', 'pot_Prod_f10kt50k', 'pot_Prod_u10k', 'pot_Prod_o50k', 'pot_Serv_f10kt50k', 'pot_Serv_u10k', 'pot_Serv_o50k')

  new_6 <- sapply(ranglist_pot, function(xx) data_1[,grep(xx, names(data_1))]/data_final$potTotal)
  new_6 <- as.data.frame(t(new_6))
  colnames(new_6) <- paste0('W_', colnames(new_6))
  data_final <- cbind(data_final, new_6)

  R1 <- mapply(function(x,y,z) (data_1[,grep(x, names(data_1))] - data_1[,grep(y, names(data_1))])/data_1[,grep(z, names(data_1))], x=ranglist_int00, y=ranglist_pot, z=ranglist_sal)
  R1 <- as.data.frame(t(R1))
  names(R1) <- substring(names(R1), 6)
  colnames(R1) <- paste0('R_int00_pot', colnames(R1))
  data_final <- cbind(data_final, R1)

  R2 <- mapply(function(x,y,z) (data_1[,grep(x, names(data_1))] - data_1[,grep(y, names(data_1))])/data_1[,grep(z, names(data_1))], x=ranglist_int30, y=ranglist_pot, z=ranglist_sal)
  R2 <- as.data.frame(t(R2))
  names(R2) <- substring(names(R2), 6)
  colnames(R2) <- paste0('R_int30_pot', colnames(R2))
  data_final <- cbind(data_final, R2)

  R3 <- mapply(function(x,y,z) (data_1[,grep(x, names(data_1))] - data_1[,grep(y, names(data_1))])/data_1[,grep(z, names(data_1))], x=ranglist_int00, y=ranglist_pre, z=ranglist_sal)
  R3 <- as.data.frame(t(R3))
  names(R3) <- substring(names(R3), 6)
  colnames(R3) <- paste0('R_int00_pre', colnames(R3))
  data_final <- cbind(data_final, R3)

  R4 <- mapply(function(x,y,z) (data_1[,grep(x, names(data_1))] - data_1[,grep(y, names(data_1))])/data_1[,grep(z, names(data_1))], x=ranglist_int30, y=ranglist_pre, z=ranglist_sal)
  R4 <- as.data.frame(t(R4))
  names(R4) <- substring(names(R4), 6)
  colnames(R4) <- paste0('R_int30_pre', colnames(R4))
  data_final <- cbind(data_final, R4)
  
  dlist[[i]] <- data_final


  # data_p <- bdc %>% 
  #   select(year, previsionmonto, previsionPot) %>% 
  #   mutate(year = lubridate::ymd(year)) %>% 
  #   group_by(year) %>% 
  #   summarise_all(sum)
  
}    

dataset <- data.frame(Reduce(rbind, dlist))
dataset <- dataset %>% 
  mutate(panel2 = paste0(panel, '_', agencia))

data_ret <- dataset %>% 
  select(year, starts_with('R_int00_pot'))

data_wgt <- dataset %>% 
  select(year, starts_with('W_sal')) %>% 
  pivot_longer(starts_with('W_'), names_to = 'Id', values_to = 'valor') %>% 
  mutate(valor = as.numeric(valor))


data_r <- data_ret %>% 
  pivot_longer(starts_with('R_int00_pot'), names_to = 'ID', values_to = 'valor') %>% 
  mutate(valor = as.numeric(valor)) %>% 
  arrange(year) %>% 
  mutate(fecha = year) %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  dplyr::filter(!between(year, as.Date('2020-04-07'), as.Date('2021-01-31'))) %>%
  mutate(fecha = fifelse(fecha>as.Date('2020-04-07'), fecha - 301, fecha))

data_r2 <- data_r %>% 
  pivot_wider(names_from = ID, values_from = valor) %>% 
  select(-year)

data_w <- dataset %>% 
  dplyr::filter(year == max(year)) %>% 
  select(starts_with('W_int00'))

write.csv(data_r2, 'C:/!CIDRE/R_outputs/rendimientos.csv')
write.csv(data_w, 'C:/!CIDRE/R_outputs/pesos.csv')

fig_r <- plot_ly(data_r, x=~fecha, y=~valor, color=~ID, mode='lines', type = 'scatter')
fig_r


fig <- ggplot(data_r, aes(x=year, y=valor, colors=ID)) + geom_line() + geom_smooth()
fig

fig_w <- plot_ly(data_wgt, x=~year, y=~valor, color=~Id, mode ='none', type = 'scatter', 
stackgroup = 'one', groupnorm = 'percent')
fig_w

#----------------------------------------------------------------------------------------------------

enero <- dataset %>% 
  select(year, int00_Agro_f10kt50k, pot_Agro_f10kt50k, sal_Agro_f10kt50k) 


preflist <- c('sal', 'int00', 'int30', 'pre', 'pot')
new <- sapply(preflist, function(xx) rowSums(dataset[,grep(xx, names(dataset)), drop=FALSE]))
new <- as.data.frame(new)
colnames(new) <- paste0(colnames(new), 'Total')
dataset <- cbind(dataset, new)

ranglist_sal <- c('sal_Agro_f10kt50k', 'sal_Agro_u10k', 'sal_Agro_o50k', 'sal_Com_f10kt50k', 'sal_Com_u10k', 'sal_Com_o50k', 'sal_Prod_f10kt50k', 'sal_Prod_u10k', 'sal_Prod_o50k', 'sal_Serv_f10kt50k', 'sal_Serv_u10k', 'sal_Serv_o50k')

new_2 <- sapply(ranglist_sal, function(xx) dataset[,grep(xx, names(dataset))]/dataset$salTotal)
new_2 <- as.data.frame(new_2)
colnames(new_2) <- paste0('W_', colnames(new_2))
dataset <- cbind(dataset, new_2)

ranglist_int00 <- c('int00_Agro_f10kt50k', 'int00_Agro_u10k', 'int00_Agro_o50k', 'int00_Com_f10kt50k', 'int00_Com_u10k', 'int00_Com_o50k', 'int00_Prod_f10kt50k', 'int00_Prod_u10k', 'int00_Prod_o50k', 'int00_Serv_f10kt50k', 'int00_Serv_u10k', 'int00_Serv_o50k')

new_3 <- sapply(ranglist_int00, function(xx) dataset[,grep(xx, names(dataset))]/dataset$int00Total)
new_3 <- as.data.frame(new_3)
colnames(new_3) <- paste0('W_', colnames(new_3))
dataset <- cbind(dataset, new_3)

ranglist_pot <- c('pot_Agro_f10kt50k', 'pot_Agro_u10k', 'pot_Agro_o50k', 'pot_Com_f10kt50k', 'pot_Com_u10k', 'pot_Com_o50k', 'pot_Prod_f10kt50k', 'pot_Prod_u10k', 'pot_Prod_o50k', 'pot_Serv_f10kt50k', 'pot_Serv_u10k', 'pot_Serv_o50k')


R1 <- mapply(function(x,y,z) (dataset[,grep(x, names(dataset))] - dataset[,grep(y, names(dataset))])/dataset[,grep(z, names(dataset))], x=ranglist_int00, y=ranglist_pot, z=ranglist_sal)
R1 <- as.data.frame(R1)
names(R1) <- substring(names(R1), 6)
colnames(R1) <- paste0('R_int00_pot', colnames(R1))
dataset <- cbind(dataset, R1)

df <- data.frame(Reduce(rbind, dlist))

data <- df %>% 
  pivot_longer(!year, names_to = 'id', values_to = 'prev')

fig <- plot_ly(data, x=~year, y=~prev, color=~id, mode='lines')
fig

#---------------------------------------------------------------------------    
data <- read_csv('C:/!CIDRE/BaseDeCartera/csvDaily/base20201217.csv')  
data <- data %>%
  dplyr::filter(!str_detect(estadocredito, "CANCEL") & !str_detect(estadocredito, 'CASTIG')) %>%     
  dplyr::filter(!str_detect(productocrediticio, "BDP") & !str_detect(productocrediticio, 'COR')) %>% 
  mutate(saldo = as.numeric(saldo)) %>% 
  mutate(montodesembolsado = as.numeric(montodesembolsado)) %>%
  mutate(saldo = ifelse(str_detect(agencia, 'MONTEAGUDO') & str_detect(financiador, 'FONDECO'), saldo*0.9, saldo)) %>% 
  #mutate(Dfechaultimodesembolso = lubridate::dmy(fechaultimodesembolso)) %>%
  mutate(municipio = ifelse(str_detect(municipio, 'LA PAZ (MURILLO)'), 'LA PAZ',
                            ifelse(str_detect(municipio, 'ENTRERIOS'), 'ENTRE RIOS',
                                   ifelse(str_detect(municipio, 'EUCXALIPTUS'), 'EUCALIPTUS',
                                          ifelse(str_detect(municipio, 'PUERTO CARABUCO'), 'LA PAZ',
                                                 ifelse(str_detect(municipio, 'AYOPAYA (VILLA INDEPENDENCIA)'), 'INDEPENDENCIA',
                                                        ifelse(str_detect(municipio, 'PUERTO GONZALO MORENO'), 'PUERTO GONZALES MORENO',
                                                               ifelse(str_detect(municipio, 'JUAN JOSE PEREZ(CHARAZANI)'), 'CHARAZANI',
                                                                      ifelse(str_detect(municipio, 'SALINAS DE GARCI MENDOZA'), 'SALINAS DE GARCIA MENDOZA',
                                                                             ifelse(str_detect(municipio, 'SAN PEDRO DE BUENA VISTA'), 'S.P. DE BUENA VISTA',
                                                                                    ifelse(str_detect(municipio, 'SAN PEDRO DE CURAHUARA'), 'SAN PEDRO CUARAHUARA',
                                                                                           ifelse(str_detect(municipio, 'SANTIAGO DE HUARI'), 'HUARI',
                                                                                                  ifelse(str_detect(municipio, 'SANTUARIO DE QUILLACAS'), 'QUILLACAS',
                                                                                                         ifelse(str_detect(municipio, 'TOCO'), 'TOKO',
                                                                                                                ifelse(str_detect(municipio, 'TOTORA') & str_detect(departamento, 'ORURO'), 'SAN PEDRO DE TOTORA',
                                                                                                                       ifelse(str_detect(municipio, 'ANCORAINES'), 'ANCORAIMES',
                                                                                                                              ifelse(str_detect(municipio, 'ASCENCIO DE GUARAYOS'), 'ASCENCION DE GUARAYOS',
                                                                                                                                     ifelse(str_detect(municipio, 'SAAVEDRA')& str_detect(departamento, 'SANTA CRUZ'), 'GRAL. SAAVEDRA',municipio)))))))))))))))))) %>% 
  left_join(asfiUR, by = c('departamento', 'municipio')) %>% 
  mutate(sectorruralurbano = ifelse(!is.na(clasifAsfiUR), clasifAsfiUR, sectorruralurbano)) %>% 
  mutate(sectorruralurbano = ifelse(sectorruralurbano == 'U', 'URBANO', 
                                    ifelse(sectorruralurbano == 'R', 'RURAL', sectorruralurbano))) %>%
  mutate(credFid = 'RRPP') %>% 
  mutate(credFid = ifelse(str_detect(productocrediticio, 'BDP') |str_detect(productocrediticio, 'FID'), 
                          'FID', credFid)) %>% 
  mutate(codGarantia = substr(tipogarantiaprincipal, 1, 3 )) %>%
  mutate(codGarantia = ifelse(codGarantia == "NO ", "IPN", codGarantia)) %>% 
  mutate(Garantia = ifelse(codGarantia == 'HI1'  | str_detect(codGarantia, 'HO1') |
                             str_detect(codGarantia, 'HR1')  | str_detect(codGarantia, 'HT1'), 'Hipotecaria de vivienda',
                           ifelse(str_detect(codGarantia, 'HI2')  | str_detect(codGarantia, 'HO2') |
                                    str_detect(codGarantia, 'HR2')  | str_detect(codGarantia, 'HT2') |
                                    str_detect(codGarantia, 'HN')   | str_detect(codGarantia, 'HV'), 'Otras hipotecas',
                                  ifelse(str_detect(codGarantia, 'P03')  | str_detect(codGarantia, 'P04') |
                                           str_detect(codGarantia, 'PD3')  | str_detect(codGarantia, 'PD4'), 'Prend. Maquinaria', 
                                         ifelse(str_detect(codGarantia, 'IPJ')  | str_detect(codGarantia, 'IPN')  | 
                                                  str_detect(tipogarantiaprincipal, 'IPN'), 'Personal',       
                                                ifelse(str_detect(codGarantia, 'OT1')  | str_detect(codGarantia, 'P09'), 'Semovientes y Docs. en custodia',       
                                                       ifelse(str_detect(codGarantia, 'P05')  | str_detect(codGarantia, 'P02') |
                                                                str_detect(codGarantia, 'OT9')  | str_detect(codGarantia, 'IPQ') |
                                                                str_detect(codGarantia, 'P01')  | str_detect(codGarantia, 'P06'), 'Quirografaria y otras prendas',      
                                                              'Sin Registro'))))))) %>% 
  mutate(Destino = ifelse(str_detect(productocrediticio, 'AGRO'), 'Agropecuario' ,
                          ifelse(str_detect(productocrediticio, 'VBI'), 'Productivo',
                                 ifelse(str_detect(productocrediticio, 'VIVIENDA'), 'Vivienda',
                                        ifelse(str_detect(productocrediticio, 'NO PRODUCTIVO') & str_detect(grupocaedeccredito, 'H,') , 'Comercio',
                                               ifelse(str_detect(productocrediticio, 'NO PRODUCTIVO') & !str_detect(grupocaedeccredito, 'H,') , 'Servicios',
                                                      ifelse(str_detect(productocrediticio, 'CONSUMO'), 'Consumo', 'Productivo'))))))) %>%
  mutate(Rango_Desembolso = ifelse(montodesembolsado <= 35000 & tipomoneda == 'Bs.', '1. Menor a 5K',
                                   ifelse(montodesembolsado <= 5102  & tipomoneda != 'Bs.', '1. Menor a 5K', 
                                          ifelse(montodesembolsado > 35000  & montodesembolsado <= 70000  & tipomoneda == 'Bs.', '2. 5K - 10K',
                                                 ifelse(montodesembolsado > 5102   & montodesembolsado <= 10204  & tipomoneda != 'Bs.', '2. 5K - 10K',
                                                        ifelse(montodesembolsado > 70000  & montodesembolsado <= 140000 & tipomoneda == 'Bs.', '3. 10K - 20K',
                                                               ifelse(montodesembolsado > 10204  & montodesembolsado <= 20408  & tipomoneda != 'Bs.', '3. 10K - 20K',
                                                                      ifelse(montodesembolsado > 140000 & montodesembolsado <= 350000 & tipomoneda == 'Bs.', '4. 20K - 50K',
                                                                             ifelse(montodesembolsado > 20408  & montodesembolsado <= 51020  & tipomoneda != 'Bs.', '4. 20K - 50K',
                                                                                    ifelse(montodesembolsado > 350000 & montodesembolsado <= 700000 & tipomoneda == 'Bs.', '5. 50K - 100K',
                                                                                           ifelse(montodesembolsado > 51020  & montodesembolsado <= 102040 & tipomoneda != 'Bs.', '5. 50K - 100K',
                                                                                                  ifelse(montodesembolsado > 700000 & tipomoneda == 'Bs.', '6. 100K+',
                                                                                                         ifelse(montodesembolsado > 102040 & tipomoneda != 'Bs.', '6. 100K+', 'NA'))))))))))))) %>%
  mutate(agencia = ifelse(str_detect(agencia, 'POTOSI'), 'REG. POTOSI',
                          ifelse(str_detect(agencia, 'SUCRE'), 'REG. CHUQSACA',
                                 ifelse(str_detect(agencia, 'SAN IGNACIO'), 'SAN IGNACIO',
                                        ifelse(str_detect(agencia, 'SATELITE'), 'AG. SATELITE NORTE',
                                               ifelse(str_detect(agencia, 'STA CRUZ'), 'REG. STA CRUZ',
                                                      ifelse(str_detect(agencia, 'REG. SANTA CRUZ'), 'REG. STA CRUZ', agencia))))))) %>%
  mutate(agencia = gsub('SUC. ', 'REG. ', agencia)) %>%
  mutate(saldo = ifelse(str_detect(tipomoneda, 'Bs'), saldo/6.86, saldo)) %>%
  mutate(montodesembolsado = ifelse(str_detect(tipomoneda, 'Bs'), montodesembolsado/6.86, montodesembolsado)) %>%
  mutate(saldoMora = ifelse(!str_detect(estadocredito, 'VIGENTE'), saldo, NA)) %>%
  mutate(saldoMora_PAR0 = ifelse(diasenmora > 0, saldo, NA)) %>%
  mutate(saldoVigente = ifelse(str_detect(estadocredito, 'VIGENTE'), saldo, NA)) %>%
  mutate(saldoReprog = ifelse(str_detect(estadocredito, 'REPROG'), saldo, NA)) %>%
  mutate(saldoReprogMora = ifelse(str_detect(estadocredito, 'VENCIDO-REPROG') | str_detect(estadocredito, 'EJECUCION-REPROG'), saldo, NA)) %>%
  # mutate(previsionmonto = ifelse(str_detect(tipomoneda, 'Bs'), previsionmonto/6.86, previsionmonto)) %>%
  mutate(montoacapitalproxpago = ifelse(str_detect(tipomoneda, 'Bs'), montoacapitalproxpago/6.86, montoacapitalproxpago)) %>%
  mutate(ultpagoacapital = ifelse(str_detect(tipomoneda, 'Bs'), ultpagoacapital/6.86, ultpagoacapital)) %>%
  # mutate(fechaproxpago  = as.Date(fechaproxpago, format = "%d/%m/%Y")) %>%
  # mutate(fechadeclaraciondelcliente  = as.Date(fechadeclaraciondelcliente, format = "%d/%m/%Y")) %>%
  mutate(Dfechaultimodesembolso  = as.Date(fechaultimodesembolso, format = "%d/%m/%Y")) %>%
  # mutate(fechaprimerdesembolso  = as.Date(fechaprimerdesembolso, format = "%d/%m/%Y")) %>%
  # mutate(fechaultpagocapital  = as.Date(fechaultpagocapital, format = "%d/%m/%Y")) %>%
  # mutate(mondes = as.yearmon(fechaultimodesembolso)) %>%
  # mutate(monrec = as.yearmon(fechaultpagocapital)) %>%
  mutate(Dfechadeclaraciondelcliente = as.Date(fechadeclaraciondelcliente, format = "%d/%m/%Y")) %>%
  mutate(Dfechadeclaraciondelcliente2 = as.Date(fechadeclaraciondelcliente, format = "%d/%m/%Y")) %>%
  mutate(yeardec = year(Dfechadeclaraciondelcliente)) %>% 
  mutate(tc = 6.86) %>% 
  mutate(totalpasivos = as.numeric(totalpasivos)) %>% 
  mutate(totalpasivos = ifelse(is.na(totalpasivos), 0,totalpasivos)) %>%
  mutate(totalactivos = ifelse(!str_detect(moneda, 'Bs'), totalactivos*tc, totalactivos),
         totalpasivos = ifelse(!str_detect(moneda, 'Bs'), totalpasivos*tc, totalpasivos),
         patrimonio = ifelse(!str_detect(moneda, 'Bs'), patrimonio*tc, patrimonio),
         ingresofijo = ifelse(!str_detect(moneda, 'Bs'), ingresofijo*tc, ingresofijo),
         ingresoporventas = ifelse(!str_detect(moneda, 'Bs'), ingresoporventas*tc, ingresoporventas),
         ingresovariable = ifelse(!str_detect(moneda, 'Bs'), ingresovariable*tc, ingresovariable),
         gastosfijos = ifelse(!str_detect(moneda, 'Bs'), gastosfijos*tc, gastosfijos),
         gastosvariables = ifelse(!str_detect(moneda, 'Bs'), gastosvariables*tc, gastosvariables)) %>% 
  mutate(ingresofijo = replace(ingresofijo, is.na(ingresofijo), 0)) %>%
  mutate(ingresoporventas = replace(ingresoporventas, is.na(ingresoporventas), 0)) %>%
  mutate(gastosfijos = replace(gastosfijos, is.na(gastosfijos), 0)) %>%
  mutate(gastosvariables = replace(gastosvariables, is.na(gastosvariables), 0)) %>%
  # mutate(ingresofijo = case_when(ingresofijo=='NA' & ingresoporventas!='NA' ~  0, TRUE~ingresofijo)) %>%
  # mutate(ingresoporventas = case_when(ingresoporventas=='NA' & ingresofijo!='NA' ~  0, TRUE~ingresoporventas)) %>%
  # mutate(gastosfijos = case_when(gastosfijos=='NA' & gastosvariables!='NA' ~  0, TRUE~gastosfijos)) %>%
  # mutate(gastosvariables = case_when(gastosvariables=='NA' & gastosfijos!='NA' ~  0, TRUE~gastosvariables)) %>%
  # case_when(ingresofijo!='NA' & ingresoporventas=='NA' ~ ingresoporventas==0) %>%
  # case_when(gastosfijos=='NA' & gastosvariables!='NA' ~ gastosfijos==0) %>%
  # case_when(gastosfijos!='NA' & gastosvariables=='NA' ~ gastosvariables==0) %>%
  mutate(ytot = ingresofijo + ingresoporventas) %>% 
  mutate(etot = gastosfijos + gastosvariables) %>%
  mutate(previsionmonto = ifelse(str_detect(tipomoneda, 'Bs'), previsionmonto/6.86, previsionmonto)) %>% 
  mutate(Garantia = ifelse(is.na(Garantia), 'personal', Garantia)) %>% 
  mutate(califPotVivienda = ifelse(diasenmora <= 30 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'A',
                                   ifelse(diasenmora >= 31 & diasenmora <= 90 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'B',
                                          ifelse(diasenmora >= 91 & diasenmora <= 180 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'C',
                                                 ifelse(diasenmora >= 181 & diasenmora <= 270 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'D',
                                                        ifelse(diasenmora >= 271 & diasenmora <= 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'E',
                                                               ifelse(diasenmora > 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'F', 'NA'))))))) %>%
  mutate(califPot = ifelse(diasenmora <= 30 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'A',
                           ifelse(diasenmora >= 31 & diasenmora <= 90 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'B',
                                  ifelse(diasenmora >= 91 & diasenmora <= 180 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'C',
                                         ifelse(diasenmora >= 181 & diasenmora <= 270 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'D',
                                                ifelse(diasenmora >= 271 & diasenmora <= 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'E',
                                                       ifelse(diasenmora > 360 & str_detect(categoriadecalificacion, 'VIVIENDA'), 'F',
                                                              ifelse(diasenmora <= 20 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'A',
                                                                     ifelse(diasenmora >= 21 & diasenmora <= 30 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'B',
                                                                            ifelse(diasenmora >= 31 & diasenmora <= 56 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'C',
                                                                                   ifelse(diasenmora >= 57 & diasenmora <= 76 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'D',
                                                                                          ifelse(diasenmora >= 77 & diasenmora <= 90 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'E',
                                                                                                 ifelse(diasenmora > 90 & str_detect(categoriadecalificacion, 'AGROPECUARIO'), 'F',
                                                                                                        ifelse(diasenmora <=5 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'A',
                                                                                                               ifelse(diasenmora >=6 & diasenmora <=30 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'B',
                                                                                                                      ifelse(diasenmora >=31 & diasenmora <=55 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'C',
                                                                                                                             ifelse(diasenmora >=56 & diasenmora <=75 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'D',
                                                                                                                                    ifelse(diasenmora >=76 & diasenmora <=90 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'E',
                                                                                                                                           ifelse(diasenmora >90 & !str_detect(categoriadecalificacion, 'AGROPECUARIO') & !str_detect(categoriadecalificacion, 'VIVIENDA'), 'F', 'NA'))))))))))))))))))) %>%
  mutate(contagio = ifelse(cantidadcreditos > 1, 1, 0)) %>% 
  group_by(nrocliente) %>% 
  mutate(contagio = mean(contagio)) %>% 
  mutate(maxCalif = max(califPot)) %>% 
  ungroup() %>% 
  #% prevision
  mutate(pctPreviPot = ifelse(maxCalif == 'A' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0,
                              ifelse(maxCalif == 'B' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.025,
                                     ifelse(maxCalif == 'C' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.2,
                                            ifelse(maxCalif == 'D' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.5,
                                                   ifelse(maxCalif == 'E' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.8,
                                                          ifelse(maxCalif == 'F' & str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 1,
                                                                 ifelse(maxCalif == 'A' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.0025,
                                                                        ifelse(maxCalif == 'B' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.05,
                                                                               ifelse(maxCalif == 'C' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                      ifelse(maxCalif == 'D' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                             ifelse(maxCalif == 'E' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                    ifelse(maxCalif == 'F' & !str_detect(sectorcaedeccredito, 'PRODUCCION') & str_detect(tipomoneda, 'Bs'), 1,
                                                                                                           ifelse(maxCalif == 'A' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.0025,
                                                                                                                  ifelse(maxCalif == 'B' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.05,
                                                                                                                         ifelse(maxCalif == 'C' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                                                                ifelse(maxCalif == 'D' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                                                                       ifelse(maxCalif == 'E' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                                                              ifelse(maxCalif == 'F' & str_detect(sectorcaedeccredito, 'VIVIENDA') & str_detect(tipomoneda, 'Bs'), 1, 
                                                                                                                                                     ifelse(maxCalif == 'A' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.025,
                                                                                                                                                            ifelse(maxCalif == 'B' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.05,
                                                                                                                                                                   ifelse(maxCalif == 'C' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                                                                                                          ifelse(maxCalif == 'D' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                                                                                                                 ifelse(maxCalif == 'E' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                                                                                                        ifelse(maxCalif == 'F' & !str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(sectorcaedeccredito, 'CONSUMO') & !str_detect(tipomoneda, 'Bs'), 1,
                                                                                                                                                                                               ifelse(maxCalif == 'A' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.025,
                                                                                                                                                                                                      ifelse(maxCalif == 'B' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.05,
                                                                                                                                                                                                             ifelse(maxCalif == 'C' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.2,
                                                                                                                                                                                                                    ifelse(maxCalif == 'D' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.5,
                                                                                                                                                                                                                           ifelse(maxCalif == 'E' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 0.8,
                                                                                                                                                                                                                                  ifelse(maxCalif == 'F' & str_detect(sectorcaedeccredito, 'VIVIENDA') & !str_detect(tipomoneda, 'Bs'), 1, 'NA'))))))))))))))))))))))))))))))) %>% 
  mutate(M = ifelse(saldo < valorgarantiashipotecariasus, saldo, valorgarantiashipotecariasus)) %>%
  mutate(califPotCode = ifelse(califPot == 'A', 1, 
                               ifelse(califPot == 'B', 2,
                                      ifelse(califPot == 'C', 3,
                                             ifelse(califPot == 'D', 4,
                                                    ifelse(califPot == 'E', 5,
                                                           ifelse(califPot == 'F', 6, 'NA'))))))) %>%
  mutate(pctPreviPot = as.numeric(pctPreviPot)) %>% 
  #mutate(previsionPot = saldo * pctPreviPot) %>% 
  mutate(previsionPot = ifelse(diasenmora <=0, previsionmonto,
                          ifelse(Garantia != 'Hipotecaria de vivienda'  & diasenmora >= 1, saldo * pctPreviPot,
                            ifelse(Garantia == 'Hipotecaria de vivienda' & diasenmora >= 1, pctPreviPot * (saldo - 0.5*M), previsionmonto)))) 



data_p <- data %>% 
  select(nrocliente, nrocredito, calificacion, califPot, maxCalif, califPotVivienda, califPotCode, tipomoneda, diasenmora, categoriadecalificacion, sectorcaedeccredito,
         porcentajedeprevision, previsionmonto, previsionPot, pctPreviPot, contagio, saldo, Garantia, M, valorgarantiashipotecariasus) %>% 
  mutate(deterioro = ifelse(califPot>calificacion,1,0))   
  dplyr::filter(nrocliente == '98017')

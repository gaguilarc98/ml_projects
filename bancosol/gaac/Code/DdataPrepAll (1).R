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

# Data in
N_Campos <- readxl::read_excel("C:/!bso/girCartera/Clasificacion_Sector_Economico_Modificado.xls", sheet = "subclase") %>% 
  select(CAEDEC_DEST, cat)
ventamm <- readxl::read_excel("C:/!bso/girCartera/ventamm2.xlsx") 
########################################################################################
bdcList <- list()
mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2015', '2016', '2017')
for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
    
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                             mos2[i], years[k],'.txt'), 
                      encoding = 'Latin-1', sep = '|') %>% 
           left_join(N_Campos, by="CAEDEC_DEST") %>% 
        # left_join(ventamm, by="CAEDEC_DEST") %>% 
    
        dplyr::filter(MODULO != 131) %>% 
        mutate(fbase = paste0(mos2[i], years[k])) %>% 
        mutate(fdes = dmy(FDESEMBOLSO)) %>% 
        mutate(cosechaY = year(fdes)) %>% 
        mutate(cosechaM = as.yearmon(fdes)) %>% 
        mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
        mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
        mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
        mutate(catMora = case_when(as.numeric(DIASMORA) == 0~'1. 0 dias',
                                   as.numeric(DIASMORA) > 0 & as.numeric(DIASMORA) <=5 ~ '2.1-5 dias',
                                   as.numeric(DIASMORA) > 5 & as.numeric(DIASMORA) <=30 ~ '3.6-30 dias',
                                   as.numeric(DIASMORA) > 30 & as.numeric(DIASMORA) <=90 ~ '4.31-90 dias',
                                   as.numeric(DIASMORA) > 90 ~ '5.90+ dias', TRUE~'NA')) %>% 
        mutate(mon = substr(fbase,1,3)) %>% 
        mutate(year = substr(fbase,4,7)) %>% 
        mutate(mes = case_when(mon == 'Ene'~'jan',
                               mon == 'Feb'~'feb',
                               mon == 'Mar'~'mar',
                               mon == 'Abr'~'apr',
                               mon == 'May'~'may',
                               mon == 'Jun'~'jun',
                               mon == 'Jul'~'jul',
                               mon == 'Ago'~'aug',
                               mon == 'Sep'~'sep',
                               mon == 'Oct'~'oct',
                               mon == 'Nov'~'nov',
                               mon == 'Dic'~'dec',)) %>% 
        mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
        mutate(monDate = as.yearmon(dayDate)) %>% 
        mutate(montous = ifelse(monDate != cosechaM, 0, montous)) %>% 
        mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
        mutate(CAEDEC_DEST = ifelse(str_length(CAEDEC_DEST) == 4, 
                                    paste0('0', CAEDEC_DEST), CAEDEC_DEST)) %>% 
        mutate(CIU = as.character(CIU)) %>% 
        mutate(CIU = ifelse(str_length(CIU) == 4, 
                                    paste0('0', CIU), CIU)) %>% 
        mutate(divCaedecD = substr(CAEDEC_DEST,1,2)) %>% 
        mutate(grupoCaedecD = case_when(divCaedecD == '01'~'A',
                                       divCaedecD == '02'~'B',
                                       divCaedecD == '03'~'B',
                                       divCaedecD == '05'~'B',
                                       divCaedecD == '11'~'C',
                                       divCaedecD == '10'~'D',
                                       divCaedecD == '11'~'D',
                                       divCaedecD == '12'~'D',
                                       divCaedecD == '13'~'D',
                                       divCaedecD == '14'~'D',
                                       divCaedecD == '15'~'E',
                                       divCaedecD == '16'~'E',
                                       divCaedecD == '17'~'E',
                                       divCaedecD == '18'~'E',
                                       divCaedecD == '19'~'E',
                                       divCaedecD == '20'~'E',
                                       divCaedecD == '21'~'E',
                                       divCaedecD == '22'~'E',
                                       divCaedecD == '23'~'E',
                                       divCaedecD == '24'~'E',
                                       divCaedecD == '25'~'E',
                                       divCaedecD == '26'~'E',
                                       divCaedecD == '27'~'E',
                                       divCaedecD == '28'~'E',
                                       divCaedecD == '29'~'E',
                                       divCaedecD == '30'~'E',
                                       divCaedecD == '31'~'E',
                                       divCaedecD == '32'~'E',
                                       divCaedecD == '33'~'E',
                                       divCaedecD == '34'~'E',
                                       divCaedecD == '35'~'E',
                                       divCaedecD == '36'~'E',
                                       divCaedecD == '37'~'E',
                                       divCaedecD == '40'~'F',
                                       divCaedecD == '41'~'F',
                                       divCaedecD == '45'~'G',
                                       divCaedecD == '50'~'H',
                                       divCaedecD == '51'~'H',
                                       divCaedecD == '52'~'H',
                                       divCaedecD == '55'~'I',
                                       divCaedecD == '60'~'J',
                                       divCaedecD == '61'~'J',
                                       divCaedecD == '62'~'J',
                                       divCaedecD == '63'~'J',
                                       divCaedecD == '64'~'J',
                                       divCaedecD == '65'~'K',
                                       divCaedecD == '66'~'K',
                                       divCaedecD == '67'~'K',
                                       divCaedecD == '70'~'L',
                                       divCaedecD == '71'~'L',
                                       divCaedecD == '72'~'L',
                                       divCaedecD == '73'~'L',
                                       divCaedecD == '74'~'L',
                                       divCaedecD == '75'~'M',
                                       divCaedecD == '80'~'N',
                                       divCaedecD == '85'~'O',
                                       divCaedecD == '90'~'O',
                                       divCaedecD == '91'~'O',
                                       divCaedecD == '92'~'O',
                                       divCaedecD == '93'~'O',
                                       divCaedecD == '95'~'P',
                                       divCaedecD == '98'~'Q',
                                       divCaedecD == '99'~'Z',)) %>% 
        mutate(caedec3dD = case_when(grupoCaedecD == 'A'~'5.Productivo GDI',
                                    grupoCaedecD == 'B'~'5.Productivo GDI',
                                    grupoCaedecD == 'C'~'4.Productivo GDE',
                                    grupoCaedecD == 'D'~'4.Productivo GDE',
                                    grupoCaedecD == 'E'~'5.Productivo GDI',
                                    grupoCaedecD == 'F'~'4.Productivo GDE',
                                    grupoCaedecD == 'G'~'3.Productivo VIV',
                                    grupoCaedecD == 'H'~'2.Comercio',
                                    grupoCaedecD == 'I'~'1.Servicios',
                                    grupoCaedecD == 'J'~'1.Servicios',
                                    grupoCaedecD == 'K'~'1.Servicios',
                                    grupoCaedecD == 'L'~'1.Servicios',
                                    grupoCaedecD == 'M'~'1.Servicios',
                                    grupoCaedecD == 'N'~'1.Servicios',
                                    grupoCaedecD == 'O'~'1.Servicios',
                                    grupoCaedecD == 'P'~'1.Servicios',
                                    grupoCaedecD == 'Q'~'1.Servicios',
                                    grupoCaedecD == 'Z'~'1.Servicios',)) %>% 
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
        mutate(caedec3dC = case_when(grupoCaedecC == 'A'~'5.Productivo GDI',
                                     grupoCaedecC == 'B'~'5.Productivo GDI',
                                     grupoCaedecC == 'C'~'4.Productivo GDE',
                                     grupoCaedecC == 'D'~'4.Productivo GDE',
                                     grupoCaedecC == 'E'~'5.Productivo GDI',
                                     grupoCaedecC == 'F'~'4.Productivo GDE',
                                     grupoCaedecC == 'G'~'3.Productivo VIV',
                                     grupoCaedecC == 'H'~'2.Comercio',
                                     grupoCaedecC == 'I'~'1.Servicios',
                                     grupoCaedecC == 'J'~'1.Servicios',
                                     grupoCaedecC == 'K'~'1.Servicios',
                                     grupoCaedecC == 'L'~'1.Servicios',
                                     grupoCaedecC == 'M'~'1.Servicios',
                                     grupoCaedecC == 'N'~'1.Servicios',
                                     grupoCaedecC == 'O'~'1.Servicios',
                                     grupoCaedecC == 'P'~'1.Servicios',
                                     grupoCaedecC == 'Q'~'1.Servicios',
                                     grupoCaedecC == 'Z'~'1.Servicios',)) %>% 
        mutate(ctaCont = substr(RUBRO,1,3)) %>% 
        mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                     ctaCont == '134'~saldous,
                                     ctaCont == '136'~saldous,
                                     ctaCont == '137'~saldous,
                                     TRUE ~ 0)) %>% 
        mutate(saldoReprog = case_when(ctaCont == '135'~saldous,
                                       ctaCont == '136'~saldous,
                                       ctaCont == '137'~saldous,
                                       TRUE ~ 0)) %>% 
        mutate(saldoRepVig = ifelse(ctaCont == '135', saldous, 0)) %>%
        mutate(saldoRepMora = ifelse(MODULO == 121 & DIASMORA > 0 , saldous, 0)) %>% 
        mutate(saldoRepPaR0 = case_when(ctaCont == '136'~saldous,
                                        ctaCont == '137'~saldous,
                                        TRUE ~ 0)) %>% 
        mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIG'), saldous, 0)) %>% 
        mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
        mutate(car = saldoMora + saldoRepVig) %>% 
        mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
        mutate(labGrupoD = case_when(grupoCaedecD == 'A'~ 'A. Agricola',
                                    grupoCaedecD == 'B'~ 'B. Caza, Pesca',
                                    grupoCaedecD == 'C'~ 'C. Ext. Gas y Pet.',
                                    grupoCaedecD == 'D'~ 'D. Ext. Minerales',
                                    grupoCaedecD == 'E'~ 'E. Ind. y Manu.',
                                    grupoCaedecD == 'F'~ 'F. Dist. EE y agua',
                                    grupoCaedecD == 'G'~ 'G. Construcción',
                                    grupoCaedecD == 'H'~ 'H. Comercio',
                                    grupoCaedecD == 'I'~ 'I. Hoteles',
                                    grupoCaedecD == 'J'~ 'J. Transporte',
                                    grupoCaedecD == 'K'~ 'K. Inter. Fin.',
                                    grupoCaedecD == 'L'~ 'L. Serv. Inmob.',
                                    grupoCaedecD == 'M'~ 'M. Adm. Pública',
                                    grupoCaedecD == 'N'~ 'N. Educación',
                                    grupoCaedecD == 'O'~ 'O. Serv. Hosp + Otros',
                                    grupoCaedecD == 'P'~ 'P. Serv. Doméstico',
                                    grupoCaedecD == 'Q'~ 'Q. Org. Extraterritoriales',
                                    grupoCaedecD == 'Z'~ 'Z. Jubilados, Est. y AC',
                                    TRUE ~ 'Otros')) %>% 
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
        mutate(SECTOR_CARTERA = case_when(
          TIPO_CREDITO == "H0" | TIPO_CREDITO == "H1"| TIPO_CREDITO == "H2" ~ "6. VIVIENDA NO CONTROLADA",
          (TIPO_CREDITO == "H3" | TIPO_CREDITO == "H4") & MONEDA == 0 ~ "3. VIVIENDA CONTROLADA",
          TIPO_CREDITO == "N0" | TIPO_CREDITO == "N1"| TIPO_CREDITO == "N2" ~ "9. CONSUMO",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV =='Pequeña' & TASAACT<=7) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") ~ "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
          (CAEDEC_DEST==31600|CAEDEC_DEST==31700| CAEDEC_DEST==51508|CAEDEC_DEST==52592| CAEDEC_DEST==34400|CAEDEC_DEST==34500|
             CAEDEC_DEST==50103) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") ~ "5. OTRA PRODUCCION NO CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") &
            cat=="H" ~ "7. COMERCIO" ,
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="I"|cat=="J"| cat=="K"|cat=="L"|
               cat=="M"|cat=="N"| cat=="O"|cat=="P"|
               cat=="Q"|cat=="Z") ~ "8. SERVICIOS",
          TRUE ~ "0" ))  %>%
        mutate(categ = case_when(SECTOR_CARTERA == '3. VIVIENDA CONTROLADA' ~ 'viviendaTC',
                                 SECTOR_CARTERA ==  '1. PRODUCCION AGROPECUARIA CONTROLADA' |
                                   SECTOR_CARTERA ==  '2. OTRA PRODUCCION CONTROLADA'~ 'productivoTC',
                                 SECTOR_CARTERA ==  '4. PRODUCCION AGROPECUARIA NO CONTROLADA ' |
                                   SECTOR_CARTERA ==  '5. OTRA PRODUCCION NO CONTROLADA' | 
                                   SECTOR_CARTERA ==  '6. VIVIENDA NO CONTROLADA'~ 'productivoTNC',))  %>%
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
        mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
        mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rangos = case_when(saldous < 500 ~'menor500USD',
                                  saldous > 500 & saldous <= 1000 ~'500-1k',
                                  saldous > 1000 & saldous <= 3000 ~'1k-3k',
                                  saldous > 3000 & saldous <= 5000 ~'3k-5k',
                                  saldous > 5000 & saldous <= 8000 ~'5k-8k',
                                  saldous > 8000 & saldous <= 10000 ~'8k-10k',
                                  saldous > 10000 & saldous <= 20000 ~'15k-20k',
                                  saldous > 20000 ~'mayor20k')) %>% 
                 
        mutate(rangom = case_when(montous < 500 ~'menor500USD',
                                  montous > 500 & saldous <= 1000 ~'500-1k',
                                  montous > 1000 & saldous <= 3000 ~'1k-3k',
                                  montous > 3000 & saldous <= 5000 ~'3k-5k',
                                  montous > 5000 & saldous <= 8000 ~'5k-8k',
                                  montous > 8000 & saldous <= 10000 ~'8k-10k',
                                  montous > 10000 & saldous <= 20000 ~'15k-20k',
                                  montous > 20000 ~'mayor20k')) %>% 
     
        mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
                                    substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
                                    substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
                                    substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
        mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
        mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal))
      saveRDS(bdc, paste0('c:/!bso/girCartera/rds/ec_',
                          mos2[i], years[k], '.rds'))
      bdcList[[i]] <- bdc
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
########################################################################################
bdcList <- list()
mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2018', '2019','2020')
for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                             mos2[i], years[k],'.txt'), 
                      encoding = 'Latin-1',fill = T) %>% 
        left_join(N_Campos, by="CAEDEC_DEST") %>% 
        left_join(ventamm, by="CAEDEC_DEST") %>%
        
        dplyr::filter(MODULO != 131) %>% 
        mutate(fbase = paste0(mos2[i], years[k])) %>% 
        mutate(fdes = dmy(FDESEMBOLSO)) %>% 
        mutate(cosechaY = year(fdes)) %>% 
        mutate(cosechaM = as.yearmon(fdes)) %>% 
        mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
        mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
        mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
        mutate(catMora = case_when(as.numeric(DIASMORA) == 0~'1. 0 dias',
                                   as.numeric(DIASMORA) > 0 & as.numeric(DIASMORA) <=5 ~ '2.1-5 dias',
                                   as.numeric(DIASMORA) > 5 & as.numeric(DIASMORA) <=30 ~ '3.6-30 dias',
                                   as.numeric(DIASMORA) > 30 & as.numeric(DIASMORA) <=90 ~ '4.31-90 dias',
                                   as.numeric(DIASMORA) > 90 ~ '5.90+ dias', TRUE~'NA')) %>% 
        mutate(mon = substr(fbase,1,3)) %>% 
        mutate(year = substr(fbase,4,7)) %>% 
        mutate(mes = case_when(mon == 'Ene'~'jan',
                               mon == 'Feb'~'feb',
                               mon == 'Mar'~'mar',
                               mon == 'Abr'~'apr',
                               mon == 'May'~'may',
                               mon == 'Jun'~'jun',
                               mon == 'Jul'~'jul',
                               mon == 'Ago'~'aug',
                               mon == 'Sep'~'sep',
                               mon == 'Oct'~'oct',
                               mon == 'Nov'~'nov',
                               mon == 'Dic'~'dec',)) %>% 
        mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
        mutate(monDate = as.yearmon(dayDate)) %>% 
        mutate(montous = ifelse(monDate != cosechaM, 0, montous)) %>% 
        mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
        mutate(CAEDEC_DEST = ifelse(str_length(CAEDEC_DEST) == 4, 
                                    paste0('0', CAEDEC_DEST), CAEDEC_DEST)) %>% 
        mutate(CIU = as.character(CIU)) %>% 
        mutate(CIU = ifelse(str_length(CIU) == 4, 
                            paste0('0', CIU), CIU)) %>% 
        mutate(divCaedecD = substr(CAEDEC_DEST,1,2)) %>% 
        mutate(grupoCaedecD = case_when(divCaedecD == '01'~'A',
                                        divCaedecD == '02'~'B',
                                        divCaedecD == '03'~'B',
                                        divCaedecD == '05'~'B',
                                        divCaedecD == '11'~'C',
                                        divCaedecD == '10'~'D',
                                        divCaedecD == '11'~'D',
                                        divCaedecD == '12'~'D',
                                        divCaedecD == '13'~'D',
                                        divCaedecD == '14'~'D',
                                        divCaedecD == '15'~'E',
                                        divCaedecD == '16'~'E',
                                        divCaedecD == '17'~'E',
                                        divCaedecD == '18'~'E',
                                        divCaedecD == '19'~'E',
                                        divCaedecD == '20'~'E',
                                        divCaedecD == '21'~'E',
                                        divCaedecD == '22'~'E',
                                        divCaedecD == '23'~'E',
                                        divCaedecD == '24'~'E',
                                        divCaedecD == '25'~'E',
                                        divCaedecD == '26'~'E',
                                        divCaedecD == '27'~'E',
                                        divCaedecD == '28'~'E',
                                        divCaedecD == '29'~'E',
                                        divCaedecD == '30'~'E',
                                        divCaedecD == '31'~'E',
                                        divCaedecD == '32'~'E',
                                        divCaedecD == '33'~'E',
                                        divCaedecD == '34'~'E',
                                        divCaedecD == '35'~'E',
                                        divCaedecD == '36'~'E',
                                        divCaedecD == '37'~'E',
                                        divCaedecD == '40'~'F',
                                        divCaedecD == '41'~'F',
                                        divCaedecD == '45'~'G',
                                        divCaedecD == '50'~'H',
                                        divCaedecD == '51'~'H',
                                        divCaedecD == '52'~'H',
                                        divCaedecD == '55'~'I',
                                        divCaedecD == '60'~'J',
                                        divCaedecD == '61'~'J',
                                        divCaedecD == '62'~'J',
                                        divCaedecD == '63'~'J',
                                        divCaedecD == '64'~'J',
                                        divCaedecD == '65'~'K',
                                        divCaedecD == '66'~'K',
                                        divCaedecD == '67'~'K',
                                        divCaedecD == '70'~'L',
                                        divCaedecD == '71'~'L',
                                        divCaedecD == '72'~'L',
                                        divCaedecD == '73'~'L',
                                        divCaedecD == '74'~'L',
                                        divCaedecD == '75'~'M',
                                        divCaedecD == '80'~'N',
                                        divCaedecD == '85'~'O',
                                        divCaedecD == '90'~'O',
                                        divCaedecD == '91'~'O',
                                        divCaedecD == '92'~'O',
                                        divCaedecD == '93'~'O',
                                        divCaedecD == '95'~'P',
                                        divCaedecD == '98'~'Q',
                                        divCaedecD == '99'~'Z',)) %>% 
        mutate(caedec3dD = case_when(grupoCaedecD == 'A'~'5.Productivo GDI',
                                     grupoCaedecD == 'B'~'5.Productivo GDI',
                                     grupoCaedecD == 'C'~'4.Productivo GDE',
                                     grupoCaedecD == 'D'~'4.Productivo GDE',
                                     grupoCaedecD == 'E'~'5.Productivo GDI',
                                     grupoCaedecD == 'F'~'4.Productivo GDE',
                                     grupoCaedecD == 'G'~'3.Productivo VIV',
                                     grupoCaedecD == 'H'~'2.Comercio',
                                     grupoCaedecD == 'I'~'1.Servicios',
                                     grupoCaedecD == 'J'~'1.Servicios',
                                     grupoCaedecD == 'K'~'1.Servicios',
                                     grupoCaedecD == 'L'~'1.Servicios',
                                     grupoCaedecD == 'M'~'1.Servicios',
                                     grupoCaedecD == 'N'~'1.Servicios',
                                     grupoCaedecD == 'O'~'1.Servicios',
                                     grupoCaedecD == 'P'~'1.Servicios',
                                     grupoCaedecD == 'Q'~'1.Servicios',
                                     grupoCaedecD == 'Z'~'1.Servicios',)) %>% 
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
        mutate(caedec3dC = case_when(grupoCaedecC == 'A'~'5.Productivo GDI',
                                     grupoCaedecC == 'B'~'5.Productivo GDI',
                                     grupoCaedecC == 'C'~'4.Productivo GDE',
                                     grupoCaedecC == 'D'~'4.Productivo GDE',
                                     grupoCaedecC == 'E'~'5.Productivo GDI',
                                     grupoCaedecC == 'F'~'4.Productivo GDE',
                                     grupoCaedecC == 'G'~'3.Productivo VIV',
                                     grupoCaedecC == 'H'~'2.Comercio',
                                     grupoCaedecC == 'I'~'1.Servicios',
                                     grupoCaedecC == 'J'~'1.Servicios',
                                     grupoCaedecC == 'K'~'1.Servicios',
                                     grupoCaedecC == 'L'~'1.Servicios',
                                     grupoCaedecC == 'M'~'1.Servicios',
                                     grupoCaedecC == 'N'~'1.Servicios',
                                     grupoCaedecC == 'O'~'1.Servicios',
                                     grupoCaedecC == 'P'~'1.Servicios',
                                     grupoCaedecC == 'Q'~'1.Servicios',
                                     grupoCaedecC == 'Z'~'1.Servicios',)) %>% 
        mutate(ctaCont = substr(RUBRO,1,3)) %>% 
        mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                     ctaCont == '134'~saldous,
                                     ctaCont == '136'~saldous,
                                     ctaCont == '137'~saldous,
                                     TRUE ~ 0)) %>% 
        mutate(saldoReprog = case_when(ctaCont == '135'~saldous,
                                       ctaCont == '136'~saldous,
                                       ctaCont == '137'~saldous,
                                       TRUE ~ 0)) %>% 
        mutate(saldoRepVig = ifelse(ctaCont == '135', saldous, 0)) %>%
        mutate(saldoRepMora = ifelse(MODULO == 121 & DIASMORA > 0 , saldous, 0)) %>% 
        mutate(saldoRepPaR0 = case_when(ctaCont == '136'~saldous,
                                        ctaCont == '137'~saldous,
                                        TRUE ~ 0)) %>% 
        mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIG'), saldous, 0)) %>% 
        mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
        mutate(car = saldoMora + saldoRepVig) %>% 
        mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
        mutate(labGrupoD = case_when(grupoCaedecD == 'A'~ 'A. Agricola',
                                     grupoCaedecD == 'B'~ 'B. Caza, Pesca',
                                     grupoCaedecD == 'C'~ 'C. Ext. Gas y Pet.',
                                     grupoCaedecD == 'D'~ 'D. Ext. Minerales',
                                     grupoCaedecD == 'E'~ 'E. Ind. y Manu.',
                                     grupoCaedecD == 'F'~ 'F. Dist. EE y agua',
                                     grupoCaedecD == 'G'~ 'G. Construcción',
                                     grupoCaedecD == 'H'~ 'H. Comercio',
                                     grupoCaedecD == 'I'~ 'I. Hoteles',
                                     grupoCaedecD == 'J'~ 'J. Transporte',
                                     grupoCaedecD == 'K'~ 'K. Inter. Fin.',
                                     grupoCaedecD == 'L'~ 'L. Serv. Inmob.',
                                     grupoCaedecD == 'M'~ 'M. Adm. Pública',
                                     grupoCaedecD == 'N'~ 'N. Educación',
                                     grupoCaedecD == 'O'~ 'O. Serv. Hosp + Otros',
                                     grupoCaedecD == 'P'~ 'P. Serv. Doméstico',
                                     grupoCaedecD == 'Q'~ 'Q. Org. Extraterritoriales',
                                     grupoCaedecD == 'Z'~ 'Z. Jubilados, Est. y AC',
                                     TRUE ~ 'Otros')) %>% 
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
        mutate(SECTOR_CARTERA = case_when(
          TIPO_CREDITO == "H0" | TIPO_CREDITO == "H1"| TIPO_CREDITO == "H2" ~ "6. VIVIENDA NO CONTROLADA",
          (TIPO_CREDITO == "H3" | TIPO_CREDITO == "H4") & MONEDA == 0 ~ "3. VIVIENDA CONTROLADA",
          TIPO_CREDITO == "N0" | TIPO_CREDITO == "N1"| TIPO_CREDITO == "N2" ~ "9. CONSUMO",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV =='Pequeña' & TASAACT<=7) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") ~ "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
          (CAEDEC_DEST==31600|CAEDEC_DEST==31700| CAEDEC_DEST==51508|CAEDEC_DEST==52592| CAEDEC_DEST==34400|CAEDEC_DEST==34500|
             CAEDEC_DEST==50103) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") ~ "5. OTRA PRODUCCION NO CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") &
            cat=="H" ~ "7. COMERCIO" ,
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="I"|cat=="J"| cat=="K"|cat=="L"|
               cat=="M"|cat=="N"| cat=="O"|cat=="P"|
               cat=="Q"|cat=="Z") ~ "8. SERVICIOS",
          TRUE ~ "0" ))  %>%
        mutate(categ = case_when(SECTOR_CARTERA == '3. VIVIENDA CONTROLADA' ~ 'viviendaTC',
                                 SECTOR_CARTERA ==  '1. PRODUCCION AGROPECUARIA CONTROLADA' |
                                   SECTOR_CARTERA ==  '2. OTRA PRODUCCION CONTROLADA'~ 'productivoTC',
                                 SECTOR_CARTERA ==  '4. PRODUCCION AGROPECUARIA NO CONTROLADA ' |
                                   SECTOR_CARTERA ==  '5. OTRA PRODUCCION NO CONTROLADA' | 
                                   SECTOR_CARTERA ==  '6. VIVIENDA NO CONTROLADA'~ 'productivoTNC',))  %>%
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rangos = case_when(saldous < 500 ~'menor500USD',
                                  saldous > 500 & saldous <= 1000 ~'500-1k',
                                  saldous > 1000 & saldous <= 3000 ~'1k-3k',
                                  saldous > 3000 & saldous <= 5000 ~'3k-5k',
                                  saldous > 5000 & saldous <= 8000 ~'5k-8k',
                                  saldous > 8000 & saldous <= 10000 ~'8k-10k',
                                  saldous > 10000 & saldous <= 20000 ~'15k-20k',
                                  saldous > 20000 ~'mayor20k')) %>% 
        
        mutate(rangom = case_when(montous < 500 ~'menor500USD',
                                  montous > 500 & saldous <= 1000 ~'500-1k',
                                  montous > 1000 & saldous <= 3000 ~'1k-3k',
                                  montous > 3000 & saldous <= 5000 ~'3k-5k',
                                  montous > 5000 & saldous <= 8000 ~'5k-8k',
                                  montous > 8000 & saldous <= 10000 ~'8k-10k',
                                  montous > 10000 & saldous <= 20000 ~'15k-20k',
                                  montous > 20000 ~'mayor20k')) %>% 
        mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
        mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
        mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
                                    substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
                                    substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
                                    substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
        mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
        mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal))
      saveRDS(bdc, paste0('c:/!bso/girCartera/rds/ec_',
                          mos2[i], years[k], '.rds'))
      #bdcList[[i]] <- bdc
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
#############################################################################
bdcList <- list()
mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2021')
for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                          mos2[i], years[k],'.txt'), 
                   encoding = 'Latin-1') %>% 
        left_join(N_Campos, by="CAEDEC_DEST") %>% 
        left_join(ventamm, by="CAEDEC_DEST") %>%
        
        dplyr::filter(MODULO != 131) %>% 
        mutate(fbase = paste0(mos2[i], years[k])) %>% 
        mutate(fdes = dmy(FDESEMBOLSO)) %>% 
        mutate(cosechaY = year(fdes)) %>% 
        mutate(cosechaM = as.yearmon(fdes)) %>% 
        mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
        mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
        mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
        mutate(catMora = case_when(as.numeric(DIASMORA) == 0~'1. 0 dias',
                                   as.numeric(DIASMORA) > 0 & as.numeric(DIASMORA) <=5 ~ '2.1-5 dias',
                                   as.numeric(DIASMORA) > 5 & as.numeric(DIASMORA) <=30 ~ '3.6-30 dias',
                                   as.numeric(DIASMORA) > 30 & as.numeric(DIASMORA) <=90 ~ '4.31-90 dias',
                                   as.numeric(DIASMORA) > 90 ~ '5.90+ dias', TRUE~'NA')) %>% 
        mutate(mon = substr(fbase,1,3)) %>% 
        mutate(year = substr(fbase,4,7)) %>% 
        mutate(mes = case_when(mon == 'Ene'~'jan',
                               mon == 'Feb'~'feb',
                               mon == 'Mar'~'mar',
                               mon == 'Abr'~'apr',
                               mon == 'May'~'may',
                               mon == 'Jun'~'jun',
                               mon == 'Jul'~'jul',
                               mon == 'Ago'~'aug',
                               mon == 'Sep'~'sep',
                               mon == 'Oct'~'oct',
                               mon == 'Nov'~'nov',
                               mon == 'Dic'~'dec',)) %>% 
        mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
        mutate(monDate = as.yearmon(dayDate)) %>% 
        mutate(montous = ifelse(monDate != cosechaM, 0, montous)) %>% 
        mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
        mutate(CAEDEC_DEST = ifelse(str_length(CAEDEC_DEST) == 4, 
                                    paste0('0', CAEDEC_DEST), CAEDEC_DEST)) %>% 
        mutate(CIU = as.character(CIU)) %>% 
        mutate(CIU = ifelse(str_length(CIU) == 4, 
                            paste0('0', CIU), CIU)) %>% 
        mutate(divCaedecD = substr(CAEDEC_DEST,1,2)) %>% 
        mutate(grupoCaedecD = case_when(divCaedecD == '01'~'A',
                                        divCaedecD == '02'~'B',
                                        divCaedecD == '03'~'B',
                                        divCaedecD == '05'~'B',
                                        divCaedecD == '11'~'C',
                                        divCaedecD == '10'~'D',
                                        divCaedecD == '11'~'D',
                                        divCaedecD == '12'~'D',
                                        divCaedecD == '13'~'D',
                                        divCaedecD == '14'~'D',
                                        divCaedecD == '15'~'E',
                                        divCaedecD == '16'~'E',
                                        divCaedecD == '17'~'E',
                                        divCaedecD == '18'~'E',
                                        divCaedecD == '19'~'E',
                                        divCaedecD == '20'~'E',
                                        divCaedecD == '21'~'E',
                                        divCaedecD == '22'~'E',
                                        divCaedecD == '23'~'E',
                                        divCaedecD == '24'~'E',
                                        divCaedecD == '25'~'E',
                                        divCaedecD == '26'~'E',
                                        divCaedecD == '27'~'E',
                                        divCaedecD == '28'~'E',
                                        divCaedecD == '29'~'E',
                                        divCaedecD == '30'~'E',
                                        divCaedecD == '31'~'E',
                                        divCaedecD == '32'~'E',
                                        divCaedecD == '33'~'E',
                                        divCaedecD == '34'~'E',
                                        divCaedecD == '35'~'E',
                                        divCaedecD == '36'~'E',
                                        divCaedecD == '37'~'E',
                                        divCaedecD == '40'~'F',
                                        divCaedecD == '41'~'F',
                                        divCaedecD == '45'~'G',
                                        divCaedecD == '50'~'H',
                                        divCaedecD == '51'~'H',
                                        divCaedecD == '52'~'H',
                                        divCaedecD == '55'~'I',
                                        divCaedecD == '60'~'J',
                                        divCaedecD == '61'~'J',
                                        divCaedecD == '62'~'J',
                                        divCaedecD == '63'~'J',
                                        divCaedecD == '64'~'J',
                                        divCaedecD == '65'~'K',
                                        divCaedecD == '66'~'K',
                                        divCaedecD == '67'~'K',
                                        divCaedecD == '70'~'L',
                                        divCaedecD == '71'~'L',
                                        divCaedecD == '72'~'L',
                                        divCaedecD == '73'~'L',
                                        divCaedecD == '74'~'L',
                                        divCaedecD == '75'~'M',
                                        divCaedecD == '80'~'N',
                                        divCaedecD == '85'~'O',
                                        divCaedecD == '90'~'O',
                                        divCaedecD == '91'~'O',
                                        divCaedecD == '92'~'O',
                                        divCaedecD == '93'~'O',
                                        divCaedecD == '95'~'P',
                                        divCaedecD == '98'~'Q',
                                        divCaedecD == '99'~'Z',)) %>% 
        mutate(caedec3dD = case_when(grupoCaedecD == 'A'~'5.Productivo GDI',
                                     grupoCaedecD == 'B'~'5.Productivo GDI',
                                     grupoCaedecD == 'C'~'4.Productivo GDE',
                                     grupoCaedecD == 'D'~'4.Productivo GDE',
                                     grupoCaedecD == 'E'~'5.Productivo GDI',
                                     grupoCaedecD == 'F'~'4.Productivo GDE',
                                     grupoCaedecD == 'G'~'3.Productivo VIV',
                                     grupoCaedecD == 'H'~'2.Comercio',
                                     grupoCaedecD == 'I'~'1.Servicios',
                                     grupoCaedecD == 'J'~'1.Servicios',
                                     grupoCaedecD == 'K'~'1.Servicios',
                                     grupoCaedecD == 'L'~'1.Servicios',
                                     grupoCaedecD == 'M'~'1.Servicios',
                                     grupoCaedecD == 'N'~'1.Servicios',
                                     grupoCaedecD == 'O'~'1.Servicios',
                                     grupoCaedecD == 'P'~'1.Servicios',
                                     grupoCaedecD == 'Q'~'1.Servicios',
                                     grupoCaedecD == 'Z'~'1.Servicios',)) %>% 
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
        mutate(caedec3dC = case_when(grupoCaedecC == 'A'~'5.Productivo GDI',
                                     grupoCaedecC == 'B'~'5.Productivo GDI',
                                     grupoCaedecC == 'C'~'4.Productivo GDE',
                                     grupoCaedecC == 'D'~'4.Productivo GDE',
                                     grupoCaedecC == 'E'~'5.Productivo GDI',
                                     grupoCaedecC == 'F'~'4.Productivo GDE',
                                     grupoCaedecC == 'G'~'3.Productivo VIV',
                                     grupoCaedecC == 'H'~'2.Comercio',
                                     grupoCaedecC == 'I'~'1.Servicios',
                                     grupoCaedecC == 'J'~'1.Servicios',
                                     grupoCaedecC == 'K'~'1.Servicios',
                                     grupoCaedecC == 'L'~'1.Servicios',
                                     grupoCaedecC == 'M'~'1.Servicios',
                                     grupoCaedecC == 'N'~'1.Servicios',
                                     grupoCaedecC == 'O'~'1.Servicios',
                                     grupoCaedecC == 'P'~'1.Servicios',
                                     grupoCaedecC == 'Q'~'1.Servicios',
                                     grupoCaedecC == 'Z'~'1.Servicios',)) %>% 
        mutate(ctaCont = substr(RUBRO,1,3)) %>% 
        mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                     ctaCont == '134'~saldous,
                                     ctaCont == '136'~saldous,
                                     ctaCont == '137'~saldous,
                                     TRUE ~ 0)) %>% 
        mutate(saldoReprog = case_when(ctaCont == '135'~saldous,
                                       ctaCont == '136'~saldous,
                                       ctaCont == '137'~saldous,
                                       TRUE ~ 0)) %>% 
        mutate(saldoRepVig = ifelse(ctaCont == '135', saldous, 0)) %>%
        mutate(saldoRepMora = ifelse(MODULO == 121 & DIASMORA > 0 , saldous, 0)) %>% 
        mutate(saldoRepPaR0 = case_when(ctaCont == '136'~saldous,
                                        ctaCont == '137'~saldous,
                                        TRUE ~ 0)) %>% 
        mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIG'), saldous, 0)) %>% 
        mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
        mutate(car = saldoMora + saldoRepVig) %>% 
        mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
        mutate(labGrupoD = case_when(grupoCaedecD == 'A'~ 'A. Agricola',
                                     grupoCaedecD == 'B'~ 'B. Caza, Pesca',
                                     grupoCaedecD == 'C'~ 'C. Ext. Gas y Pet.',
                                     grupoCaedecD == 'D'~ 'D. Ext. Minerales',
                                     grupoCaedecD == 'E'~ 'E. Ind. y Manu.',
                                     grupoCaedecD == 'F'~ 'F. Dist. EE y agua',
                                     grupoCaedecD == 'G'~ 'G. Construcción',
                                     grupoCaedecD == 'H'~ 'H. Comercio',
                                     grupoCaedecD == 'I'~ 'I. Hoteles',
                                     grupoCaedecD == 'J'~ 'J. Transporte',
                                     grupoCaedecD == 'K'~ 'K. Inter. Fin.',
                                     grupoCaedecD == 'L'~ 'L. Serv. Inmob.',
                                     grupoCaedecD == 'M'~ 'M. Adm. Pública',
                                     grupoCaedecD == 'N'~ 'N. Educación',
                                     grupoCaedecD == 'O'~ 'O. Serv. Hosp + Otros',
                                     grupoCaedecD == 'P'~ 'P. Serv. Doméstico',
                                     grupoCaedecD == 'Q'~ 'Q. Org. Extraterritoriales',
                                     grupoCaedecD == 'Z'~ 'Z. Jubilados, Est. y AC',
                                     TRUE ~ 'Otros')) %>% 
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
        mutate(SECTOR_CARTERA = case_when(
          TIPO_CREDITO == "H0" | TIPO_CREDITO == "H1"| TIPO_CREDITO == "H2" ~ "6. VIVIENDA NO CONTROLADA",
          (TIPO_CREDITO == "H3" | TIPO_CREDITO == "H4") & MONEDA == 0 ~ "3. VIVIENDA CONTROLADA",
          TIPO_CREDITO == "N0" | TIPO_CREDITO == "N1"| TIPO_CREDITO == "N2" ~ "9. CONSUMO",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV =='Pequeña' & TASAACT<=7) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="A"|cat=="B") ~ "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
          (CAEDEC_DEST==31600|CAEDEC_DEST==31700| CAEDEC_DEST==51508|CAEDEC_DEST==52592| CAEDEC_DEST==34400|CAEDEC_DEST==34500|
             CAEDEC_DEST==50103) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") ~ "5. OTRA PRODUCCION NO CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
               DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
               CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
               CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
               DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
            (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
            (TIPOTASA=="F" & MONEDA==0) &
            (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") &
            cat=="H" ~ "7. COMERCIO" ,
          (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
             TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
             TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
            (cat=="I"|cat=="J"| cat=="K"|cat=="L"|
               cat=="M"|cat=="N"| cat=="O"|cat=="P"|
               cat=="Q"|cat=="Z") ~ "8. SERVICIOS",
          TRUE ~ "0" ))  %>%
        mutate(categ = case_when(SECTOR_CARTERA == '3. VIVIENDA CONTROLADA' ~ 'viviendaTC',
                                 SECTOR_CARTERA ==  '1. PRODUCCION AGROPECUARIA CONTROLADA' |
                                   SECTOR_CARTERA ==  '2. OTRA PRODUCCION CONTROLADA'~ 'productivoTC',
                                 SECTOR_CARTERA ==  '4. PRODUCCION AGROPECUARIA NO CONTROLADA ' |
                                   SECTOR_CARTERA ==  '5. OTRA PRODUCCION NO CONTROLADA' | 
                                   SECTOR_CARTERA ==  '6. VIVIENDA NO CONTROLADA'~ 'productivoTNC',))  %>%
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
        mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
        mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rangos = case_when(saldous < 500 ~'menor500USD',
                                  saldous > 500 & saldous <= 1000 ~'500-1k',
                                  saldous > 1000 & saldous <= 3000 ~'1k-3k',
                                  saldous > 3000 & saldous <= 5000 ~'3k-5k',
                                  saldous > 5000 & saldous <= 8000 ~'5k-8k',
                                  saldous > 8000 & saldous <= 10000 ~'8k-10k',
                                  saldous > 10000 & saldous <= 20000 ~'15k-20k',
                                  saldous > 20000 ~'mayor20k')) %>% 
        
        mutate(rangom = case_when(montous < 500 ~'menor500USD',
                                  montous > 500 & saldous <= 1000 ~'500-1k',
                                  montous > 1000 & saldous <= 3000 ~'1k-3k',
                                  montous > 3000 & saldous <= 5000 ~'3k-5k',
                                  montous > 5000 & saldous <= 8000 ~'5k-8k',
                                  montous > 8000 & saldous <= 10000 ~'8k-10k',
                                  montous > 10000 & saldous <= 20000 ~'15k-20k',
                                  montous > 20000 ~'mayor20k')) %>% 
        mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal))
      saveRDS(bdc, paste0('c:/!bso/girCartera/rds/ec_',
                          mos2[i], years[k], '.rds'))
      #bdcList[[i]] <- bdc
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

##############################################

####################################################################
mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2022')
#years <- c('2014', '2015', '2016')
for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                          mos2[i], years[k],'.txt'), 
                   encoding = 'Latin-1', fill = T) %>% 
        left_join(N_Campos, by="CAEDEC_DEST") %>% 
        left_join(ventamm, by="CAEDEC_DEST") %>%
        #bdc <- fread(paste0('C:/Users/aeid/OneDrive - Banco Solidario S.A/bdc/BaseCartera', 
        #                  mos2[i], years[k],'.txt'), 
        #            encoding = 'Latin-1', fill = T) %>% 
        #select(OPERACION, RUBRO, SALDO, CALIFICACION, CAEDEC_DEST,
        #      MONEDA, DIASMORA, FDESEMBOLSO, MODULO) %>% 
        dplyr::filter(MODULO != 131) %>% 
        mutate(fbase = paste0(mos2[i], years[k])) %>% 
        #select(RUBRO, SALDO, CALIFICACION, CAEDEC_DEST, MONEDA, FDESEMBOLSO,
        #       AGENCIA, PREVCONST) %>% 
        mutate(fdes = dmy(FDESEMBOLSO)) %>% 
        mutate(cosechaY = year(fdes)) %>% 
        mutate(cosechaM = as.yearmon(fdes)) %>% 
        mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
        mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
        mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
        mutate(catMora = case_when(as.numeric(DIASMORA) == 0~'1. 0 dias',
                                   as.numeric(DIASMORA) > 0 & as.numeric(DIASMORA) <=5 ~ '2.1-5 dias',
                                   as.numeric(DIASMORA) > 5 & as.numeric(DIASMORA) <=30 ~ '3.6-30 dias',
                                   as.numeric(DIASMORA) > 30 & as.numeric(DIASMORA) <=90 ~ '4.31-90 dias',
                                   as.numeric(DIASMORA) > 90 ~ '5.90+ dias', TRUE~'NA')) %>% 
        mutate(mon = substr(fbase,1,3)) %>% 
        mutate(year = substr(fbase,4,7)) %>% 
        mutate(mes = case_when(mon == 'Ene'~'jan',
                               mon == 'Feb'~'feb',
                               mon == 'Mar'~'mar',
                               mon == 'Abr'~'apr',
                               mon == 'May'~'may',
                               mon == 'Jun'~'jun',
                               mon == 'Jul'~'jul',
                               mon == 'Ago'~'aug',
                               mon == 'Sep'~'sep',
                               mon == 'Oct'~'oct',
                               mon == 'Nov'~'nov',
                               mon == 'Dic'~'dec',)) %>% 
        mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
        mutate(monDate = as.yearmon(dayDate)) %>% 
        mutate(montous = ifelse(monDate != cosechaM, 0, montous)) %>% 
        mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
        mutate(CAEDEC_DEST = ifelse(str_length(CAEDEC_DEST) == 4, 
                                    paste0('0', CAEDEC_DEST), CAEDEC_DEST)) %>% 
        mutate(CIU = as.character(CIU)) %>% 
        mutate(CIU = ifelse(str_length(CIU) == 4, 
                            paste0('0', CIU), CIU)) %>% 
        mutate(divCaedecD = substr(CAEDEC_DEST,1,2)) %>% 
        mutate(grupoCaedecD = case_when(divCaedecD == '01'~'A',
                                        divCaedecD == '02'~'B',
                                        divCaedecD == '03'~'B',
                                        divCaedecD == '05'~'B',
                                        divCaedecD == '11'~'C',
                                        divCaedecD == '10'~'D',
                                        divCaedecD == '11'~'D',
                                        divCaedecD == '12'~'D',
                                        divCaedecD == '13'~'D',
                                        divCaedecD == '14'~'D',
                                        divCaedecD == '15'~'E',
                                        divCaedecD == '16'~'E',
                                        divCaedecD == '17'~'E',
                                        divCaedecD == '18'~'E',
                                        divCaedecD == '19'~'E',
                                        divCaedecD == '20'~'E',
                                        divCaedecD == '21'~'E',
                                        divCaedecD == '22'~'E',
                                        divCaedecD == '23'~'E',
                                        divCaedecD == '24'~'E',
                                        divCaedecD == '25'~'E',
                                        divCaedecD == '26'~'E',
                                        divCaedecD == '27'~'E',
                                        divCaedecD == '28'~'E',
                                        divCaedecD == '29'~'E',
                                        divCaedecD == '30'~'E',
                                        divCaedecD == '31'~'E',
                                        divCaedecD == '32'~'E',
                                        divCaedecD == '33'~'E',
                                        divCaedecD == '34'~'E',
                                        divCaedecD == '35'~'E',
                                        divCaedecD == '36'~'E',
                                        divCaedecD == '37'~'E',
                                        divCaedecD == '40'~'F',
                                        divCaedecD == '41'~'F',
                                        divCaedecD == '45'~'G',
                                        divCaedecD == '50'~'H',
                                        divCaedecD == '51'~'H',
                                        divCaedecD == '52'~'H',
                                        divCaedecD == '55'~'I',
                                        divCaedecD == '60'~'J',
                                        divCaedecD == '61'~'J',
                                        divCaedecD == '62'~'J',
                                        divCaedecD == '63'~'J',
                                        divCaedecD == '64'~'J',
                                        divCaedecD == '65'~'K',
                                        divCaedecD == '66'~'K',
                                        divCaedecD == '67'~'K',
                                        divCaedecD == '70'~'L',
                                        divCaedecD == '71'~'L',
                                        divCaedecD == '72'~'L',
                                        divCaedecD == '73'~'L',
                                        divCaedecD == '74'~'L',
                                        divCaedecD == '75'~'M',
                                        divCaedecD == '80'~'N',
                                        divCaedecD == '85'~'O',
                                        divCaedecD == '90'~'O',
                                        divCaedecD == '91'~'O',
                                        divCaedecD == '92'~'O',
                                        divCaedecD == '93'~'O',
                                        divCaedecD == '95'~'P',
                                        divCaedecD == '98'~'Q',
                                        divCaedecD == '99'~'Z',)) %>% 
        mutate(caedec3dD = case_when(grupoCaedecD == 'A'~'5.Productivo GDI',
                                     grupoCaedecD == 'B'~'5.Productivo GDI',
                                     grupoCaedecD == 'C'~'4.Productivo GDE',
                                     grupoCaedecD == 'D'~'4.Productivo GDE',
                                     grupoCaedecD == 'E'~'5.Productivo GDI',
                                     grupoCaedecD == 'F'~'4.Productivo GDE',
                                     grupoCaedecD == 'G'~'3.Productivo VIV',
                                     grupoCaedecD == 'H'~'2.Comercio',
                                     grupoCaedecD == 'I'~'1.Servicios',
                                     grupoCaedecD == 'J'~'1.Servicios',
                                     grupoCaedecD == 'K'~'1.Servicios',
                                     grupoCaedecD == 'L'~'1.Servicios',
                                     grupoCaedecD == 'M'~'1.Servicios',
                                     grupoCaedecD == 'N'~'1.Servicios',
                                     grupoCaedecD == 'O'~'1.Servicios',
                                     grupoCaedecD == 'P'~'1.Servicios',
                                     grupoCaedecD == 'Q'~'1.Servicios',
                                     grupoCaedecD == 'Z'~'1.Servicios',)) %>% 
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
        mutate(caedec3dC = case_when(grupoCaedecC == 'A'~'5.Productivo GDI',
                                     grupoCaedecC == 'B'~'5.Productivo GDI',
                                     grupoCaedecC == 'C'~'4.Productivo GDE',
                                     grupoCaedecC == 'D'~'4.Productivo GDE',
                                     grupoCaedecC == 'E'~'5.Productivo GDI',
                                     grupoCaedecC == 'F'~'4.Productivo GDE',
                                     grupoCaedecC == 'G'~'3.Productivo VIV',
                                     grupoCaedecC == 'H'~'2.Comercio',
                                     grupoCaedecC == 'I'~'1.Servicios',
                                     grupoCaedecC == 'J'~'1.Servicios',
                                     grupoCaedecC == 'K'~'1.Servicios',
                                     grupoCaedecC == 'L'~'1.Servicios',
                                     grupoCaedecC == 'M'~'1.Servicios',
                                     grupoCaedecC == 'N'~'1.Servicios',
                                     grupoCaedecC == 'O'~'1.Servicios',
                                     grupoCaedecC == 'P'~'1.Servicios',
                                     grupoCaedecC == 'Q'~'1.Servicios',
                                     grupoCaedecC == 'Z'~'1.Servicios',)) %>% 
        mutate(ctaCont = substr(RUBRO,1,3)) %>% 
        mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                     ctaCont == '134'~saldous,
                                     ctaCont == '136'~saldous,
                                     ctaCont == '137'~saldous,
                                     TRUE ~ 0)) %>% 
        mutate(saldoReprog = case_when(ctaCont == '135'~saldous,
                                       ctaCont == '136'~saldous,
                                       ctaCont == '137'~saldous,
                                       TRUE ~ 0)) %>% 
        mutate(saldoRepVig = ifelse(ctaCont == '135', saldous, 0)) %>%
        mutate(saldoRepMora = ifelse(MODULO == 121 & DIASMORA > 0 , saldous, 0)) %>% 
        mutate(saldoRepPaR0 = case_when(ctaCont == '136'~saldous,
                                        ctaCont == '137'~saldous,
                                        TRUE ~ 0)) %>% 
        mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIG'), saldous, 0)) %>% 
        mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
        mutate(car = saldoMora + saldoRepVig) %>% 
        mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
        mutate(labGrupoD = case_when(grupoCaedecD == 'A'~ 'A. Agricola',
                                     grupoCaedecD == 'B'~ 'B. Caza, Pesca',
                                     grupoCaedecD == 'C'~ 'C. Ext. Gas y Pet.',
                                     grupoCaedecD == 'D'~ 'D. Ext. Minerales',
                                     grupoCaedecD == 'E'~ 'E. Ind. y Manu.',
                                     grupoCaedecD == 'F'~ 'F. Dist. EE y agua',
                                     grupoCaedecD == 'G'~ 'G. Construcción',
                                     grupoCaedecD == 'H'~ 'H. Comercio',
                                     grupoCaedecD == 'I'~ 'I. Hoteles',
                                     grupoCaedecD == 'J'~ 'J. Transporte',
                                     grupoCaedecD == 'K'~ 'K. Inter. Fin.',
                                     grupoCaedecD == 'L'~ 'L. Serv. Inmob.',
                                     grupoCaedecD == 'M'~ 'M. Adm. Pública',
                                     grupoCaedecD == 'N'~ 'N. Educación',
                                     grupoCaedecD == 'O'~ 'O. Serv. Hosp + Otros',
                                     grupoCaedecD == 'P'~ 'P. Serv. Doméstico',
                                     grupoCaedecD == 'Q'~ 'Q. Org. Extraterritoriales',
                                     grupoCaedecD == 'Z'~ 'Z. Jubilados, Est. y AC',
                                     TRUE ~ 'Otros')) %>% 
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
        mutate(categ = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
                                 SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
                                   SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
                                   SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
                                   SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
                                   SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
                                 SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
                                   SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
                                   SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
        mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
        mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
        mutate(categ = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
                                 SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
                                   SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
                                   SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
                                   SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
                                   SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
                                 SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
                                   SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
                                   SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%
        mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
        mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
        mutate(intus = saldous * TASAACT/100) %>% 
        mutate(opTot = 1) %>% 
        mutate(rangos = case_when(saldous < 500 ~'menor500USD',
                                  saldous > 500 & saldous <= 1000 ~'500-1k',
                                  saldous > 1000 & saldous <= 3000 ~'1k-3k',
                                  saldous > 3000 & saldous <= 5000 ~'3k-5k',
                                  saldous > 5000 & saldous <= 8000 ~'5k-8k',
                                  saldous > 8000 & saldous <= 10000 ~'8k-10k',
                                  saldous > 10000 & saldous <= 20000 ~'15k-20k',
                                  saldous > 20000 ~'mayor20k')) %>% 
        
        mutate(rangom = case_when(montous < 500 ~'menor500USD',
                                  montous > 500 & saldous <= 1000 ~'500-1k',
                                  montous > 1000 & saldous <= 3000 ~'1k-3k',
                                  montous > 3000 & saldous <= 5000 ~'3k-5k',
                                  montous > 5000 & saldous <= 8000 ~'5k-8k',
                                  montous > 8000 & saldous <= 10000 ~'8k-10k',
                                  montous > 10000 & saldous <= 20000 ~'15k-20k',
                                  montous > 20000 ~'mayor20k')) %>% 
        mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
                                    substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
                                    substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
                                    substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
        mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
        mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal))
      saveRDS(bdc, paste0('C:/!bso/girCartera/rds/ec_',
                          mos2[i], years[k], '.rds'))
      
      #bdcList[[i]] <- bdc
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

################################################################################
################################################################################
mos2 <- c('Abr')
years <- c('2016')

bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraMay2016.txt'), 
                encoding = 'Latin-1', fill = T) %>%
  left_join(N_Campos, by="CAEDEC_DEST") %>% 
  left_join(ventamm, by="CAEDEC_DEST") %>%  
  
  dplyr::filter(MODULO != 131) %>% 
  mutate(fbase = paste0(mos2, years)) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(cosechaM = as.yearmon(fdes)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(catMora = case_when(as.numeric(DIASMORA) == 0~'1. 0 dias',
                             as.numeric(DIASMORA) > 0 & as.numeric(DIASMORA) <=5 ~ '2.1-5 dias',
                             as.numeric(DIASMORA) > 5 & as.numeric(DIASMORA) <=30 ~ '3.6-30 dias',
                             as.numeric(DIASMORA) > 30 & as.numeric(DIASMORA) <=90 ~ '4.31-90 dias',
                             as.numeric(DIASMORA) > 90 ~ '5.90+ dias', TRUE~'NA')) %>% 
  mutate(mon = substr(fbase,1,3)) %>% 
  mutate(year = substr(fbase,4,7)) %>% 
  mutate(mes = case_when(mon == 'May'~'may')) %>% 
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
  mutate(monDate = as.yearmon(dayDate)) %>% 
  mutate(montous = ifelse(monDate != cosechaM, 0, montous)) %>% 
  mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
  mutate(CAEDEC_DEST = ifelse(str_length(CAEDEC_DEST) == 4, 
                              paste0('0', CAEDEC_DEST), CAEDEC_DEST)) %>% 
  mutate(CIU = as.character(CIU)) %>% 
  mutate(CIU = ifelse(str_length(CIU) == 4, 
                      paste0('0', CIU), CIU)) %>% 
  mutate(divCaedecD = substr(CAEDEC_DEST,1,2)) %>% 
  mutate(grupoCaedecD = case_when(divCaedecD == '01'~'A',
                                  divCaedecD == '02'~'B',
                                  divCaedecD == '03'~'B',
                                  divCaedecD == '05'~'B',
                                  divCaedecD == '11'~'C',
                                  divCaedecD == '10'~'D',
                                  divCaedecD == '11'~'D',
                                  divCaedecD == '12'~'D',
                                  divCaedecD == '13'~'D',
                                  divCaedecD == '14'~'D',
                                  divCaedecD == '15'~'E',
                                  divCaedecD == '16'~'E',
                                  divCaedecD == '17'~'E',
                                  divCaedecD == '18'~'E',
                                  divCaedecD == '19'~'E',
                                  divCaedecD == '20'~'E',
                                  divCaedecD == '21'~'E',
                                  divCaedecD == '22'~'E',
                                  divCaedecD == '23'~'E',
                                  divCaedecD == '24'~'E',
                                  divCaedecD == '25'~'E',
                                  divCaedecD == '26'~'E',
                                  divCaedecD == '27'~'E',
                                  divCaedecD == '28'~'E',
                                  divCaedecD == '29'~'E',
                                  divCaedecD == '30'~'E',
                                  divCaedecD == '31'~'E',
                                  divCaedecD == '32'~'E',
                                  divCaedecD == '33'~'E',
                                  divCaedecD == '34'~'E',
                                  divCaedecD == '35'~'E',
                                  divCaedecD == '36'~'E',
                                  divCaedecD == '37'~'E',
                                  divCaedecD == '40'~'F',
                                  divCaedecD == '41'~'F',
                                  divCaedecD == '45'~'G',
                                  divCaedecD == '50'~'H',
                                  divCaedecD == '51'~'H',
                                  divCaedecD == '52'~'H',
                                  divCaedecD == '55'~'I',
                                  divCaedecD == '60'~'J',
                                  divCaedecD == '61'~'J',
                                  divCaedecD == '62'~'J',
                                  divCaedecD == '63'~'J',
                                  divCaedecD == '64'~'J',
                                  divCaedecD == '65'~'K',
                                  divCaedecD == '66'~'K',
                                  divCaedecD == '67'~'K',
                                  divCaedecD == '70'~'L',
                                  divCaedecD == '71'~'L',
                                  divCaedecD == '72'~'L',
                                  divCaedecD == '73'~'L',
                                  divCaedecD == '74'~'L',
                                  divCaedecD == '75'~'M',
                                  divCaedecD == '80'~'N',
                                  divCaedecD == '85'~'O',
                                  divCaedecD == '90'~'O',
                                  divCaedecD == '91'~'O',
                                  divCaedecD == '92'~'O',
                                  divCaedecD == '93'~'O',
                                  divCaedecD == '95'~'P',
                                  divCaedecD == '98'~'Q',
                                  divCaedecD == '99'~'Z',)) %>% 
  mutate(caedec3dD = case_when(grupoCaedecD == 'A'~'5.Productivo GDI',
                               grupoCaedecD == 'B'~'5.Productivo GDI',
                               grupoCaedecD == 'C'~'4.Productivo GDE',
                               grupoCaedecD == 'D'~'4.Productivo GDE',
                               grupoCaedecD == 'E'~'5.Productivo GDI',
                               grupoCaedecD == 'F'~'4.Productivo GDE',
                               grupoCaedecD == 'G'~'3.Productivo VIV',
                               grupoCaedecD == 'H'~'2.Comercio',
                               grupoCaedecD == 'I'~'1.Servicios',
                               grupoCaedecD == 'J'~'1.Servicios',
                               grupoCaedecD == 'K'~'1.Servicios',
                               grupoCaedecD == 'L'~'1.Servicios',
                               grupoCaedecD == 'M'~'1.Servicios',
                               grupoCaedecD == 'N'~'1.Servicios',
                               grupoCaedecD == 'O'~'1.Servicios',
                               grupoCaedecD == 'P'~'1.Servicios',
                               grupoCaedecD == 'Q'~'1.Servicios',
                               grupoCaedecD == 'Z'~'1.Servicios',)) %>% 
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
  mutate(caedec3dC = case_when(grupoCaedecC == 'A'~'5.Productivo GDI',
                               grupoCaedecC == 'B'~'5.Productivo GDI',
                               grupoCaedecC == 'C'~'4.Productivo GDE',
                               grupoCaedecC == 'D'~'4.Productivo GDE',
                               grupoCaedecC == 'E'~'5.Productivo GDI',
                               grupoCaedecC == 'F'~'4.Productivo GDE',
                               grupoCaedecC == 'G'~'3.Productivo VIV',
                               grupoCaedecC == 'H'~'2.Comercio',
                               grupoCaedecC == 'I'~'1.Servicios',
                               grupoCaedecC == 'J'~'1.Servicios',
                               grupoCaedecC == 'K'~'1.Servicios',
                               grupoCaedecC == 'L'~'1.Servicios',
                               grupoCaedecC == 'M'~'1.Servicios',
                               grupoCaedecC == 'N'~'1.Servicios',
                               grupoCaedecC == 'O'~'1.Servicios',
                               grupoCaedecC == 'P'~'1.Servicios',
                               grupoCaedecC == 'Q'~'1.Servicios',
                               grupoCaedecC == 'Z'~'1.Servicios',)) %>% 
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                               ctaCont == '134'~saldous,
                               ctaCont == '136'~saldous,
                               ctaCont == '137'~saldous,
                               TRUE ~ 0)) %>% 
  mutate(saldoReprog = case_when(ctaCont == '135'~saldous,
                                 ctaCont == '136'~saldous,
                                 ctaCont == '137'~saldous,
                                 TRUE ~ 0)) %>% 
  mutate(saldoRepVig = ifelse(ctaCont == '135', saldous, 0)) %>%
  mutate(saldoRepMora = ifelse(MODULO == 121 & DIASMORA > 0 , saldous, 0)) %>% 
  mutate(saldoRepPaR0 = case_when(ctaCont == '136'~saldous,
                                  ctaCont == '137'~saldous,
                                  TRUE ~ 0)) %>% 
  mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIG'), saldous, 0)) %>% 
  mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
  mutate(car = saldoMora + saldoRepVig) %>% 
  mutate(par1 = ifelse(DIASMORA >0, saldous, 0)) %>% 
  mutate(labGrupoD = case_when(grupoCaedecD == 'A'~ 'A. Agricola',
                               grupoCaedecD == 'B'~ 'B. Caza, Pesca',
                               grupoCaedecD == 'C'~ 'C. Ext. Gas y Pet.',
                               grupoCaedecD == 'D'~ 'D. Ext. Minerales',
                               grupoCaedecD == 'E'~ 'E. Ind. y Manu.',
                               grupoCaedecD == 'F'~ 'F. Dist. EE y agua',
                               grupoCaedecD == 'G'~ 'G. Construcción',
                               grupoCaedecD == 'H'~ 'H. Comercio',
                               grupoCaedecD == 'I'~ 'I. Hoteles',
                               grupoCaedecD == 'J'~ 'J. Transporte',
                               grupoCaedecD == 'K'~ 'K. Inter. Fin.',
                               grupoCaedecD == 'L'~ 'L. Serv. Inmob.',
                               grupoCaedecD == 'M'~ 'M. Adm. Pública',
                               grupoCaedecD == 'N'~ 'N. Educación',
                               grupoCaedecD == 'O'~ 'O. Serv. Hosp + Otros',
                               grupoCaedecD == 'P'~ 'P. Serv. Doméstico',
                               grupoCaedecD == 'Q'~ 'Q. Org. Extraterritoriales',
                               grupoCaedecD == 'Z'~ 'Z. Jubilados, Est. y AC',
                               TRUE ~ 'Otros')) %>% 
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
  mutate(SECTOR_CARTERA = case_when(
    TIPO_CREDITO == "H0" | TIPO_CREDITO == "H1"| TIPO_CREDITO == "H2" ~ "6. VIVIENDA NO CONTROLADA",
    (TIPO_CREDITO == "H3" | TIPO_CREDITO == "H4") & MONEDA == 0 ~ "3. VIVIENDA CONTROLADA",
    TIPO_CREDITO == "N0" | TIPO_CREDITO == "N1"| TIPO_CREDITO == "N2" ~ "9. CONSUMO",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="A"|cat=="B") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="A"|cat=="B") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV =='Pequeña' & TASAACT<=7) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="A"|cat=="B") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="A"|cat=="B") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="A"|cat=="B") ~ "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
    (CAEDEC_DEST==31600|CAEDEC_DEST==31700| CAEDEC_DEST==51508|CAEDEC_DEST==52592| CAEDEC_DEST==34400|CAEDEC_DEST==34500|
       CAEDEC_DEST==50103) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="C"|cat=="D"| cat=="E"|cat=="F"| cat=="G") ~ "5. OTRA PRODUCCION NO CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
         DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
         CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
         CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
         DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
         CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
         CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
         DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
         CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
         CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
         DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==55101|CAEDEC_DEST==55102| CAEDEC_DEST==55103|CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
         CAEDEC_DEST==60222|CAEDEC_DEST==61200| CAEDEC_DEST==62101|CAEDEC_DEST==60221| CAEDEC_DEST==71110|CAEDEC_DEST==71120|
         CAEDEC_DEST==63041|CAEDEC_DEST==63042| CAEDEC_DEST==92320|CAEDEC_DEST==92330) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
         DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Micro" & TASAACT<=11.5) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
         DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Pequeña" & TASAACT<=7) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
         DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="DMediana" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
         DESC_OBJCRED=="CAPITAL DE OPERACION ACT. PRINCIPAL"| DESC_OBJCRED=="CAPITAL DE OPERACION ACT. SECUNDARIAS") &
      (CAEDEC_DEST==72200|CAEDEC_DEST==73101| CAEDEC_DEST==73102|CAEDEC_DEST==73200| CAEDEC_DEST==92110|CAEDEC_DEST==92141) &
      (TIPOTASA=="F" & MONEDA==0) &
      (TAM_ACTIV=="Grande" & TASAACT<=6) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") &
      cat=="H" ~ "7. COMERCIO" ,
    (TIPO_CREDITO == "M0" | TIPO_CREDITO == "M1"| TIPO_CREDITO == "M2" |
       TIPO_CREDITO == "M7" | TIPO_CREDITO == "M8"| TIPO_CREDITO == "P1" |
       TIPO_CREDITO == "P8" | TIPO_CREDITO == "P9"| TIPO_CREDITO == "P2" | TIPO_CREDITO == "P3") & 
      (cat=="I"|cat=="J"| cat=="K"|cat=="L"|
         cat=="M"|cat=="N"| cat=="O"|cat=="P"|
         cat=="Q"|cat=="Z") ~ "8. SERVICIOS",
    TRUE ~ "0" ))  %>%
  mutate(categ = case_when(SECTOR_CARTERA == '3. VIVIENDA CONTROLADA' ~ 'viviendaTC',
                           SECTOR_CARTERA ==  '1. PRODUCCION AGROPECUARIA CONTROLADA' |
                             SECTOR_CARTERA ==  '2. OTRA PRODUCCION CONTROLADA'~ 'productivoTC',
                           SECTOR_CARTERA ==  '4. PRODUCCION AGROPECUARIA NO CONTROLADA ' |
                             SECTOR_CARTERA ==  '5. OTRA PRODUCCION NO CONTROLADA' | 
                             SECTOR_CARTERA ==  '6. VIVIENDA NO CONTROLADA'~ 'productivoTNC',))  %>%
  mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
  mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
  mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  mutate(opTot = 1) %>% 
  mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
  mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
  mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
  mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
  mutate(saldous = ifelse(categ == 'Castigada', 0, saldous)) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  mutate(opTot = 1) %>% 
  mutate(rangos = case_when(saldous < 500 ~'menor500USD',
                            saldous > 500 & saldous <= 1000 ~'500-1k',
                            saldous > 1000 & saldous <= 3000 ~'1k-3k',
                            saldous > 3000 & saldous <= 5000 ~'3k-5k',
                            saldous > 5000 & saldous <= 8000 ~'5k-8k',
                            saldous > 8000 & saldous <= 10000 ~'8k-10k',
                            saldous > 10000 & saldous <= 20000 ~'15k-20k',
                            saldous > 20000 ~'mayor20k')) %>% 
  
  mutate(rangom = case_when(montous < 500 ~'menor500USD',
                            montous > 500 & saldous <= 1000 ~'500-1k',
                            montous > 1000 & saldous <= 3000 ~'1k-3k',
                            montous > 3000 & saldous <= 5000 ~'3k-5k',
                            montous > 5000 & saldous <= 8000 ~'5k-8k',
                            montous > 8000 & saldous <= 10000 ~'8k-10k',
                            montous > 10000 & saldous <= 20000 ~'15k-20k',
                            montous > 20000 ~'mayor20k')) %>% F
  mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
                              substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
                              substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
                              substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
  mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal))
saveRDS(bdc, paste0('c:/!bso/girCartera/rds/ec_', mos2, years, '.rds'))

  
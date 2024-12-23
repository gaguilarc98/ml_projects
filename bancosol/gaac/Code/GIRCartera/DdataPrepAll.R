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
library(janitor)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FUNCTIONS TO PROCESS BDC____####
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
treatment <- function(x,m="Ene",y="2015"){
  x %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),~dmy(.x))) %>% 
    mutate(fbase = paste0(m, y)) %>% 
    mutate(monDate = as.yearmon(paste0(m,'. ',y))) %>% 
    mutate(fdes = FDESEMBOLSO) %>% 
    mutate(cosechaY = year(fdes)) %>% 
    mutate(cosechaM = as.yearmon(fdes)) %>% 
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                        labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
    mutate(catMora = cut(as.numeric(DIASMORA),breaks=c(-Inf,0,5,30,90,Inf),labels=c('1. 0 dias','2. 1-5 dias','3. 6-30 dias','4. 31-90 dias','5. 90+ dias'))) %>% 
    mutate(mon = substr(fbase,1,3)) %>% 
    mutate(year = substr(fbase,4,7)) %>% 
    mutate(montous = ifelse(monDate != cosechaM, 0, montous)) %>% 
    mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
    mutate(CAEDEC_DEST = ifelse(str_length(CAEDEC_DEST) == 4, 
                                paste0('0', CAEDEC_DEST), CAEDEC_DEST)) %>% 
    mutate(CIU = as.character(CIU)) %>% 
    mutate(CIU = ifelse(str_length(CIU) == 4, 
                        paste0('0', CIU), CIU)) %>% 
    mutate(divCaedecD = substr(CAEDEC_DEST,1,2)) %>% 
    
    mutate(grupoCaedecD = cut(as.numeric(divCaedecD),breaks=c(0,1,5,9,14,37,41,45,52,55,64,67,74,75,80,93,95,98,99),
                              labels=c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'))) %>%
    mutate(grupoCaedecD = as.character(grupoCaedecD)) %>% 
    mutate(grupoCaedecD = ifelse(divCaedecD=='11','C',grupoCaedecD)) %>% 
    mutate(caedec3dD = cases(grupoCaedecD,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                             c('5.Productivo GDI','5.Productivo GDI','4.Productivo GDE','4.Productivo GDE','5.Productivo GDI',
                               '4.Productivo GDE','3.Productivo VIV','2.Comercio','1.Servicios','1.Servicios','1.Servicios','1.Servicios',
                               '1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios'))) %>%
    mutate(divCaedecC = substr(CIU,1,2)) %>%
    mutate(grupoCaedecC = cut(as.numeric(divCaedecC),breaks=c(0,1,5,9,14,37,41,45,52,55,64,67,74,75,80,93,95,98,99),
                              labels=c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'))) %>%
    mutate(grupoCaedecC = as.character(grupoCaedecC)) %>% 
    mutate(grupoCaedecC = ifelse(divCaedecC=='11','C',grupoCaedecC)) %>% 
    mutate(caedec3dC = cases(grupoCaedecC,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                             c('5.Productivo GDI','5.Productivo GDI','4.Productivo GDE','4.Productivo GDE','5.Productivo GDI',
                               '4.Productivo GDE','3.Productivo VIV','2.Comercio','1.Servicios','1.Servicios','1.Servicios','1.Servicios',
                               '1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios'))) %>%
    mutate(labGrupoD = cases(grupoCaedecD,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                             c('A. Agricola','B. Caza, Pesca','C. Ext. Gas y Pet.','D. Ext. Minerales','E. Ind. y Manu.',
                               'F. Dist. EE y agua','G. Construcción','H. Comercio','I. Hoteles','J. Transporte','K. Inter. Fin.',
                               'L. Serv. Inmob.', 'M. Adm. Pública','N. Educación','O. Serv. Hosp + Otros','P. Serv. Doméstico',
                               'Q. Org. Extraterritoriales','Z. Jubilados, Est. y AC'))) %>%
    mutate(labGrupoC = cases(grupoCaedecC,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                             c('A. Agricola','B. Caza, Pesca','C. Ext. Gas y Pet.','D. Ext. Minerales','E. Ind. y Manu.',
                               'F. Dist. EE y agua','G. Construcción','H. Comercio','I. Hoteles','J. Transporte','K. Inter. Fin.',
                               'L. Serv. Inmob.', 'M. Adm. Pública','N. Educación','O. Serv. Hosp + Otros','P. Serv. Doméstico',
                               'Q. Org. Extraterritoriales','Z. Jubilados, Est. y AC'))) %>%
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
    mutate(ctaCont = substr(RUBRO,1,3)) %>% 
    mutate(subCtaCont = substr(RUBRO,1,5)) %>%
    mutate(saldoMora = ifelse(ctaCont %in% c('133','134','136','137'),saldous,0)) %>% 
    mutate(saldoReprog = ifelse(ctaCont %in% c('135','136','137'),saldous,0)) %>% 
    mutate(saldoRepVig = ifelse(ctaCont == '135', saldous, 0)) %>%
    mutate(par0Reprog = ifelse(MODULO == 121 & DIASMORA > 0 , saldous, 0)) %>% 
    mutate(par30Reprog = case_when(ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
    mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIG'), saldous, 0)) %>% 
    mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
    mutate(car = saldoMora + saldoRepVig) %>% 
    mutate(par0 = ifelse(DIASMORA >0, saldous, 0)) %>%
    mutate(par30 = ifelse(DIASMORA >30, saldous, 0)) %>%
    mutate(intus = saldous * TASAACT/100) %>% 
    mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>%
    mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
    mutate(rangos = cut(saldous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                        labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
    mutate(tipoCred = cases(substr(TIPO_CREDITO,1,1), c('M','H','N','P'), c('Micro','Vivienda','Consumo','PyMe'))) %>% 
    mutate(opTot = ifelse(saldoCast > 0, 0,1)) %>% 
    mutate(Sucursal = cut(AGENCIA, breaks = c(100,200,250,300,400,500,600,700,800,900,1000),
                          labels= c('Chuquisaca','La Paz','El Alto','Cochabamba','Oruro',
                                    'Potosí','Tarija','Santa Cruz','Beni','Pando'), right=FALSE)) %>% 
    mutate(Regional = cut(AGENCIA, breaks = c(100,200,250,300,400,500,600,700,800,900,1000),
                          labels= c('Sur','Occidente','El Alto','Centro','Occidente',
                                    'Sur','Centro','Oriente','Oriente','El Alto'), right=FALSE)) %>% 
    mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>%
    mutate(par0Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 0,
                            saldous, 0)) %>%
    mutate(par30Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 30,
                             saldous, 0)) %>%
    mutate(across(c(rangos,rangom,catMora, Sucursal, Regional),~as.character(.x)))
}
sector_cartera <- function(x){
  x %>% 
    mutate(SECTOR_CARTERA = case_when(
      TIPO_CREDITO %in% c("H0","H1","H2") ~ "9.Vivienda No controlada",
      (TIPO_CREDITO %in% c("H3","H4") & MONEDA == 0) ~ "6.Vivienda Controlada",
      TIPO_CREDITO %in% c("N0","N1","N2") ~ "9. CONSUMO",
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         cat %in% c("A","B") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ '1.Prod. Agropec. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (cat=="A"|cat=="B") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV =='Pequeña' & TASAACT<=7)) ~ '1.Prod. Agropec. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (cat=="A"|cat=="B") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ '1.Prod. Agropec. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (cat=="A"|cat=="B") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ '1.Prod. Agropec. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (cat=="A"|cat=="B")) ~ '7.Prod.Agropec.No Controlada',
      (CAEDEC_DEST  %in% c(31600,31700,51508,52592,34400,34500,50103)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV=="Pequeña" & TASAACT<=7)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
         (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         cat %in% c("C","D","E","F","G")) ~ '8.Otra Prod.No Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
            DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                            62101,60221,71110,71120,63041,63042,92320,92330) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
            DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                            62101,60221,71110,71120,63041,63042,92320,92330) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Pequeña" & TASAACT<=7)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
            DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                            62101,60221,71110,71120,63041,63042,92320,92330) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
            DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                            62101,60221,71110,71120,63041,63042,92320,92330) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                             "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                             "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Pequeña" & TASAACT<=7)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                             "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
         DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                             "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
         CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
         (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ '2.Otra prod. Controlada',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") &
         cat=="H") ~ '10.Comercio',
      (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") &
         cat %in% c("I","J","K","L","M","N","O","P","Q","Z")) ~ '11.Servicios',
      TRUE ~ "0" )) 
}
categ <- function(x) {
  x %>% 
  mutate(categ = cases(SECTOR_CARTERA,c('6.Vivienda Controlada','1.Prod. Agropec. Controlada','2.Otra prod. Controlada','3.C2.Sector Turismo',
                                        '4.C3.Prod Intelectual','5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                        '8.Otra Prod.No Controlada','9.Vivienda No controlada'),
                       c('viviendaTC','productivoTC','productivoTC','productivoTC','productivoTC','productivoTC',
                         'productivoTNC','productivoTNC','productivoTNC'))) %>% 
    mutate(categ = ifelse(is.na(categ),'Otros',categ))
}
####____DIFERIDO____####
####VERIFICAR si se debe cambiar rubDif por ctaCont.
diferida <- function(x){
  x %>%
    mutate(SALDO_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_CAPITAL_DIFERIDO), 0, SALDO_CAPITAL_DIFERIDO)) %>%
    mutate(SALDO_INT_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_INT_CAPITAL_DIFERIDO), 0, SALDO_INT_CAPITAL_DIFERIDO)) %>%
    mutate(saldoDifFranz = ifelse((SALDO_CAPITAL_DIFERIDO + SALDO_INT_CAPITAL_DIFERIDO) > 0,
                                  saldous, 0)) %>%
    mutate(rubDif = substr(RUBRO_CAPITAL_DIFERIDO,1,3))%>% 
    mutate(saldoDif = ifelse((rubDif %in% c('131','133','134','135','136','137')) & MONEDA == 0,
                             as.numeric(SALDO_CAPITAL_DIFERIDO)/6.86, 0)) %>% #Hacer con ctaCont en lugar de rubDif porque hay rubDif 0 que tiene saldo diferido
    mutate(saldoDif = ifelse((rubDif %in% c('131','133','134','135','136','137')) & MONEDA != 0, 
                             as.numeric(SALDO_CAPITAL_DIFERIDO), saldoDif)) %>% 
    select(-rubDif) %>% 
    mutate(par0Dif = ifelse(DIASMORA > 0, saldoDif, 0)) %>%
    mutate(par30Dif = ifelse(DIASMORA > 30, saldoDif, 0)) %>%
    mutate(par0DifFranz = ifelse(DIASMORA > 0, saldoDifFranz, 0)) %>%
    mutate(par30DifFranz = ifelse(DIASMORA > 30, saldoDifFranz, 0)) %>%
    mutate(saldoReprogDif = ifelse(MODULO == 121 & saldoDif >0 , saldoReprog, 0)) %>%
    mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0, 
                                  saldoReprog, 0)) %>%
    mutate(saldoReprogDifMora = ifelse(MODULO == 121 & saldoDif > 0 & DIASMORA > 0, 
                                       saldoReprog, 0)) %>%
    mutate(saldoReprogDifMora30 = ifelse(MODULO == 121 & saldoDif > 0 & DIASMORA > 30, 
                                         saldoReprog, 0)) %>%
    mutate(saldoRefinDifMora = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0 & DIASMORA > 0,
                                      saldoReprog, 0)) %>%
    mutate(saldoRefinDifMora30 = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0 & DIASMORA > 30,
                                        saldoReprog, 0))
}
####____READING ADDITIONAL DATABASES____####
N_Campos <- readxl::read_excel("D:/!bso/bases/excel/CodCaedecDestSector.xls", sheet = "subclase") %>% 
  select(CAEDEC_DEST, cat)
ventamm <- readxl::read_excel("D:/!bso/bases/excel/ventamm.xlsx")
codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx", sheet = "Full")
################################################################################
####____2015-2017____####
#2016 ahora funciona bien el fread con encoding UTF-8
#Mayo 2016 tiene un csv el cual se lee con fread y encoding Latin-1. Con el arreglo funciona y con UTF-8
#2015 funciona con fread en encoding UTF-8
#Febrero 2015 aún no funciona. Con el arreglo funciona y con UTF-8
#2017 funciona con fread y encoding UTF-8
mes <- c("May")
years <- c("2015")
mes <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2015','2016', '2017','2018')
for(i in 1:length(mes)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mes[i],years[k]))
      
      # bdc <- read_delim(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
      #                                    mes[i], years[k],'.txt'),
      #                   locale=locale(encoding="latin1"),delim = '|')
      # colCls <- sapply(bdc, class)
      # colCls[which(colCls=="integer64")] <- "character"
      
      # bdc <- fread('D:/!bso/bases/txt/BaseCarteraFeb2015.txt',
      #              encoding = 'UTF-8', fill = T) %>%
      
      # bdc <- read_delim('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraFeb2015_backup.txt',locale=locale(encoding="latin1"),
      #                   delim = '\t') %>%
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                          mes[i], years[k],'.txt'), encoding = "UTF-8",fill = T) %>% #Para febrero el fread no lee bien
      # bdc <- fread(paste0('D:/!bso/bases/BaseCartera',
      #                       mes[i], years[k],'.txt'), encoding = "UTF-8",fill = T) %>% #Para 2016
      # bdc <- read_delim(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
      #                          mes[i], years[k],'.txt'),locale=locale(encoding="latin1"),delim = '\t') %>% 
        remove_empty("cols") %>% 
        left_join(N_Campos, by="CAEDEC_DEST") %>% 
        left_join(select(codAge,-Sucursal,-Regional),by="AGENCIA") %>% 
        left_join(ventamm, by="CAEDEC_DEST") %>%
        dplyr::filter(MODULO != 131) %>% 
        treatment(m=mes[i],y=years[k]) %>% 
        sector_cartera() %>% 
        categ()
        
      saveRDS(bdc, paste0('D:/!bso/girCartera/rds/ec_',
                          mes[i], years[k], '.rds'))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
# PARA AGREGAR CAMPOS DIFERIDOS ANTES DE 2021
#   mutate(SALDO_CAPITAL_DIFERIDO = 0) %>%
#   mutate(SALDO_INT_CAPITAL_DIFERIDO =  0) %>%
#   mutate(saldoDifFranz =  0) %>%
#   mutate(saldoDif =  0) %>%
#   mutate(par0Dif = 0) %>% 
#   mutate(par30Dif = 0) %>% 
#   mutate(par0DifFranz = 0) %>% 
#   mutate(par30DifFranz = 0) %>% 
#   mutate(saldoReprogDif = ifelse(MODULO == 121 & saldoDif > 0 ,
#                                  saldoReprog, 0)) %>% 
#   mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0 ,
#                                 saldoReprog, 0)) %>% 
#   mutate(saldoReprogDifMora = 0) %>% 
#   mutate(saldoReprogDifMora30 =  0) %>% 
#   mutate(saldoRefinDifMora = 0) %>% 
#   mutate(saldoRefinDifMora30 = 0)

####____2018-2020____###########################################################
#2018 Funciona con fread
#Ene18,May18,Ago18 Funciona con fread enconding UTF-8
#Feb18,Mar18,Abr18,Jun18,Jul18,Sep18,Oct18,Nov18,Dic18 Funciona con fread encoding Latin-1
#Con el arreglo 2018 funciona todo con UTF-8
#2019 Funciona con fread encoding Latin-1
#Hasta Mayo 2019 hay agencias que no se encuentran en codAge
#2020 Funciona con fread enconding Latin-1
bdcList <- list()
mes <- c('Feb', 'Mar', 'Abr','Jun','Jul', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2018')
mes <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2019','2020')
i <- 8
k <- 1
# for(i in 1:length(mes)) {
#   for(k in 1:length(years)) {
#     print(paste0(mes[i],years[k]))
#     bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
#                         mes[i], years[k],'.txt'),
#                  encoding = 'Latin-1', fill = T) %>% #UTF-8 para 2018-2019 y Latin-1 2020
#     # bdc <- fread(paste0('D:/!bso/backup/BaseCartera',mes[i],years[k],'.txt'),
#     #              encoding = 'Latin-1', fill = T) %>%
#       # select(1:71) %>% 
#       remove_empty("cols")
#     bdc <- as.data.frame(bdc)
#     write.table(bdc,file = paste0("D:/!bso/bases/BaseCartera",mes[i],years[k],'.txt'),
#                   sep = "|",fileEncoding = "UTF-8",quote = FALSE,row.names = FALSE)
#     # write_delim(bdc,file = paste0("D:/!bso/bases/BaseCartera",mes[i],years[k],'.txt'),
#     #             delim = '|',quote = "none",)
#   }
# }
for(i in 1:length(mes)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mes[i],years[k]))
      
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                          mes[i], years[k],'.txt'),
                   encoding = 'UTF-8', fill = T) %>% #UTF-8 para 2018-2019 y Latin-1 2020
        remove_empty("cols") %>% 
        left_join(N_Campos, by="CAEDEC_DEST") %>% 
        left_join(ventamm, by="CAEDEC_DEST") %>%
        left_join(select(codAge,-Sucursal,-Regional), by="AGENCIA") %>% 
        dplyr::filter(MODULO != 131) %>% 
        treatment(m=mes[i],y=years[k]) %>%
        sector_cartera() %>%
        categ()
      
      saveRDS(bdc, paste0('D:/!bso/girCartera/rds/ec_',
                          mes[i], years[k], '.rds'))  
      #bdcList[[i]] <- bdc
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

####____2021____################################################################
#2021 funciona con fread encoding Latin-1
bdcList <- list()
mes <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2021')
for(i in 1:length(mes)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mes[i],years[k]))
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                          mes[i], years[k],'.txt'), 
                   encoding = 'Latin-1', fill = T) %>% 
        remove_empty("cols") %>% 
        left_join(N_Campos, by="CAEDEC_DEST") %>% 
        left_join(ventamm, by="CAEDEC_DEST") %>%
        left_join(codAge, by="AGENCIA") %>% 
        dplyr::filter(MODULO != 131) %>% 
        treatment(m=mes[i],y=years[k]) %>% 
        sector_cartera() %>% 
        categ() %>% 
        diferida()
      
      saveRDS(bdc, paste0('D:/!bso/girCartera/rds/ec_',
                          mes[i], years[k], '.rds'))
      #bdcList[[i]] <- bdc
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
####____2022-2023____###########################################################
#2022 y 2023 funcionan con fread encoding Latin-1
mes <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2022','2023')
i <- 10
k <- 2
for(i in 1:length(mes)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mes[i],years[k]))
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                          mes[i], years[k],'_old.txt'), 
                   encoding = 'Latin-1', fill = T) %>%
        remove_empty("cols") %>% 
        # mutate(CALIFICACION_old = CALIFICACION) %>% 
        # mutate(DIASMORA_old = DIASMORA) %>%
        # mutate(DIASMORA = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 0, DIASMORA)) %>%
        # mutate(CALIFICACION = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 'A', CALIFICACION)) %>%
      # bdc <- fread(paste0('D:/!bso/bases/bdcmensual/BaseCartera', 
      #                     mes[i], years[k],'.txt'), 
      #              encoding = 'Latin-1', fill = T) %>% 
        left_join(N_Campos, by="CAEDEC_DEST") %>% 
        left_join(ventamm, by="CAEDEC_DEST") %>%
        left_join(select(codAge,-Sucursal,-Regional), by="AGENCIA") %>%
        dplyr::filter(MODULO != 131) %>% 
        treatment(m=mes[i],y=years[k]) %>% 
        # sector_cartera() %>% 
        categ() %>% 
        diferida() %>% 
        # mutate(categ2 = cases(SECTOR_CARTERA,c('6.Vivienda Controlada','1.Prod. Agropec. Controlada','2.Otra prod. Controlada','3.C2.Sector Turismo',
        #                                       '4.C3.Prod Intelectual','5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
        #                                       '8.Otra Prod.No Controlada','9.Vivienda No Controlada'),
        #                      c('viviendaTC','productivoTC','productivoTC','productivoTC','productivoTC','productivoTC',
        #                        'productivoTNC','productivoTNC','productivoTNC'))) %>% 
        # mutate(categ2 = ifelse(is.na(categ2),'Otros',categ2)) %>% 
        # mutate(categ3 = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
        #                          SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
        #                            SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
        #                            SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
        #                            SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
        #                            SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
        #                          SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
        #                            SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
        #                            SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%
        # mutate(categ3 = ifelse(is.na(categ3), 'Otros', categ3)) %>% 
        glimpse()
      
      saveRDS(bdc, paste0('D:/!bso/girCartera/rds/ec_',mes[i], years[k], '.rds'))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
####____CUADRE CTACONT____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Oct2023.rds")
bdc%>%
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"),1,0)) %>%
  dplyr::filter(esFSL==0 | esFSL==1, ctaCont %in% c('131','133','134','135','136','137')) %>%
  group_by(ctaCont) %>% 
  summarise(Saldo=sum(saldous)) %>% 
  adorn_totals("row")


x <- bdcprev %>% 
  dplyr::filter(esFSL==1)

table(x$CALIFICACION)

x %>% dplyr::filter(CALIFICACION.x!=CALIFICACION.y) %>% 
  select(CTACLIENTE, OPERACION, TIPO_OPER.x, TIPO_OPER.y, MODULO.x, MODULO.y,
         CALIFICACION.x, CALIFICACION.y)
x %>% dplyr::filter(ctaCont.x!=ctaCont.y) %>% 
  select(CTACLIENTE, OPERACION, TIPO_OPER.x, TIPO_OPER.y, MODULO.x, MODULO.y,
         ctaCont.x, ctaCont.y)

#CORRECCION MANUAL CARTERA JULIO 2023
bdcprev <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds") %>% 
  dplyr::filter(OPERACION==3012230) %>% 
  select(CTACLIENTE, OPERACION, ends_with("TIT"), TIPO_CREDITO, tipoCred)
bdcprev$TIPO_CREDITO[bdcprev$OPERACION==3012230]
bdcprev$tipoCred[bdcprev$OPERACION==3012230]

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Jul2023.rds") 
bdc$TIPO_CREDITO[bdc$OPERACION==3012230] <- "M1"
bdc$tipoCred[bdc$OPERACION==3012230] <- "Micro"
saveRDS(bdc, "D:/!bso/girCartera/rds/ec_Jul2023.rds")
################################################################################
####____REVISION DE CAMBIO DE CTACLIENTE____####
Reclass <- read_xlsx("D:/Files/Shared/Anexo2_Cuentas_reclasificadas.xlsx")

cicJun <- readRDS("D:/!bso/CIC/rds/cic_Jun2023.rds")
cicJul <- readRDS("D:/!bso/CIC/rds/cic_Jul2023.rds")

n_distinct(cicJun$CI)
n_distinct(cicJun$CTACLIENTE)
x <- cicJun %>% 
  group_by(CI) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  mutate(nctas = n_distinct(CTACLIENTE)) %>% 
  mutate(nBFS = ifelse(!is.na(CodEnvioOrigen) & CodEnvioOrigen=="IBBFS", 1, 0)) %>% 
  mutate(nBSO = ifelse(is.na(CodEnvioOrigen), 1, 0)) %>% 
  mutate(nBFS = max(nBFS)) %>% 
  mutate(nBSO = max(nBSO)) %>% 
  ungroup() %>% 
  arrange(CI) %>% relocate(CI, CodEnvioOrigen, nBFS, nBSO, .after = OPERACION)

bdcJun <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds")
bdcJul <- readRDS("D:/!bso/girCartera/rds/ec_Jul2023.rds")

cic <- readRDS("D:/!bso/CIC/rds/cic_Abr2023.rds")
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Abr2023.rds")

n_distinct(cic$CI)
n_distinct(cic$CTACLIENTE)
n_distinct(bdcJul$CI)
n_distinct(bdcJul$CTACLIENTE)

y <- bdcJul %>% 
  group_by(CI) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  arrange(CI)

y2 <- bdcJul %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
  group_by(NDOC) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  arrange(NDOC) %>% relocate(NDOC, .after=CI)


####____MENSUAL MANUAL____####
#Para febrero 2015 usar read_delim con delim="|"
#Para abril 2015 usar read_delim con delim="\t"
mes <- c('Feb')
years <- c('2015')

bdc <- read_delim(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                         mes, years,'.txt'),
                  locale=locale(encoding="latin1"),delim = '|') %>%   
  left_join(N_Campos, by="CAEDEC_DEST") %>% 
  left_join(ventamm, by="CAEDEC_DEST") %>%  
  
  dplyr::filter(MODULO != 131) %>% 
  mutate(fbase = paste0(mes, years)) %>% 
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
  mutate(mes = cases(mon,c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic'),
                     c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))) %>% 
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
  mutate(grupoCaedecD = cut(as.numeric(divCaedecD),breaks=c(0,1,5,9,14,37,41,45,52,55,64,67,74,75,80,93,95,98,99),
                            labels=c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'))) %>%
  mutate(grupoCaedecD = as.character(grupoCaedecD)) %>% 
  mutate(grupoCaedecD = ifelse(divCaedecD=='11','C',grupoCaedecD)) %>% 
  mutate(caedec3dD = cases(grupoCaedecD,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                           c('5.Productivo GDI','5.Productivo GDI','4.Productivo GDE','4.Productivo GDE','5.Productivo GDI',
                             '4.Productivo GDE','3.Productivo VIV','2.Comercio','1.Servicios','1.Servicios','1.Servicios','1.Servicios',
                             '1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios'))) %>%
  mutate(divCaedecC = substr(CIU,1,2)) %>% 
  mutate(grupoCaedecC = cut(as.numeric(divCaedecC),breaks=c(0,1,5,9,14,37,41,45,52,55,64,67,74,75,80,93,95,98,99),
                            labels=c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'))) %>%
  mutate(grupoCaedecC = as.character(grupoCaedecC)) %>% 
  mutate(grupoCaedecC = ifelse(divCaedecC=='11','C',grupoCaedecC)) %>% 
  mutate(caedec3dC = cases(grupoCaedecC,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                           c('5.Productivo GDI','5.Productivo GDI','4.Productivo GDE','4.Productivo GDE','5.Productivo GDI',
                             '4.Productivo GDE','3.Productivo VIV','2.Comercio','1.Servicios','1.Servicios','1.Servicios','1.Servicios',
                             '1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios'))) %>%
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(labGrupoD = cases(grupoCaedecD,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                           c('A. Agricola','B. Caza, Pesca','C. Ext. Gas y Pet.','D. Ext. Minerales','E. Ind. y Manu.',
                             'F. Dist. EE y agua','G. Construcción','H. Comercio','I. Hoteles','J. Transporte','K. Inter. Fin.',
                             'L. Serv. Inmob.', 'M. Adm. Pública','N. Educación','O. Serv. Hosp + Otros','P. Serv. Doméstico',
                             'Q. Org. Extraterritoriales','Z. Jubilados, Est. y AC'))) %>%
  mutate(labGrupoC = cases(grupoCaedecC,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                           c('A. Agricola','B. Caza, Pesca','C. Ext. Gas y Pet.','D. Ext. Minerales','E. Ind. y Manu.',
                             'F. Dist. EE y agua','G. Construcción','H. Comercio','I. Hoteles','J. Transporte','K. Inter. Fin.',
                             'L. Serv. Inmob.', 'M. Adm. Pública','N. Educación','O. Serv. Hosp + Otros','P. Serv. Doméstico',
                             'Q. Org. Extraterritoriales','Z. Jubilados, Est. y AC'))) %>%
  mutate(SECTOR_CARTERA = case_when(
    TIPO_CREDITO %in% c("H0","H1","H2") ~ "6. VIVIENDA NO CONTROLADA",
    (TIPO_CREDITO %in% c("H3","H4") & MONEDA == 0) ~ "3. VIVIENDA CONTROLADA",
    TIPO_CREDITO %in% c("N0","N1","N2") ~ "9. CONSUMO",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       cat %in% c("A","B") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (cat=="A"|cat=="B") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV =='Pequeña' & TASAACT<=7)) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (cat=="A"|cat=="B") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (cat=="A"|cat=="B") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ "1. PRODUCCION AGROPECUARIA CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (cat=="A"|cat=="B")) ~ "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
    (CAEDEC_DEST  %in% c(31600,31700,51508,52592,34400,34500,50103)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV=="Pequeña" & TASAACT<=7)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       cat %in% c("C","D","E","F","G") & (TIPOTASA=="F" & MONEDA==0) &
       (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       cat %in% c("C","D","E","F","G")) ~ "5. OTRA PRODUCCION NO CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
          DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                          62101,60221,71110,71120,63041,63042,92320,92330) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
          DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                          62101,60221,71110,71120,63041,63042,92320,92330) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Pequeña" & TASAACT<=7)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
          DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                          62101,60221,71110,71120,63041,63042,92320,92330) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       (DESC_OBJCRED=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
          DESC_OBJCRED=="CAPITAL DE INVERSION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(55101,55102,55103,55201,60100,60212,60222,61200,
                          62101,60221,71110,71120,63041,63042,92320,92330) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                           "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Micro" & TASAACT<=11.5)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                           "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Pequeña" & TASAACT<=7)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                           "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="DMediana" & TASAACT<=6)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") & 
       DESC_OBJCRED %in% c("CAPITAL DE INVERSION ACT. PRINCIPAL","CAPITAL DE INVERSION ACT. SECUNDARIAS",
                           "CAPITAL DE OPERACION ACT. PRINCIPAL","CAPITAL DE OPERACION ACT. SECUNDARIAS") &
       CAEDEC_DEST %in% c(72200,73101,73102,73200,92110,92141) &
       (TIPOTASA=="F" & MONEDA==0) & (TAM_ACTIV=="Grande" & TASAACT<=6)) ~ "2. OTRA PRODUCCION CONTROLADA",
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") &
       cat=="H") ~ "7. COMERCIO" ,
    (TIPO_CREDITO %in% c("M0","M1","M2","M7","M8","P1","P8","P9","P2","P3") &
       cat %in% c("I","J","K","L","M","N","O","P","Q","Z")) ~ "8. SERVICIOS",
    TRUE ~ "0" ))  %>%
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(categ = cases(SECTOR_CARTERA,c('3. VIVIENDA CONTROLADA','1. PRODUCCION AGROPECUARIA CONTROLADA',
                                        '2. OTRA PRODUCCION CONTROLADA','4. PRODUCCION AGROPECUARIA NO CONTROLADA ',
                                        '5. OTRA PRODUCCION NO CONTROLADA','6. VIVIENDA NO CONTROLADA'),
                       c('viviendaTC','productivoTC','productivoTC','productivoTNC','productivoTNC','productivoTNC'))) %>%
  mutate(categ = ifelse(is.na(categ),'Otros',categ))  %>%
  mutate(saldoMora = ifelse(ctaCont %in% c('133','134','136','137'),saldous,0)) %>% 
  mutate(saldoReprog = ifelse(ctaCont %in% c('135','136','137'),saldous,0)) %>% 
  mutate(saldoRepVig = ifelse(ctaCont == '135', saldous, 0)) %>%
  mutate(par0Reprog = ifelse(MODULO == 121 & DIASMORA > 0 , saldous, 0)) %>% 
  mutate(par30Reprog = case_when(ctaCont == '136'~saldous,
                                 ctaCont == '137'~saldous,
                                 TRUE ~ 0)) %>% 
  mutate(saldoCast = ifelse(str_detect(ESTADO,'CASTIG'), saldous, 0)) %>% 
  mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
  mutate(car = saldoMora + saldoRepVig) %>% 
  mutate(par0 = ifelse(DIASMORA >0, saldous, 0)) %>%
  mutate(par30 = ifelse(DIASMORA >30, saldous, 0)) %>%
  mutate(intus = saldous * TASAACT/100) %>% 
  mutate(opTot = 1) %>% 
  mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>%
  mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
  mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
  mutate(rangos = cut(saldous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
  mutate(tipoCred = cases(substr(TIPO_CREDITO,1,1), c('M','H','N','P'), c('Micro','Vivienda','Consumo','PyMe'))) %>% 
  left_join(codAge,by="AGENCIA")

saveRDS(bdc, paste0('D:/!bso/girCartera/rdsGAR/ec_', mes, years, '.rds'))

####____EJEMPLOS DIFERIDAS DON GONZALO____####
bdcSep <- readRDS("D:/!bso/girCartera/rds/ec_Sep2022.rds")
set.seed(05062023)
Ejemplos <- bdcSep %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  dplyr::filter(ctaCont %in% c('131','135')) %>%
  dplyr::filter(FDESEMBOLSO + PLAZODIAS < as.Date("2022-08-01")) %>% #Criterio añadido
  mutate(TieneSaldoDif = ifelse(saldoDif>0,1,0)) %>% 
  dplyr::filter(TieneSaldoDif==1) %>% 
  group_by(ctaCont, TieneSaldoDif) %>% 
  sample_n(size = 5) %>% 
  ungroup() %>% 
  select(-TieneSaldoDif) %>% 
  select(1:93)

write_xlsx(Ejemplos,"D:/!bso/girCartera/Ejemplos_OpDiferidas_Sep2022_Dif_Iniciado.xlsx")

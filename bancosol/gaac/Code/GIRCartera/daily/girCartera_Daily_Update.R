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
library(scales)
library(openxlsx)
library(janitor)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FUNCIONES____####
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
treatmentDaily <- function(x,yyyymmdd="20150101"){
  x %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),~dmy(.x))) %>% 
    mutate(fdes = FDESEMBOLSO) %>% 
    mutate(dayDate = ymd(yyyymmdd)) %>% 
    mutate(month = month(dayDate)) %>% 
    mutate(year = year(dayDate)) %>% 
    mutate(cosechaY = year(fdes)) %>% 
    mutate(cosechaM = as.yearmon(fdes)) %>% 
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                        labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
    mutate(catMora = cut(as.numeric(DIASMORA),breaks=c(-Inf,0,5,30,90,Inf),labels=c('1. 0 dias','2. 1-5 dias','3. 6-30 dias','4. 31-90 dias','5. 90+ dias'))) %>% 
    mutate(montous = ifelse(fdes != dayDate, 0, montous)) %>% 
    mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
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
    mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>%
    mutate(par0Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 0,
                            saldous, 0)) %>%
    mutate(par30Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 30,
                             saldous, 0)) %>%
    mutate(across(c(rangos,rangom,catMora),~as.character(.x)))
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
####____READING ADDITIONAL DATABASES____####
N_Campos <- readxl::read_excel("D:/!bso/bases/excel/CodCaedecDestSector.xls", sheet = "subclase") %>% 
  select(CAEDEC_DEST, cat)
ventamm <- readxl::read_excel("D:/!bso/bases/excel/ventamm.xlsx")
codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
################################################################################
####____DAILY BDC____####
#2021
file_list <- list.files('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria',
                        pattern = "2021")
flist <- file_list[-c(163:172)]
for (i in 1:length(flist)){
  tryCatch({
    print(flist[i])
    bdcDiaria <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria/',
                              flist[i]),  encoding = 'UTF-8', fill = T) %>%
      #left_join(N_Campos, by="CAEDEC_DEST") %>% 
      #left_join(ventamm, by="CAEDEC_DEST") %>%
      left_join(N_Campos, by="CAEDEC_DEST") %>% 
      left_join(ventamm, by="CAEDEC_DEST") %>%
      left_join(codAge,by="AGENCIA") %>% 
      dplyr::filter(MODULO != 131) %>% 
      treatmentDaily(yyyymmdd = substr(flist[i], 13, nchar(flist[i])-4)) %>% 
      sector_cartera() %>% 
      categ() 
    fn <- substr(flist[i], 13, nchar(flist[i])-4)
    saveRDS(bdcDiaria, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_', fn, '.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
#2022-2023
file_list <- list.files('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria',
                        pattern = "202310")
flist <- file_list
flist <- c("BaseCartera_20231028.txt")
flist <- c("BaseCartera_20230114.txt")
i <- 1
for (i in 1:length(flist)){
  tryCatch({
    print(flist[i])
    bdcDiaria <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria/',
                        flist[i]),  encoding = 'UTF-8', fill = T) %>%
      #left_join(N_Campos, by="CAEDEC_DEST") %>% 
      #left_join(ventamm, by="CAEDEC_DEST") %>%
      left_join(N_Campos, by="CAEDEC_DEST") %>% 
      left_join(codAge,by="AGENCIA") %>% 
      left_join(ventamm, by="CAEDEC_DEST") %>%
      dplyr::filter(MODULO != 131) %>% 
      treatmentDaily(yyyymmdd = substr(flist[i], 13, nchar(flist[i])-4)) %>% 
      categ() 
    fn <- substr(flist[i], 13, nchar(flist[i])-4)
    saveRDS(bdcDiaria, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_', fn, '.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

bdcDiaria <- readRDS('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_20230119.rds')

####____CONSOLIDADOS MENSUALES_____####
year <- c(2021:2023)
month <- str_pad(1:12,2,side="left",pad="0")
my <- as.vector(sapply(year, function(x){paste0(x,month)}))
my <- my[-c(1:which(my=="202105"),which(my=="202305"):length(my))]
#ACTUALIZAR MY PARA 
my <- c("202310") 
dia <- str_pad(1:31,2,side="left",pad="0")
i <- 1
j <- 15
for (i in 1:length(my)) {
  bdcList <- list()
  k <- 1
  for (j in 1:length(dia)) {
    tryCatch({
      print(paste(my[i],dia[j]))
      bdc <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_',
                            my[i],dia[j],'.rds')) %>% 
        mutate(saldous = ifelse(saldoCast>0,saldoCast,saldous)) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        mutate(dayDate = as.Date(dayDate)) %>%
        select(CTACLIENTE, OPERACION, OPERACION_ORI_REF, ctaCont, fdes, dayDate, 
               MONTOUS, saldous, previus, intus, DIASMORA, CALIFICACION) 
      bdcList[[k]] <- bdc
      k <- k+1
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  bdcFull <- rbindlist(bdcList)
  saveRDS(bdcFull, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_',
                          my[i],'.rds'))
}

####____DAILY DELINQUENCY DATA LOOP____####
mm <- "202303"
year <- c(2022:2023)
month <- str_pad(1:12,2,side="left",pad="0")
my <- as.vector(sapply(year, function(x){paste0(x,month)}))
my <- my[-c(which(my==mm):length(my))]
i <- 2
for (i in 1:length(my)) {
  print(my[i])
  if(i==1){
    daily <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_",
                            my[i],".rds")) %>% 
      select(CTACLIENTE, DIASMORA) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
      ungroup()
  }
  else{
    dAux <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_",
                           my[i],".rds")) %>% 
      select(CTACLIENTE, DIASMORA) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
      ungroup()
    daily <- daily %>% 
      full_join(dAux, by="CTACLIENTE", suffix=c("_old","_new")) %>% 
      group_by(CTACLIENTE) %>% 
      mutate(maxMoraIM_cl = ifelse(!is.na(maxMoraIM_cl_old) | !is.na(maxMoraIM_cl_new), 
                                   max(maxMoraIM_cl_old,maxMoraIM_cl_new, na.rm = T),0)) %>% 
      ungroup() %>% 
      select(-maxMoraIM_cl_old, -maxMoraIM_cl_new)
  }
}

saveRDS(daily, "D:/!bso/vipCartera/dailyMM_Feb2023.rds")

####____UPDATE A MONTH TO DAILYMM____####
dailyMM <- readRDS("D:/!bso/vipCartera/dailyMM/dailyMM_Sep2023.rds")
n_distinct(dailyMM$CTACLIENTE)
my <- "202310"
dAux <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_",
                       my,".rds")) %>% 
  select(CTACLIENTE, DIASMORA) %>% 
  group_by(CTACLIENTE) %>% 
  summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
  ungroup()
dailyMM <- dailyMM %>% 
  full_join(dAux, by="CTACLIENTE", suffix=c("_old","_new")) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(maxMoraIM_cl = ifelse(!is.na(maxMoraIM_cl_old) | !is.na(maxMoraIM_cl_new), 
                               max(maxMoraIM_cl_old,maxMoraIM_cl_new, na.rm = T),0)) %>% 
  ungroup() %>% 
  select(-maxMoraIM_cl_old, -maxMoraIM_cl_new)
n_distinct(dailyMM$CTACLIENTE)

saveRDS(dailyMM, "D:/!bso/vipCartera/dailyMM/dailyMM_Oct2023.rds")

dailyMM_old <- readRDS("D:/!bso/vipCartera/dailyMM/dailyMM_Sep2023_old.rds")
dailyMM_old <- dailyMM_old %>% 
  left_join(dailyMM,by=c("CTACLIENTE"),suffix=c("_old","_new")) %>% 
  dplyr::filter(!is.na(maxMoraIM_cl_new)) %>% 
  mutate(Check = maxMoraIM_cl_old==maxMoraIM_cl_new)

dailyMM_old %>% 
  mutate(CheckNew = ifelse(Check==FALSE & maxMoraIM_cl_old==0 & maxMoraIM_cl_new>0,1,0)) %>% 
  group_by(Check,CheckNew) %>% 
  summarise(N=n())
table(dailyMM_old$Check)

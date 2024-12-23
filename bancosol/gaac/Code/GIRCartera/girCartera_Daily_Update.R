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
categ <- function(x) {
  x %>% 
    mutate(categ = cases(SECTOR_CARTERA,c('6.Vivienda Controlada','1.Prod. Agropec. Controlada','2.Otra prod. Controlada','3.C2.Sector Turismo',
                                          '4.C3.Prod Intelectual','5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                          '8.Otra Prod.No Controlada','9.Vivienda No controlada'),
                         c('viviendaTC','productivoTC','productivoTC','productivoTC','productivoTC','productivoTC',
                           'productivoTNC','productivoTNC','productivoTNC'))) %>% 
    mutate(categ = ifelse(is.na(categ),'Otros',categ))
}
####____DAILY BDC____####
flist <- list.files('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria',
                        pattern = "202307")
for (i in 1:length(flist)){
  tryCatch({
    print(flist[i])
    bdcDiaria <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria/',
                        flist[i]),  encoding = 'UTF-8', fill = T) %>%
      #left_join(N_Campos, by="CAEDEC_DEST") %>% 
      #left_join(ventamm, by="CAEDEC_DEST") %>%
      
      dplyr::filter(MODULO != 131) %>% 
      treatmentDaily(yyyymmdd = substr(flist[i], 13, nchar(flist[i])-4)) %>% 
      #mutate(fbase = paste0(mos2[i], years[k])) %>% 
      glimpse()
    fn <- substr(flist[i], 13, nchar(flist[i])-4)
    saveRDS(bdcDiaria, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_', fn, '.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
#library(reshape)
#library(reshape2)
library(stringr)
library(lubridate)
#library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
#library(forecast)
library(quantmod)
#library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
library(tseries)
#library(xtable)
#library(openxlsx)
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
cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  #new <- vector(mode = 'character',length = length(quant))
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}

################################################################################
# Data in
N_Campos <- readxl::read_excel("D:/!bso/girCartera/Clasificacion_Sector_Economico_Modificado.xls", sheet = "subclase") %>% 
  select(CAEDEC_DEST, cat)
ventamm <- readxl::read_excel("D:/!bso/girCartera/ventamm.xlsx") 

###############################################################################
#DATAPREP
mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
mos2 <- c('Sep')
years <- c('2022')
i <- 1
k <- 1
for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc <- fread(paste0('D:/!bso/mph/BaseCartera', 
                          mos2[i], years[k],'.txt'), 
                   encoding = 'Latin-1', fill = T,nrows = 1)
      colCls <- sapply(bdc, class)
      colCls[which(colCls=="integer64")] <- "character"
      bdc <- fread(paste0('D:/!bso/mph/BaseCartera', 
                          mos2[i], years[k],'.txt'), 
                   encoding = 'Latin-1', fill = T,colClasses = colCls) %>% 
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
        mutate(catMora = cut(as.numeric(DIASMORA),breaks=c(-Inf,0,5,30,90,Inf),labels=c('1. 0 dias','2. 1-5 dias','3. 6-30 dias','4. 31-90 dias','5. 90+ dias'))) %>% 
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
        # mutate(grupoCaedecD = cases(divCaedecD,c('01','02','03','05','11','10','12','13','14','15','16','17','18','19','20','21','22','23','24',
        #                                          '25','26','27','28','29','30','31','32','33','34','35','36','37','40','41','45','50','51','52',
        #                                          '55','60','61','62','63','64','65','66','67','70','71','72','73','74','75','80','85','90','91',
        #                                          '92','93','95','98','99'),
        #                             c('A','B','B','B','C','D','D','D','D','E','E','E','E','E','E','E','E','E','E','E','E','E','E','E','E',
        #                               'E','E','E','E','E','E','E','F','F','G','H','H','H','I','J','J','J','J','J','K','K','K','L','L','L','L',
        #                               'L','M','N','O','O','O','O','O','P','Q','Z'))) %>%
        mutate(caedec3dD = cases(grupoCaedecD,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                                 c('5.Productivo GDI','5.Productivo GDI','4.Productivo GDE','4.Productivo GDE','5.Productivo GDI',
                                   '4.Productivo GDE','3.Productivo VIV','2.Comercio','1.Servicios','1.Servicios','1.Servicios','1.Servicios',
                                   '1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios','1.Servicios'))) %>%
        mutate(divCaedecC = substr(CIU,1,2)) %>%
        mutate(grupoCaedecC = cut(as.numeric(divCaedecC),breaks=c(0,1,5,9,14,37,41,45,52,55,64,67,74,75,80,93,95,98,99),
                                  labels=c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'))) %>%
        mutate(grupoCaedecC = as.character(grupoCaedecC)) %>% 
        mutate(grupoCaedecC = ifelse(divCaedecC=='11','C',grupoCaedecC)) %>% 
        # mutate(grupoCaedecD = cases(divCaedecD,c('01','02','03','05','11','10','12','13','14','15','16','17','18','19','20','21','22','23','24',
        #                                          '25','26','27','28','29','30','31','32','33','34','35','36','37','40','41','45','50','51','52',
        #                                          '55','60','61','62','63','64','65','66','67','70','71','72','73','74','75','80','85','90','91',
        #                                          '92','93','95','98','99'),
        #                             c('A','B','B','B','C','D','D','D','D','E','E','E','E','E','E','E','E','E','E','E','E','E','E','E','E',
        #                               'E','E','E','E','E','E','E','F','F','G','H','H','H','I','J','J','J','J','J','K','K','K','L','L','L','L',
        #                               'L','M','N','O','O','O','O','O','P','Q','Z'))) %>%
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
        mutate(categ = cases(SECTOR_CARTERA,c('6.Vivienda Controlada','1.Prod. Agropec. Controlada','2.Otra prod. Controlada','3.C2.Sector Turismo',
                                              '4.C3.Prod Intelectual','5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                              '8.Otra Prod.No Controlada','9.Vivienda No Controlada'),
                             c('viviendaTC','productivoTC','productivoTC','productivoTC','productivoTC','productivoTC',
                               'productivoTNC','productivoTNC','productivoTNC'))) %>%
        mutate(categ = ifelse(is.na(categ),'Otros',categ))  %>% 
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
        mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                            labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
        mutate(tipoCred = cases(substr(TIPO_CREDITO,1,1), c('M','H','N','P'), c('Micro','Vivienda','Consumo','PyMe'))) %>% 
        mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
        mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal)) %>% 
        ###___AQUI
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
        mutate(SALDO_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_CAPITAL_DIFERIDO), 0, SALDO_CAPITAL_DIFERIDO)) %>%
        mutate(SALDO_INT_CAPITAL_DIFERIDO = ifelse(is.na(SALDO_INT_CAPITAL_DIFERIDO), 0, SALDO_INT_CAPITAL_DIFERIDO)) %>%
        mutate(saldoDifFranz = ifelse((SALDO_CAPITAL_DIFERIDO + SALDO_INT_CAPITAL_DIFERIDO) > 0,
                                        saldous, 0)) %>%
        mutate(rubDif = substr(RUBRO_CAPITAL_DIFERIDO,1,3))%>% 
        mutate(saldoDif = ifelse((rubDif %in% c('131','133','134','135','136','137')) & MONEDA == 0,
                                   as.numeric(SALDO_CAPITAL_DIFERIDO)/6.86, 0)) %>% 
        mutate(saldoDif = ifelse((rubDif %in% c('131','133','134','135','136','137')) & MONEDA != 0, 
                                   as.numeric(SALDO_CAPITAL_DIFERIDO), saldoDif)) %>% 
        select(-rubDif) %>% 
        mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>%
        mutate(par0Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 0,
                                     saldous, 0)) %>%
        mutate(par30Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 30,
                                       saldous, 0)) %>%
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
                                            saldoReprog, 0)) %>%
        mutate(sucursal = cases(sucursal,c('1','10','2','3','4','5','6','7','8','9'),
                                c('Chuquisaca','El Alto','La Paz','Cochabamba','Oruro',
                                'Potosí','Tarija','Santa Cruz','Beni','Pando')))
        ###___AQUI
      saveRDS(bdc, paste0('D:/!bso/girCartera/rds/ec_',
                          mos2[i], years[k], '.rds'))
      
      #bdcList[[i]] <- bdc
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}


################################################################################
#PROCESS
gc()

bdcList <- list()
short_list <- c('ec_Ene2022.rds', 'ec_Feb2022.rds', 'ec_Mar2022.rds',
                'ec_Abr2022.rds', 'ec_May2022.rds', 'ec_Jun2022.rds',
                'ec_Jul2022.rds', 'ec_Ago2022.rds', 'ec_Sep2022.rds')
short_list <- c('ec_Sep2022.rds')
for(i in 1:length(short_list)) {
  bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/', short_list[i]))
  sumBDC_full <- bdcNC %>%
    select(monDate, categ, sucursal, rangos, rangom, saldous, montous,
           previus, intus, categ, opDes, saldoDif,saldoReprog, saldoMora,
           saldoCast, par0, par30, opTot, par0Reprog, saldoRepVig, par30Reprog,
           saldoDifFranz, par0DifFranz, par30Dif, par0Dif,
           par30DifFranz, MODULO, SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO,
           saldoRef, saldoRefinDif, saldoRefinDifMora, saldoRefinDifMora30, par0Ref,
           par30Ref,RUBRO_CAPITAL_DIFERIDO, tipoCred, Sector_Destino, Sector_Actividad) %>%
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
                PaR_30_Reprogramada = par0Reprog,
                Cartera_Castigada = saldoCast,
                PaR_0_Bruta = par0,
                PaR_30_Bruta = par30,
                Cartera_Reprogramada_Vigente = saldoRepVig,
                PaR_0_Reprogramada = par30Reprog,
                Cartera_Diferida_ASFI = saldoDif,
                Cartera_Diferida_RC = saldoDifFranz,
                PaR_0_Refinanciada = par0Ref,
                PaR_30_Refinanciada = par30Ref,
                Cartera_Refinanciada = saldoRef,
                PaR_0_Diferida_ASFI = par0Dif,
                PaR_30_Diferida_ASFI = par30Dif ,
                PaR_0_Diferida_RC = par0DifFranz,
                PaR_30_Diferida_RC = par30DifFranz ,
                Fecha = dayDate,
                Tipo_Credito = tipoCred) %>%
  select(-starts_with('saldo'), -starts_with('RUBRO')) %>% 
  
  glimpse()

bdcAgg_2 <- bdcFinalExp %>%
  ungroup() %>%
  select(-Sucursal, -Rango_Desembolso, -Rango_Saldo_Actual, -Sector_Cartera,
         -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  group_by(Fecha) %>%
  summarise_all(sum, na.rm = T) %>% 
  mutate(mora =PaR_30_Bruta/Cartera_Bruta*100) %>% 
  mutate(mora2 = PaR_1_Bruta/Cartera_Bruta*100)

write_xlsx(bdcAgg_2,path = "D:/!bso/girCartera/rds/bdxSep.xlsx")
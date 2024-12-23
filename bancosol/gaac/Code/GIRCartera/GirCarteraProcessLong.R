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
#############################################################################
# Appending
long_list <- c('ec_Ene2015.rds', 'ec_Feb2015.rds', 'ec_Mar2015.rds',
               'ec_Abr2015.rds', 'ec_May2015.rds', 'ec_Jun2015.rds',
               'ec_Jul2015.rds', 'ec_Ago2015.rds', 'ec_Sep2015.rds',
               'ec_Oct2015.rds', 'ec_Nov2015.rds', 'ec_Dic2015.rds',
               'ec_Ene2016.rds', 'ec_Feb2016.rds', 'ec_Mar2016.rds',
                'ec_Abr2016.rds', 'ec_May2016.rds', 'ec_Jun2016.rds',
                'ec_Jul2016.rds', 'ec_Ago2016.rds', 'ec_Sep2016.rds',
                'ec_Oct2016.rds', 'ec_Nov2016.rds', 'ec_Dic2016.rds',
                'ec_Ene2017.rds', 'ec_Feb2017.rds', 'ec_Mar2017.rds',
                'ec_Abr2017.rds', 'ec_May2017.rds', 'ec_Jun2017.rds',
                'ec_Jul2017.rds', 'ec_Ago2017.rds', 'ec_Sep2017.rds',
               'ec_Oct2017.rds', 'ec_Nov2017.rds', 'ec_Dic2017.rds',
               'ec_Ene2018.rds', 'ec_Feb2018.rds', 'ec_Mar2018.rds',
               'ec_Abr2018.rds', 'ec_May2018.rds', 'ec_Jun2018.rds',
               'ec_Jul2018.rds', 'ec_Ago2018.rds', 'ec_Sep2018.rds',
               'ec_Oct2018.rds', 'ec_Nov2018.rds', 'ec_Dic2018.rds',
               'ec_Ene2019.rds', 'ec_Feb2019.rds', 'ec_Mar2019.rds',
               'ec_Abr2019.rds', 'ec_May2019.rds', 'ec_Jun2019.rds',
               'ec_Jul2019.rds', 'ec_Ago2019.rds', 'ec_Sep2019.rds',
               'ec_Oct2019.rds', 'ec_Nov2019.rds', 'ec_Dic2019.rds',
               'ec_Ene2020.rds', 'ec_Feb2020.rds', 'ec_Mar2020.rds',
               'ec_Abr2020.rds', 'ec_May2020.rds', 'ec_Jun2020.rds',
               'ec_Jul2020.rds', 'ec_Ago2020.rds', 'ec_Sep2020.rds',
               'ec_Oct2020.rds', 'ec_Nov2020.rds', 'ec_Dic2020.rds')
bdcList_1<- list()
i <- 2
for(i in 1:length(long_list)) {
  print(long_list[i])
  bdcNC <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/', long_list[i])) 
  sumBDC_full <- bdcNC %>% 
    select(monDate, categ, sucursal, rangos, rangom, REFINANCIAMIENTO_GENUINO,
           saldous, montous, previus, intus, categ, opDes, saldoReprog,
           saldoMora, par0Reprog, saldoCast, par0, par30, opTot,
           saldoRepVig, par30Reprog, MODULO, DIASMORA, MONEDA, labGrupoC,labGrupoD, 
           tipoCred, CAEDEC_DEST, CIU) %>% 
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
    mutate(SALDO_CAPITAL_DIFERIDO = 0) %>%
    mutate(SALDO_INT_CAPITAL_DIFERIDO =  0) %>%
    mutate(saldoDifFranz =  0) %>%
    mutate(saldoDif =  0) %>%
    mutate(saldoRef = ifelse(REFINANCIAMIENTO_GENUINO != '-', saldous, 0)) %>%
    mutate(par0Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 0,
                            saldous, 0)) %>%
    mutate(par30Ref = ifelse(REFINANCIAMIENTO_GENUINO != '-' & DIASMORA > 30,
                             saldous, 0)) %>%
    mutate(par0Dif = 0) %>% 
    mutate(par30Dif = 0) %>% 
    mutate(par0DifFranz = 0) %>% 
    mutate(par30DifFranz = 0) %>% 
    mutate(saldoReprogDif = ifelse(MODULO == 121 & saldoDif > 0 ,
                                   saldoReprog, 0)) %>% 
    mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0 ,
                                  saldoReprog, 0)) %>% 
    mutate(saldoReprogDifMora = 0) %>% 
    mutate(saldoReprogDifMora30 =  0) %>% 
    mutate(saldoRefinDifMora = 0) %>% 
    mutate(saldoRefinDifMora30 = 0) %>% 
    select(-REFINANCIAMIENTO_GENUINO, -DIASMORA, -MONEDA, -MODULO,
           -starts_with('SALDO', ignore.case = F), -CAEDEC_DEST,
           -labGrupoD, -labGrupoC, -CIU) %>% 
    ungroup() %>% 
    group_by(monDate, categ, sucursal, rangos, rangom, tipoCred, 
             Sector_Destino, Sector_Actividad) %>% 
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
                Saldo_Mora = saldoMora,
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
  select(-starts_with('saldo'))%>% 
  glimpse()

bdcAgg_1 <- bdcFinalExp_1 %>%
  ungroup() %>%
  select(-Sucursal, -Rango_Desembolso, -Rango_Saldo_Actual, -Sector_Cartera, 
         -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  group_by(Fecha) %>%
  summarise_all(sum,na.rm=T)

###############################################################################
#############################################################################
# After dec-2020
gc()
bdcList <- list()
short_list <- c('ec_Ene2021.rds', 'ec_Feb2021.rds', 'ec_Mar2021.rds',
                'ec_Abr2021.rds', 'ec_May2021.rds', 'ec_Jun2021.rds',
                'ec_Jul2021.rds', 'ec_Ago2021.rds', 'ec_Sep2021.rds',
                'ec_Oct2021.rds', 'ec_Nov2021.rds', 'ec_Dic2021.rds',
                'ec_Ene2022.rds', 'ec_Feb2022.rds', 'ec_Mar2022.rds',
                'ec_Abr2022.rds', 'ec_May2022.rds', 'ec_Jun2022.rds',
                'ec_Jul2022.rds', 'ec_Ago2022.rds', 'ec_Sep2022.rds',
                'ec_Oct2022.rds', 'ec_Nov2022.rds')

for(i in 1:length(short_list)) {
  print(short_list[i])
  bdcNC <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/', short_list[i]))
  sumBDC_full <- bdcNC %>%
    select(monDate, categ, sucursal, rangos, rangom, labGrupoC, labGrupoD,
           saldous, montous, previus, intus, categ, opDes, saldoReprog,
           saldoMora, par0Reprog, saldoCast, par0, par30, opTot, 
           par0Reprog, saldoRepVig, par30Reprog, saldoDif,
           saldoDifFranz, par0DifFranz, par30Dif, par0Dif,
           par30DifFranz, MODULO, SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO,
           saldoRef, saldoRefinDif, saldoRefinDifMora, saldoRefinDifMora30, par0Ref,
           par30Ref,RUBRO_CAPITAL_DIFERIDO, tipoCred, Sector_Destino, Sector_Actividad,
           REFINANCIAMIENTO_GENUINO, DIASMORA, MONEDA, RUBRO_CAPITAL_DIFERIDO, sectCart, 
           CAEDEC_DEST, CIU) %>%
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
                Saldo_Mora = saldoMora,
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
  summarise_all(sum, na.rm = T)
  
################################################################################
ggplot(bdcAggC,aes(x=Fecha,y=Cartera_Bruta))+geom_line(size=1.25)+theme_minimal()
###############################################################################
# Checks and figs
bdcAggC <- bdcAgg_1 %>%
  bind_rows(bdcAgg_2) %>%
  dplyr::filter(!(year(Fecha) >= 2021 & Cartera_Diferida_ASFI == 0)) %>% 
  ungroup() %>% 
  group_by(Fecha) %>%
  summarise_all(sum, na.rm = T) %>% 
  ungroup()



ggplot(bdcAggC, aes(x = Fecha, y = Cartera_Bruta)) + 
  geom_line(size = 1.25) + geom_point()+ theme_minimal() 

write.xlsx(bdcAggC, 'C:/!bso/girCartera/tablaGIR.xlsx', rowNames = F)
write.csv(bdcAggC, 'C:/!bso/girCartera/tablaGIR.csv', row.names = F)

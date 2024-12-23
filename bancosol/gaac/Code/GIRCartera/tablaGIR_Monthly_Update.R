####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(xtable)
library(openxlsx)
library(scales)
library(janitor)
library(data.table)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____CONSTRUCCION DE TABLA GIR____####
year <- c(2015:2023)
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
meses <- c("Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022",
           "Ene2022","Feb2022","Mar2022","Abr2022","May2022","Jun2022","Jul2022",
           "Ago2022","Sep2022","Oct2022","Nov2022","Dic2022",
           "Ene2023","Feb2023","Mar2023","Abr2023","May2023","Jun2023","Jul2023",
           "Ago2023","Sep2023","Oct2023")
meses <- c("Ago2022", "Sep2022","Oct2022")
n <- length(mes)
i <- 1
k <- 3
girList <- list()
for(i in 1:length(year)) {
  for (k in 1:length(mes)) {
    tryCatch({
      print(paste0(mes[k],year[i]))
      bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', mes[k],year[i],'.rds')) 
      if(year[i]<=2020){
        bdcNC <- bdcNC %>% 
          mutate(SALDO_CAPITAL_DIFERIDO = 0) %>%
          mutate(SALDO_INT_CAPITAL_DIFERIDO =  0) %>%
          mutate(saldoDifFranz =  0) %>%
          mutate(saldoDif =  0) %>%
          mutate(par0Dif = ifelse(DIASMORA > 0, saldoDif, 0)) %>%
          mutate(par30Dif = ifelse(DIASMORA > 30, saldoDif, 0)) %>%
          mutate(par0DifFranz = ifelse(DIASMORA > 0, saldoDifFranz, 0)) %>%
          mutate(par30DifFranz = ifelse(DIASMORA > 30, saldoDifFranz, 0)) %>%
          mutate(saldoReprogDif = ifelse(MODULO == 121 & saldoDif >0 , saldoReprog, 0)) %>%
          mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0, 
                                        saldoReprog, 0)) %>%
          mutate(saldoReprogDifMora = 0) %>% 
          mutate(saldoReprogDifMora30 =  0) %>% 
          mutate(saldoRefinDifMora = 0) %>% 
          mutate(saldoRefinDifMora30 = 0)
      }
      girNew <- bdcNC %>%
        dplyr::filter(MODULO!=29) %>% 
        mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
        mutate(Sector_Actividad = case_when(Sector_Actividad=='H. Comercio' & divCaedecC %in% c('50','51')~'H1. Ventas al por mayor',
                                            Sector_Actividad=='H. Comercio' & divCaedecC %in% c('52')~'H2. Ventas al por menor',
                                            TRUE~Sector_Actividad)) %>% 
        mutate(categ=ifelse(SECTOR_CARTERA=='9.Vivienda No controlada','Otros',categ)) %>% 
        select(Fecha, categ, Sucursal, rangos, rangom, GENERO, tipoCred, Sector_Destino,
               Sector_Actividad, ctaCont, saldous, montous, previus, intus, opDes, saldoReprog,
               saldoMora, par0Reprog, saldoCast, par0, opTot, saldoRepVig, par30Reprog, 
               saldoDifFranz,saldoDif,saldoRef,par0Ref,par30Ref, par0Dif, par30Dif,
               par0DifFranz,par30DifFranz,saldoReprogDif,saldoRefinDif,
               saldoReprogDifMora,saldoReprogDifMora30,saldoRefinDifMora,saldoRefinDifMora30) %>%
        mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
        # mutate(categPar30=case_when(par30Ref >0 ~ 'Par30Refin',
        #                             par30Reprog >0~ 'Par30Reprog',
        #                             par30Dif>0~'par30dif')) %>%
        # mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
        # mutate(categPar1=case_when(par0Ref >0 ~ 'Par1Refin',
        #                            par0Reprog >0~ 'Par1Reprog',
        #                            par0Dif>0~'par1dif')) %>% 
        # mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
        group_by(Fecha, categ, Sucursal, rangos, rangom, tipoCred, 
                 Sector_Destino, Sector_Actividad, ctaCont,GENERO) %>%
        summarise_all(sum) %>% 
        ungroup()
      girList[[k+(i-1)*n]] <- girNew
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

girList <- list()
length(meses)
for (i in 1:3) {
  tryCatch({
    print(paste0(meses[i]))
    bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', meses[i],'.rds')) 
    if(as.numeric(str_sub(meses[i],4,7))<=2020){
      bdcNC <- bdcNC %>% 
        mutate(SALDO_CAPITAL_DIFERIDO = 0) %>%
        mutate(SALDO_INT_CAPITAL_DIFERIDO =  0) %>%
        mutate(saldoDifFranz =  0) %>%
        mutate(saldoDif =  0) %>%
        mutate(par0Dif = ifelse(DIASMORA > 0, saldoDif, 0)) %>%
        mutate(par30Dif = ifelse(DIASMORA > 30, saldoDif, 0)) %>%
        mutate(par0DifFranz = ifelse(DIASMORA > 0, saldoDifFranz, 0)) %>%
        mutate(par30DifFranz = ifelse(DIASMORA > 30, saldoDifFranz, 0)) %>%
        mutate(saldoReprogDif = ifelse(MODULO == 121 & saldoDif >0 , saldoReprog, 0)) %>%
        mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0, 
                                      saldoReprog, 0)) %>%
        mutate(saldoReprogDifMora = 0) %>% 
        mutate(saldoReprogDifMora30 =  0) %>% 
        mutate(saldoRefinDifMora = 0) %>% 
        mutate(saldoRefinDifMora30 = 0)
    }
    girNew <- bdcNC %>%
      dplyr::filter(MODULO!=29) %>% 
      mutate(MontoDes = ifelse(MONEDA==0, MONTO/6.86, MONTO)) %>% 
      mutate(RangoM20k = ifelse(MontoDes>=20000, ">= 20k","< 20k")) %>% 
      mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
      mutate(Sector_Actividad = case_when(Sector_Actividad=='H. Comercio' & divCaedecC %in% c('50','51')~'H1. Ventas al por mayor',
                                          Sector_Actividad=='H. Comercio' & divCaedecC %in% c('52')~'H2. Ventas al por menor',
                                          TRUE~Sector_Actividad)) %>% 
      mutate(categ=ifelse(SECTOR_CARTERA=='9.Vivienda No controlada','Otros',categ)) %>% 
      select(Fecha, categ, Sucursal, RangoM20k, rangos, rangom, GENERO, tipoCred, Sector_Destino,
             Sector_Actividad, ctaCont, saldous, montous, previus, intus, opDes, saldoReprog,
             saldoMora, par0Reprog, saldoCast, par0, opTot, saldoRepVig, par30Reprog, 
             saldoDifFranz,saldoDif,saldoRef,par0Ref,par30Ref, par0Dif, par30Dif,
             par0DifFranz,par30DifFranz,saldoReprogDif,saldoRefinDif,
             saldoReprogDifMora,saldoReprogDifMora30,saldoRefinDifMora,saldoRefinDifMora30) %>%
      mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
      # mutate(categPar30=case_when(par30Ref >0 ~ 'Par30Refin',
      #                             par30Reprog >0~ 'Par30Reprog',
      #                             par30Dif>0~'par30dif')) %>%
      # mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
      # mutate(categPar1=case_when(par0Ref >0 ~ 'Par1Refin',
      #                            par0Reprog >0~ 'Par1Reprog',
      #                            par0Dif>0~'par1dif')) %>% 
      # mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
      group_by(Fecha, categ, Sucursal, RangoM20k, rangos, rangom, tipoCred, 
               Sector_Destino, Sector_Actividad, ctaCont,GENERO) %>%
      summarise_all(sum) %>% 
      ungroup()
    girList[[i]] <- girNew
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


girFull <- rbindlist(girList) %>% 
  ungroup() %>% 
  dplyr::rename(Sector_Cartera = categ,
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
                PaR_30_Reprogramada = par30Reprog,
                Cartera_Castigada = saldoCast,
                PaR_1_Bruta = par0,
                Cartera_Reprogramada_Vigente = saldoRepVig,
                PaR_1_Reprogramada = par0Reprog,
                Cartera_Diferida_ASFI = saldoDif,
                Cartera_Diferida_RC = saldoDifFranz,
                PaR_1_Refinanciada = par0Ref,
                PaR_30_Refinanciada = par30Ref,
                Cartera_Refinanciada = saldoRef,
                PaR_1_Diferida_ASFI = par0Dif,
                PaR_30_Diferida_ASFI = par30Dif,
                PaR_1_Diferida_RC = par0DifFranz,
                PaR_30_Diferida_RC = par30DifFranz,
                Tipo_Credito = tipoCred) %>%
  select(-starts_with('saldo')) %>% 
  glimpse()

girExp <- girFull %>% 
  dplyr::filter((month(Fecha)==12 & year(Fecha)<=2020) | year(Fecha)>2020) %>% 
  mutate(GENERO=ifelse(GENERO!='F' & GENERO!='M', 'J', GENERO)) %>% 
  mutate(Cartera_Bruta_Normal=Cartera_Bruta-Cartera_Diferida_ASFI-Cartera_Reprogramada-
         Cartera_Refinanciada,
       Par_30_Normal = PaR_30_Bruta-PaR_30_Reprogramada - PaR_30_Diferida_ASFI - PaR_30_Refinanciada,
       Par_1_Normal = PaR_1_Bruta-PaR_1_Reprogramada - PaR_1_Diferida_ASFI - PaR_1_Refinanciada)

fwrite(girExp,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.txt',
          row.names = F, sep="|", quote = F)
write_xlsx(girExp,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.xlsx')

x <- girExp %>% 
       group_by(Fecha,Sucursal,ctaCont) %>% 
       summarise(Saldo=sum(Cartera_Bruta)) %>% 
       ungroup() %>% 
       pivot_wider(names_from = ctaCont,values_from = Saldo) %>% 
       adorn_totals('col')
  
####____CARGA DE TABLA GIR ANTIGUA____####
girold <- readxl::read_xlsx('D:/!bso/girCartera/tablaGIR/tablaGIR.xlsx',sheet = "Sheet1")
girold <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.csv') %>% 
  mutate(Fecha = as.Date(Fecha)) %>% 
  dplyr::filter(Fecha < as.Date("2023-07-01")) %>% 
  glimpse() 

bdcNC <- readRDS('D:/!bso/girCartera/rds/ec_Jul2023.rds') %>% 
  glimpse()

girNew <- bdcNC %>%
  dplyr::filter(MODULO!=29) %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  mutate(categ = ifelse(SECTOR_CARTERA=='9.Vivienda No controlada','Otros',categ)) %>% 
  mutate(Sector_Actividad = case_when(Sector_Actividad=='H. Comercio' & divCaedecC %in% c('50','51')~'H1. Ventas al por mayor',
                                      Sector_Actividad=='H. Comercio' & divCaedecC %in% c('52')~'H2. Ventas al por menor',
                                      TRUE~Sector_Actividad)) %>% 
  mutate(ctaCont = as.integer(ctaCont)) %>% 
  select(Fecha, categ, Sucursal, rangos, rangom, GENERO,tipoCred, Sector_Destino,
         Sector_Actividad, ctaCont, saldous, montous, previus, intus, opDes, saldoReprog,
         saldoMora, par0Reprog, saldoCast, par0, opTot, saldoRepVig, par30Reprog, 
         saldoDifFranz,saldoDif,saldoRef,par0Ref,par30Ref, par0Dif, par30Dif,
         par0DifFranz,par30DifFranz,saldoReprogDif,saldoRefinDif,
         saldoReprogDifMora,saldoReprogDifMora30,saldoRefinDifMora,saldoRefinDifMora30) %>%
  mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
  # mutate(categPar30=case_when(par30Ref >0 ~ 'Par30Refin',
  #                             par30Reprog >0~ 'Par30Reprog',
  #                             par30Dif>0~'par30dif')) %>%
  # mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
  # mutate(categPar1=case_when(par0Ref >0 ~ 'Par1Refin',
  #                            par0Reprog >0~ 'Par1Reprog',
  #                            par0Dif>0~'par1dif')) %>% 
  # mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
  group_by(Fecha, categ, Sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad, ctaCont, GENERO) %>%
  summarise_all(sum) %>% 
  ungroup()

bdcFinalExp <- girNew %>% 
  ungroup() %>% 
  dplyr::rename(Sector_Cartera = categ,
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
                PaR_30_Reprogramada = par30Reprog,
                Cartera_Castigada = saldoCast,
                PaR_1_Bruta = par0,
                Cartera_Reprogramada_Vigente = saldoRepVig,
                PaR_1_Reprogramada = par0Reprog,
                Cartera_Diferida_ASFI = saldoDif,
                Cartera_Diferida_RC = saldoDifFranz,
                PaR_1_Refinanciada = par0Ref,
                PaR_30_Refinanciada = par30Ref,
                Cartera_Refinanciada = saldoRef,
                PaR_1_Diferida_ASFI = par0Dif,
                PaR_30_Diferida_ASFI = par30Dif,
                PaR_1_Diferida_RC = par0DifFranz,
                PaR_30_Diferida_RC = par30DifFranz,
                Tipo_Credito = tipoCred) %>%
  select(-starts_with('saldo')) %>% 
  glimpse()


checkOld <- girold %>% 
  group_by(Fecha,Sector_Cartera) %>% 
  summarise(C=sum(Cartera_Bruta))
checkNew <- bdcFinalExp %>% 
  group_by(Fecha,Sector_Cartera) %>% 
  summarise(C=sum(Cartera_Bruta))

bdcExp <- girold %>% 
  bind_rows(bdcFinalExp) %>% 
  mutate(GENERO=ifelse(GENERO!='F' & GENERO!='M', 'J', GENERO)) %>% 
  mutate(Cartera_Bruta_Normal=Cartera_Bruta-Cartera_Diferida_ASFI-Cartera_Reprogramada-
           Cartera_Refinanciada,
         Par_30_Normal = PaR_30_Bruta-PaR_30_Reprogramada - PaR_30_Diferida_ASFI - PaR_30_Refinanciada,
         Par_1_Normal = PaR_1_Bruta-PaR_1_Reprogramada - PaR_1_Diferida_ASFI - PaR_1_Refinanciada)

bdcExp <- bdcExp %>% 
  dplyr::filter((year(Fecha)<=2021 & month(Fecha)==12) | year(Fecha)>2021)
fwrite(bdcExp,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.csv',
       row.names = F)
write_xlsx(bdcExp,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.xlsx')

write_xlsx(bdcExp,'D:/!bso/girCartera/tablaGIR/tablaGIR_Jun2023.xlsx')
#GUARDADO EN MAQUINA PROPIA
write.xlsx(bdcExp, 'D:/!bso/girCartera/tablaGIR/tablaGIR2023.xlsx')
write.csv(bdcExp, 'D:/!bso/girCartera/tablaGIR/tablaGIR2023.csv',row.names = F)

####____CUADRO SOLUCION DIFERIDO____####
bdc_prevmonth <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds") %>% 
  select(CTACLIENTE, OPERACION, saldoDif, ctaCont, MODULO, TIPO_OPER)
bdc_lastmonth <- readRDS("D:/!bso/girCartera/rds/ec_Jul2023.rds") %>% 
  select(CTACLIENTE, OPERACION, saldoDif, ctaCont, MODULO, TIPO_OPER, fdes)

bdc_prevmonth %>% 
  dplyr::filter(MODULO ==118 | (MODULO==121 & str_detect(TIPO_OPER,"MIGR"))) %>% 
  summarise(Saldodif=sum(saldoDif))
bdc_lastmonth %>% 
  dplyr::filter(MODULO ==118 | (MODULO==121 & str_detect(TIPO_OPER,"MIGR"))) %>% 
  summarise(Saldodif=sum(saldoDif))

bdc_lastmonth <- bdc_lastmonth %>% 
  dplyr::filter(!(MODULO ==118 | (MODULO==121 & str_detect(TIPO_OPER,"MIGR"))))
bdc_lastmonth %>% 
  summarise(Saldodif=sum(saldoDif))

sum(bdc_prevmonth$saldoDif)
sum(bdc_lastmonth$saldoDif)
bdc_lastmonth %>% 
  left_join(bdc_prevmonth, by=c("OPERACION")) %>% 
  group_by(OPERACION) %>% #Julio para operaciones de FSL
  dplyr::filter(row_number()==1) %>% #Julio para operaciones de FSL
  ungroup() %>% #Julio para operaciones de FSL
  mutate(Saldodif = saldoDif.y-saldoDif.x) %>% 
  summarise(Diff=sum(Saldodif,na.rm = T))

bdc_prevmonth %>% 
  anti_join(bdc_lastmonth, by=c("OPERACION")) %>% 
  summarise(Saldo =sum(saldoDif, na.rm = T))

####____COBERTURA DE PREV GENERICA____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Ago2023.rds")
bdc %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  dplyr::filter(CALIFICACION!="A") %>% 
  group_by(CALIFICACION) %>% 
  summarise(S=sum(saldous), P=sum(previus)) %>% 
  adorn_totals("row") %>% 
  mutate(Cob = P/S)

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
meses <- c("Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Mar2023","Abr2023","May2023")
girList <- list()
Clientes_Ajuste <- readRDS("D:/!bso/features/Clientes_AjusteRef_Ene15Oct23.rds") %>% 
  select(CTACLIENTE,OPERACION,fdes_original)
for(i in 1:length(meses)) {
  tryCatch({
    print(paste0(meses[i]))
    bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', meses[i],'.rds')) 
    if(as.numeric(substr(meses[i],4,7))<=2020){
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
      left_join(Clientes_Ajuste,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(Periodo_Desembolso = year(fdes_original)) %>%
      mutate(Periodo_Desembolso = year(monDate)-Periodo_Desembolso) %>% 
      mutate(Periodo_Desembolso = ifelse(Periodo_Desembolso>4,"<t-4",paste0('t-',Periodo_Desembolso))) %>% 
      mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                      ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                      OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
      mutate(Es_Diferida = ifelse(saldoDif > 0, 'Diferida', 'No Diferida')) %>%
      mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
      mutate(categ=ifelse(SECTOR_CARTERA=='9.Vivienda No controlada','Otros',categ)) %>% 
      select(Fecha, categ, Sucursal, rangos, rangom, GENERO, tipoCred, Sector_Destino,
             Sector_Actividad, ctaCont, Periodo_Desembolso, Tipo_Cartera, Es_Diferida,
             saldous, montous, previus, intus, opDes, saldoReprog,
             saldoMora, par0Reprog, saldoCast, par0, opTot, saldoRepVig, par30Reprog, 
             saldoDifFranz,saldoDif,saldoRef,par0Ref,par30Ref, par0Dif, par30Dif,
             par0DifFranz,par30DifFranz,saldoReprogDif,saldoRefinDif,
             saldoReprogDifMora,saldoReprogDifMora30,saldoRefinDifMora,saldoRefinDifMora30) %>%
      mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
      # mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
      # mutate(categPar1=case_when(par0Ref >0 ~ 'Par1Refin',
      #                            par0Reprog >0~ 'Par1Reprog',
      #                            par0Dif>0~'par1dif')) %>% 
      # mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
      group_by(Fecha, categ, Sucursal, rangos, rangom, tipoCred, 
               Sector_Destino, Sector_Actividad, ctaCont,GENERO, 
               Periodo_Desembolso, Tipo_Cartera, Es_Diferida) %>%
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

fwrite(girExp,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.csv',
       row.names = F)
write_xlsx(girExp,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR_Refin.xlsx')
write_xlsx(girExp,'D:/!bso/girCartera/tablaGIR_mora_v2.xlsx')

x <- girExp %>% 
  group_by(Fecha,Sucursal,ctaCont) %>% 
  summarise(Saldo=sum(Cartera_Bruta)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ctaCont,values_from = Saldo) %>% 
  adorn_totals('col')

####____CARGA DE TABLA GIR ANTIGUA____####
girold <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.csv') %>% 
  mutate(Fecha = as.Date(Fecha)) %>% 
  glimpse() 

# girold <- read_xlsx('D:/!bso/girCartera/tablaGIR_mora.xlsx', sheet = "Sheet 1")
girold <- fread('D:/!bso/girCartera/tablaGIR_mora.csv',sep = '|',encoding = "UTF-8")
girold <- girold %>% 
  dplyr::filter(Fecha <= as.Date("2023-06-01"))

bdcNC <- readRDS('D:/!bso/girCartera/rds/ec_Oct2023.rds') %>% 
  glimpse()

girNew <- bdcNC %>%
  dplyr::filter(MODULO!=29) %>% 
  left_join(Clientes_Ajuste,by=c("CTACLIENTE","OPERACION")) %>%
  mutate(Periodo_Desembolso = year(fdes_original)) %>%
  mutate(Periodo_Desembolso = year(monDate)-Periodo_Desembolso) %>%
  mutate(Periodo_Desembolso = ifelse(Periodo_Desembolso>4,"<t-4",paste0('t-',Periodo_Desembolso))) %>%
  mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                  ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                  OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
  mutate(Es_Diferida = ifelse(saldoDif > 0, 'Diferida', 'No Diferida')) %>%
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  mutate(categ=ifelse(SECTOR_CARTERA=='9.Vivienda No controlada','Otros',categ)) %>% 
  select(Fecha, categ, Sucursal, rangos, rangom, GENERO, tipoCred, Sector_Destino,
         Sector_Actividad, ctaCont, Periodo_Desembolso, Tipo_Cartera, Es_Diferida,
         saldous, montous, previus, intus, opDes, saldoReprog,
         saldoMora, par0Reprog, saldoCast, par0, opTot, saldoRepVig, par30Reprog, 
         saldoDifFranz,saldoDif,saldoRef,par0Ref,par30Ref, par0Dif, par30Dif,
         par0DifFranz,par30DifFranz,saldoReprogDif,saldoRefinDif,
         saldoReprogDifMora,saldoReprogDifMora30,saldoRefinDifMora,saldoRefinDifMora30) %>%
  mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
  # mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
  # mutate(categPar1=case_when(par0Ref >0 ~ 'Par1Refin',
  #                            par0Reprog >0~ 'Par1Reprog',
  #                            par0Dif>0~'par1dif')) %>% 
  # mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
  group_by(Fecha, categ, Sucursal, rangos, rangom, tipoCred, 
           Sector_Destino, Sector_Actividad, ctaCont,GENERO, 
           Periodo_Desembolso, Tipo_Cartera, Es_Diferida) %>%
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

girold <- girold %>% 
  mutate(ctaCont=as.character(ctaCont)) %>% 
  bind_rows(bdcFinalExp) %>% 
  mutate(GENERO=ifelse(GENERO!='F' & GENERO!='M', 'J', GENERO)) %>% 
  mutate(Cartera_Bruta_Normal=Cartera_Bruta-Cartera_Diferida_ASFI-Cartera_Reprogramada-
           Cartera_Refinanciada,
         Par_30_Normal = PaR_30_Bruta-PaR_30_Reprogramada - PaR_30_Diferida_ASFI - PaR_30_Refinanciada,
         Par_1_Normal = PaR_1_Bruta-PaR_1_Reprogramada - PaR_1_Diferida_ASFI - PaR_1_Refinanciada)

fwrite(girold,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.csv',
       row.names = F)
write_xlsx(girold,'//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/tablaGIR/tablaGIR.xlsx')

#GUARDADO EN MAQUINA PROPIA
write_xlsx(girold, 'D:/!bso/girCartera/tablaGIR_mora_new.xlsx')
fwrite(girold, 'D:/!bso/girCartera/tablaGIR_mora.csv',row.names = F,sep='|',quote=F)

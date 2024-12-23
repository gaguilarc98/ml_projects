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
meses <- c("Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Ago2023","Sep2023","Oct2023")
girList <- list()
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
Clientes_Ajuste <- readRDS("D:/!bso/features/Clientes_Ene15Oct23.rds") %>% 
  select(CTACLIENTE,OPERACION,fdes_original = fdes)
i <- 8
for(i in 1:length(meses)) {
  tryCatch({
    print(paste0(meses[i]))
    bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', meses[i],'.rds')) 
    if(as.numeric(substr(meses[i],4,7))<=2020){
      bdcNC <- bdcNC %>% 
        mutate(saldoDif =  0)
    }
    girNew <- bdcNC %>%
      left_join(codMod, by="MODULO") %>% 
      dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
      mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
      mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
      mutate(SaldoVigente = ifelse(ctaCont %in% c('131','135'), saldous, 0)) %>% 
      mutate(SaldoReprog = ifelse(ctaCont %in% c('135','136','137'), saldous, 0)) %>% 
      mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
      mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
      mutate(OpsVigente = ifelse(ctaCont %in% c('131','135'), 1, 0)) %>% 
      mutate(OpsReprog = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
      mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
      mutate(OpsDiferida = ifelse(saldoDif>0, 1, 0)) %>%
      left_join(Clientes_Ajuste, by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(GENERO=ifelse(is.na(GENERO) | GENERO=="", 'J', GENERO)) %>% 
      mutate(ObjetoCredito = case_when(OBJETO_CRED %in% c(1,3) ~ 'OPERACION',
                                       OBJETO_CRED %in% c(2,4) ~ 'INVERSION',
                                       OBJETO_CRED %in% c(9) ~ 'COMPRA BIENES MUEBLES',
                                       OBJETO_CRED %in% c(10) ~ 'LIBRE DISPONIBILIDAD',
                                       OBJETO_CRED %in% c(5,6,50,55,60,90,95) ~ 'COMPRA/ADQ/CONTRATO VIVIENDA',
                                       OBJETO_CRED %in% c(7,8,70,75,80,85) ~ 'CONST/REPAR VIVIENDA',
                                       TRUE ~ 'OTROS')) %>% 
      mutate(TipoCredito = case_when(substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                      substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                      substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                      TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                      TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
      mutate(Sector_Actividad = case_when(Sector_Actividad == "Agropecuario" ~ "Agropecuario",
                                         Sector_Actividad %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales","F. Dist. EE y agua") ~ "Industria",
                                         Sector_Actividad == "E. Ind. y Manu." ~ "Manufactura",
                                         Sector_Actividad == "G. Construcción" ~ "Construcción",
                                         Sector_Actividad == "H. Comercio" ~ "Comercio",
                                         Sector_Actividad == "I. Hoteles" ~ "Hoteles",
                                         Sector_Actividad == "J. Transporte" ~ "Transporte",
                                         Sector_Actividad %in% c("Asalariados", "K. Inter. Fin.","L. Serv. Inmob.","O. Serv. Hosp + Otros", "Q. Org. Extraterritoriales","Otros") ~"Servicios",
                                         TRUE~ 'Otros')) %>% 
      mutate(Sector_Destino = case_when(Sector_Destino == "Agropecuario" ~ "Agropecuario",
                                          Sector_Destino %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales","F. Dist. EE y agua") ~ "Industria",
                                          Sector_Destino == "E. Ind. y Manu." ~ "Manufactura",
                                          Sector_Destino == "G. Construcción" ~ "Construcción",
                                          Sector_Destino == "H. Comercio" ~ "Comercio",
                                          Sector_Destino == "I. Hoteles" ~ "Hoteles",
                                          Sector_Destino == "J. Transporte" ~ "Transporte",
                                          Sector_Destino %in% c("Asalariados", "K. Inter. Fin.","L. Serv. Inmob.","O. Serv. Hosp + Otros","Q. Org. Extraterritoriales","Otros") ~"Servicios",
                                          TRUE~ 'Otros')) %>% 
      mutate(PlazoAnios = case_when(floor(PLAZODIAS/365) <= 2 ~ '2 o menos', 
                                    floor(PLAZODIAS/365) < 7 ~ as.character(floor(PLAZODIAS/365)),
                                    TRUE ~ '7 o más')) %>% 
      mutate(AnioDesembolso = ifelse(year(fdes_original) <= 2017, '<= 2017',
                                 as.character(year(fdes_original)))) %>% 
      # mutate(DesembolsoPandemia = case_when(fdes_original<as.Date("2020-03-01")~"1. Pre-pandemia",
      #                                       fdes_original<as.Date("2022-01-01")~"2. Pandemia",
      #                                       fdes_original>=as.Date("2022-01-01")~"3. Post-pandemia",
      #                                       TRUE~NA)) %>% 
      mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                      ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                      OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
      mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
      select(Fecha, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  TipoCredito, 
             ObjetoCredito, Sector_Destino, Sector_Actividad, AnioDesembolso, 
             PlazoAnios, Tipo_Cartera, SaldoBruto, SaldoMora, SaldoVigente,
             SaldoReprog, SaldoDiferido=saldoDif, Par0=par0, PrevisionEspecifica=previus, 
             InteresAnual=intus, OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsDiferida,
             OpsPar0) %>% 
      mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
      group_by(Fecha, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  TipoCredito, 
               ObjetoCredito, Sector_Destino, Sector_Actividad, AnioDesembolso, 
               PlazoAnios, Tipo_Cartera) %>%
      summarise_all(sum) %>% 
      ungroup()
    girList[[i]] <- girNew
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

girFull <- rbindlist(girList) 

fwrite(girFull,'D:/!bso/girCartera/MoraDescriptives.csv',
       row.names = F, sep = "|", quote = F)

####____CARGA DE TABLA GIR ANTIGUA____####
girOld <- fread('D:/!bso/girCartera/MoraDescriptives.csv',sep = '|',encoding = "UTF-8")
girOld <- girold %>% 
  dplyr::filter(Fecha <= as.Date("2023-06-01"))

bdcNC <- readRDS('D:/!bso/girCartera/rds/ec_Oct2023.rds') %>% 
  glimpse()

girNew <- bdcNC %>%
  left_join(codMod, by="MODULO") %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
  mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
  mutate(SaldoVigente = ifelse(ctaCont %in% c('131','135'), saldous, 0)) %>% 
  mutate(SaldoReprog = ifelse(ctaCont %in% c('135','136','137'), saldous, 0)) %>% 
  mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
  mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
  mutate(OpsVigente = ifelse(ctaCont %in% c('131','135'), 1, 0)) %>% 
  mutate(OpsReprog = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
  mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
  mutate(OpsDiferida = ifelse(saldoDif>0, 1, 0)) %>%
  left_join(Clientes_Ajuste, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(GENERO=ifelse(is.na(GENERO) | GENERO=="", 'J', GENERO)) %>% 
  mutate(ObjetoCredito = case_when(OBJETO_CRED %in% c(1,3) ~ 'OPERACION',
                                   OBJETO_CRED %in% c(2,4) ~ 'INVERSION',
                                   OBJETO_CRED %in% c(9) ~ 'COMPRA BIENES MUEBLES',
                                   OBJETO_CRED %in% c(10) ~ 'LIBRE DISPONIBILIDAD',
                                   OBJETO_CRED %in% c(5,6,50,55,60,90,95) ~ 'COMPRA/ADQ/CONTRATO VIVIENDA',
                                   OBJETO_CRED %in% c(7,8,70,75,80,85) ~ 'CONST/REPAR VIVIENDA',
                                   TRUE ~ 'OTROS')) %>% 
  mutate(TipoCredito = case_when(substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                 substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                 substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                 TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                 TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
  mutate(Sector_Actividad = case_when(Sector_Actividad == "Agropecuario" ~ "Agropecuario",
                                      Sector_Actividad %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales","F. Dist. EE y agua") ~ "Industria",
                                      Sector_Actividad == "E. Ind. y Manu." ~ "Manufactura",
                                      Sector_Actividad == "G. Construcción" ~ "Construcción",
                                      Sector_Actividad == "H. Comercio" ~ "Comercio",
                                      Sector_Actividad == "I. Hoteles" ~ "Hoteles",
                                      Sector_Actividad == "J. Transporte" ~ "Transporte",
                                      Sector_Actividad %in% c("Asalariados", "K. Inter. Fin.","L. Serv. Inmob.","O. Serv. Hosp + Otros", "Q. Org. Extraterritoriales","Otros") ~"Servicios",
                                      TRUE~ 'Otros')) %>% 
  mutate(Sector_Destino = case_when(Sector_Destino == "Agropecuario" ~ "Agropecuario",
                                    Sector_Destino %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales","F. Dist. EE y agua") ~ "Industria",
                                    Sector_Destino == "E. Ind. y Manu." ~ "Manufactura",
                                    Sector_Destino == "G. Construcción" ~ "Construcción",
                                    Sector_Destino == "H. Comercio" ~ "Comercio",
                                    Sector_Destino == "I. Hoteles" ~ "Hoteles",
                                    Sector_Destino == "J. Transporte" ~ "Transporte",
                                    Sector_Destino %in% c("Asalariados", "K. Inter. Fin.","L. Serv. Inmob.","O. Serv. Hosp + Otros","Q. Org. Extraterritoriales","Otros") ~"Servicios",
                                    TRUE~ 'Otros')) %>% 
  mutate(PlazoAnios = case_when(floor(PLAZODIAS/365) <= 2 ~ '2 o menos', 
                                floor(PLAZODIAS/365) < 7 ~ as.character(floor(PLAZODIAS/365)),
                                TRUE ~ '7 o más')) %>% 
  mutate(AnioDesembolso = ifelse(year(fdes_original) <= 2017, '<= 2017',
                                 as.character(year(fdes_original)))) %>% 
  # mutate(DesembolsoPandemia = case_when(fdes_original<as.Date("2020-03-01")~"1. Pre-pandemia",
  #                                       fdes_original<as.Date("2022-01-01")~"2. Pandemia",
  #                                       fdes_original>=as.Date("2022-01-01")~"3. Post-pandemia",
  #                                       TRUE~NA)) %>% 
  mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                  ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                  OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  select(Fecha, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  TipoCredito, 
         ObjetoCredito, Sector_Destino, Sector_Actividad, AnioDesembolso, 
         PlazoAnios, Tipo_Cartera, SaldoBruto, SaldoMora, SaldoVigente,
         SaldoReprog, SaldoDiferido=saldoDif, Par0=par0, PrevisionEspecifica=previus, 
         InteresAnual=intus, OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsDiferida,
         OpsPar0) %>% 
  mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
  group_by(Fecha, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  TipoCredito, 
           ObjetoCredito, Sector_Destino, Sector_Actividad, AnioDesembolso, 
           PlazoAnios, Tipo_Cartera) %>%
  summarise_all(sum) %>% 
  ungroup()

girOld <- girOld %>% 
  bind_rows(girNew)

fwrite(girold, 'D:/!bso/girCartera/MoraDescriptives.csv',row.names = F,sep='|',quote=F)

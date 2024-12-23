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
library(janitor)
library(ggplot2)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
################################################################################
####____CAH MENSUAL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mycap <- mycap[-c(which(mycap=="Feb2023"):length(mycap))]

# mycap <- c("Ene2021")
for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    cah <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/01_Base_Capataciones_CAH_Mensual/BaseCaptaciones_CA_Mensual_',
                        mycap[i],".txt"), sep="|") %>% 
      select(NRO_CUENTA,FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
             TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
             COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
             MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
      mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
      dplyr::filter(SALDO_SUS>0) %>% 
      mutate(TASA_REFERENCIAL=as.numeric(TASA_REFERENCIAL)/100)
    cah_j <- cah %>% 
      dplyr::filter(str_detect(TIPO_PERSONA, 'Jur')) %>%
      mutate(rango= cut(SALDO_SUS,breaks=c(0,seq(1e3,9e3,1e3),seq(10e3,50e3,10e3),100e3,Inf),
                        labels=c("01. 0-1 M USD","02. 1-2 M USD","03. 2-3 M USD","04. 3-4 M USD","05. 4-5 M USD","06. 5-6 M USD","07. 6-7 M USD","08. 6-7 M USD","09. 7-8 M USD",
                                 "10. 9-10 M USD","11. 10-20 M USD","12. 20-30 M USD","13. 30-40 M USD","14. 40-50 M USD","15. 50-100 M USD","16. >100 M USD"))) %>% 
      mutate(rango=as.character(rango)) %>% 
      rename(TASA_PP=TASA_REFERENCIAL) %>% 
      mutate(NRO_CUENTAS = 1) %>% 
      select(FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO,
             TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
             COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
             MARCA_INSTITUCIONAL_FINANZAS, rango, SALDO_SUS, TASA_PP,NRO_CUENTAS)
    cah_n <- cah %>% 
      dplyr::filter(!str_detect(TIPO_PERSONA, 'Jur')) %>% 
      mutate(NOMBRE = 'Persona Natural') %>%
      mutate(rango = cut(SALDO_SUS,breaks=c(0,seq(1e3,9e3,1e3),seq(10e3,50e3,10e3),100e3,Inf),
                         labels=c("01. 0-1 M USD","02. 1-2 M USD","03. 2-3 M USD","04. 3-4 M USD","05. 4-5 M USD","06. 5-6 M USD","07. 6-7 M USD","08. 6-7 M USD","09. 7-8 M USD",
                                  "10. 9-10 M USD","11. 10-20 M USD","12. 20-30 M USD","13. 30-40 M USD","14. 40-50 M USD","15. 50-100 M USD","16. >100 M USD"))) %>%  
      mutate(rango=as.character(rango)) %>% 
      mutate(SALDO_TASA = SALDO_SUS * TASA_REFERENCIAL) %>% 
      group_by(FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO,
               TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
               COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
               MARCA_INSTITUCIONAL_FINANZAS,rango) %>%
      summarise(SALDO_SUS = sum(as.numeric(SALDO_SUS), na.rm = T), 
                SALDO_TASA = sum(as.numeric(SALDO_TASA), na.rm = T),
                NRO_CUENTAS = n_distinct(NRO_CUENTA)) %>% 
      mutate(TASA_PP=SALDO_TASA/SALDO_SUS) %>% 
      select(-SALDO_TASA)
    cah_final <- cah_j %>% 
      bind_rows(cah_n)
    write_rds(cah_final,paste0("D:/!bso/Captaciones/cahMes/cah_",mycap[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

mycap <- list.files('D:/!bso/Captaciones/cahMes')
cahList <- list()
for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    cahList[[i]] <- readRDS(paste0('D:/!bso/Captaciones/cahMes/',mycap[i]))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

cahFull <- bind_rows(cahList)
cahFull <- cahFull %>% 
  mutate(GENERO=case_when(GENERO==""~"Jurídico",
                          GENERO=="F"~"Femenino",
                          GENERO=="M"~"Masculino",)) %>% 
  dplyr::rename(Fecha=FECHA_SALDO,
                Rango_Saldo=rango) %>% 
  mutate(Fecha=as.Date(Fecha,"%d/%m/%Y"))
write_xlsx(cahFull,'D:/!bso/Captaciones/cahDiarioDic31Feb26.xlsx')
write_rds(cahFull,'D:/!bso/Captaciones/cahDiarioDic31Feb26.rds')
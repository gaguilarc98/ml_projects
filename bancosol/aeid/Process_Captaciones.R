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
####____FUNCTION____####
caja_ahorro <- function(x){
  cah_j <- x %>% 
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
  cah_n <- x %>% 
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
  return(cah_final)
}
deposito <- function(x){
  dpf_j <- x %>% 
    dplyr::filter(str_detect(TIPO_PERSONA, 'Jur')) %>% 
    mutate(SALDO_TASA = as.numeric(SALDO_SUS) * as.numeric(TASA_REFERENCIAL)) %>% 
    mutate(rango= cut(SALDO_SUS,breaks=c(0,seq(1e3,9e3,1e3),seq(10e3,50e3,10e3),100e3,Inf),
                      labels=c("01. 0-1 M USD","02. 1-2 M USD","03. 2-3 M USD","04. 3-4 M USD","05. 4-5 M USD","06. 5-6 M USD","07. 6-7 M USD","08. 6-7 M USD","09. 7-8 M USD",
                               "10. 9-10 M USD","11. 10-20 M USD","12. 20-30 M USD","13. 30-40 M USD","14. 40-50 M USD","15. 50-100 M USD","16. >100 M USD"))) %>% 
    mutate(rango=as.character(rango)) %>% 
    mutate(NRO_CUENTAS_DPF = 1) %>% 
    select(FECHA_SALDO,NOMBRE,MONEDA,CTA_CONTABLE_SALDO,
           TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED,
           COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
           MARCA_INSTITUCIONAL_FINANZAS, PLAZO_RENOVACION, PLAZO_ORIGEN,
           SALDO_SUS, NOMBRE, SALDO_TASA, GENERO, SUCURSAL, rango,NRO_CUENTAS_DPF)
  dpf_n <- x %>% 
    dplyr::filter(!str_detect(TIPO_PERSONA, 'Jur')) %>% 
    mutate(NOMBRE = 'Persona Natural') %>%
    # mutate(carnetnit = 'Persona Natural') %>% 
    mutate(SALDO_TASA = as.numeric(SALDO_SUS) * as.numeric(TASA_REFERENCIAL)) %>%
    mutate(rango = cut(SALDO_SUS,breaks=c(0,seq(1e3,9e3,1e3),seq(10e3,50e3,10e3),100e3,Inf),
                       labels=c("01. 0-1 M USD","02. 1-2 M USD","03. 2-3 M USD","04. 3-4 M USD","05. 4-5 M USD","06. 5-6 M USD","07. 6-7 M USD","08. 6-7 M USD","09. 7-8 M USD",
                                "10. 9-10 M USD","11. 10-20 M USD","12. 20-30 M USD","13. 30-40 M USD","14. 40-50 M USD","15. 50-100 M USD","16. >100 M USD"))) %>%  
    mutate(rango=as.character(rango)) %>% 
    group_by(FECHA_SALDO,NOMBRE,MONEDA,CTA_CONTABLE_SALDO,
             TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED,
             COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
             MARCA_INSTITUCIONAL_FINANZAS, PLAZO_RENOVACION, PLAZO_ORIGEN, GENERO, 
             SUCURSAL, rango) %>% 
    summarise(SALDO_SUS = sum(as.numeric(SALDO_SUS), na.rm = T), 
              SALDO_TASA = sum(as.numeric(SALDO_TASA), na.rm = T),
              NRO_CUENTAS_DPF = n_distinct(NRO_CUENTA))
  dpf_final <- dpf_j %>% 
    bind_rows(dpf_n)
  return(dpf_final)
}
####____DIARIO____##############################################################
####____CAJAS DE AHORRO____#####################################################
#____PARA UN DIA
arch <- 'BaseCaptaciones_CA_20230427.txt'
cah <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/',
                    arch), sep="|") %>% 
  select(NRO_CUENTA,FECHA_SALDO,NOMBRE,GENERO,EDAD,MONEDA,CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
         TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
         COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
         MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
  mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
  dplyr::filter(SALDO_SUS>0) %>% 
  mutate(TASA_REFERENCIAL = as.numeric(TASA_REFERENCIAL)/100)
cah_final <- caja_ahorro(cah)

write_rds(cah_final,paste0("D:/!bso/Captaciones/cah/cah_",substr(arch,20,27),'.rds'))
#____PARA EXPORTAR A EXCEL
CAH <- cah_final %>% 
  mutate(GENERO=case_when(GENERO==""~"Jurídico",
                          GENERO=="F"~"Femenino",
                          GENERO=="M"~"Masculino",)) %>% 
  dplyr::rename(Fecha=FECHA_SALDO,
                Rango_Saldo=rango) %>% 
  mutate(Fecha=as.Date(Fecha,"%d/%m/%Y")) %>% 
  select(Fecha,NOMBRE,MONEDA,CTA_CONTABLE_SALDO,TIPO_PERSONA,ESTADO,INSTITUCIONAL,
         TIENE_SOLNET,TIENE_CRED,COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
         MARCA_INSTITUCIONAL_FINANZAS,SALDO_SUS,GENERO,NRO_CUENTAS,Rango_Saldo,TASA_PP)
write.xlsx(CAH,'D:/!bso/Captaciones/cahDaily.xlsx')
####____DEPOSITOS A PLAZO FIJO____##############################################
#____PARA UN DIA
arch <- 'BaseCaptaciones_DPF_20230424.txt'
dpf <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/04_Base_Capataciones_DPF_Diario/',
                           arch),encoding="UTF-8",sep="|",fill=T)
dpf_final <- deposito(dpf)

write_rds(dpf_final,paste0("D:/!bso/Captaciones/dpf/dpf_",substr(arch,21,28),'.rds'))
#____PARA EXPORTAR A EXCEL
write.xlsx(dpf_final,'D:/!bso/Captaciones/dpfDaily.xlsx')
####____CAJAS DE AHORRO____#####################################################
#____PARA UN MES
arch <- 'BaseCaptaciones_CA_Mensual_Feb2023.txt'
cah_mensual <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/01_Base_Capataciones_CAH_Mensual/',
                            arch),encoding="UTF-8",sep="|") %>% 
  select(NRO_CUENTA,FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
         TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
         COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
         MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
  mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
  dplyr::filter(SALDO_SUS>0) %>% 
  mutate(TASA_REFERENCIAL=as.numeric(TASA_REFERENCIAL)/100)
cah_final <- caja_ahorro(cah_mensual)
write_rds(cah_final, paste0('D:/!bso/Captaciones/cahMes/cahM_',
                            substr(arch,28,34), '.rds'))
####____DEPOSITOS A PLAZO FIJO____##############################################
#____PARA UN MES
arch <- 'BaseCaptaciones_DPF_Mensual_Feb2023.txt'
dpf_mensual <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/02_Base_Capataciones_DPF_Mensual/',
                            arch),encoding="UTF-8",sep="|",fill=T)
dpf_final <- deposito(dpf_mensual)
write_rds(dpf_final, paste0('D:/!bso/Captaciones/dpfMes/dpfM_',
                            substr(arch,29,35), '.rds'))
####____DPF MENSUAL EN LOOP____#################################################
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2021,2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mycap <- mycap[-c(which(mycap=="Abr2023"):length(mycap))]

for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    dpf_mensual <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/02_Base_Capataciones_DPF_Mensual/BaseCaptaciones_DPF_Mensual_',
                                mycap[i],'.txt'),encoding="UTF-8",sep="|",fill=T)
    dpf_final <- deposito(dpf_mensual)
    write_rds(dpf_final, paste0('D:/!bso/Captaciones/dpfMes/dpfM_',
                                mycap[i], '.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
####____CAH MENSUAL EN LOOP____#################################################
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mycap <- mycap[-c(which(mycap=="Mar2023"):length(mycap))]

for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    cah_mensual <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/01_Base_Capataciones_CAH_Mensual/BaseCaptaciones_CA_Mensual_',
                                mycap[i],'.txt'),encoding="UTF-8",sep="|") %>% 
      select(NRO_CUENTA,FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
             TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
             COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
             MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
      mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
      dplyr::filter(SALDO_SUS>0) %>% 
      mutate(TASA_REFERENCIAL=as.numeric(TASA_REFERENCIAL)/100)
    cah_final <- caja_ahorro(cah_mensual)
    write_rds(cah_final, paste0('D:/!bso/Captaciones/cahMes/cahM_',
                                mycap[i], '.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
####____DPF CONSOLIDADO____#####################################################
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2021,2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mycap <- mycap[-c(which(mycap=="Abr2023"):length(mycap))]

dpfList <- list()
for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    dpfList[[i]] <- readRDS(paste0('D:/!bso/Captaciones/dpfMes/dpfM_',
                           mycap[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dpfFull <- bind_rows(dpfList)
dpfFull <- dpfFull %>% 
  mutate(GENERO=case_when(is.na(GENERO)~"Jurídico",
                          GENERO=="F"~"Femenino",
                          GENERO=="M"~"Masculino",)) %>% 
  dplyr::rename(RangoSaldo=rango,
                Fecha=FECHA_SALDO) %>% 
  mutate(Fecha=as.Date(Fecha,"%d/%m/%Y"))
write_xlsx(dpfFull,'D:/!bso/Captaciones/dpfMensualEne21Mar23.xlsx')
write_rds(dpfFull,'D:/!bso/Captaciones/dpfMensualEne22Ene23.rds')

####____CAH CONSOLIDADO____#####################################################
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mycap <- mycap[-c(which(mycap=="Feb2023"):length(mycap))]

cahList <- list()
for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    cahList[[i]] <- readRDS(paste0('D:/!bso/Captaciones/cahMes/cahM_',
                                    mycap[i],'.rds'))
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
################################################################################
####____CAPTACIONES DPF DIARIO EN LOOP____######################################
nmes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(x,nmes)}))
mycap <- mycap[-c(1:which(mycap=="202211"),which(mycap=="202304"):length(mycap))]
i <- 4
j <- 1
for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    arch <- list.files("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/04_Base_Capataciones_DPF_Diario/",
                       pattern = paste0("^BaseCaptaciones_DPF_",mycap[i],".*.",".txt$"))
    arch <- sort(arch)
    for (j in 1:length(arch)) {
      print(arch[j])
      dpf <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/04_Base_Capataciones_DPF_Diario/',
                                  arch[j]),encoding="UTF-8",sep="|",fill=T)
      dpf_final <- deposito(dpf)
      write_rds(dpf_final,paste0("D:/!bso/Captaciones/dpf/dpf_",substr(arch[j],21,28),'.rds'))
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
################################################################################
####____CAPTACIONES CAH DIARIO EN LOOP____######################################
nmes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(x,nmes)}))
mycap <- mycap[-c(1:which(mycap=="202211"),which(mycap=="202304"):length(mycap))]

for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    arch <- list.files("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/",
                       pattern = paste0("^BaseCaptaciones_CA_",mycap[i],".*.",".txt$"))
    arch <- sort(arch)
    for (j in 1:length(arch)) {
      print(arch[j])
      cah <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/',
                          arch[j]), sep="|") %>% 
        select(NRO_CUENTA,FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
               TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
               COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
               MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
        mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
        dplyr::filter(SALDO_SUS>0) %>% 
        mutate(TASA_REFERENCIAL=as.numeric(TASA_REFERENCIAL)/100)
      cah_final <- caja_ahorro(cah)
      write_rds(cah_final,paste0("D:/!bso/Captaciones/cah/cah_",substr(arch[j],20,27),'.rds'))
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
################################################################################
####____CONSOLIDADO DPF DIARIO EN LOOP____######################################
long_list<-list.files('D:/!bso/Captaciones/dpf')
dpfList <- list()

for (i in 1:length(long_list)) {
  print(long_list[i])
  dpf<-readRDS(paste0('D:/!bso/Captaciones/dpf/', long_list[i]))
  dpfList[[i]]<-dpf
}
dpfFull<-bind_rows(dpfList)

dpfExp <- dpfFull %>% 
  mutate(GENERO=case_when(GENERO==""~"Jurídico",
                          GENERO=="F"~"Femenino",
                          GENERO=="M"~"Masculino",)) %>%
  dplyr::rename(Rango_Saldo=rango,
                Fecha=FECHA_SALDO) %>% 
  mutate(Fecha=as.Date(Fecha,"%d/%m/%Y"))

# write.xlsx(bdcExp, 'D:/!bso/Captaciones/dpfDiarioDic22Feb23.xlsx')
write_rds(dpfExp, 'D:/!bso/Captaciones/dpfDiarioDic01Mar01.rds')
#____Agregar un dia
dpfFull <- dpf_final %>% 
  mutate(GENERO=case_when(GENERO==""~"Jurídico",
                          GENERO=="F"~"Femenino",
                          GENERO=="M"~"Masculino",)) %>%
  dplyr::rename(Rango_Saldo=rango,
                Fecha=FECHA_SALDO) %>% 
  mutate(Fecha=as.Date(Fecha,"%d/%m/%Y"))
dpfFin <- readRDS('D:/!bso/Captaciones/dpfDiarioDic01Feb28.rds') %>% 
  bind_rows(dpfFull)

write_rds(dpfFin, 'D:/!bso/Captaciones/dpfDiarioDic01Mar01.rds')

####____CONSOLIDADO CAH DIARIO EN LOOP____######################################
mycap <- list.files('D:/!bso/Captaciones/cah')
cahList <- list()
mycap <- c("20230421","20230422","20230423")
for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    cahList[[i]] <- readRDS(paste0('D:/!bso/Captaciones/cah/cah_',mycap[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

cahFull <- bind_rows(cahList)
cahFull <- cahFull %>% 
  mutate(GENERO=case_when(GENERO==""~"Jurídico",
                          GENERO=="F"~"Femenino",
                          GENERO=="M"~"Masculino",)) %>% 
  dplyr::rename(Fecha=FECHA_SALDO,
                Rango_Saldo=rango) %>% 
  mutate(Fecha=as.Date(Fecha,"%d/%m/%Y")) %>% 
  relocate(Fecha,NOMBRE,MONEDA,CTA_CONTABLE_SALDO,TIPO_PERSONA,ESTADO,INSTITUCIONAL,
           TIENE_SOLNET,TIENE_CRED,COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
           MARCA_INSTITUCIONAL_FINANZAS,SALDO_SUS,GENERO,NRO_CUENTAS,Rango_Saldo,TASA_PP) %>% 
  select(-TIENE_DPF)
write_xlsx(cahFull,'D:/!bso/Captaciones/cahDiarioAbr21Abr23.xlsx')
write_rds(cahFull,'D:/!bso/Captaciones/cahDiarioDic31Feb26.rds')
#____Agregar un dia
cahFull <- cah_final %>% 
  mutate(GENERO=case_when(GENERO==""~"Jurídico",
                          GENERO=="F"~"Femenino",
                          GENERO=="M"~"Masculino",)) %>% 
  dplyr::rename(Fecha=FECHA_SALDO,
                Rango_Saldo=rango) %>% 
  mutate(Fecha=as.Date(Fecha,"%d/%m/%Y"))
cah_Fin<-readRDS('D:/!bso/Captaciones/cahDiarioDic31Feb28.rds') %>% 
  bind_rows(cahFull) %>% 
  relocate(Fecha,NOMBRE,MONEDA,CTA_CONTABLE_SALDO,TIPO_PERSONA,ESTADO,INSTITUCIONAL,
           TIENE_SOLNET,TIENE_CRED,COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
           MARCA_INSTITUCIONAL_FINANZAS,SALDO_SUS,GENERO,NRO_CUENTAS,Rango_Saldo,TASA_PP)

write_rds(cah_Fin, 'D:/!bso/Captaciones/cahDiarioDic31Mar01.rds')
################################################################################
####____UNION CAH DPF____####
cahFull <- readRDS('D:/!bso/Captaciones/cahDiarioDic31Mar01.rds') %>% 
  dplyr::filter(Fecha>=as.Date('2023-02-10')| Fecha==as.Date('2023-01-31')
                | Fecha==as.Date('2022-12-31')) %>% 
  mutate(SALDO_TASA=SALDO_SUS*TASA_PP) %>%
  rename(NRO_CUENTAS_DPF=NRO_CUENTAS) %>% 
  relocate(Fecha,NOMBRE,MONEDA,CTA_CONTABLE_SALDO,TIPO_PERSONA,ESTADO,
           INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED,COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,
           NOM_REGIONAL_ASOC,MARCA_INSTITUCIONAL_FINANZAS,TIENE_DPF,TASA_PP)

dpfFin<-readRDS('D:/!bso/Captaciones/dpfDiarioDic01Mar01.rds') %>% 
  bind_rows(cahFull) %>% 
  dplyr::filter(Fecha>=as.Date('2023-02-10')| Fecha==as.Date('2023-01-31')
                | Fecha==as.Date('2022-12-31'))

write.xlsx(dpfFin, 'D:/!bso/Captaciones/cahDpfDaily.xlsx') 

check <- dpfFin %>% 
  group_by(Fecha,CTA_CONTABLE_SALDO) %>% 
  summarise(Saldo=sum(SALDO_SUS)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = CTA_CONTABLE_SALDO,values_from = Saldo,values_fill = 0)

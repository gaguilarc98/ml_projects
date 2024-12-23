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
# require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

####____FUNCIONES____####
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
####____CAJAS DE AHORRO____#####################################################
#____PARA UN DIA
bases <- c("20230520","20230521_v2","20230522","20230523","20230524","20230525")
i <- 1
cahList <- list()
for (i in 1:length(bases)) {
  print(bases[i])
  if(i==1){
    cah_total <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/BaseCaptaciones_CA_',
                        bases[i],'.txt'), sep="|") %>% 
      select(FECHA_SALDO, NRO_CUENTA, SALDO_SUS, CTA_CONTABLE_SALDO, FECHA_APERTURA, CTA_CLIENTE, OPERACION,
             DESC_TIPO_OPERACION, NOMBRE, CI, SUCURSAL) %>% 
      dplyr::filter(CTA_CLIENTE != 999999999)
    k <- 1
  }else{
    cah <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/BaseCaptaciones_CA_',
                        bases[i],'.txt'), sep="|") %>% 
      select(FECHA_SALDO, NRO_CUENTA, SALDO_SUS, CTA_CONTABLE_SALDO, FECHA_APERTURA, CTA_CLIENTE, OPERACION,
             DESC_TIPO_OPERACION, NOMBRE, CI, SUCURSAL) %>% 
      dplyr::filter(CTA_CLIENTE != 999999999)
    cahApertura <- cah %>% 
      anti_join(cah_total,by="NRO_CUENTA") %>% 
      dplyr::filter(FECHA_SALDO==FECHA_APERTURA)
    cah_total <- cah_total %>% 
      bind_rows(cahApertura)
    
    cah_new <- cah %>% 
      anti_join(cah_total,by="NRO_CUENTA")
    cahList[[k]] <- cah_new
    k <- k+1
  }
}
cahFull <- rbindlist(cahList)


cahApertura <- cahFull %>% 
  dplyr::filter(FECHA_SALDO==FECHA_APERTURA)

cahFassil <- cahFull %>% 
  dplyr::filter(FECHA_SALDO!=FECHA_APERTURA)
write_xlsx(cahFassil, "D:/!bso/Captaciones/cahFassil_22May25May.xlsx")





table(cah$DESC_TIPO_OPERACION)
%>% 
  select(NRO_CUENTA,FECHA_SALDO,NOMBRE,GENERO,EDAD,MONEDA,CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
         TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
         COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
         MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
  mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
  dplyr::filter(SALDO_SUS>0) %>% 
  mutate(TASA_REFERENCIAL = as.numeric(TASA_REFERENCIAL)/100)
cah_final <- caja_ahorro(cah)

write_rds(cah_final,paste0("D:/!bso/Captaciones/cah/cah_",substr(arch,20,27),'.rds'))

####____21 vs 28____####
cah_prev <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/BaseCaptaciones_CA_',
                          "20230519",'.txt'), sep="|") %>% 
  select(FECHA_SALDO, NRO_CUENTA, SALDO_SUS, CTA_CONTABLE_SALDO, FECHA_APERTURA, CTA_CLIENTE, OPERACION,
         DESC_TIPO_OPERACION, NOMBRE, CI, SUCURSAL) %>% 
  dplyr::filter(CTA_CLIENTE != 999999999)

cah_total <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/BaseCaptaciones_CA_',
                          "20230521",'.txt'), sep="|") %>% 
  select(FECHA_SALDO, NRO_CUENTA, SALDO_SUS, CTA_CONTABLE_SALDO, FECHA_APERTURA, CTA_CLIENTE, OPERACION,
         DESC_TIPO_OPERACION, NOMBRE, CI, SUCURSAL) %>% 
  dplyr::filter(CTA_CLIENTE != 999999999)

cah_new <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/03_Base_Capataciones_CAH_Diario/BaseCaptaciones_CA_',
                          "20230528",'.txt'), sep="|") %>% 
  select(FECHA_SALDO, NRO_CUENTA, SALDO_SUS, CTA_CONTABLE_SALDO, FECHA_APERTURA, CTA_CLIENTE, OPERACION,
         DESC_TIPO_OPERACION, NOMBRE, CI, SUCURSAL) %>% 
  dplyr::filter(CTA_CLIENTE != 999999999)

#CON CARNET
cah_clean <- cah_total %>% 
  mutate(esMigrada = ifelse(str_detect(DESC_TIPO_OPERACION,"Migrada"),1,0)) %>% 
  dplyr::filter(!((is.na(CI) | CI=="") & esMigrada==0)) %>% 
  dplyr::filter(!is.na(CI)) %>% 
  dplyr::filter(CI!="") %>% 
  group_by(CI) %>% 
  dplyr::filter(sum(esMigrada)>0) %>% 
  summarise(SALDO=sum(SALDO_SUS),esMigrada=max(esMigrada), ctasMigradas = sum(esMigrada), nCtas = n()) %>% 
  ungroup()

cah_joinCI <- cah_new %>% 
  group_by(CI) %>% 
  summarise(SALDO=sum(SALDO_SUS)) %>% 
  ungroup() %>% 
  left_join(cah_clean,by="CI") %>% 
  dplyr::filter(esMigrada==1)

#SIN CARNET
cah_clean2 <- cah_total %>% 
  mutate(esMigrada = ifelse(str_detect(DESC_TIPO_OPERACION,"Migrada"),1,0)) %>% 
  dplyr::filter(!((is.na(CI) | CI=="") & esMigrada==0)) %>% 
  dplyr::filter(is.na(CI) | CI=="")

cah_joinCta <- cah_new %>% 
  dplyr::filter(!CI %in% cah_joinCI$CI) %>% 
  left_join(cah_clean2,by="NRO_CUENTA") %>% 
  dplyr::filter(esMigrada==1)

cah_joinCTA <- cah_new %>% 
  
  
n_distinct(cah_clean$CI)
cah_clean %>% 
  group_by(CI) %>% 
  dplyr::filter(max(row_number())>1) %>% 
  ungroup() %>% 
  summarise(n=n_distinct(CI))

cah_join <- cah_new %>%  
  semi_join(cah_clean,by="NRO_CUENTA")

cah_not <- cah_clean %>% 
  anti_join(cah_new,by="NRO_CUENTA")

cah_complement <- cah_clean %>% 
  group
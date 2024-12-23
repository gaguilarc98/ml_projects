####____CARGA DE LIBRERIAS Y FUNCIONES_____####
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
####____CHECK LLAVEPRIMARIA Y MORA INTRAMES____####
excel_sheets("D:/!bso/accion/pruebas_dpd_llave/Etapa1_v5_enero2018.xlsx")
keys <- read_excel("D:/!bso/accion/pruebas_dpd_llave/Etapa1_v5_enero2018.xlsx",sheet = "MS81479brllaves") %>% 
  select(LLAVEPRIMARIA,CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion)

sapply(keys,function(x){length(which(is.na(x)))})
bdc <- read_excel("D:/!bso/accion/pruebas_dpd_llave/Etapa1_v5_enero2018.xlsx",sheet = "MS81479brbase") 
bdc_wkeys <- bdc %>% 
  left_join(keys, by="LLAVEPRIMARIA") %>% 
  dplyr::rename(maxDPD = `Máxima DPD`,
                diasDPD = `Días vencidos (DPD)`)
sapply(bdc_wkeys,function(x){length(which(is.na(x)))})

keys23 <- read_excel("D:/!bso/accion/pruebas_dpd_llave/Etapa1_v3_enero2023.xlsx",sheet = "MS81479brllaves") %>% 
  select(LLAVEPRIMARIA,CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion)

sapply(keys23,function(x){length(which(is.na(x)))})
bdc23 <- read_excel("D:/!bso/accion/pruebas_dpd_llave/Etapa1_v3_enero2023.xlsx",sheet = "MS81479brbase") 
bdc_wkeys23 <- bdc23 %>% 
  left_join(keys23, by="LLAVEPRIMARIA") %>% 
  dplyr::rename(maxDPD = `Máxima DPD`,
                diasDPD = `Días vencidos (DPD)`)
sapply(bdc_wkeys23,function(x){length(which(is.na(x)))})

bdcJoin <- bdc_wkeys23 %>% 
  inner_join(bdc_wkeys,by=c("CTACLIENTE","OPERACION"))

keysOld18 <- read_excel('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/2018/01_Etapa1_enero2018.xlsx',
                       sheet = "MS81479brllaves") %>% 
  select(LLAVEPRIMARIA,CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion)
bdcOld18 <- read_excel('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/2018/01_Etapa1_enero2018.xlsx',
                       sheet = "MS81479brbase")
bdc_old_wkeys <- bdcOld18 %>% 
  left_join(keysOld18,by="LLAVEPRIMARIA")

keysOld23 <- read_excel('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/Entrega_1_12abr/Entrega_ti/2023/01_Etapa1_enero2023.xlsx',
                       sheet = "MS81479brllaves") %>% 
  select(LLAVEPRIMARIA,CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion) %>%
  dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
  distinct_all()
bdcOld23 <- read_excel('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/Entrega_1_12abr/Entrega_ti/2023/01_Etapa1_enero2023.xlsx',
                       sheet = "MS81479brbase")
bdc_old_wkeys23 <- bdcOld23 %>% 
  left_join(keysOld23,by="LLAVEPRIMARIA")

bdcOld23 %>% 
  group_by(ESTADO) %>% 
  summarise(Saldo=sum(`Saldo pendiente`))
  
rds <- readRDS("D:/!bso/girCartera/rds/ec_Ene2023.rds") %>% 
  select(CTACLIENTE, OPERACION, ESTADO, saldous, saldoCast)

bdc_wkeys23Test <- bdc_wkeys23 %>% 
  select(CTACLIENTE,OPERACION, ESTADO, `Saldo pendiente`) %>% 
  inner_join(rds,by=c("CTACLIENTE","OPERACION"))

x <- bdc_wkeys23Test %>%
  dplyr::filter(ESTADO.x!=ESTADO.y) %>% 
  # summarise(sum(saldous),sum(`Saldo pendiente`))
  dplyr::filter(abs(saldous-`Saldo pendiente`)>0.003)

rds18 <- readRDS("D:/!bso/girCartera/rds/ec_Ene2018.rds") %>% 
  select(CTACLIENTE, OPERACION, ESTADO, saldous, saldoCast,
         FVEN_PROXPAGO, FVEN_ULTPAGO, FULT_PAGO, DIASMORA)

bdc_wkeysTest <- bdc_wkeys %>% 
  select(CTACLIENTE,OPERACION, ESTADO, `Saldo pendiente`) %>% 
  inner_join(rds18,by=c("CTACLIENTE","OPERACION"))

x <- bdc_wkeys23Test %>%
  dplyr::filter(ESTADO.x!="CASTIGADA") %>% 
  dplyr::filter(abs(saldous-`Saldo pendiente`)>0.003)
summarise(x,sum(saldous),sum(`Saldo pendiente`))
x %>% group_by(ESTADO.x) %>% 
  summarise(Saldo_rds = sum(saldous),Saldo_accion=sum(`Saldo pendiente`))

rdsJoin <- rds %>% 
  inner_join(rds18,by=c("CTACLIENTE","OPERACION"))
bdcJoin_wrds <- bdcJoin %>% 
  inner_join(rdsJoin,by=c("CTACLIENTE","OPERACION"))

rdsJoin_wold <- rds %>% 
  inner_join(bdc_old_wkeys23,by=c("CTACLIENTE","OPERACION"))
rdsJoin_wold %>% 
  group_by(ESTADO.x) %>% 
  summarise(Saldo=sum(saldous),SaldoACCION=sum(`Saldo pendiente`))

####____CHECK CASTIGOS____####

####____READING MAX DPD ACCION____####
excel_sheets("D:/!bso/accion/Base_febrero2020_revDPD.xlsx")
keys <- read_excel("D:/!bso/accion/Base_febrero2020_revDPD.xlsx",sheet = "MS81479brllaves") %>% 
  select(LLAVEPRIMARIA,CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion)
bdc <- read_excel("D:/!bso/accion/Base_febrero2020_revDPD.xlsx",sheet = "MS81479brbase") 
bdc <- bdc %>% 
  left_join(keys, by="LLAVEPRIMARIA") %>% 
  dplyr::rename(maxDPD = `Máxima DPD`,
                diasDPD = `Días vencidos (DPD)`)

table(bdc_wkeys$ESTADO)
bdc_wkeys %>% #Summary de días vencidos (DPD) por ESTADO
  group_by(ESTADO) %>% 
  summarise(Min_diasDPD = min(diasDPD), Max_diasDPD = max(diasDPD))
bdc_wkeys %>% #Check para Operaciones Vencidas y en Ejecución
  dplyr::filter(ESTADO %in% c("OP VENCIDA","JUDICIAL")) %>% 
  glimpse() %>% 
  dplyr::filter(maxDPD!=diasDPD) %>% 
  summarise(N_FAIL = n()) %>% 
  glimpse()

bdc_wkeys %>% #Check para Operaciones Vigente y en Suspenso con días de mora al cierre
  dplyr::filter(ESTADO %in% c("VIGENTE","SUSPENSO")) %>% 
  dplyr::filter(diasDPD>0) %>% 
  glimpse() %>% 
  dplyr::filter(diasDPD!=maxDPD) %>% 
  summarise(N_FAIL = n()) %>% 
  glimpse()
#_______________________________________________________________________________
#____CONCLUSION: VERIFICAR PARA LAS OPERACIONES EN SUSPENSO_____________________
#_______________________________________________________________________________

bdcFiltrada <- bdc_wkeys %>% #Check para Operaciones Vigente y en Suspenso sin días de mora al cierre
  relocate(CTACLIENTE, OPERACION, .after = LLAVEPRIMARIA) %>% 
  dplyr::filter(ESTADO %in% c("VIGENTE","SUSPENSO")) %>% 
  dplyr::filter(diasDPD==0) %>% 
  glimpse()

baseCartera <- readRDS("D:/!bso/girCartera/rds/ec_Feb2020.rds") %>% 
  select(CTACLIENTE,OPERACION,FVEN_PROXPAGO, FVEN_ULTPAGO, FULT_PAGO)
Pagosprev <- fread("D:/!bso/bases/csv/PagosCarteraJul2019_Dic2019.csv",sep = ",",
               fill = T,encoding = "UTF-8")
Pagosprev2 <- fread("D:/!bso/bases/csv/PagosCarteraEne2019_Jun2019.csv",sep = ",",
                   fill = T,encoding = "UTF-8")
Pagosprev3 <- fread("D:/!bso/bases/csv/PagosCarteraJul2018_Dic2018.csv",sep = ",",
                   fill = T,encoding = "UTF-8")
Pagosprev4 <- fread("D:/!bso/bases/csv/PagosCarteraEne2018_Jun2018.csv",sep = ",",
                    fill = T,encoding = "UTF-8")
Pagos <- fread("D:/!bso/bases/csv/PagosCarteraEne2020_Jun2020.csv",sep = ",",
                   fill = T,encoding = "UTF-8") %>% 
  # bind_rows(Pagosprev, Pagosprev2, Pagosprev3, Pagosprev4) %>% 
  mutate(across(c(FechaPrevistaPago,FechaPago,FechaTrx),~as.Date(.x))) %>% 
  # dplyr::filter(as.yearmon(FechaTrx)=="Feb. 2020") %>% 
  dplyr::filter(as.yearmon(FechaPrevistaPago)=="Feb. 2020") %>% 
  mutate(maxDiasMIM = ifelse(FechaPrevistaPago>=FechaPago,0, FechaPago-FechaPrevistaPago))
basePagos <- Pagos %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(maxDiasMIM = max(maxDiasMIM))

OpsRepetidas <- basePagos %>% 
  group_by(Cuenta,Operacion) %>% 
  mutate(obs = max(row_number())) %>% 
  dplyr::filter(obs>1) %>% 
  ungroup() %>% 
  arrange(Cuenta, Operacion)

bdcFiltradaJoin <- bdcFiltrada %>% 
  # left_join(basePagos,by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
  left_join(rds18, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(`Fecha de la primera cuota` = as.Date(`Fecha de la primera cuota`)) %>% 
  rename(FechaCuotaIni = `Fecha de la primera cuota`) %>% 
  # mutate(maxDiasMIM_old=maxDiasMIM) %>% 
  mutate(maxDiasMIM = ifelse(!is.na(FULT_PAGO) & !is.na(FVEN_ULTPAGO) & FULT_PAGO>FVEN_ULTPAGO &
                               FVEN_ULTPAGO>=as.Date("2018-01-01") & FVEN_PROXPAGO>=as.Date("2018-02-01"),FULT_PAGO-FVEN_ULTPAGO,0)) %>% 
  mutate(Check = ifelse(maxDPD==maxDiasMIM,1,0)) %>% 
  mutate(Check = ifelse(is.na(Check) & as.yearmon(FechaCuotaIni) > "Ene. 2018",1,Check)) %>%
  # mutate(Check = ifelse(is.na(Check) & !is.na(FULT_PAGO) & FULT_PAGO<=as.Date("2020-03-01"),1,Check)) %>% 
  # mutate(Check = ifelse(is.na(Check) & !is.na(FVEN_PROXPAGO) & FVEN_PROXPAGO>=as.Date("2020-03-01"),1,Check)) %>% 
  # mutate(Check = ifelse(is.na(Check) & Frecuencia %in% c("Semestral","Trimestral","Anual","Bimestral","Cuatrimestral","Irregular","Quintumestral"),1,Check)) %>% 
  relocate(Check, maxDPD, maxDiasMIM, .after = OPERACION)

x <- bdcFiltradaJoin %>% dplyr::filter(Check==0)
y <- x %>% 
  mutate(MAXDM = ifelse(!is.na(FULT_PAGO) & !is.na(FVEN_ULTPAGO) & FULT_PAGO>FVEN_ULTPAGO &
                           FVEN_ULTPAGO>=as.Date("2018-01-01") & FVEN_PROXPAGO>=as.Date("2018-02-01"),as.Date('2018-01-31')-FVEN_ULTPAGO,0)) %>% 
  mutate(Check2 = ifelse(MAXDM==maxDPD,1,0)) %>% 
  relocate(Check2,MAXDM, .after = OPERACION)

bdcFiltradaJoin %>% group_by(Check) %>% summarise(Nops=n())

Discrepancia_Total <- bdcFiltradaJoin %>% 
  dplyr::filter(Check==0)
Discrepancia_Pagos <- bdcFiltradaJoin %>% 
  dplyr::filter(maxDiasMIM_old!=maxDPD)
Discrepancia_Cartera <- bdcFiltradaJoin %>% 
  dplyr::filter(maxDiasMIM_old!=maxDiasMIM)

####____NUEVO CRUCE ENERO 2023____####
keys <- read_excel("D:/!bso/accion/pruebas_dpd_llave/Etapa1_enero2023.xlsx",sheet = "Hoja2") %>% 
  select(LLAVEPRIMARIA,CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion)

sapply(keys,function(x){length(which(is.na(x)))})
bdc <- read_excel("D:/!bso/accion/pruebas_dpd_llave/Etapa1_enero2023.xlsx",sheet = "Hoja1") 
bdc_wkeys <- bdc %>% 
  left_join(keys, by="LLAVEPRIMARIA") %>% 
  dplyr::rename(maxDPD = `Máxima DPD`,
                diasDPD = `Días vencidos (DPD)`)

rds18 <- readRDS("D:/!bso/girCartera/rds/ec_Ene2023.rds") %>% 
  select(CTACLIENTE, OPERACION, ESTADO, saldous, saldoCast,
         FVEN_PROXPAGO, FVEN_ULTPAGO, FULT_PAGO, DIASMORA)

bdcFiltrada <- bdc_wkeys %>% #Check para Operaciones Vigente y en Suspenso sin días de mora al cierre
  relocate(CTACLIENTE, OPERACION, .after = LLAVEPRIMARIA) %>% 
  dplyr::filter(ESTADO %in% c("VIGENTE","SUSPENSO")) %>% 
  dplyr::filter(diasDPD==0) %>% 
  glimpse()
bdcFiltradaJoin <- bdcFiltrada %>% 
  # left_join(basePagos,by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
  left_join(rds18, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(maxDPD = as.integer(maxDPD)) %>% 
  # mutate(`Fecha de la primera cuota` = as.Date(`Fecha de la primera cuota`)) %>% 
  # rename(FechaCuotaIni = `Fecha de la primera cuota`) %>% 
  # mutate(maxDiasMIM_old=maxDiasMIM) %>% 
  mutate(maxDiasMIM = ifelse(!is.na(FULT_PAGO) & !is.na(FVEN_ULTPAGO) & FULT_PAGO>FVEN_ULTPAGO &
                               FVEN_ULTPAGO>=as.Date("2023-01-01") & FVEN_PROXPAGO>=as.Date("2023-02-01"),FULT_PAGO-FVEN_ULTPAGO,0)) %>% 
  mutate(Check = ifelse(maxDPD==maxDiasMIM,1,0)) %>% 
  # mutate(Check = ifelse(is.na(Check) & as.yearmon(FechaCuotaIni) > "Ene. 2018",1,Check)) %>%
  # mutate(Check = ifelse(is.na(Check) & !is.na(FULT_PAGO) & FULT_PAGO<=as.Date("2020-03-01"),1,Check)) %>% 
  # mutate(Check = ifelse(is.na(Check) & !is.na(FVEN_PROXPAGO) & FVEN_PROXPAGO>=as.Date("2020-03-01"),1,Check)) %>% 
  # mutate(Check = ifelse(is.na(Check) & Frecuencia %in% c("Semestral","Trimestral","Anual","Bimestral","Cuatrimestral","Irregular","Quintumestral"),1,Check)) %>% 
  relocate(Check, maxDPD, maxDiasMIM, .after = OPERACION)

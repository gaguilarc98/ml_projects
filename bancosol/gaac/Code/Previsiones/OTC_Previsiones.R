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
library(stringr)
library(forcats) 
library(scales)
library(janitor)
library(fastDummies)
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
####____OBJETIVO____####
#Realizar la validacion de codigo OTC - ONC
####____Lectura de BDC JUL23 de PRODUCCION____####
bdcJulTest <- fread("D:/!bso/prevCorrecciones/BaseCarteraJul2023.txt",sep='|', encoding = "Latin-1",
                    fill = T) %>% 
  mutate(RUBRO = as.character(RUBRO)) %>% 
  select(CTACLIENTE, OPERACION, SUBOPERACION, MODULO, TIPO_OPER, ESTADO, MONEDA, 
         MONTO, SALDO, TIPO_CREDITO, FDESEMBOLSO, RUBRO, CALIFICACION, 
         PREVCONST, PRODUCTIVO, SECTOR_CARTERA, SOL_ASFI669  =`2°ASFI669/SOLUCIÓN`)

write_xlsx(bdcJulTest, "D:/!bso/prevCorrecciones/bdcTestJul2023.xlsx")

####____CAMBIOS DE PREV REQUERIDOS____####
corte <- "Jul. 2023"
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  glimpse()

dfTotal <- readRDS("D:/!bso/features/ClientesTTF_Ene2015Jul2023.rds")

dfSegment <- dfTotal %>% 
  dplyr::filter(fdes>= as.Date("2021-08-02") & fdes<=as.Date("2022-07-29")) %>% 
  dplyr::filter(str_sub(TIPO_CREDITO,1,1) %in% c('C','M','P','H') & MONEDA==0) %>% 
  select(CTACLIENTE, OPERACION, TIPO_CREDITO, MONEDA, FDES_ORIGINAL = fdes, fueReprog, fueRefin)

dfCalif <- dfOps %>% 
  semi_join(dfSegment, by=c("CTACLIENTE","OPERACION")) %>% 
  dplyr::filter(monDate <= corte) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(PeorCalif = max(CALIFICACION)) %>% 
  ungroup() %>% 
  select(CTACLIENTE, OPERACION, PeorCalif, CALIFICACION, monDate) %>% 
  pivot_wider(names_from = monDate, values_from = CALIFICACION)

dfSegment <- dfSegment %>% 
  left_join(dfCalif, by=c("CTACLIENTE","OPERACION")) %>% 
  dplyr::filter(PeorCalif !="A") %>%
  mutate(COD_OTC_ONC = "OTC")

####____Lectura Base de Cartera Jul 2023____####
bdcJul <- readRDS("D:/!bso/girCartera/rds/ec_Jul2023.rds")

####____Obtención de código OTC-ONC con HISTORICO____####
bdcJulTest_Exc <- bdcJulTest %>% 
  left_join(select(dfTotal, CTACLIENTE, OPERACION, FDES_ORIGINAL = fdes),
            by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(PROD = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo","4.C3.Prod Intelectual",
                                                   "5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada","8.Otra Prod.No Controlada"), 1, 2)) %>% 
  dplyr::filter(FDES_ORIGINAL>= as.Date("2021-08-02") & FDES_ORIGINAL<=as.Date("2022-07-29")) %>% 
  dplyr::filter(str_sub(TIPO_CREDITO,1,1) %in% c('C','M','P','H') & MONEDA==0) 

dfCalif <- dfOps %>% 
  semi_join(bdcJulTest_Exc, by=c("CTACLIENTE","OPERACION")) %>% 
  dplyr::filter(monDate <= corte) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(PeorCalif = max(CALIFICACION)) %>% 
  ungroup() %>% 
  select(CTACLIENTE, OPERACION, PeorCalif, CALIFICACION, monDate) %>% 
  pivot_wider(names_from = monDate, values_from = CALIFICACION)

dfSegment <- bdcJulTest_Exc %>% 
  left_join(dfCalif, by=c("CTACLIENTE","OPERACION")) %>% 
  dplyr::filter(PeorCalif !="A") %>% 
  mutate(COD_OTC_ONC = "OTC")

write_xlsx(dfSegment, "D:/!bso/prevCorrecciones/Excepcion_Jul2023.xlsx")

#COMPARANDO CON ANEXO DE OBSERVACIONES
Obs <- read_xlsx("D:/!bso/prevCorrecciones/Obs_codigo_ONC.xlsx")

ObsJul <- Obs %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(PeorCalifJulJunMay = max(`CALIFICACION JUL 23`,`CALIFICACION JUN 23`,`CALIFICACION MAYO 23`)) %>% 
  ungroup() %>% 
  dplyr::filter(PeorCalifJulJunMay != "A") %>% 
  mutate(CodCorrecto = "OTC") %>% 
  select(CTACLIENTE, OPERACION, CodCorrecto)

ObsJul_Join <- Obs %>% 
  inner_join(dfSegment, by=c("CTACLIENTE","OPERACION"))

ObsJul_AntiJoin <- Obs %>% 
  anti_join(dfSegment, by=c("CTACLIENTE","OPERACION"))
####____LECTURA DE BDC JUL####
bdc <- fread("D:/!bso/prevCorrecciones/BaseCarteraJul2023.txt",sep='|', encoding = "Latin-1",
             fill = T) %>% 
  dplyr::filter(MODULO !=131) %>% 
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  # dplyr::filter(ctaCont!="865") %>% 
  mutate(saldous = ifelse(MONEDA==0, SALDO/6.86, SALDO)) %>% 
  mutate(saldous = ifelse(ctaCont=="865",0,saldous)) %>% 
  mutate(previus = ifelse(MONEDA==0, PREVCONST/6.86, PREVCONST)) %>% 
  left_join(dfSegment, by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(COD_OTC_ONC = "ONC"))

x <- bdc %>% 
  dplyr::filter(TIPO_CREDITO.x != TIPO_CREDITO.y) %>%
  select(CTACLIENTE, OPERACION, COD_OTC_ONC, TIPO_CREDITO.x, TIPO_CREDITO.y)

bdcFixNeeded <- bdc %>% 
  mutate(PorNormaZero = ifelse(str_sub(TIPO_CREDITO,1,1) %in% c('P','M') & productivo==1 & CALIFICACION=="A",1,0)) %>% 
  dplyr::filter(PorNormaZero==0)
  dplyr::filter(COD_OTC_ONC=="OTC" & PREVCONST==0)

sum(bdc$saldous)
sum(bdcJul$saldous)

bdc %>% group_by(ctaCont) %>% summarise(Saldo=sum(saldous))
bdcJul %>% group_by(ctaCont) %>% summarise(Saldo=sum(saldous))
#BaseObservaciones
Obs <- read_xlsx("D:/!bso/prevCorrecciones/Obs_codigo_ONC.xlsx")

bdcClean <- bdc %>% 
  dplyr::filter(!MODULO %in% c(29, 131)) %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  mutate(saldous = ifelse(MONEDA==0, SALDO/6.86, SALDO)) %>% 
  left_join(select(dfSegment, CTACLIENTE, OPERACION, IncumpleExcepcion), 
            by = c("CTACLIENTE","OPERACION")) %>% 
  mutate(productivo = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo","4.C3.Prod Intelectual",
                                                   "5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada","8.Otra Prod.No Controlada"), 1, 2)) %>% 
  replace_na(list(IncumpleExcepcion=0))

bdcIncumple <- bdcClean %>% 
  mutate(PorNormaZero = ifelse(str_sub(TIPO_CREDITO,1,1) %in% c('P','M') & productivo==1 & CALIFICACION=="A",1,0)) %>% 
  dplyr::filter(PorNormaZero==1) %>% 
  dplyr::filter(IncumpleExcepcion==1)

####____IF_F____####
iff <- read_xlsx("D:/!bso/prevCorrecciones/IF_F_Reporte_Compl_Cartera.xlsx")
Obs <- read_xlsx("D:/!bso/prevCorrecciones/Obs_codigo_ONC.xlsx")

ObsJul <- Obs %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(PeorCalifJulJunMay = max(`CALIFICACION JUL 23`,`CALIFICACION JUN 23`,`CALIFICACION MAYO 23`)) %>% 
  ungroup() %>% 
  dplyr::filter(PeorCalifJulJunMay != "A") %>% 
  mutate(CodCorrecto = "OTC") %>% 
  select(CTACLIENTE, OPERACION, CodCorrecto)

iff_ObsJul <- iff %>% 
  inner_join(ObsJul, by=c("Cuenta"="CTACLIENTE", "Operacion"="OPERACION"))
  

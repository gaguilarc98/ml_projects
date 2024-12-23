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
####____DESCRIPTIVES____####
#FECHA_SALDO: Fecha del cierre del mes
#NOMBRE: Nombre de la persona Natural/Jurídica
#GENERO: Género de la persona Natural(M/F)/Jurídica
#MONEDA: ME, MN
#CTA_CONTABLE_SALDO: Cuenta de registro de movimiento
#TIPO_PERSONA: Natural/Jurídica
#ESTADO: Descripción del estado de la cuenta
#TIENE_CRED: Si el cliente tiene crédito
#TIENE_DPF: Si el cliente tiene dpf
#NOM_REGIONAL_ASOC: Regional/Sucursal
#PERIODO APERTURA: Lustro/año de apertura
#TIPO_OPERACION: Código del tipo de operación
#DESC_TIPO_OPERACION: Descripción del tipo de operación
#FORMA DE MANEJO: Conjunta, individual, indistinta u otros
#SALDO_SUS: Saldo en caja
#TASA_PIZARRA: Tasa de interés nominal que no considera capitalizaciones o recargos adicionales
####____FUNCTION____####
caja_mensual <- function(x){
  cah <- x %>% 
    mutate(across(c(FECHA_SALDO,FECHA_APERTURA),~ as.Date(.x,"%d/%m/%Y"))) %>% 
    mutate(cosechaY = year(FECHA_APERTURA)) %>% 
    mutate(PERIODO_APERTURA=case_when(cosechaY<2005~"(,2005)",
                                      cosechaY>=2005 & cosechaY<2010~"[2005,2010)",
                                      cosechaY>=2010 & cosechaY<2015~"[2010,2015)",
                                      cosechaY>=2015 ~ as.character(cosechaY))) %>% 
    mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
    dplyr::filter(SALDO_SUS>0) %>% 
    mutate(RANGO_SALDO= cut(SALDO_SUS,breaks=c(0,seq(1e3,9e3,1e3),seq(10e3,50e3,10e3),100e3,Inf),
                      labels=c("01. 0-1 M USD","02. 1-2 M USD","03. 2-3 M USD","04. 3-4 M USD","05. 4-5 M USD","06. 5-6 M USD","07. 6-7 M USD","08. 6-7 M USD","09. 7-8 M USD",
                               "10. 9-10 M USD","11. 10-20 M USD","12. 20-30 M USD","13. 30-40 M USD","14. 40-50 M USD","15. 50-100 M USD","16. >100 M USD"))) %>% 
    mutate(RANGO_SALDO=as.character(RANGO_SALDO)) %>% 
    mutate(TASA_PIZARRA=as.numeric(TASA_PIZARRA)/100)
  cah_j <- cah %>% 
    dplyr::filter(str_detect(TIPO_PERSONA, 'Jur')) %>%
    mutate(NRO_CUENTAS = 1) %>% 
    select(FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO,TIPO_PERSONA,ESTADO,
           TIENE_CRED,TIENE_DPF,NOM_REGIONAL_ASOC,RANGO_SALDO,TASA_PIZARRA,PERIODO_APERTURA,
           TIPO_OPERACION,DESC_TIPO_OPERACION, SALDO_SUS,NRO_CUENTAS)
  cah_n <- cah %>% 
    dplyr::filter(!str_detect(TIPO_PERSONA, 'Jur')) %>% 
    mutate(NOMBRE = 'Persona Natural') %>%
    group_by(FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO,TIPO_PERSONA,ESTADO,
             TIENE_CRED,TIENE_DPF,NOM_REGIONAL_ASOC,RANGO_SALDO,TASA_PIZARRA,PERIODO_APERTURA,
             TIPO_OPERACION,DESC_TIPO_OPERACION) %>%
    summarise(SALDO_SUS = sum(as.numeric(SALDO_SUS), na.rm = T), 
              NRO_CUENTAS = n_distinct(NRO_CUENTA))
  cah_final <- cah_j %>% 
    bind_rows(cah_n) 
  return(cah_final)
}
####____CAH MENSUAL EN LOOP____#################################################
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2021,2022,2023)
mycap <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mycap <- mycap[-c(which(mycap=="Mar2023"):length(mycap))]

cahList <- list()
for (i in 1:length(mycap)) {
  tryCatch({
    print(mycap[i])
    cah_mensual <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/01_Base_Capataciones_CAH_Mensual/BaseCaptaciones_CA_Mensual_',
                                mycap[i],'.txt'),encoding="UTF-8",sep="|") %>% 
      select(NRO_CUENTA,FECHA_SALDO,NOMBRE,GENERO,MONEDA,CTA_CONTABLE_SALDO, 
             TIPO_PERSONA,ESTADO,TIENE_CRED,TIENE_DPF,NOM_REGIONAL_ASOC,
             TIPO_OPERACION,DESC_TIPO_OPERACION, FORMA_DE_MANEJO,SALDO_SUS,
             TASA_PIZARRA, FECHA_APERTURA)
    cah_final <- caja_mensual(cah_mensual)
    cahList[[i]] <- cah_final
    write_rds(cah_final, paste0('D:/!bso/Captaciones/cahMes/cahM_',
                                mycap[i], '.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

cahFull <- bind_rows(cahList)

write.xlsx(cahFull,'D:/!bso/Captaciones/cah_mensual_ene21_feb23.xlsx')

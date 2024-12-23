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
####____VERIFICACION DE DIAS DE INCUMPLIMIENTO____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Mar2023.rds")
cic <- readRDS("D:/!bso/CIC/rds/cic_Mar2023.rds")

cicMora <- cic %>% 
  mutate(across(starts_with("Fecha"),~as.Date(.x))) %>% 
  mutate(DiasMora = ifelse(!is.na(FechaIncumplimiento),FechaCorte-FechaIncumplimiento,0)) %>% 
  select(CTACLIENTE, OPERACION, DiasMora, FechaCorte, FechaInicio, FechaIncumplimiento,
         CuentaContable.y)
bdcMora <- bdc %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','623','865')) %>% 
  select(CTACLIENTE, OPERACION, DIASMORA, FVEN_ULTPAGO, FVEN_PROXPAGO, FULT_PAGO,
         FDESEMBOLSO, ctaCont, MODULO, TIPO_OPER) %>% 
  left_join(cicMora, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(DMDif = DIASMORA==DiasMora)
  
table(bdcMora$DMDif, bdcMora$ctaCont)

####____LOOP FOR DIASMORA Y ESTADO____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018,2019,2020,2021,2022,2023)
mycic <- as.vector(sapply(year,function(x){paste0(mes,x)})) 
i <- 1
for (i in 1:length(mycic)) {
  tryCatch({
    print(mycic[i])
    cic <- readRDS(paste0('D:/!bso/CIC/rds/cic_', mycic[i], '.rds')) %>% 
      mutate(across(starts_with("Fecha"),~as.Date(.x))) %>% 
      mutate(across(c(Activo, Patrimonio, IngresosVentasServicios),~.x/6.86)) %>% 
      mutate(DiasMora = ifelse(!is.na(FechaIncumplimiento),FechaCorte-FechaIncumplimiento,0)) %>% 
      mutate(ESTADO = case_when(CuentaContable.y %in% c('131','135') ~ 'VIGENTE',
                                CuentaContable.y %in% c('133','136') ~ 'VENCIDA',
                                CuentaContable.y %in% c('134','137') ~ 'EJECUCION',
                                CuentaContable.y %in% c('865') ~ 'CASTIGADA',
                                CuentaContable.y %in% c('623') ~ 'CONTINGENTE',
                                TRUE~'OTRO'))
    if(i>=31){
      cic <- cic %>% 
        mutate(PrevCiclica = ifelse(Moneda=="MN", PrevCiclica/6.86, PrevCiclica)) %>% 
        mutate(IntSuspenso = ifelse(Moneda=="MN", IntSuspenso/6.86, IntSuspenso))
    }
    saveRDS(cic, paste0('D:/!bso/CIC/rds/cic_', mycic[i], '.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

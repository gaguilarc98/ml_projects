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
library(stringr) # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____DEBIDA DILIGIENCIA BID____####
meses <- c("Dic2020","Dic2021","Dic2022","Jun2023")
bdcicList <- list()
i <- 1
for (i in 1:length(meses)) {
  print(meses[i])
  cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",meses[i],".rds"))
  bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",meses[i],".rds"))
  bdcVar <- bdc %>% 
    select(monDate, CTACLIENTE, OPERACION, ctaCont, saldous, CALIFICACION, previus) %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137'))
  
  cicVar <- cic %>% 
    select(CTACLIENTE, OPERACION, MontoContratado, SaldoBruto, PrevCiclica,
           PrevEspecifica, PrevEspecificaDif, Calificacion, CuentaContable.x,
           CuentaContable.y, starts_with("Garantia"))
  
  bdcCIC <- bdcVar %>% 
    left_join(cicVar, by=c("CTACLIENTE","OPERACION")) %>% 
    select(-CTACLIENTE, -OPERACION) %>% 
    group_by(monDate, ctaCont, CuentaContable.x, CuentaContable.y, CALIFICACION, Calificacion) %>% 
    summarise_all(sum, na.rm=T) %>% 
    ungroup()
  
  bdcicList[[i]] <- bdcCIC
}
bdcicFull <- rbindlist(bdcicList) %>% 
  mutate(monDate = as.Date(monDate, frac=1))
write_xlsx(bdcicFull, "D:/!bso/CIC/bid_Jun2023.xlsx")

####____25 Mayores Clientes por Saldo____####
cic <- readRDS("D:/!bso/CIC/rds/cic_Jun2023.rds")
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds") 
bdcVar <- bdc %>% 
  select(CTACLIENTE, OPERACION, ctaCont, saldous, CALIFICACION, previus,
         Sucursal, TIPO_CREDITO, Sector_Actividad, Sector_Destino, GENERO) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137'))

cicVar <- cic %>% 
  select(CTACLIENTE, OPERACION, Calificacion, CuentaContable.x,
         CuentaContable.y, NombreRazonSocial, Nombre, Genero, TipoCredito,
         MontoContratado, SaldoBruto, SaldoMora, PrevCiclica,
         PrevEspecifica, PrevEspecificaDif,  starts_with("Garantia"))

bdcCIC <- bdcVar %>% 
  left_join(cicVar, by=c("CTACLIENTE","OPERACION")) %>% 
  arrange(desc(saldous)) %>% 
  mutate(ClienteMayor = ifelse(row_number()<=25, 1, 0)) %>% 
  arrange(desc(SaldoMora)) %>% 
  mutate(MoraMayor = ifelse(row_number()<=25, 1, 0)) %>% 
  dplyr::filter(ClienteMayor==1 | MoraMayor==1)

write_xlsx(bdcCIC, "D:/!bso/CIC/bid_25clientesMayores.xlsx")

sapply(bdcCIC, function(x){sum(is.na(x))})

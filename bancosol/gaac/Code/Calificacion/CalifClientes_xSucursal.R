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
library(fastDummies)
library(openxlsx)
library(sqldf)
# require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____READING INFOCRED AND BANTOTAL____####
year <- c("2018","2019","2020","2021","2022","2023")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mybdc <- as.vector(sapply(year, function(x){paste0(month,x)}))

infoList <- list()
sucList <- list()
i <- 65
mybdc <- "May2023"
for(i in 1:length(mybdc)){
  tryCatch({
    print(mybdc[i])
    bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc[i],'.rds')) %>%
      mutate(Fecha = as.yearmon(monDate)) %>%
      mutate(Sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', substr(as.character(AGENCIA),1,1))) %>% 
      mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
                                  Sucursal == '10' ~ 'El Alto',
                                  Sucursal == '2' ~ 'La Paz',
                                  Sucursal == '3' ~ 'Cochabamba',
                                  Sucursal == '4' ~ 'Oruro',
                                  Sucursal == '5' ~ 'Potosí',
                                  Sucursal == '6' ~ 'Tarija',
                                  Sucursal == '7' ~ 'Santa Cruz',
                                  Sucursal == '8' ~ 'Beni',
                                  Sucursal == '9' ~ 'Pando',)) %>% 
      select(Fecha, CTACLIENTE, OPERACION, CI, GENERO, ESTADO, Sucursal, DIASMORA,
             MODULO, previus, saldous, saldoMora, saldoCast)
    
    infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mybdc[i],'.rds'))
    
    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      mutate(Fecha = as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
      group_by(CI,esBSO) %>% 
      mutate(maxSaldo = ifelse(saldo==max(saldo),1,0)) %>% 
      mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
      ungroup() %>% 
      group_by(CI) %>%
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      # mutate(siglaPeorSF = ifelse(maxSaldo==1 & peorCalif==1 & esBSO==0,SIGLA,'_')) %>% 
      mutate(califPeorSF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>%
      # mutate(califPeorBSO = ifelse(peorCalif==1 & esBSO==1,CALIFICACION,'_')) %>% 
      # mutate(ESTADOPeorSF = ifelse(peorCalif==1 & esBSO==0,ESTADO,'_')) %>% 
      # mutate(across(siglaPeorSF:ESTADOPeorSF, ~ max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
      mutate(across(califPeorSF, ~ max(.x,na.rm=T))) %>%
      mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMora*noesBSO,na.rm=T)) %>% 
      mutate(montoNoBSO = sum(MontoOriginal*noesBSO,na.rm=T)) %>%
      ungroup() %>% 
      dplyr::filter(califPeorSF!="_" & SIGLA=="BSO")
    
    bdcSummary <- bdcBSO_full %>% 
      dplyr::filter(MODULO!=131) %>%
      dplyr::filter(ESTADO!="CASTIGADA") %>%
      group_by(Fecha, Sucursal) %>% 
      summarise(SaldoTotalBSO = sum(saldous),ClientesBSO=n_distinct(CI)) %>% 
      ungroup()
    
    infoJoin <- infoClean %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha"), 
                suffix=c("_info","_bdc")) %>% 
      left_join(bdcSummary, by=c("Fecha","Sucursal"))
    # infoList[[i]] <- infoJoin
    
    comper <- infoJoin %>% 
      group_by(Fecha, Sucursal) %>% 
      summarise(Saldo = sum(saldous), 
                SaldoCast = sum(saldoCast_info),
                SaldoComp = Saldo+SaldoCast,
                SaldoSF = sum(saldoNoBSO), 
                ClientesComp = n_distinct(CTACLIENTE), 
                SaldoTotalBSO = max(SaldoTotalBSO), 
                ClientesBSO = max(ClientesBSO), 
                pctSaldoComp = SaldoComp/SaldoTotalBSO, 
                pctClientesComp = ClientesComp/ClientesBSO)
    sucList[[i]] <- comper
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

sucFull <- rbindlist(sucList) %>% 
  ungroup()

sucFull <- sucFull %>% 
  mutate(Fecha = as.Date(Fecha, frac=1)) %>% 
  rename(`Saldo Activo USD en BSO` = Saldo,
         `Saldo Castigado USD en BSO de compartidos` = SaldoCast,
         `Saldo USD en BSO de compartidos` = SaldoComp,
         `Saldo USD en SF de compartidos` = SaldoSF,
         `Clientes Compartidos` = ClientesComp,
         `Saldo USD Total BSO`  = SaldoTotalBSO,
         `Clientes Totales BSO` = ClientesBSO,
         pct_Clientes_Compartidos = pctClientesComp,
         pct_Saldo_Compartido = pctSaldoComp)

write_xlsx(sucFull, "D:/!bso/califClientes/Clientes_Compartidos_MKT_Abr2023_xRegional.xlsx")  

####____AGREGAR UN MES AL ARCHIVO ANTERIOR____####
#Ejecutar el loop anterior solo para el mes correspondiente
#Luego regresar a esta sección
sucFull <- read_xlsx("D:/!bso/califClientes/Clientes_Compartidos_MKT_Abr2023_xRegional.xlsx") %>% 
  # mutate(pct_Saldo_Compartido = ifelse(Fecha >= as.Date("2022-07-01"),pct_Saldo_Compartido/100,pct_Saldo_Compartido)) %>% 
  # mutate(pct_Clientes_Compartidos = ifelse(Fecha >= as.Date("2022-07-01"),pct_Clientes_Compartidos/100,pct_Clientes_Compartidos)) %>% 
  glimpse()

comper <- comper %>% 
  mutate(Fecha = as.Date(Fecha, frac=1)) %>% 
  rename(`Saldo Activo USD en BSO` = Saldo,
         `Saldo Castigado USD en BSO de compartidos` = SaldoCast,
         `Saldo USD en BSO de compartidos` = SaldoComp,
         `Saldo USD en SF de compartidos` = SaldoSF,
         `Clientes Compartidos` = ClientesComp,
         `Saldo USD Total BSO`  = SaldoTotalBSO,
         `Clientes Totales BSO` = ClientesBSO,
         pct_Clientes_Compartidos = pctClientesComp,
         pct_Saldo_Compartido = pctSaldoComp)
sucFull <- sucFull %>% 
  bind_rows(comper)
#####
#ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA
#ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA
#ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA ALERTA
#Cambiar nombre de archivo
write_xlsx(sucFull, "D:/!bso/califClientes/Clientes_Compartidos_MKT_May2023_xRegional.xlsx")  

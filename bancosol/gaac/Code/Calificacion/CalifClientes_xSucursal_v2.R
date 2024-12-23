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
mybdc <- "Mar2023"
for(i in 1:length(mybdc)){
  tryCatch({
    print(mybdc[i])
    bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc[i],'.rds')) %>%
      dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>%
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
      select(Fecha, CTACLIENTE, OPERACION, CI, Sucursal, ctaCont, SALDO_USD = saldous, 
             SALDO_MORA = saldoMora)
    x <- bdcBSO_full %>% group_by(CTACLIENTE) %>% dplyr::filter(n_distinct(CI)>1)
    print(paste0("NRO DE CTACLIENTE CON NCI 1+: ",nrow(x)))
    
    infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mybdc[i],'.rds'))
    
    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(Fecha = as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
      group_by(CI) %>%
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(SALDO_USD = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(SALDO_VIG = sum(saldoVig*noesBSO,na.rm=T)) %>% 
      mutate(SALDO_MORA = sum(saldoMora*noesBSO,na.rm=T)) %>% 
      mutate(SALDO_CAST = sum(saldoCast*noesBSO,na.rm=T)) %>% 
      mutate(MONTO_USD = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
      ungroup() %>% 
      dplyr::filter(SIGLA=="BSO") %>%  #califPeorSF!="_"
      select(Fecha, CTACLIENTE, OPERACION, CI, starts_with("SALDO_"), MONTO_USD)
    
    
    infoJoin <- bdcBSO_full %>% 
      left_join(infoClean, by = c("CTACLIENTE","OPERACION","Fecha"), 
                suffix = c("_BSO","_SF")) %>% 
      group_by(CI_SF) %>% 
      mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO)) %>% 
      ungroup() %>% 
      group_by(CI) %>% 
      mutate(esComp = ifelse(!is.na(SALDO_USD_SF), 1, 0)) %>% 
      arrange(desc(esComp)) %>% 
      mutate(across(c(starts_with("SALDO"),"MONTO_USD"),~ifelse(row_number()==1, .x,0))) 
    
    comper <- infoJoin %>% 
      mutate(esCastigado = ifelse(ctaCont=='865',1,0)) %>% 
      group_by(Fecha, Sucursal) %>% 
      summarise(SaldoCompartidoBSO = sum(SALDO_USD_BSO*esComp, na.rm = T),
                SaldoCompartidoActivoSF = sum(SALDO_USD_SF, na.rm = T),
                SaldoCompartidoCastigadoSF = sum(SALDO_CAST, na.rm = T),
                ClientesCompartidos = n_distinct(CTACLIENTE[esComp==1]),
                SaldoTotalBSO = sum(SALDO_USD_BSO, na.rm = T),
                ClientesTotalesBSO = n_distinct(CTACLIENTE),
                PorcentajeSaldoCompartido = SaldoCompartidoBSO/SaldoTotalBSO,
                PorcentajeClienteCompartido = ClientesCompartidos/ClientesTotalesBSO)
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


#####____COMPARTIDOS EXCLUSIVOS____####
mybdc <- "Abr2023"

print(mybdc)
bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc,'.rds')) %>%
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
  mutate(FECHA = as.Date(monDate,frac=1)) %>%
  mutate(MONTO_USD = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(ESTADO = case_when(ctaCont %in% c("131","135")~"1. VIGENTE",
                            ctaCont %in% c("133","136")~"2. VENCIDA",
                            ctaCont %in% c("134","137")~"3. EJECUCION",
                            ctaCont == "865"~"4. CASTIGADA",)) %>% 
  mutate(SALDO_VIG = ifelse(ctaCont %in% c('131','135'),saldous,0)) %>% 
  select(FECHA, Sucursal, CTACLIENTE, OPERACION, CI, GENERO, ESTADO, MODULO, 
         FDESEMBOLSO, CALIFICACION, MONTO_USD, SALDO_USD = saldous, 
         SALDO_MORA = saldoMora, SALDO_VIG, SALDO_CAST = saldoCast) %>% 
  # group_by(FECHA, CTACLIENTE) %>% 
  # summarise(across(c(CI, GENERO, ESTADO, CALIFICACION),~max(.x)),
  #           across(c(MONTO_USD, SALDO_USD, SALDO_MORA, SALDO_VIG, SALDO_CAST),~sum(.x))) %>% 
  glimpse()


infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mybdc,'.rds'))

infoClean <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI) %>% 
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(ESTADO = max(ESTADO[esBSO==0])) %>% 
  mutate(CALIFICACION = max(CALIFICACION[esBSO==0])) %>% 
  mutate(SALDO_USD = sum(saldo*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_VIG = sum(saldoVig*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_MORA = sum(saldoMora*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_CAST = sum(saldoCast*noesBSO,na.rm=T)) %>% 
  mutate(MONTO_USD = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::filter(SIGLA=="BSO") %>% #CALIF_PEOR_SF!="_"
  select(CI, CTACLIENTE, OPERACION, ESTADO, CALIFICACION, SALDO_USD, SALDO_VIG, SALDO_MORA, 
         SALDO_CAST, MONTO_USD)


infoJoin <- bdcBSO_full %>% 
  inner_join(infoClean, by = c("CTACLIENTE","OPERACION"), 
             suffix=c("_BSO","_SF")) %>% 
  mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO)) 

infoAntiJoin <- bdcBSO_full %>% 
  anti_join(infoClean,by=c("CTACLIENTE","OPERACION"))

n_distinct(infoJoin$CTACLIENTE)==n_distinct(infoJoin$CI)


infoFullJoin <- bdcBSO_full %>% 
  left_join(infoClean, by= c("CTACLIENTE","OPERACION"),suffix=c("_BSO","_SF")) %>% 
  mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO)) 

  
n_distinct(infoFullJoin$CTACLIENTE)==n_distinct(infoFullJoin$CI)

Comp <- infoJoin %>% 
  group_by(FECHA,  CI) %>% 
  arrange(desc(FDESEMBOLSO)) %>% 
  mutate(CTACLIENTE = CTACLIENTE[row_number()==1]) %>% 
  mutate(Sucursal = Sucursal[row_number()==1]) %>% 
  mutate(GENERO = GENERO[row_number()==1]) %>% 
  summarise(across(c(CTACLIENTE, Sucursal, GENERO, CALIFICACION_SF, CALIFICACION_BSO, 
                     SALDO_USD_SF, SALDO_MORA_SF, SALDO_VIG_SF, SALDO_CAST_SF, MONTO_USD_SF),~max(.x)),
            across(c(SALDO_USD_BSO, SALDO_MORA_BSO, SALDO_VIG_BSO, MONTO_USD_BSO),~sum(.x)))

Exc <- infoAntiJoin %>% 
  group_by(FECHA,  CI) %>% 
  arrange(desc(FDESEMBOLSO)) %>% 
  mutate(CTACLIENTE = CTACLIENTE[row_number()==1]) %>% 
  mutate(Sucursal = Sucursal[row_number()==1]) %>% 
  mutate(GENERO = GENERO[row_number()==1]) %>% 
  summarise(across(c(CTACLIENTE, Sucursal, GENERO, CALIFICACION),~max(.x)),
            across(c(SALDO_USD, SALDO_MORA, SALDO_VIG, SALDO_CAST, MONTO_USD),~sum(.x)))

n_distinct(bdcBSO_full$CTACLIENTE)== nrow(Exc)+nrow(Comp)
lista <- list(Compartidos = Comp,Exclusivos = Exc)
write.xlsx(lista,'D:/!bso/califClientes/Compartidos_Exclusivos_May2023_v2.xlsx')

#####
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
      dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
      mutate(FECHA = as.Date(monDate,frac=1)) %>%
      mutate(MONTO_USD = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      group_by(CI) %>% 
      mutate(CALIFICACION = max(CALIFICACION)) %>% 
      ungroup() %>% 
      dplyr::filter(ctaCont!='865') %>% 
      mutate(SALDO_VIG = ifelse(ctaCont %in% c('131','135'),saldous,0)) %>% 
      select(FECHA, Sucursal, CTACLIENTE, OPERACION, CI, GENERO, 
             FDESEMBOLSO, CALIFICACION, MONTO_USD, SALDO_USD = saldous, 
             SALDO_MORA = saldoMora, SALDO_VIG, SALDO_CAST = saldoCast) %>% 
      glimpse()
    n_distinct(bdcBSO_full$CTACLIENTE)==n_distinct(bdcBSO_full$CI)
    
    infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mybdc[i],'.rds'))
    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
      group_by(CI) %>% 
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(CALIFICACION = max(CALIFICACION[esBSO==0])) %>% 
      mutate(SALDO_USD = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(SALDO_VIG = sum(saldoVig*noesBSO,na.rm=T)) %>% 
      mutate(SALDO_MORA = sum(saldoMora*noesBSO,na.rm=T)) %>% 
      mutate(SALDO_CAST = sum(saldoCast*noesBSO,na.rm=T)) %>% 
      mutate(MONTO_USD = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
      ungroup() %>% 
      dplyr::filter(SIGLA=="BSO") %>% #CALIF_PEOR_SF!="_"
      select(CI, CTACLIENTE, OPERACION, CALIFICACION, SALDO_USD, SALDO_VIG, SALDO_MORA, 
             SALDO_CAST, MONTO_USD)
    
    infoJoin <- bdcBSO_full %>%
      inner_join(infoClean, by = c("CTACLIENTE","OPERACION"),
                 suffix=c("_BSO","_SF")) %>%
      mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO))
    # 
    # infoAntiJoin <- bdcBSO_full %>% 
    #   anti_join(infoClean,by=c("CTACLIENTE","OPERACION"))
    # 
    # n_distinct(infoJoin$CTACLIENTE)==n_distinct(infoJoin$CI)
    # 
    Comp <- infoJoin %>% 
      group_by(FECHA,  CI) %>% 
      arrange(desc(FDESEMBOLSO)) %>% 
      mutate(CTACLIENTE = CTACLIENTE[row_number()==1]) %>% 
      mutate(Sucursal = Sucursal[row_number()==1]) %>% 
      mutate(GENERO = GENERO[row_number()==1]) %>% 
      summarise(across(c(CTACLIENTE, Sucursal, GENERO, CALIFICACION_SF, CALIFICACION_BSO, 
                         SALDO_USD_SF, SALDO_MORA_SF, SALDO_VIG_SF, SALDO_CAST_SF, MONTO_USD_SF),~max(.x)),
                across(c(SALDO_USD_BSO, SALDO_MORA_BSO, SALDO_VIG_BSO, MONTO_USD_BSO),~sum(.x))) %>% 
      ungroup() %>% 
      group_by(FECHA, Sucursal) %>% 
      summarise(across(c(SALDO_USD_SF, SALDO_MORA_SF, SALDO_VIG_SF,
                         SALDO_USD_BSO, SALDO_MORA_BSO, SALDO_VIG_BSO),~sum(.x)),
                CLIENTES_COMP = n_distinct(CI))
    
    bdcJoin <- bdcBSO_full %>% 
      left_join(infoClean, by= c("CTACLIENTE","OPERACION"),suffix=c("_BSO","_SF")) %>% 
      mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO)) %>% 
      group_by(Sucursal) %>% 
      summarise(SALDO_TOTAL_BSO = sum(SALDO_USD_BSO),
                CLIENTES_TOTAL_BSO = n_distinct(CI)) 
    comper <- Comp %>% 
      left_join(bdcJoin, by ="Sucursal") %>% 
      mutate(PCT_SALDO_COMPARTIDO = SALDO_USD_BSO/SALDO_TOTAL_BSO,
             PCT_CLIENTE_COMPARTIDO = CLIENTES_COMP/CLIENTES_TOTAL_BSO)
      
    sucList[[i]] <- comper
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

sucFull <- rbindlist(sucList) %>% 
  ungroup()
write_xlsx(sucFull, "D:/!bso/califClientes/Clientes_Compartidos_MKT_May2023_xRegional_v2.xlsx")  
####AUDITORIA EXTERNA
infoFullJoin <- bdcBSO_full %>% 
  left_join(infoClean, by= c("CTACLIENTE","OPERACION"),suffix=c("_BSO","_SF")) %>% 
  mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO)) %>% 
  select(-CI_BSO, -CI_SF) %>% 
  mutate(CALIFICACION_SF = ifelse(is.na(CALIFICACION_SF),"_",CALIFICACION_SF)) %>% 
  group_by(CI) %>% 
  summarise(CALIFICACION_BSO = max(CALIFICACION_BSO),
            CALIFICACION_SF = max(CALIFICACION_SF)) %>% 
  ungroup()

write_xlsx(infoFullJoin, "D:/!bso/requests/BD_CARTERA_INFO_CALMay2023_v2.xlsx")

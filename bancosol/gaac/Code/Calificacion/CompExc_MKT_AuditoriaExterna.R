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
####____#####
####____READING INFOCRED AND BANTOTAL____####
year <- c("2018","2019","2020","2021","2022","2023")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mybdc <- as.vector(sapply(year, function(x){paste0(month,x)}))

infoList <- list()
sucList <- list()
i <- 65
mybdc <- "Jul2023"
i <- 1
for(i in 1:length(mybdc)){
  tryCatch({
    print(mybdc[i])
    bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc[i],'.rds')) %>%
      dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
      # mutate(CI_sext = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA$","")) %>%
      # group_by(CI_sext, NOMBRE_TIT) %>%
      # arrange(desc(FDESEMBOLSO)) %>% 
      # mutate(CI=CI[row_number()==1]) %>% 
      # ungroup() %>% 
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
    
    #Hasta este punto terminan los pasos necesarios para actualizar un mes 
    #Continuar en la siguiente sección 
    #####_____PUNTO DE CONTROL____ #####
    #A partir de este solo correr para el loop entero
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

####____ADDING A MONTH TO COMPARTIDOS X SUCURSAL____####
sucFull <- read_xlsx("D:/!bso/califClientes/Clientes_Compartidos_MKT_Jun2023_xRegional.xlsx")  
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
compSuc <- Comp %>% 
  left_join(bdcJoin, by ="Sucursal") %>% 
  mutate(PCT_SALDO_COMPARTIDO = SALDO_USD_BSO/SALDO_TOTAL_BSO,
         PCT_CLIENTE_COMPARTIDO = CLIENTES_COMP/CLIENTES_TOTAL_BSO)

sucFull <- sucFull %>% 
  bind_rows(compSuc) 
write_xlsx(sucFull, "D:/!bso/califClientes/Clientes_Compartidos_MKT_Jun2023_xRegional.xlsx")  

####____LISTA DE COMPARTIDOS/EXCLUSIVOS AL CIERRE DE MES____####
#Correr el loop anterior para el cierre de mes correspondiente
infoJoin <- bdcBSO_full %>% 
  inner_join(infoClean, by = c("CTACLIENTE","OPERACION"), 
             suffix=c("_BSO","_SF")) %>% 
  mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO)) 

infoAntiJoin <- bdcBSO_full %>% 
  anti_join(infoClean,by=c("CTACLIENTE","OPERACION"))

n_distinct(infoJoin$CTACLIENTE)==n_distinct(infoJoin$CI)

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
write.xlsx(lista,'D:/!bso/califClientes/Compartidos_Exclusivos_Jun2023.xlsx')

####____AUDITORIA EXTERNA____####
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

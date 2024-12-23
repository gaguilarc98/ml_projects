remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

################################################################################
#LEER BASE
asfi <- read.xlsx('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/bases_formato_ASFI/REPORTE_ASFI_CAMPOS_DIC2022.xlsx') 
################################################################################
asfi_tabNov <- readRDS('C:/!bso/ValidacionRafael/BaseAsfiNov22.rds') %>% 
  select(`07_COD_TIPO_RELACION`,`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `30_MONTO_DES_BS`,
         `31_MONEDA`, `46_FECHA_PROX_PAGO_K`,`48_FECHA_ULT_PAGO_K`, `52_PRODUCTO`, 
         `67_DEST_CRED`,`68_GR_DEST_CRED`, `70_TIPO_GARANTIA`,`71_GTIA_COD_0_BS`,
         `72_GTIA_COD_1_BS`, `74_GTIA_COD_8_BS`, `75_COD_POND_ACTIVOS`, 
         `76_CTA_CONTABLE_NO_DIFERIDA`,`80_SALDO_TOTAL_BS`,`88_ESTADO`, `91_DIAS_INCUMPLIMIENTO`, 
         `92_CALIF_ENT`, `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`, 
         L_FechaIdentificacion3, L_EstadoGarantiaEntidad, FechaCancelacion, `07_COD_TIPO_RELACION`) 


asfi_tabDic<-asfi %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `30_MONTO_DES_BS`,
         `31_MONEDA`, `46_FECHA_PROX_PAGO_K`,`48_FECHA_ULT_PAGO_K`, `52_PRODUCTO`, 
         `67_DEST_CRED`,`68_GR_DEST_CRED`, `70_TIPO_GARANTIA`,`71_GTIA_COD_0_BS`,
         `72_GTIA_COD_1_BS`, `74_GTIA_COD_8_BS`, `75_COD_POND_ACTIVOS`, 
         `76_CTA_CONTABLE_NO_DIFERIDA`,`80_SALDO_TOTAL_BS`,`88_ESTADO`, `91_DIAS_INCUMPLIMIENTO`, 
         `92_CALIF_ENT`, `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`, 
         L_FechaIdentificacion3, L_EstadoGarantiaEntidad, `07_COD_TIPO_RELACION`)

baseBdic<- asfi_tabDic %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `46_FECHA_PROX_PAGO_K`,
         `48_FECHA_ULT_PAGO_K`, `80_SALDO_TOTAL_BS`,`88_ESTADO`,
         `91_DIAS_INCUMPLIMIENTO`, `92_CALIF_ENT`) %>% 
  dplyr::filter(`46_FECHA_PROX_PAGO_K`<=`48_FECHA_ULT_PAGO_K`)
################################################################################
#Criterios para el inciso B
fbase<-readRDS('C:/!bso/girCartera/rds/ec_Nov2022.rds')

bdc136<-fbase %>% 
  mutate(Operacion=paste0(CTACLIENTE, '-', OPERACION)) %>% 
  select(Operacion, FVEN_ULTPAGO) %>% 
  mutate(fvenUp=as.Date(FVEN_ULTPAGO, format='%d/%m/%y')) %>% 
  select(-FVEN_ULTPAGO)
  
baseBnov<- asfi_tabNov %>% 
  dplyr::rename(TIPO_REL=`07_COD_TIPO_RELACION`) %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `46_FECHA_PROX_PAGO_K`,
         `48_FECHA_ULT_PAGO_K`, `80_SALDO_TOTAL_BS`,`88_ESTADO`,
         `91_DIAS_INCUMPLIMIENTO`, TIPO_REL) %>% 
  dplyr::filter(TIPO_REL %in% c("1A","4A","5A","6A","7A")) %>%
  dplyr::filter(`88_ESTADO` %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía")) %>% 
  dplyr::rename(Operacion=`27_NRO_OPERACION`)

base136<-baseBnov %>%
  dplyr::filter(`88_ESTADO`!='Castigo') %>% 
  mutate(FechaProxPago=dmy(`46_FECHA_PROX_PAGO_K`)) %>% 
  mutate(FechaUltPago=dmy(`48_FECHA_ULT_PAGO_K`)) %>%
  mutate(FechaVen=dmy(`48_FECHA_ULT_PAGO_K`)) %>%
  dplyr::filter(FechaProxPago<FechaUltPago) %>% 
  distinct_all() %>% 
  left_join(val, by='Operacion') %>% 
  left_join(bdc136, by='Operacion') %>% 
  dplyr::filter(year(FechaUltPago)>=2021)

write.xlsx(base136, 'C:/!bso/ValidacionRafael/anexo1.xlsx')

Rafa <- read_excel('C:/!bso/ValidacionRafael/rafavalidacion.xlsx',sheet = 'Obs 2 - Anexo I', skip =1)
length(which(Rafa$`27_NRO_OPERACION...27` %in% baseBdic$`27_NRO_OPERACION`))
Rafa$`27_NRO_OPERACION`[which(!(Rafa$`27_NRO_OPERACION` %in% baseBdic$`27_NRO_OPERACION`))]

Rafa <- read_excel('C:/!bso/ValidacionRafael/rafavalidacion.xlsx',sheet = 'Obs 2 - Anexo I', skip =1)
length(which(Rafa$`27_NRO_OPERACION...27` %in% baseBnov$`27_NRO_OPERACION`))
Rafa$`27_NRO_OPERACION`[which(!(Rafa$`27_NRO_OPERACION` %in% baseBnov$`27_NRO_OPERACION`))]

val<-Rafa %>% 
  select(`27_NRO_OPERACION...27`) %>%
  dplyr::rename(Operacion=`27_NRO_OPERACION...27`) %>% 
  mutate(rafael=1)

################################################################################
#Criterios para el inciso c
baseC<-asfi_tabNov %>%
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, 
         `76_CTA_CONTABLE_NO_DIFERIDA`, `80_SALDO_TOTAL_BS`,`88_ESTADO`,
         `92_CALIF_ENT`) %>% 
  dplyr::filter(`29_TIPO_CRED` %in% c('H0',' H1', 'H2', 'H3', 'H4', 'M0', 'M1', 'M2',  
                                      'M7', 'M8', 'N0', 'N1', 'N2', 'P1', 'P3')) %>% 
  dplyr::filter(`76_CTA_CONTABLE_NO_DIFERIDA`=='13105') %>%          
  mutate(Inconsistencia='29_TIPO_CRED==H1 & 76_CTA_CONTABLE_NO_DIFERIDA==13105') %>% 
  mutate(Expli_Inc='Creditos contabilizados en la subcuenta equivocada') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

################################################################################
#Criterios para el inciso D


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
asfi <- read.xlsx('D:/!bso/bases/excel/BaseCarteraASFI_NOV2022.xlsx') 
write_rds(baseASFI,'D:/!bso/bases/rds/REPORTE_ASFI_NOV2022.rds')
asfi_tab <- readRDS('D:/!bso/bases/rds/REPORTE_ASFI_NOV2022_DEP.rds')

pp <- read.xlsx('D:/!bso/bases/excel/cta_tipoCred_estado.xlsx') %>% 
  mutate(CTA=as.numeric(CTA))
unique(pp$TIPO_CRED[which(pp$CTA==13101)])
################################################################################
asfi_tab <- asfi_tab %>%
  dplyr::filter(`07_COD_TIPO_RELACION` %in% c("1A","4A","5A","6A","7A")) %>% 
  dplyr::filter(`88_ESTADO` %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía")) %>% 
  mutate(`76_CTA_CONTABLE_NO_DIFERIDA`=as.numeric(`76_CTA_CONTABLE_NO_DIFERIDA`)) %>% 
  dplyr::
  dplyr::rename(CTA=`76_CTA_CONTABLE_NO_DIFERIDA`,
         ESTADO=`88_ESTADO`,
         TIPO_CRED=`29_TIPO_CRED`)

baseX <- asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, TIPO_CRED, `46_FECHA_PROX_PAGO_K`,
         `48_FECHA_ULT_PAGO_K`, CTA, `80_SALDO_TOTAL_BS`,ESTADO,
         `91_DIAS_INCUMPLIMIENTO`, `92_CALIF_ENT`, `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`) %>% 
  left_join(pp,by=c("TIPO_CRED","ESTADO","CTA"))
length(which(is.na(baseA$VER)))

baseA <- asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, TIPO_CRED, `46_FECHA_PROX_PAGO_K`,
         `48_FECHA_ULT_PAGO_K`, CTA, `80_SALDO_TOTAL_BS`,ESTADO,
         `91_DIAS_INCUMPLIMIENTO`, `92_CALIF_ENT`, `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`) %>% 
  dplyr::filter(`91_DIAS_INCUMPLIMIENTO`< 30 & ESTADO!='Vigente' & 
                  CTA=='13853') %>% 
  mutate(Inconsistencia='91_DIAS_INCUMPLIMIENTO< 30 & 88_ESTADO!=Vigente & 
                  76_CTA_CONTABLE_NO_DIFERIDA==13853') %>% 
  mutate(Expli_Inc='La inconsistencia se encuentra entre los días de incumplimiento 
         y la subcuenta contable') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()


####____RAQUEL____####
asfi_tab <- read.xlsx('C:/!bso/excel/BaseCarteraASFI_NOV2022.xlsx') %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `30_MONTO_DES_BS`,
         `31_MONEDA`, `46_FECHA_PROX_PAGO_K`,`48_FECHA_ULT_PAGO_K`, `52_PRODUCTO`, 
         `67_DEST_CRED`,`68_GR_DEST_CRED`, `70_TIPO_GARANTIA`,`71_GTIA_COD_0_BS`,
         `72_GTIA_COD_1_BS`, `74_GTIA_COD_8_BS`, `75_COD_POND_ACTIVOS`, 
         `76_CTA_CONTABLE_NO_DIFERIDA`,`80_SALDO_TOTAL_BS`,`88_ESTADO`, `91_DIAS_INCUMPLIMIENTO`, 
         `92_CALIF_ENT`, `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`, 
         L_FechaIdentificacion3, L_EstadoGarantiaEntidad, FechaCancelacion) 


baseA <- asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `46_FECHA_PROX_PAGO_K`,
         `48_FECHA_ULT_PAGO_K`, `76_CTA_CONTABLE_NO_DIFERIDA`, `80_SALDO_TOTAL_BS`,`88_ESTADO`,
         `91_DIAS_INCUMPLIMIENTO`, `92_CALIF_ENT`, `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`) %>% 
  dplyr::filter(`91_DIAS_INCUMPLIMIENTO`< 30 & `88_ESTADO`!='Vigente' & 
                  `76_CTA_CONTABLE_NO_DIFERIDA`=='13853') %>% 
  mutate(Inconsistencia='91_DIAS_INCUMPLIMIENTO< 30 & 88_ESTADO!=Vigente & 
                  76_CTA_CONTABLE_NO_DIFERIDA==13853') %>% 
  mutate(Expli_Inc='La inconsistencia se encuentra entre los días de incumplimiento 
         y la subcuenta contable') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseC<-asfi_tab %>%
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, 
         `76_CTA_CONTABLE_NO_DIFERIDA`, `80_SALDO_TOTAL_BS`,`88_ESTADO`,
          `92_CALIF_ENT`) %>% 
  dplyr::filter(`29_TIPO_CRED`=='H1' & `76_CTA_CONTABLE_NO_DIFERIDA`=='13105') %>% 
  mutate(Inconsistencia='29_TIPO_CRED==H1 & 76_CTA_CONTABLE_NO_DIFERIDA==13105') %>% 
  mutate(Expli_Inc='Creditos contabilizados en la subcuenta equivocada') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseD<-asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, 
         `76_CTA_CONTABLE_NO_DIFERIDA`, `80_SALDO_TOTAL_BS`,`88_ESTADO`,
         `92_CALIF_ENT`) %>% 
  dplyr::filter(`76_CTA_CONTABLE_NO_DIFERIDA`=='13505' & `88_ESTADO`=='Vigente'
                & `92_CALIF_ENT`== 'A') %>% 
  dplyr::filter(`29_TIPO_CRED`=='H2' | `29_TIPO_CRED`=='H1') %>% 
  mutate(Inconsistencia='76_CTA_CONTABLE_NO_DIFERIDA==13505 & 88_ESTADO==Vigente
                & 92_CALIF_ENT== A & 29_TIPO_CRED==H2 | 29_TIPO_CRED==H1') %>% 
  mutate(Expli_Inc='Creditos sin garantia hipotecaria erroneamente considerados para 
         la subcuenta 135.05') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseE<-asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `68_GR_DEST_CRED`, `70_TIPO_GARANTIA`,
         `71_GTIA_COD_0_BS`, `75_COD_POND_ACTIVOS`, `76_CTA_CONTABLE_NO_DIFERIDA`, `80_SALDO_TOTAL_BS`,
         `88_ESTADO`, `92_CALIF_ENT`) %>% 
  dplyr::filter(`29_TIPO_CRED`=='M2' & `76_CTA_CONTABLE_NO_DIFERIDA`%in%c('13105', '13505')  &
                  `88_ESTADO`=='Vigente' & `92_CALIF_ENT`== 'A' & `75_COD_POND_ACTIVOS`=='VI' & `68_GR_DEST_CRED`%in%c('E', 'D', 'G')) %>% 
  dplyr::filter(`70_TIPO_GARANTIA`%in%c('P04', 'P03', 'P03-P03',"P04-P04", 'P03-P03-P03-P03')) %>% 
  mutate(Inconsistencia='29_TIPO_CRED==M2 & 76_CTA_CONTABLE_NO_DIFERIDA%in%c(13105, 13505)  &
                  88_ESTADO==Vigente & 92_CALIF_ENT== A & 75_COD_POND_ACTIVOS==VI & 68_GR_DEST_CRED%in%c(E, D, G) &
                  70_TIPO_GARANTIA%in%c(P04, P03, P03-P03,P04-P04, P03-P03-P03-P03)') %>% 
  mutate(Expli_Inc='Microcreditos con codigo de ponderacion de activos incorrecto') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()


baseF<-asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `52_PRODUCTO`, `70_TIPO_GARANTIA`, 
         `74_GTIA_COD_8_BS`, `75_COD_POND_ACTIVOS`, `80_SALDO_TOTAL_BS`,`88_ESTADO`,
         `92_CALIF_ENT`) %>% 
  dplyr::filter(`29_TIPO_CRED`=='N2' & `75_COD_POND_ACTIVOS`=='VI'
                & `52_PRODUCTO`=='SOL VIVIENDA' & `88_ESTADO`=='Vigente'
                & `92_CALIF_ENT`== 'A' & `70_TIPO_GARANTIA`=='HI1') %>% 
  mutate(Inconsistencia='29_TIPO_CRED==N2 & 75_COD_POND_ACTIVOS==VI
                & 52_PRODUCTO==SOL VIVIENDA & 88_ESTADO==Vigente
                & 92_CALIF_ENT== A & 70_TIPO_GARANTIA==HI1') %>% 
  mutate(Expli_Inc='Credito catalogado como Sol Vivienda, cuando tiene todas las caracteristicas de
         ser Consumo debidamente garantizado') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseG<-asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `30_MONTO_DES_BS`, `52_PRODUCTO`, `70_TIPO_GARANTIA`, 
         `71_GTIA_COD_0_BS`, `74_GTIA_COD_8_BS`, `75_COD_POND_ACTIVOS`, `80_SALDO_TOTAL_BS`,
         `88_ESTADO`,`92_CALIF_ENT`) %>% 
  dplyr::filter(`52_PRODUCTO` %in% c('SOL VIVIENDA','SOL INDIVIDUAL','SOL PRODUCTIVO',
                                     'CREDITOS REPROGRAMADOS','SOL AGROPECUARIO')) %>% 
  dplyr::filter(`29_TIPO_CRED` %in% c('M1','M0','M7')) %>% 
  dplyr::filter(`92_CALIF_ENT`== 'F' | `92_CALIF_ENT`=='A') %>% 
  dplyr::filter(`70_TIPO_GARANTIA` %in% c('HI1','HO1','HV1','HI1-HO1-HO1','HO1-HO1-HO1-HO1')) %>% 
  dplyr::filter(`88_ESTADO` %in% c('Vigente','Vencido')) %>% 
  mutate(Inconsistencia='52_PRODUCTO%in% c(SOL VIVIENDA,SOL INDIVIDUAL,SOL PRODUCTIVO,
                        CREDITOS REPROGRAMADOS,SOL AGROPECUARIO & 29_TIPO_CRED %in% c(M1,M0,M7) &
                        92_CALIF_ENT== F | 92_CALIF_ENT==A & 70_TIPO_GARANTIA %in% c(HI1,HO1,HV1,HI1-HO1-HO1,HO1-HO1-HO1-HO1)
                        & 88_ESTADO %in% c(Vigente,Vencido') %>% 
  mutate(Expli_Inc='El valor de garantia es mayor al monto otorgado') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseH<-asfi_tab %>%
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `46_FECHA_PROX_PAGO_K`,`48_FECHA_ULT_PAGO_K`,
         `80_SALDO_TOTAL_BS`,`88_ESTADO`, `91_DIAS_INCUMPLIMIENTO`, `70_TIPO_GARANTIA`, `52_PRODUCTO`,
         `92_CALIF_ENT`) %>%
  dplyr::filter(`29_TIPO_CRED` %in% c( 'N0', 'N1')) %>%
  dplyr::filter(`52_PRODUCTO`%in% c('SOL EFECTIVO', 'SOL VEHÍCULO', 'SOL VIVIENDA',
                                    'CREDITOS REPROGRAMADOS')) %>% 
  dplyr::filter(`70_TIPO_GARANTIA` %in% c('HI1', 'HO1', 'HI1-HI1', 'HV1')) %>% 
  dplyr::filter(`88_ESTADO`%in% c('Vigente', 'Ejecución')) %>%
  dplyr::filter(`92_CALIF_ENT`%in% c('A', 'F')) %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseI<-asfi_tab %>%
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `30_MONTO_DES_BS`, `52_PRODUCTO`, `70_TIPO_GARANTIA`, 
         `71_GTIA_COD_0_BS`, `72_GTIA_COD_1_BS`, `74_GTIA_COD_8_BS`, `75_COD_POND_ACTIVOS`, `80_SALDO_TOTAL_BS`,
         `88_ESTADO`,`92_CALIF_ENT`) %>% 
  dplyr::filter(`52_PRODUCTO`=='SOL PRODUCTIVO' | `52_PRODUCTO`=='SOL INDIVIDUAL') %>%
  dplyr::filter(is.na(`70_TIPO_GARANTIA`)) %>% 
  dplyr::filter(`72_GTIA_COD_1_BS`=='0.00') %>% 
  dplyr::filter(`74_GTIA_COD_8_BS`=='0.00') %>% 
  dplyr::filter(`29_TIPO_CRED`=='M2' & `75_COD_POND_ACTIVOS`=='VI' & 
                  `88_ESTADO`=='Vigente') %>% 
  mutate(Inconsistencia= '52_PRODUCTO`==SOL PRODUCTIVO | 52_PRODUCTO==SOL INDIVIDUAL & 
         is.na(70_TIPO_GARANTIA & 72_GTIA_COD_1_BS==0.00 & 74_GTIA_COD_8_BS==0.00 &
         29_TIPO_CRED==M2 & 75_COD_POND_ACTIVOS==VI & 
                  88_ESTADO==Vigente  & 92_CALIF_ENT== A') %>% 
  mutate(Expli_Inc='Creditos sin garantias, clasificados como M2') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()
  
baseJ<- asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `30_MONTO_DES_BS`, `52_PRODUCTO`, `70_TIPO_GARANTIA`, 
         `71_GTIA_COD_0_BS`, `72_GTIA_COD_1_BS`, `74_GTIA_COD_8_BS`, `75_COD_POND_ACTIVOS`, `80_SALDO_TOTAL_BS`,
         `88_ESTADO`,`92_CALIF_ENT`) %>% 
  dplyr::filter(`29_TIPO_CRED`=='M1' & `70_TIPO_GARANTIA`=='HI1' & `75_COD_POND_ACTIVOS`=='VI'
                & `52_PRODUCTO`=='SOL VIVIENDA') %>% 
  mutate(Inconsistencia='29_TIPO_CRED==M1 & 70_TIPO_GARANTIA==HI1 & 75_COD_POND_ACTIVOS==VI
                & 52_PRODUCTO==SOL VIVIENDA') %>% 
  mutate(Expli_Inc='Valor de garantía mayor al monto otorgado') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseK<-asfi_tab %>%
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `31_MONEDA`, 
         `52_PRODUCTO`, `67_DEST_CRED`,`68_GR_DEST_CRED`, `80_SALDO_TOTAL_BS`,
         `88_ESTADO`,`92_CALIF_ENT`, `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`) %>% 
  dplyr::filter(`29_TIPO_CRED`=='M1' & `68_GR_DEST_CRED`=='E' & `100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO`=='2'
               & `52_PRODUCTO`=='SOL PRODUCTIVO' & `92_CALIF_ENT`== 'A' & `67_DEST_CRED`== '34400') %>% 
  mutate(Inconsistencia='29_TIPO_CRED==M1 & 68_GR_DEST_CRED==E & 100_CREDITO_PRODUCTIVO_NO.PRODUCTIVO==2
               & 52_PRODUCTO==SOL PRODUCTIVO & 92_CALIF_ENT== A & 67_DEST_CRED==34400') %>% 
  mutate(Expli_Inc='Catalogado como No productivo') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseL<-asfi_tab %>%
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `52_PRODUCTO`, `70_TIPO_GARANTIA`,
         `74_GTIA_COD_8_BS`, `80_SALDO_TOTAL_BS`,`88_ESTADO`, `92_CALIF_ENT`, 
         L_FechaIdentificacion3, L_EstadoGarantiaEntidad, FechaCancelacion) %>% 
  dplyr::filter(`88_ESTADO`=='Vigente' & `92_CALIF_ENT`== 'A') %>% 
  dplyr::filter(`29_TIPO_CRED`=='M2' | `29_TIPO_CRED`=='H0') %>% 
  dplyr::filter(`70_TIPO_GARANTIA`=='HI1'| `70_TIPO_GARANTIA`=='HO1') %>% 
  dplyr::filter(`52_PRODUCTO`=='SOL PRODUCTIVO' |`52_PRODUCTO`=='SOL VIVIENDA' |
                  `52_PRODUCTO`=='CREDITOS REPROGRAMADOS') %>% 
  dplyr::filter(`74_GTIA_COD_8_BS`=='0.00') %>% 
  mutate(Inconsistencia='88_ESTADO==Vigente & 92_CALIF_ENT== A & 29_TIPO_CRED==M2 | 29_TIPO_CRED==H0
         & 70_TIPO_GARANTIA==HI1| 70_TIPO_GARANTIA==HO1 & 52_PRODUCTO==SOL PRODUCTIVO |52_PRODUCTO==SOL VIVIENDA |
                  52_PRODUCTO==CREDITOS REPROGRAMADOS & 74_GTIA_COD_8_BS==0.00') %>% 
  mutate(Expli_Inc='Operaciones con garantia hipotecaria de grado 1, sin embargo aun se encuentran en 
         proceso de perfeccionamiento') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

baseN<-asfi_tab %>% 
  select(`27_NRO_OPERACION`, `28_FECHA_OP`, `29_TIPO_CRED`, `52_PRODUCTO`, `70_TIPO_GARANTIA`,
         `76_CTA_CONTABLE_NO_DIFERIDA`, `80_SALDO_TOTAL_BS`,`88_ESTADO`, `92_CALIF_ENT`) %>% 
  dplyr::filter(`52_PRODUCTO`=='SOL AGROPECUARIO' | `52_PRODUCTO`=='SOL PRODUCTIVO') %>%
  dplyr::filter(`29_TIPO_CRED`=='M7'| `29_TIPO_CRED`=='M0') %>% 
  dplyr::filter(`88_ESTADO`=='Ejecución' & `92_CALIF_ENT`=='F' & `76_CTA_CONTABLE_NO_DIFERIDA`=='13405') %>% 
  dplyr::filter(`70_TIPO_GARANTIA` %in% c('OT4-P03-P06-P06','OT4','OT4-P03-P01-P06', 'OT4-P03')) %>% 
  mutate(Inconsistencia= '52_PRODUCTO==SOL AGROPECUARIO | 52_PRODUCTO==SOL PRODUCTIVO & 
         29_TIPO_CRED==M7| 29_TIPO_CRED==M0 & 88_ESTADO==Ejecución & 92_CALIF_ENT==F & 
         76_CTA_CONTABLE_NO_DIFERIDA==13405 & 70_TIPO_GARANTIA %in% c(OT4-P03-P06-P06,OT4,OT4-P03-P01-P06)') %>% 
  mutate(Expli_Inc= 'Operaciones en estado ejecución con fondo de garantia de creditos 
         para el sector productivo') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()
  
baseTotalV2<-list(incisoA=baseA, incisoC=baseC, incisoD=baseD,incisoE=baseE, incisoF=baseF, 
                incisoG=baseG, incisoH=baseH, incisoI=baseI,incisoJ=baseJ, incisoK=baseK, 
                incisoL=baseL, incisoN=baseN, eeff=eeff)

write.xlsx(baseTotalV2, 'C:/!bso/ValidacionRafael/baseTotalV2.xlsx')


####
Rafa <- read_excel('C:/!bso/ValidacionRafael/rafavalidacion.xlsx',sheet = 'Obs 8 - Anexo II', skip =1)
length(which(Rafa$`27_NRO_OPERACION...27` %in% baseH$`27_NRO_OPERACION`))
Rafa$`27_NRO_OPERACION`[which(!(Rafa$`27_NRO_OPERACION` %in% baseH$`27_NRO_OPERACION`))]


Rafa <- read_excel('C:/!bso/ValidacionRafael/rafavalidacion.xlsx',sheet = 'Obs 14',skip=1)
length(which(Rafa$`27_NRO_OPERACION` %in% baseN$`27_NRO_OPERACION`))
Rafa$`27_NRO_OPERACION`[which(!(Rafa$`27_NRO_OPERACION` %in% baseN$`27_NRO_OPERACION`))]

################################################################################
bdc<-readRDS('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds/ec_Nov2022.rds') %>% 
  # dplyr::filter(ESTADO!='CASTIGADA') %>% 
  # dplyr::filter(MODULO!=131) %>% 
  mutate(saldous=ifelse(ESTADO=='CASTIGADA', saldoCast, saldous)) %>% 
  select(CALIFICACION, saldous, ctaCont) %>% 
    group_by(CALIFICACION, ctaCont) %>% 
    summarise_all(sum) %>%
  mutate(saldobs=saldous*6.86) %>% 
  select(-saldous) %>% 
  pivot_wider(values_from = saldobs, names_from = CALIFICACION) %>% 
  arrange(ctaCont) %>% 
  replace(is.na(.), 0) %>% 
  adorn_totals('row') %>% 
  dplyr::rename(Cuenta=ctaCont) %>% 
  mutate(Total=A+B+C+D+E+`F`) 


eeff<-read.xlsx('C:/!bso/eeff_a_20221130.xlsx') %>% 
  glimpse() %>% 
  dplyr::filter(Cuenta%in% c('131.00', '133.00', '134.00', '135.00', '136.00', 
                             '137.00','623.00', '865.00')) %>%
  mutate(Cuenta=substr(Cuenta,1,3)) %>% 
  dplyr::rename(`Saldo en Bs`=Total) %>% 
  select(-Descripcion) %>% 
  left_join(bdc, by="Cuenta") %>% 
  adorn_totals('row') %>% 
  relocate(`Saldo en Bs`,.after = 'Total') %>% 
  mutate(Diferencia=`Saldo en Bs`-Total)
                
bdc1<-readRDS('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds/ec_Nov2022.rds') %>% 
  glimpse()


base<-asfi%>% 
  select(`78_CTA_CONTABLE_DIFERIDA`, `95_INTERESES_SUSPENSO_Bs`,
         `92_CALIF_ENT`) %>% 
  mutate(`95_INTERESES_SUSPENSO_Bs`=as.numeric(`95_INTERESES_SUSPENSO_Bs`)) %>% 
  mutate(`78_CTA_CONTABLE_DIFERIDA`=substr(`78_CTA_CONTABLE_DIFERIDA`,1,3)) %>% 
  dplyr::filter(`78_CTA_CONTABLE_DIFERIDA`%in% c('133', '131', '134')) %>% 
  group_by(`78_CTA_CONTABLE_DIFERIDA`, `92_CALIF_ENT`) %>% 
  summarise_all(sum, na.rm=TRUE) %>%
  dplyr::rename(CALIFICACION=`92_CALIF_ENT`) %>% 
  pivot_wider(values_from = `95_INTERESES_SUSPENSO_Bs`, names_from = CALIFICACION) 
  
eeff1<-read.xlsx('C:/!bso/eeff_a_20221130.xlsx') %>% 
  glimpse() %>% 
  dplyr::filter(Cuenta%in% c('131.00', '133.00', '134.00')) %>%
  mutate(Cuenta=substr(Cuenta,1,3)) 

  
  
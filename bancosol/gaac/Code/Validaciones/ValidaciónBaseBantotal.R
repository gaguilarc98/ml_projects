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
library(tseries)
library(scales)
library(openxlsx)
library(janitor)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
################################################################################
#Leer bases
bdc<-fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraNov2020.txt', 
           fill=T, encoding='Latin-1') %>% 
  dplyr::filter(MODULO!=131) %>% 
  dplyr::filter(ESTADO!='CASTIGADA') %>% 
  mutate(subCtaCont=as.double(substr(as.character(RUBRO),1,5)))
bdc<- read_rds('D:/!bso/girCartera/rds/ec_Nov2022.rds') %>%
  dplyr::filter(MODULO!=131) %>% 
  dplyr::filter(ESTADO!='CASTIGADA') %>% 
  mutate(subCtaCont=as.double(substr(RUBRO,1,5))) %>% 
  select(CTACLIENTE, OPERACION,TIPO_CREDITO, subCtaCont) %>% 
  dplyr::filter(OPERACION == 3314134)


  
bdic<- readRDS('C:/!bso/girCartera/rds/ec_Dic2022.rds') %>%
  dplyr::filter(MODULO!=131) %>% 
  mutate(subCtaCont=as.double(substr(RUBRO,1,5)))

ctas <- read_excel('D:/!bso/bases/excel/ctasTipoCredito.xlsx', sheet='cta') %>% 
  rename(subCtaCont = subCta) %>% 
  mutate(subCtaCont = as.character(subCtaCont))

names <- read_excel('D:/!bso/bases/excel/modNames.xlsx')

prorrogas <- read_excel('D:/!bso/prorrogas/Prorrogas_Jul2023.xlsx') %>% 
  select(CTACLIENTE, OPERACION) %>% 
  mutate(OBSERVACION = 'Con prórroga')
################################################################################
baseA <- bdc %>% 
  mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),
                ~as.Date(.x,"%d/%m/%y"))) %>%
  select(CTACLIENTE, OPERACION, FVEN_PROXPAGO, FVEN_ULTPAGO, ESTADO, DIASMORA) %>% 
  dplyr::filter(FVEN_PROXPAGO< FVEN_ULTPAGO)

#No se puede replicar validación porque no se cuenta con información en FULT_PAGO
bdca<-bdic %>% 
  select(CTACLIENTE, OPERACION, FVEN_PROXPAGO, FVEN_ULTPAGO, ESTADO, DIASMORA) %>% 
  dplyr::filter(OPERACION == 2277814)
  
baseB <-bdic %>% 
  mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),
                ~as.Date(.x,"%d/%m/%y"))) %>%
  dplyr::filter(TIPO_CREDITO %in% c('H0', 'H1', 'H2', 'H3', 'H4', 'M0', 'M1',
                                'M2', 'M7', 'M8', 'N0', 'N1', 'N2', 'P1',
                                'P3', 'P9')) %>% 
  mutate(ultimoDia=as.Date(monDate, frac=1)) %>% 
  mutate(moraReal=as.numeric(ultimoDia-FVEN_ULTPAGO)) %>% 
  dplyr::filter(FVEN_PROXPAGO<FULT_PAGO & !(DIASMORA==moraReal)) %>% 
  select(CTACLIENTE, OPERACION, FVEN_PROXPAGO, FVEN_ULTPAGO, ESTADO, 
         DIASMORA, moraReal, FULT_PAGO)


baseC<-bdc %>%
  anti_join(ctas, by= c('TIPO_CREDITO', 'subCtaCont')) %>%
  dplyr::filter(subCtaCont!= 86501) %>% 
  select(CTACLIENTE, OPERACION, TIPO_CREDITO, subCtaCont, ESTADO)

#Inconsistencia encontrada
#TIPO_CREDITO P9 no existe en el Manual de Cuentas CIC.

basek<-bdc %>% 
  left_join(names, by='MODULO') %>% 
  mutate(prodNoprodD=ifelse(grupoCaedecD <= 'G', 'Prod', 'No Prod')) %>% 
  dplyr::filter(labGrupoD %in% c('A. Agricola ', 'B. Caza, Pesca', 'C. Ext. Gas y Pet', 
                                 'D. Ext. Minerales', 'E. Ind. y Manu',
                                 'F. Dist. EE y agua', 'G. Construcción', 'H. Comercio',
                                 'I. Hoteles', 'J. Transporte', 'K. Inter. Fin.', 'L. Serv. Inmob.',
                                 'M. Adm. Pública', 'N. Educación', 'O. Serv. Hosp + Otros',
                                 'P. Serv. Doméstico', 'Jubilados, Est. y AC', 'Otros'))%>%
  select(MODULO, NOMBRE_MODULO, labGrupoD, SECTOR_CARTERA, 
         prodNoprodD, OPERACION, CTACLIENTE, grupoCaedecD)

#No existe la columna que indique si es productivo o no

objeto<-list(InconsistenciaA=baseA, InconsistenciaB=baseB, InconsistenciaC=baseC)

write.xlsx(objeto, 'C:/!bso/ValidacionRafael/validacionNov22.xlsx')
################################################################################
#############################################################################
# tODO2022
long_list <- list.files('D:/!bso/girCartera/rds')
long_list <- c("ec_May2023.rds")
i <- 1
for(i in 1:length(long_list)){
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/', long_list[i])) %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>%  
    dplyr::filter(ESTADO!='CASTIGADA') %>%
    glimpse()
  
  baseA <- bdc %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),
                  ~as.Date(.x,"%d/%m/%y"))) %>%
    select(CTACLIENTE, OPERACION, FVEN_PROXPAGO, FVEN_ULTPAGO, ESTADO, DIASMORA) %>%
    dplyr::filter(FVEN_PROXPAGO< FVEN_ULTPAGO)
  
  #No se puede replicar validación porque no se cuenta con información en FULT_PAGO
  
  baseB <- bdc %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),
                  ~as.Date(.x,"%d/%m/%y"))) %>%
    dplyr::filter(TIPO_CREDITO %in% c('H0', 'H1', 'H2', 'H3', 'H4', 'M0', 'M1',
                                      'M2', 'M7', 'M8', 'N0', 'N1', 'N2', 'P1',
                                      'P3', 'P9')) %>%
    mutate(ultimoDia=as.Date(monDate, frac=1)) %>% 
    mutate(moraReal=as.numeric(ultimoDia-FVEN_ULTPAGO)) %>% 
    left_join(prorrogas, by=c("CTACLIENTE","OPERACION")) %>%
    mutate(OBSERVACION = ifelse(GRACIA_MESES>0, "Con periodo de gracia","Sin periodo de gracia")) %>%
    mutate(moraReal = ifelse(!is.na(OBSERVACION),0,moraReal)) %>%
    mutate(OBSERVACION = ifelse(is.na(OBSERVACION),'Inconsistencia',OBSERVACION)) %>%
    dplyr::filter(FVEN_PROXPAGO<FULT_PAGO & !(DIASMORA==moraReal)) %>% 
    select(CTACLIENTE, OPERACION, GRACIA_MESES, FDESEMBOLSO, FVEN_PROXPAGO, FVEN_ULTPAGO, 
           ESTADO, DIASMORA, moraReal, FULT_PAGO) #OBSERVACION
    
  baseC <- bdc %>%
    anti_join(ctas, by= c('TIPO_CREDITO', 'subCtaCont')) %>%
    dplyr::filter(subCtaCont!= 86501) %>% 
    select(CTACLIENTE, OPERACION, TIPO_CREDITO, subCtaCont, ESTADO) %>% 
    mutate(OBSERVACION = ifelse(TIPO_CREDITO=='P9','Pyme calificado por Días Mora debidamente garantizado con garantía real','Inconsistencia'))
  
  baseD <- bdc %>% 
    dplyr::filter((ESTADO %in% c("VIGENTE","SUSPENSO") & !ctaCont %in% c('131','135'))
                  | (ESTADO=="OP VENCIDA" & !ctaCont %in% c('133','136'))
                  | (ESTADO=="JUDICIAL" & !ctaCont %in% c('134','137'))) %>% 
    select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, ESTADO, RUBRO, ctaCont)
  #Inconsistencia encontrada
  #TIPO_CREDITO P9 no existe en el Manual de Cuentas CIC.
  
  baseP <- bdc %>% 
    mutate(CAEDEC_DEST = as.integer(CAEDEC_DEST)) %>% 
    left_join(codProd,by=c("CAEDEC_DEST"="CAEDEC")) %>% 
    # left_join(cicJoin, by=c("CTACLIENTE", "OPERACION")) %>% 
    mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD')) %>% 
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') 
                                  & grupoCaedecD %in% c('A','B','C','D','E','F','G'), 'S', 'N')) %>% #PARA PROD GRUPOS A-G
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'TURISMO' 
                                  & FDESEMBOLSO>FechaVigencia & MONEDA ==0 & str_detect(DESC_OBJCRED, "INVERSION"), 'S', PROD_NORMA)) %>% #SECTOR TURISMO
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'PRODUCCION INTELECTUAL' 
                                  & FDESEMBOLSO>FechaVigencia & MONEDA ==0, 'S', PROD_NORMA)) %>% #PRODUCCION INTELECTUAL
    mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'H&E' 
                                  & FDESEMBOLSO>FechaVigencia & MONEDA ==0, 'S', PROD_NORMA)) %>% #ELÉCTRICOS E HÍBRIDOS
    select(CTACLIENTE, OPERACION, TASAACT, FDESEMBOLSO, SECTOR_CARTERA, MONEDA,
           CAEDEC_DEST, DESC_OBJCRED, grupoCaedecD, TIPO_CREDITO, Sector, 
           FECHA_VIG_NORMA = FechaVigencia, PROD_NORMA) %>% 
    mutate(PROD_BASE = ifelse(SECTOR_CARTERA %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada',
                                                    '3.C2.Sector Turismo','4.C3.Prod Intelectual',
                                                    '5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                                    '8.Otra Prod.No Controlada'),'S','N')) %>% 
    dplyr::filter(PROD_BASE != PROD_NORMA)
    
  baseF <- bdc %>% 
    dplyr::filter(SALDO<MONTO & (is.na(FULT_PAGO) & is.na(FVEN_ULTPAGO))) %>% 
    select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, MONTO, SALDO, FDESEMBOLSO, 
           FULT_PAGO, FVEN_ULTPAGO, FVEN_PROXPAGO, ESTADO, ctaCont)
  
  baseG <- bdc %>% 
    mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
    mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"),1,0)) %>% 
    group_by(NDOC) %>%
    arrange(desc(FDESEMBOLSO)) %>%
    mutate(CI_fixed = CI[row_number()==1]) %>%
    ungroup() %>%
    select(CTACLIENTE, OPERACION, ends_with("TIT"), GENERO, CI, CI_fixed, NDOC, 
           MODULO, TIPO_OPER, esFSL, FDESEMBOLSO, ctaCont, RUBRO, AGENCIA, Sucursal) %>% 
    group_by(CI_fixed) %>% 
    mutate(ClienteTieneFSL = max(esFSL)) %>% 
    dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
    mutate(CuentasDistintas = n_distinct(CTACLIENTE)) %>% 
    mutate(CIDistintos = n_distinct(CI)) %>% 
    ungroup() %>% 
    mutate(across(ends_with("FSL"),~ifelse(.x==1,'S','N'))) %>% 
    arrange(CI_fixed)
  
  write.xlsx(baseG, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/ValidacionBDCBantotal/output/Validacion_',substr(long_list[i],4,10),'_CI.xlsx'))  
  # infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_Jun2023.rds'))
  # infoClean <- infoCheck %>% 
  #   dplyr::filter(REGULADO=="SBEF") %>% 
  #   dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  #   mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  #   mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  #   mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  #   group_by(CI) %>% 
  #   dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  #   mutate(CALIFICACION = max(CALIFICACION[esBSO==0])) %>% 
  #   mutate(SALDO_USD = sum(saldo*noesBSO,na.rm=T)) %>% 
  #   mutate(SALDO_VIG = sum(saldoVig*noesBSO,na.rm=T)) %>% 
  #   mutate(SALDO_MORA = sum(saldoMora*noesBSO,na.rm=T)) %>% 
  #   mutate(SALDO_CAST = sum(saldoCast*noesBSO,na.rm=T)) %>% 
  #   mutate(MONTO_USD = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
  #   ungroup() %>% 
  #   dplyr::filter(SIGLA=="BSO") %>% #CALIF_PEOR_SF!="_"
  #   select(CI, CTACLIENTE, OPERACION, CALIFICACION, SALDO_USD, SALDO_VIG, SALDO_MORA, 
  #          SALDO_CAST, MONTO_USD)
  # 
  # infoJoin <- bdc %>%
  #   inner_join(infoClean, by = c("CTACLIENTE","OPERACION"),
  #              suffix=c("_BSO","_SF")) %>%
  #   mutate(CI = ifelse(!is.na(CI_SF), CI_SF, CI_BSO)) %>% 
  #   select(CTACLIENTE, OPERACION, ends_with("TIT"), GENERO, 
  #          MODULO, TIPO_OPER, CI, CI_BSO, CI_SF) 
  # 
  # x <- infoJoin %>% 
  #   dplyr::filter(CI_BSO!=CI) %>% 
  #   group_by(CI_SF) %>% 
  #   dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  #   arrange(CI_SF)
  # sum(x$CTACLIENTE %in% baseG$CTACLIENTE)  
  
  exp <- data.frame(Inconsistencias=c("InconsistenciaA","InconsistenciaB","InconsistenciaC",
                                      "InconsistenciaD","InconsistenciaE","InconsistenciaF"),
                  Descripción=c("Inconsistencia: Fecha de próximo pago es menor a la fecha de último pago",
                        "Por tipo de crédito los días de mora son diferentes a los días de incumplimiento considerados en la base",
                        "Subcuentas incorrectas por tipo de crédito",
                        "Inconsistencias entre estado y cuenta contable",
                        "Revisión de clasificación de cartera al Sector Productivo",
                        "Créditos activos con saldo < monto y campos vacíos en FVEN_ULTPAGO y FULT_PAGO"))
  
#   res<-data.frame(Resumen="Tomando como referencia el informe de riesgo crediticio, referente a la
# Evaluación de la base de cartera de créditos al 30 de noviembre de 2022, se realizó la validación de
# cartera para cada mes desde enero 2022 a febrero del 2023, desde los incisos a-d. La validación a partir del inciso e no se
#                   pudo realizar debido a que la base de cartera de Bantotal no cuenta con información referente a garantía real,
#                   si el crédito es productivo o no y fecha de ingreso a estado")
  
  objeto <- list(InconsistenciaA=baseA, InconsistenciaB=baseB, InconsistenciaC=baseC, 
                 InconsistenciaD=baseD, InconsistenciaE=baseP, InconsistenciaF=baseF,
                 Diccionario=exp)
  write.xlsx(objeto, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/ValidacionBDCBantotal/output/Validacion_',substr(long_list[i],4,10),'.xlsx'))
}
  
################################################################################
#Ejemplo
eje<-bdc %>% 
  select(OPERACION, CTACLIENTE, TIPO_CREDITO, subCtaCont) %>% 
  dplyr::filter(row_number()<=10)

ctasEje<-ctas %>% 
  dplyr::filter(TIPO_CREDITO=='H1' & subCtaCont==13129)

eje_2<-eje %>% 
  anti_join(ctasEje, by=c('TIPO_CREDITO', 'subCtaCont'))

##Ejemplo de validación C de RAFAEL
bdc<- read_rds('C:/!bso/girCartera/rds/ec_Nov2022.rds') %>%
  dplyr::filter(MODULO!=131) %>% 
  dplyr::filter(ESTADO!='CASTIGADA') %>% 
  mutate(subCtaCont=as.double(substr(RUBRO,1,5))) %>% 
  select(CTACLIENTE, OPERACION,TIPO_CREDITO, subCtaCont) %>% 
  dplyr::filter(OPERACION == 3314134)

################################################################################
####CUADRE CON EEFF########
cuadre <- function(x){
  x %>% 
    dplyr::filter(MODULO!=131) %>% 
    dplyr::filter(MODULO!=29) %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
    select(monDate,ctaCont, saldous, previus, saldoCast) %>% 
    group_by(monDate, ctaCont) %>% 
    summarise_all(sum, na.rm=T) %>% 
    dplyr::filter(substr(ctaCont,1,1)!='6') %>% 
    ungroup() %>% 
    mutate(monDate = as.Date(monDate,frac=1))
}
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022:2023)
myrds <- as.vector(sapply(year, function(x){paste0(mes,x)}))
gc()
# long_list <- list.files('D:/!bso/girCartera/rds')
# long_list <- c("ec_Abr2023.rds")
cefList <- list()
for(i in 1:length(myrds)){
  tryCatch({
    print(myrds[i])
    bdc <- read_rds(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds'))
    ceeff <- bdc %>% 
      cuadre()

    cefList[[i]] <- ceeff
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
cuadreFinal <- rbindlist(cefList)

# write.xlsx(objeto, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/archivosRaque/validacionBDCBantotal/output/Cuadre_',substr(long_list[i],4,10),'.xlsx'))
write.xlsx(cuadreFinal, '//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/validacionBDCBantotal/output/Bantotal_CuadreEEFF.xlsx')
####____ADD A MONTH____####
#Read with read_xlsx to get Dates correct
cuadreFinal <- read_xlsx('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/validacionBDCBantotal/output/Bantotal_CuadreEEFF.xlsx')
myrds <- "Jul2023"
bdc <- read_rds(paste0('D:/!bso/girCartera/rds/ec_',myrds,'.rds'))
ceeff <- bdc %>% 
  cuadre()

cuadreFinal <- cuadreFinal %>% 
  bind_rows(ceeff)
write.xlsx(cuadreFinal, '//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/validacionBDCBantotal/output/Bantotal_CuadreEEFF.xlsx')
################################################################################
#castigo
gc()
castList <- list()
ct_list<-c()
ct_list <- list.files('C:/!bso/ValidacionRafael/rds')

for(i in 1:length(ct_list)){
  cCast<- read_rds(paste0('C:/!bso/ValidacionRafael/rds/', ct_list[i]))
  castF<-cCast %>% 
    select(monDate, ctaCont, saldoCast) %>% 
    group_by(monDate, ctaCont) %>% 
    summarise_all(sum, na.rm=T) %>% 
    dplyr::filter(substr(ctaCont,1,1)=='8') %>% 
    ungroup() %>% 
    group_by(monDate) %>% 
    mutate(saldoCast=sum(saldoCast))
  castList[[i]] <- castF
  
}

write.xlsx(castList, 'C:/!bso/ValidacionRafael/castigo.xlsx')
####____BASE DE CARTERA DIARIA____####
mylast <- data.frame(mycierre = seq.Date(as.Date("2022-01-01"),as.Date("2023-07-01"),by = "month")) %>% 
  mutate(mycierre = as.yearmon(mycierre)) %>% 
  mutate(mycierre =as.character(as.Date(mycierre,frac=1))) %>% 
  mutate(mycierre =str_replace_all(mycierre,"-","")) %>% 
  pull()
bdcDList <- list()
for (i in 1:length(mylast)) {
  print(mylast[i])
  bdcDiario <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_',mylast[i],'.rds'))
  bdcDiarioExp <- bdcDiario %>% 
    mutate(Fecha = dayDate) %>% 
    select(Fecha, ctaCont, saldous, saldoCast, previus) %>% 
    group_by(Fecha, ctaCont) %>% 
    summarise_all(sum)
  
  bdcDList[[i]] <- bdcDiarioExp
}
bdcDFull <- rbindlist(bdcDList)
write.xlsx(bdcDFull, "//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/validacionBDCBantotal/BDC_diarioCierreMes.xlsx")

#####____NÚMEROS DE CIERRE____#####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Ago2023.rds")
bdcSinFSL <- bdc %>% 
  dplyr::filter(!(MODULO==118 | str_detect(TIPO_OPER,"MIGR"))) %>%
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(par0Total = sum(par0)/sum(saldous)) %>% 
  group_by(Sucursal) %>% 
  summarise(Saldo = sum(saldous), par0 = sum(par0), SaldoMora = sum(saldoMora)/sum(saldous),
            Mora0=sum(par0)/sum(saldous),par0Tot = max(par0Total),
            Clientes=n_distinct(CTACLIENTE),CIs = n_distinct(CI)) %>%
  adorn_totals(c("row"))
#CARTERA BFS
bdcFSL <- bdc %>% 
  dplyr::filter((MODULO==118 | str_detect(TIPO_OPER,"MIGR"))) %>%
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>%
  mutate(par0_ = ifelse(!is.na(FVEN_PROXPAGO) & FVEN_PROXPAGO<=as.Date("2023-08-31"),
                        saldous,0)) %>% #PRORROGAS?
  mutate(par0Total = sum(par0_)/sum(saldous)) %>% 
  group_by(Sucursal) %>% 
  summarise(Saldo = sum(saldous), par0 = sum(par0_), SaldoMora = sum(saldoMora)/sum(saldous),
            Mora0=sum(par0_)/sum(saldous),par0Tot = max(par0Total),
            Clientes=n_distinct(CTACLIENTE),CIs = n_distinct(CI)) %>%
  adorn_totals(c("row"))

bdcAsesores <- bdc %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA, "Norm|Móvil")) %>% 
  group_by(Sucursal) %>% 
  summarise(Asesores = n_distinct(NOMBRE_ASESOR)) %>% 
  adorn_totals("row")

####____SOLICITUD RAFAEL RENGEL____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Ago2023.rds")
bdcReq <- bdc %>% 
  mutate(MIGRADO = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 'S', 'N')) %>% 
  mutate(RUBRO = as.character(RUBRO)) %>% 
  mutate(ESTADO = case_when(ESTADO=='SUSPENSO' ~ ESTADO,
                            ctaCont=='623' ~ ESTADO, 
                         ctaCont %in% c('131','135') ~ 'VIGENTE',
                         ctaCont %in% c('133','136') ~ 'OP VENCIDA',
                         ctaCont %in% c('134','137') ~ 'JUDICIAL',
                         ctaCont == '865' ~ 'CASTIGADA',)) %>% 
  select(CTACLIENTE, OPERACION, AGENCIA, NOMBRE_AGENCIA, SUCURSAL = Sucursal, 
         MODULO, TIPO_OPER, MONEDA, SALDO, MONTO, PREVCONST, RUBRO, 
         CUENTA_CONTABLE= ctaCont, ESTADO, CALIFICACION, DIASMORA, PLAZODIAS, 
         FDESEMBOLSO, FVEN_ULTPAGO, FULT_PAGO, FVEN_PROXPAGO,
         starts_with("TASA"), COD_TIPO_CREDITO=TIPO_CREDITO, TIPO_CREDITO=tipoCred,  
         SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO, RUBRO_CAPITAL_DIFERIDO, 
         RUBRO_INT_CAPITAL_DIFERIDO, MIGRADO, OPERACION_ORI_REF)

sapply(bdcReq, function(x){sum(is.na(x))})

write_xlsx(bdcReq, "D:/!bso/requests/BaseCartera_Ago2023.xlsx")

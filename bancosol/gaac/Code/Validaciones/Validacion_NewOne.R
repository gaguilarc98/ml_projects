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
####____BASES AUXILIARES____####
#De asignación de CuentaContable por TipoCredito
ctas <- read_excel('D:/!bso/bases/excel/ctasTipoCredito.xlsx', sheet='cta') %>% 
  rename(subCtaCont = subCta) %>% 
  mutate(subCtaCont = as.character(subCtaCont))
#De sector productivo
# codProd <- read_excel("D:/!bso/bases/excel/CodSecProd.xlsx",sheet='RPT004') %>% 
#   rename(CAEDEC = caecn, GrupoActEcon = cgaec) %>% 
#   glimpse()
SectorEcon <- read_excel("D:/!bso/bases/excel/SectorEcon.xlsx")

prorrogas <- read_excel('D:/!bso/prorrogas/Prorrogas_Sep2023.xlsx') %>% 
  select(CTACLIENTE, OPERACION) %>% 
  mutate(OBSERVACION = 'Con prórroga')
####____LOOP DE CRITERIOS DE VALIDACION____####
#Leer CIC del mes anterior para jalar la FechaIncumplimiento anterior
cic <- readRDS(paste0('D:/!bso/CIC/rds/cic_','Ago2023','.rds')) %>% 
  dplyr::filter(CuentaContable.y %in% c('131','133','134','135','136','137','865')) %>% 
  select(CTACLIENTE, OPERACION, FechaIncumplimiento, TipoCredito, CodActEconDest,
         CodActEconOri, FechaCorte, DiasMora)

myrds <- list.files('D:/!bso/girCartera/rds')
#Para un mes crear una lista con el nombre del mes correspondiente
myrds <- c("Oct2023")
i <- 1
for(i in 1:length(myrds)){
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i],'.rds')) %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>%  
    mutate(NOMBRE = str_trim(paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT))) %>% 
    mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
    mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"),1,0)) %>% 
    glimpse()
  
  base0 <- bdc %>% 
    select(CTACLIENTE, OPERACION, FDESEMBOLSO, TIPO_CREDITO, PLAZODIAS, TIPO_OPER,
           ctaCont, MONEDA, CALIFICACION, CI, SECTOR_CARTERA,
           CAEDEC_DEST, AGENCIA, DIASMORA, CALIFICACION, 
           ASESOR, NOMBRE) %>% 
    mutate(across(where(is.character), ~na_if(.x,'')))
  base0 <- base0[!complete.cases(base0),]
  
  base1 <- bdc %>% 
    select(CTACLIENTE, OPERACION, NOMBRE, CI, MODULO, TIPO_OPER, FDESEMBOLSO,
           CAEDEC_DEST, CIU, MONTO, SALDO) %>% 
    mutate(NCHAR_DEST = nchar(CAEDEC_DEST),
           NCHAR_ORIG = nchar(CIU)) %>% 
    dplyr::filter(NCHAR_DEST!=5 | NCHAR_ORIG!=5)
    
  baseA <- bdc %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),
                  ~as.Date(.x,"%d/%m/%y"))) %>%
    select(CTACLIENTE, OPERACION, FVEN_PROXPAGO, FVEN_ULTPAGO, ESTADO, DIASMORA) %>%
    dplyr::filter(FVEN_PROXPAGO<FVEN_ULTPAGO)
  
  baseB <- bdc %>% 
    select(monDate, CTACLIENTE, OPERACION, MODULO, TIPO_OPER, ESTADO, ctaCont, 
           GRACIA_MESES, FDESEMBOLSO, FULT_PAGO, FVEN_ULTPAGO, FVEN_PROXPAGO, DIASMORA) %>% 
    dplyr::filter(!ctaCont %in% c('131','135')) %>% 
    mutate(across(c(FDESEMBOLSO,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO),
                  ~as.Date(.x,"%d/%m/%y"))) %>%
    mutate(FCIERRE=as.Date(monDate, frac=1)) %>% 
    mutate(CalcMoraBantotal = ifelse(!is.na(FVEN_PROXPAGO),FCIERRE-FVEN_ULTPAGO,0)) %>% 
    left_join(cic, by=c("CTACLIENTE","OPERACION")) %>% 
    mutate(CalcMoraCIC = case_when(!is.na(FechaIncumplimiento) & !is.na(FVEN_PROXPAGO)
                                   & FVEN_PROXPAGO<=FCIERRE~as.numeric(FCIERRE-FechaIncumplimiento), 
                                   !is.na(FechaIncumplimiento) 
                                   & is.na(FVEN_PROXPAGO) ~ as.numeric(FCIERRE-FechaIncumplimiento),
                                   TRUE ~ 0)) %>% 
    mutate(DifBantotal = CalcMoraBantotal==DIASMORA) %>% 
    mutate(DifCIC = CalcMoraCIC==DIASMORA) %>% 
    # left_join(prorrogas, by=c("CTACLIENTE","OPERACION")) %>%
    # mutate(OBSERVACION = ifelse(GRACIA_MESES>0, "Con periodo de gracia","Sin periodo de gracia")) %>%
    dplyr::filter(DifCIC == FALSE)
  
  baseC <- bdc %>%
    dplyr::filter(ctaCont!= '865') %>% 
    mutate(TIPO_CREDITO = ifelse(TIPO_CREDITO=='P9','P1',TIPO_CREDITO)) %>% 
    anti_join(ctas, by = c('TIPO_CREDITO', 'subCtaCont')) %>%
    select(CTACLIENTE, OPERACION, TIPO_CREDITO, subCtaCont, ESTADO)
  
  baseD <- bdc %>% 
    dplyr::filter((ESTADO %in% c("VIGENTE","SUSPENSO") & !ctaCont %in% c('131','135'))
                  | (ESTADO=="OP VENCIDA" & !ctaCont %in% c('133','136'))
                  | (ESTADO=="JUDICIAL" & !ctaCont %in% c('134','137'))) %>% 
    select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, ESTADO, RUBRO, ctaCont, DIASMORA,
           FULT_PAGO, FVEN_ULTPAGO, FVEN_PROXPAGO,)
  #Inconsistencia encontrada
  #TIPO_CREDITO P9 no existe en el Manual de Cuentas CIC.
  
  baseE <- bdc %>% 
    left_join(SectorEcon, by = c("CAEDEC_DEST"="CodActividadEconomica")) %>% 
    rename(GrupoActEconDes=GrupoActEcon, ActEconDes=ActEcon, SectorDestino=Sector) %>% 
    left_join(select(SectorEcon,-SectorProd), by = c("CIU"="CodActividadEconomica")) %>% 
    rename(GrupoActEconOri=GrupoActEcon, ActEconOri=ActEcon, SectorOrigen=Sector) %>% 
    mutate(across(c(SectorDestino, SectorOrigen), ~ifelse(is.na(.x),'Otros', .x))) %>% 
    # left_join(cicJoin, by=c("CTACLIENTE", "OPERACION")) %>% 
    mutate(ProdRNSF = ifelse(tipoCred %in% c('Micro','PyMe') 
                             & GrupoActEconDes %in% c('A','B','C','D','E','F','G')
                             & (MONEDA==0 | (MONEDA!=0 & fdes<=as.Date("2013-12-23"))), 'S', 'N')) %>% #PARA PROD GRUPOS A-G
    mutate(ProdRNSF = ifelse(tipoCred %in% c('Micro','PyMe') & SectorProd == 'TURISMO' 
                             & (MONEDA==0 | (MONEDA!=0 & fdes<=as.Date("2013-12-23"))) & TASAACT<=11.5
                             & str_detect(DESC_OBJCRED, "INVERSION"), 'S', ProdRNSF)) %>% #SECTOR TURISMO
    mutate(ProdRNSF = ifelse(tipoCred %in% c('Micro','PyMe') & SectorProd == 'PRODUCCION INTELECTUAL' 
                             & (MONEDA==0 | (MONEDA!=0 & fdes<=as.Date("2013-12-23"))), 'S', ProdRNSF)) %>% #PRODUCCION INTELECTUAL
    mutate(ProdRNSF = ifelse(tipoCred %in% c('Micro','PyMe') & SectorProd == 'H & E' 
                             & (MONEDA==0 | (MONEDA!=0 & fdes<=as.Date("2013-12-23"))), 'S', ProdRNSF)) %>%  #ELÉCTRICOS E HÍBRIDOS
    mutate(SECTOR_CARTERA_RNSF = case_when(TIPO_CREDITO %in% c('H0','H1','H2') ~ '9.Vivienda No controlada',
                                        TIPO_CREDITO %in% c('H3','H4') ~ '6.Vivienda Controlada',
                                        tipoCred == "Consumo" ~ '12.Consumo',
                                        ProdRNSF=='S' & SectorProd=='PRODUCCION INTELECTUAL' ~ '4.C3.Prod Intelectual',
                                        ProdRNSF=='S' & SectorProd=='H & E' ~ '5.C4.Fab,Ens.,Vent.MaqAutHib',
                                        ProdRNSF=='S' & SectorProd=='TURISMO' ~ '3.C2.Sector Turismo',
                                        GrupoActEconDes %in% c('A','B') & fdes>as.Date("2014-07-09") ~ '1.Prod. Agropec. Controlada',
                                        GrupoActEconDes %in% c('A','B') & fdes<=as.Date("2014-07-09") 
                                        & TASAACT<=11.5 ~ '1.Prod. Agropec. Controlada',
                                        GrupoActEconDes %in% c('A','B') & fdes<=as.Date("2014-07-09") 
                                        & TASAACT>11.5 ~ '7.Prod.Agropec.No Controlada',
                                        ProdRNSF=='S' & fdes>as.Date("2014-07-09") ~ '2.Otra prod. Controlada',
                                        ProdRNSF=='S' & fdes<=as.Date("2014-07-09")
                                        & TASAACT<=11.5 ~ '2.Otra prod. Controlada',
                                        ProdRNSF=='S' & fdes<=as.Date("2014-07-09")
                                        & TASAACT>11.5 ~ '8.Otra Prod.No Controlada',
                                        GrupoActEconDes == 'H' ~ '10.Comercio',
                                        TRUE ~ '11.Servicios')) %>% 
    select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, TASAACT, FDESEMBOLSO, MONEDA,
           CAEDEC_DEST, DESC_OBJCRED, TAM_ACTIV, grupoCaedecD, TIPO_CREDITO, SECTOR_CARTERA, 
           SectorProd, ProdRNSF, SECTOR_CARTERA_RNSF) %>% 
    mutate(CredProd = ifelse(SECTOR_CARTERA %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada',
                                                    '3.C2.Sector Turismo','4.C3.Prod Intelectual',
                                                    '5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                                    '8.Otra Prod.No Controlada'),'S','N')) %>% 
    mutate(CredProdRNSF = ifelse(SECTOR_CARTERA_RNSF %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada',
                                                      '3.C2.Sector Turismo','4.C3.Prod Intelectual',
                                                      '5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
                                                      '8.Otra Prod.No Controlada'),'S','N')) %>% 
    dplyr::filter(CredProd != CredProdRNSF)
  
  # baseE <- bdc %>% 
  #   mutate(CAEDEC_DEST = as.integer(CAEDEC_DEST)) %>% 
  #   left_join(codProd,by=c("CAEDEC_DEST"="CAEDEC")) %>% 
  #   # left_join(cicJoin, by=c("CTACLIENTE", "OPERACION")) %>% 
  #   mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD')) %>% 
  #   mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') 
  #                              & grupoCaedecD %in% c('A','B','C','D','E','F','G'), 'S', 'N')) %>% #PARA PROD GRUPOS A-G
  #   mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'TURISMO' 
  #                              & (MONEDA==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23")))
  #                              & str_detect(DESC_OBJCRED, "INVERSION"), 'S', PROD_NORMA)) %>% #SECTOR TURISMO
  #   mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'PRODUCCION INTELECTUAL' 
  #                              & (MONEDA ==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>% #PRODUCCION INTELECTUAL
  #   mutate(PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'H & E' 
  #                              & (MONEDA ==0 | (MONEDA==101 & FDESEMBOLSO<=as.Date("2013-12-23"))), 'S', PROD_NORMA)) %>% #ELÉCTRICOS E HÍBRIDOS
  #   select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, TASAACT, FDESEMBOLSO, SECTOR_CARTERA, MONEDA,
  #          CAEDEC_DEST, DESC_OBJCRED, grupoCaedecD, TIPO_CREDITO, Sector, 
  #          FECHA_VIG_NORMA = FechaVigencia, PROD_NORMA, SEC_PROD_NORMA) %>% 
  #   mutate(PROD_BASE = ifelse(SECTOR_CARTERA %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada',
  #                                                   '3.C2.Sector Turismo','4.C3.Prod Intelectual',
  #                                                   '5.C4.Fab,Ens.,Vent.MaqAutHib','7.Prod.Agropec.No Controlada',
  #                                                   '8.Otra Prod.No Controlada'),'S','N')) %>% 
  #   dplyr::filter(PROD_BASE != PROD_NORMA)
  
  baseF <- bdc %>% 
    dplyr::filter(SALDO<MONTO & (is.na(FULT_PAGO) & is.na(FVEN_ULTPAGO))) %>% 
    select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, MONTO, SALDO, FDESEMBOLSO, 
           FULT_PAGO, FVEN_ULTPAGO, FVEN_PROXPAGO, ESTADO, ctaCont)
  
  baseG <- bdc %>% 
    select(CTACLIENTE, OPERACION, CI, NDOC, NOMBRE, GENERO,
           MODULO, TIPO_OPER, esFSL, FDESEMBOLSO, ctaCont, AGENCIA, Sucursal) %>% 
    group_by(NDOC) %>%
    arrange(desc(FDESEMBOLSO)) %>%
    mutate(CI_fixed = CI[row_number()==1]) %>%
    mutate(ClienteTieneFSL = max(esFSL)) %>% 
    dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
    mutate(CuentasDistintas = n_distinct(CTACLIENTE)) %>% 
    mutate(CIDistintos = n_distinct(CI)) %>% 
    ungroup() %>% 
    mutate(across(ends_with("FSL"), ~ifelse(.x==1,'S','N'))) %>% 
    arrange(CI_fixed)
  
  baseGG <- bdc %>% group_by(CI) %>% dplyr::filter(n_distinct(CTACLIENTE)>1)
  x <- baseG %>% anti_join(baseGG, by=c("CTACLIENTE","OPERACION"))
  baseG <- baseG %>% dplyr::filter(!NDOC %in% c(4012952))
  
  baseH <- bdc %>% 
    select(CTACLIENTE, OPERACION, MODULO, TIPO_OPER, esFSL, FDESEMBOLSO,
           FVEN_ULTPAGO, FULT_PAGO, FVEN_PROXPAGO, ctaCont, DIASMORA) %>% 
    dplyr::filter(esFSL==1) %>% 
    left_join(prorrogas, by = c("CTACLIENTE","OPERACION")) %>% 
    mutate(OBSERVACION = ifelse(!is.na(OBSERVACION) & FVEN_PROXPAGO <=as.Date("2023-09-30"), "Con atraso", OBSERVACION))
  # write.xlsx(baseG, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/ValidacionBDCBantotal/output/Validacion_',myrds[i],'_CI.xlsx'))  

  exp <- data.frame(Inconsistencias=c("Inconsistencia0","Inconsistencia1",
                                      "InconsistenciaA","InconsistenciaB","InconsistenciaC",
                                      "InconsistenciaD","InconsistenciaE","InconsistenciaF",
                                      "InconsistenciaG", "InconsistenciaH"),
                    Descripción=c("Campos necesarios con valor vacío",
                                  "Codigos de Actividad Economica incoherentes",
                                  "Inconsistencia: Fecha de próximo pago es menor a la fecha de último pago",
                                  "Por tipo de crédito los días de mora son diferentes a los días de incumplimiento considerados en la base",
                                  "Subcuentas incorrectas por tipo de crédito",
                                  "Inconsistencias entre estado y cuenta contable",
                                  "Revisión de clasificación de cartera al Sector Productivo",
                                  "Créditos activos con saldo < monto y campos vacíos en FVEN_ULTPAGO y FULT_PAGO",
                                  "Clientes identificados por no. raíz de CI con más de un valor en CTACLIENTE",
                                  "Control de días mora de créditos migrados"))
  
  #   res<-data.frame(Resumen="Tomando como referencia el informe de riesgo crediticio, referente a la
  # Evaluación de la base de cartera de créditos al 30 de noviembre de 2022, se realizó la validación de
  # cartera para cada mes desde enero 2022 a febrero del 2023, desde los incisos a-d. La validación a partir del inciso e no se
  #                   pudo realizar debido a que la base de cartera de Bantotal no cuenta con información referente a garantía real,
  #                   si el crédito es productivo o no y fecha de ingreso a estado")
  
  objeto <- list(Inconsistencia0 = base0, Inconsistencia1 = base1,
                 InconsistenciaA=baseA, InconsistenciaB=baseB, InconsistenciaC=baseC, 
                 InconsistenciaD=baseD, InconsistenciaE=baseE, InconsistenciaF=baseF,
                 InconsistenciaG=baseG, InconsistenciaH=baseH,
                 Diccionario=exp)
  write.xlsx(objeto, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/ValidacionBDCBantotal/output/Validacion_',myrds[i],'.xlsx'))
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
bdc <- read_rds('C:/!bso/girCartera/rds/ec_Nov2022.rds') %>%
  dplyr::filter(MODULO!=131) %>% 
  dplyr::filter(ESTADO!='CASTIGADA') %>% 
  mutate(subCtaCont=as.double(substr(RUBRO,1,5))) %>% 
  select(CTACLIENTE, OPERACION,TIPO_CREDITO, subCtaCont) %>% 
  dplyr::filter(OPERACION == 3314134)

####____CUADRE CON EEFF____####
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
####____LOOP FOR CUADRE____########
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
#Read using read_xlsx to get Dates correctly
cuadreFinal <- read_xlsx('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/validacionBDCBantotal/output/Bantotal_CuadreEEFF.xlsx')
myrds <- "Jul2023"
bdc <- read_rds(paste0('D:/!bso/girCartera/rds/ec_',myrds,'.rds'))
ceeff <- bdc %>% 
  cuadre()

cuadreFinal <- cuadreFinal %>% 
  bind_rows(ceeff)
write.xlsx(cuadreFinal, '//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/validacionBDCBantotal/output/Bantotal_CuadreEEFF.xlsx')

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
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Sep2023.rds")
bdcSinFSL <- bdc %>% 
  dplyr::filter(!(MODULO==118 | str_detect(TIPO_OPER,"MIGR"))) %>%
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(par0Total = sum(par0)/sum(saldous)) %>% 
  group_by(Sucursal) %>% 
  summarise(Saldo = sum(saldous), SaldoMora = sum(saldoMora)/sum(saldous),
            Mora0=sum(par0)/sum(saldous),par0Tot = max(par0Total),
            Clientes=n_distinct(CTACLIENTE),CIs = n_distinct(CI)) %>%
  adorn_totals(c("row"))

bdcFSL <- bdc %>% 
  dplyr::filter((MODULO==118 | str_detect(TIPO_OPER,"MIGR"))) %>%
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(par0Total = sum(par0)/sum(saldous)) %>% 
  group_by(Sucursal) %>% 
  summarise(Saldo = sum(saldous), SaldoMora = sum(saldoMora)/sum(saldous),
            Mora0=sum(par0)/sum(saldous),par0Tot = max(par0Total),
            Clientes=n_distinct(CTACLIENTE),CIs = n_distinct(CI)) %>%
  adorn_totals(c("row"))

bdcAsesores <- bdc %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA, "Norm|Móvil")) %>% 
  group_by(Sucursal) %>% 
  summarise(Asesores = n_distinct(NOMBRE_ASESOR)) %>% 
  adorn_totals("row")

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
library(janitor)
library(arrow)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____CARGA DE FUNCIONES____####
####____CIC FIELD CODES____####
codsheets <- excel_sheets("D:/!bso/bases/excel/codCIC.xlsx")
CICcodes <- list()
for (i in 1:length(codsheets)) {
  CICcodes[[i]] <- readxl::read_excel("D:/!bso/bases/excel/codCIC.xlsx",sheet = codsheets[i]) 
}
names(CICcodes) <- codsheets
as.vector(CICcodes$RPT004)
  
treatment <- function(x, m="Ene", y="2015"){
  x %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),~dmy(.x))) %>% 
    mutate(monDate = as.yearmon(paste0(m,'. ',y))) %>% 
    mutate(FechaCorte = as.Date(monDate, frac=1)) %>% 
    # mutate(fdes = FDESEMBOLSO) %>% 
    mutate(cosechaY = year(FDESEMBOLSO)) %>%
    mutate(cosechaM = as.yearmon(FDESEMBOLSO)) %>%
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    mutate(intus = saldous * TASAACT/100) %>% 
    # mutate(across(c(SectorDestino, SectorOrigen) = ifelse(is.na(.x),'Otros', .x))) %>% 
    # mutate(DivEconOrigen = case_when(GrupoActEconOri %in% c('A','B') ~ 'Agropecuario',
    #                                  GrupoActEconOri %in% c('C','D','F') ~ 'Industria',
    #                                  GrupoActEconOri %in% c('E') ~ 'Manufactura',
    #                                  GrupoActEconOri %in% c('G') ~ 'Construcción',
    #                                  GrupoActEconOri %in% c('I') ~ 'Hoteles y Restaurantes',
    #                                  GrupoActEconOri %in% c('J') ~ 'Transporte',
    #                                  GrupoActEconOri %in% c('K','L','M','N','O','P','Q','Z') ~ 'Servicios',
    #                                  GrupoActEconOri %in% c('H') & str_extract(CAEDEC_DEST,1,2) %in% c('50','51') ~ 'Ventas por mayor',
    #                                  GrupoActEconOri %in% c('H') & str_extract(CAEDEC_DEST,1,2) %in% c('52') ~ 'Ventas por menor',
    #                                  TRUE ~ 'Otros')) %>% 
    # mutate(DivEconDestino = case_when(GrupoActEconDes %in% c('A','B') ~ 'Agropecuario',
    #                                  GrupoActEconDes %in% c('C','D','F') ~ 'Industria',
    #                                  GrupoActEconDes %in% c('E') ~ 'Manufactura',
    #                                  GrupoActEconDes %in% c('G') ~ 'Construcción',
    #                                  GrupoActEconDes %in% c('I') ~ 'Hoteles y Restaurantes',
    #                                  GrupoActEconDes %in% c('J') ~ 'Transporte',
    #                                  GrupoActEconDes %in% c('K','L','M','N','O','P','Q','Z') ~ 'Servicios',
    #                                  GrupoActEconDes %in% c('H') & str_extract(CIU,1,2) %in% c('50','51') ~ 'Ventas por mayor',
    #                                  GrupoActEconDes %in% c('H') & str_extract(CIU,1,2) %in% c('52') ~ 'Ventas por menor',
    #                                  TRUE ~ 'Otros')) %>% 
    mutate(ctaCont = substr(RUBRO,1,3)) %>% 
    mutate(subCtaCont = substr(RUBRO,1,5)) %>%
    mutate(MontoDes = ifelse(monDate == cosechaM, montous, 0)) %>% 
    mutate(SaldoTotal = ifelse(ctaCont != '865', saldous, 0)) %>% 
    mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
    mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
    mutate(SaldoVig = ifelse(ctaCont %in% c('131','135'), saldous, 0)) %>% 
    mutate(SaldoRepro = ifelse(ctaCont %in% c('135','136','137'), saldous, 0)) %>% 
    mutate(SaldoRefin = ifelse(OPERACION_ORI_REF!=0, saldous, 0)) %>% 
    mutate(SaldoCast = ifelse(ctaCont %in% '865', saldous, 0)) %>%
    mutate(SaldoPar0 = ifelse(DIASMORA>0, saldous, 0)) %>% 
    mutate(SaldoPar30 = ifelse(DIASMORA>30, saldous, 0)) %>% 
    mutate(OpsTotal = ifelse(ctaCont != '865', 1, 0)) %>% 
    mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
    mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
    mutate(OpsVig = ifelse(ctaCont %in% c('131','135'), 1, 0)) %>% 
    mutate(OpsRepro = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
    mutate(OpsRefin = ifelse(OPERACION_ORI_REF!=0, 1, 0)) %>% 
    mutate(OpsCast = ifelse(ctaCont == '865', 1, 0)) %>% 
    mutate(OpsPar0 = ifelse(SaldoPar0>0, 1, 0)) %>% 
    mutate(OpsPar30 = ifelse(SaldoPar30>0, 1, 0)) %>% 
    mutate(OpsDes = ifelse(cosechaM==monDate, 1, 0)) %>% 
    # mutate(RangoS20k = ifelse(saldous >= 20000, '>= 20k', '< 20k')) %>%
    mutate(RangoM20k = ifelse(montous >= 20000, '>= 20k', '< 20k')) %>%
    mutate(RangoMonto = cut(montous,breaks=c(0,500,1000,5000,10000,15000,20000,Inf), include.lowest=T,
                        labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
    mutate(RangoSaldo = cut(saldous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                        labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
    mutate(TipoCredito = case_match(substr(TIPO_CREDITO,1,1),'M'~'Micro','H'~'Vivienda','N'~'Consumo','P'~'Pyme')) %>% 
    # mutate(TipoCredito = cases(substr(TIPO_CREDITO,1,1), c('M','H','N','P'), c('Micro','Vivienda','Consumo','PyMe'))) %>% 
    # mutate(TipoCartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
    #                                 ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
    #                                 OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
    mutate(ObjetoCredito = case_when(OBJETO_CRED %in% c(1,3) ~ 'OPERACION',
                                     OBJETO_CRED %in% c(2,4) ~ 'INVERSION',
                                     OBJETO_CRED %in% c(9) ~ 'COMPRA BIENES MUEBLES',
                                     OBJETO_CRED %in% c(10) ~ 'LIBRE DISPONIBILIDAD',
                                     OBJETO_CRED %in% c(5,6,50,55,60,90,95) ~ 'COMPRA/ADQ/CONTRATO VIVIENDA',
                                     OBJETO_CRED %in% c(7,8,70,75,80,85) ~ 'CONST/REPAR VIVIENDA',
                                     TRUE ~ 'OTROS')) %>% 
    mutate(PlazoAnios = case_when(floor(PLAZODIAS/365) <= 2 ~ '2 o menos', 
                                  floor(PLAZODIAS/365) < 7 ~ as.character(floor(PLAZODIAS/365)),
                                  TRUE ~ '7 o más')) %>% 
    mutate(Sucursal = cut(AGENCIA, breaks = c(100,200,250,300,400,500,600,700,800,900,1000),
                          labels= c('Chuquisaca','La Paz','El Alto','Cochabamba','Oruro',
                                    'Potosí','Tarija','Santa Cruz','Beni','Pando'), right=FALSE)) %>% 
    mutate(Regional = cut(AGENCIA, breaks = c(100,200,250,300,400,500,600,700,800,900,1000),
                          labels= c('Sur','Occidente','El Alto','Centro','Occidente',
                                    'Sur','Centro','Oriente','Oriente','El Alto'), right=FALSE)) %>% 
    mutate(across(c(RangoSaldo, RangoMonto, Sucursal, Regional), ~as.character(.x)))
}

productivo <- function(x){
  x %>% 
    mutate(ProdRNSF = ifelse(substr(TIPO_CREDITO,1,1) %in% c('M','P') 
                             & GrupoActEconDes %in% c('A','B','C','D','E','F','G')
                             & (MONEDA==0 | (MONEDA!=0 & FDESEMBOLSO<=as.Date("2013-12-23"))), 1, 2)) %>% #PARA PROD GRUPOS A-G
    mutate(ProdRNSF = ifelse(substr(TIPO_CREDITO,1,1) %in% c('M','P') & SectorProd == 'TURISMO' 
                             & (MONEDA==0 ) & TASAACT<=11.5
                             & str_detect(DESC_OBJCRED, "INVERSION"), 1, ProdRNSF)) %>% #SECTOR TURISMO
    mutate(ProdRNSF = ifelse(substr(TIPO_CREDITO,1,1) %in% c('M','P') & SectorProd == 'PRODUCCION INTELECTUAL' 
                             & (MONEDA==0), 1, ProdRNSF)) %>% #PRODUCCION INTELECTUAL
    mutate(ProdRNSF = ifelse(substr(TIPO_CREDITO,1,1) %in% c('M','P') & SectorProd == 'H & E' 
                             & (MONEDA==0), 1, ProdRNSF))
}#| (MONEDA!=0 & FDESEMBOLSO<=as.Date("2013-12-23"))
sector_cartera <- function(x){
  x %>% 
    mutate(SECTOR_CARTERA = case_when(TIPO_CREDITO %in% c('H0','H1','H2') ~ '9.Vivienda No controlada',
                                      TIPO_CREDITO %in% c('H3','H4') ~ '6.Vivienda Controlada',
                                      substr(TIPO_CREDITO,1,1) == "N" ~ '12.Consumo',
                                      ProdRNSF=='S' & SectorProd=='PRODUCCION INTELECTUAL' ~ '4.C3.Prod Intelectual',
                                      ProdRNSF=='S' & SectorProd=='H & E' ~ '5.C4.Fab,Ens.,Vent.MaqAutHib',
                                      ProdRNSF=='S' & SectorProd=='TURISMO' ~ '3.C2.Sector Turismo',
                                      GrupoActEconDes %in% c('A','B') & FDESEMBOLSO>as.Date("2014-07-09") ~ '1.Prod. Agropec. Controlada',
                                      GrupoActEconDes %in% c('A','B') & FDESEMBOLSO<=as.Date("2014-07-09") ~ '7.Prod.Agropec.No Controlada',
                                      ProdRNSF=='S' & FDESEMBOLSO>as.Date("2014-07-09") ~ '2.Otra prod. Controlada',
                                      ProdRNSF=='S' & FDESEMBOLSO<=as.Date("2014-07-09") ~ '8.Otra Prod.No Controlada',
                                      GrupoActEconDes == 'H' ~ '10.Comercio',
                                      TRUE ~ '11.Servicios'))
  # GrupoActEconDes %in% c('A','B') & FDESEMBOLSO<=as.Date("2014-07-09") 
  # & TASAACT<=11.5 ~ '1.Prod. Agropec. Controlada',
  # GrupoActEconDes %in% c('A','B') & FDESEMBOLSO<=as.Date("2014-07-09") 
  # & TASAACT>11.5 ~ '7.Prod.Agropec.No Controlada',
  # ProdRNSF=='S' & FDESEMBOLSO<=as.Date("2014-07-09") & TAM_ACTIV == "Micro"
  # & TASAACT>11.5 ~ '8.Otra Prod.No Controlada',
  # ProdRNSF=='S' & FDESEMBOLSO<=as.Date("2014-07-09") & TAM_ACTIV == "Micro"
  # & TASAACT<=11.5 ~ '2.Otra prod. Controlada',
  # ProdRNSF=='S' & FDESEMBOLSO<=as.Date("2014-07-09") & TAM_ACTIV == "Pequeña"
  # & TASAACT>7 ~ '8.Otra Prod.No Controlada',
  # ProdRNSF=='S' & FDESEMBOLSO<=as.Date("2014-07-09") & TAM_ACTIV == "Pequeña"
  # & TASAACT<=7 ~ '2.Otra prod. Controlada',
  # ProdRNSF=='S' & FDESEMBOLSO<=as.Date("2014-07-09") & TAM_ACTIV %in% c("Mediana", "Grande")
  # & TASAACT>6 ~ '8.Otra Prod.No Controlada',
  # ProdRNSF=='S' & FDESEMBOLSO<=as.Date("2014-07-09") & TAM_ACTIV %in% c("Mediana", "Grande")
  # & TASAACT<=6 ~ '2.Otra prod. Controlada',
}
categ <- function(x) {
  x %>% 
    mutate(ControlCartera = case_when(SECTOR_CARTERA %in% c('1.Prod. Agropec. Controlada','2.Otra prod. Controlada','3.C2.Sector Turismo',
                                                   '4.C3.Prod Intelectual','5.C4.Fab,Ens.,Vent.MaqAutHib') ~ 'productivoTC',
                             SECTOR_CARTERA %in% c('6.Vivienda Controlada') ~ 'viviendaTC',
                             SECTOR_CARTERA %in% c('7.Prod.Agropec.No Controlada','8.Otra Prod.No Controlada') ~ 'productivoTNC',
                             SECTOR_CARTERA %in% c('9.Vivienda No controlada') ~ 'Otros',
                             TRUE ~ 'Otros')) 
}

test <- test %>% 
  rename(TipoCredito=tipoCred) %>% 
  left_join(SectorEcon, by = c("CAEDEC_DEST"="CodActividadEconomica")) %>% 
  rename(GrupoActEconDes=GrupoActEcon, SectorDestino=Sector, DivEconDestino = DivEcon) %>% 
  left_join(select(SectorEcon,-SectorProd), by = c("CIU"="CodActividadEconomica")) %>% 
  rename(GrupoActEconOri=GrupoActEcon, SectorOrigen=Sector, DivEconOrigen = DivEcon) %>% 
  mutate(across(c(SectorDestino, SectorOrigen, DivEconDestino, DivEconOrigen), ~ifelse(is.na(.x),'Otros', .x)))

testProd <- test %>% 
  rename(SectorOld=SECTOR_CARTERA) %>% 
  productivo() %>% 
  sector_cartera()

table(testProd$SECTOR_CARTERA, testProd$SectorOld)

testin <- testProd %>% 
  dplyr::filter(!ctaCont%in% c("865","623")) %>% 
  dplyr::filter(SECTOR_CARTERA!=SectorOld) %>% 
  # dplyr::filter(!ctaCont %in% c('623','865')) %>%
  select(SECTOR_CARTERA, SectorOld, fdes, TipoCredito, TIPO_CREDITO, CAEDEC_DEST, grupoCaedecD, DESC_OBJCRED, TASAACT, TIPOTASA, TAM_ACTIV, MONEDA, ctaCont, SectorProd)
tP <- testProd %>% 
  dplyr::filter(!ctaCont %in% c('623','865')) %>% 
  select(SECTOR_CARTERA, SectorOld, fdes, tipoCred, TIPO_CREDITO, CAEDEC_DEST, grupoCaedecD, GrupoActEconDes,
         DESC_OBJCRED, TASAACT, TIPOTASA, TAM_ACTIV, MONEDA, ctaCont,ProdRNSF)

####____DIFERIDO____####
diferida <- function(x){
  x %>%
    mutate(rubDif = substr(RUBRO_CAPITAL_DIFERIDO,1,3)) %>% 
    mutate(across(c(SALDO_CAPITAL_DIFERIDO, SALDO_INT_CAPITAL_DIFERIDO),~ifelse(is.na(.x), 0, .x))) %>% 
    mutate(SaldoDifRC = ifelse((SALDO_CAPITAL_DIFERIDO + SALDO_INT_CAPITAL_DIFERIDO) > 0,
                                  saldous, 0)) %>%
    mutate(SaldoDif = case_when(ctaCont %in% c('131','133','134','135','136','137') & MONEDA == 0 ~ as.numeric(SALDO_CAPITAL_DIFERIDO)/6.86,
                                ctaCont %in% c('131','133','134','135','136','137') & MONEDA != 0 ~ as.numeric(SALDO_CAPITAL_DIFERIDO),
                                TRUE ~ 0)) %>% #Hacer con ctaCont en lugar de rubDif porque hay rubDif 0 que tiene saldo diferido
    mutate(OpsDif = ifelse(SaldoDif>0, 1, 0)) %>% 
    mutate(OpsDifRC = ifelse(SaldoDifRC>0, 1, 0)) 
}
####____READING ADDITIONAL DATABASES____####
codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx", sheet='Old')
SectorEcon <- read_excel("D:/!bso/bases/excel/SectorEcon.xlsx", sheet="Sheet1")
####____2022-2023____###########################################################
#2022 y 2023 funcionan con fread encoding Latin-1
mes <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
years <- c('2022','2023')
i <- 10
k <- 2
test <- readRDS("D:/!bso/girCartera/rds/ec_Oct2023.rds")

for(i in 1:length(mes)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mes[i],years[k]))
      bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                          mes[i], years[k],'.txt'), 
                   encoding = 'Latin-1', fill = T) %>%
        remove_empty("cols") %>% 
        select(-any_of("PRODUCTIVO")) %>% 
        dplyr::filter(MODULO != 131) %>% 
        mutate(across(c(CAEDEC_DEST, CIU, RUBRO), ~as.character(.x))) %>% 
        mutate(across(c(CAEDEC_DEST, CIU), ~str_pad(.x, width=5, side = "left", pad=0))) %>% 
        left_join(SectorEcon, by = c("CAEDEC_DEST"="CodActividadEconomica")) %>% 
        rename(GrupoActEconDes=GrupoActEcon, SectorDestino=Sector, DivEconDestino = DivEcon) %>% 
        left_join(select(SectorEcon,-SectorProd), by = c("CIU"="CodActividadEconomica")) %>% 
        rename(GrupoActEconOri=GrupoActEcon, SectorOrigen=Sector, DivEconOrigen = DivEcon) %>% 
        mutate(across(c(SectorDestino, SectorOrigen, DivEconDestino, DivEconOrigen), ~ifelse(is.na(.x),'Otros', .x))) %>% 
        # mutate(CALIFICACION_old = CALIFICACION) %>% 
        # mutate(DIASMORA_old = DIASMORA) %>%
        # mutate(DIASMORA = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 0, DIASMORA)) %>%
        # mutate(CALIFICACION = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 'A', CALIFICACION)) %>%
        left_join(select(codAge,-Sucursal,-Regional), by="AGENCIA") %>%
        treatment(m=mes[i],y=years[k]) %>% 
        productivo() %>% 
        # sector_cartera() %>% 
        categ() %>% 
        diferida() %>%
        ungroup()
      
      write_parquet(bdc, paste0('D:/!bso/girCartera/rds/bdc_', mes[i], years[k], '.parquet'), compression = "GZIP")
      saveRDS(bdc, paste0('D:/!bso/girCartera/rds/ec_',
                          mes[i], years[k], '.rds'))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

####____EFECTIVIDAD DE CORRECCION DE FECHA DE INICIO____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Dic2022.rds")
cic <- readRDS("D:/!bso/CIC/rds/cic_Dic2022.rds")
Clientes <- readRDS("D:/!bso/features/Clientes_Ene2015Ago2023.rds")

bdcJoin <- select(bdc) %>% 
  left_join()
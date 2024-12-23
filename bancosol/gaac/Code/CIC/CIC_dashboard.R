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
# library(openxlsx)
library(scales)
library(janitor)
library(data.table)
library(arrow)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

NewCols <- function(x, vars){
  add < vars[! vars %in% names(x)]
}

####____CHUNKS DE PRUEBA____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Abr2015.rds")

write_parquet(bdc,sink="D:/!bso/girCartera/rds/ec_Abr2015.parquet",compression = "GZIP")

bdcP <- read_parquet("D:/!bso/girCartera/rds/ec_Abr2015.parquet")
glimpse(bdc)
glimpse(bdcP)

v <- sapply(bdc, function(x){class(x)})
w <- sapply(bdcP, function(x){class(x)})
v==w

rdsTList <- list()
parquetTList <- list()
for (i in 1:50) {
  print(i)
  rds <- system.time(readRDS("D:/!bso/girCartera/rds/ec_Abr2015.rds"))
  par <- system.time(read_parquet("D:/!bso/girCartera/rds/ec_Abr2015.parquet"))
  
  rdsTList[[i]] <- rds
  parquetTList[[i]] <- par
}

rdsFull <- bind_rows(rdsTList)
parFull <- bind_rows(parquetTList)
glimpse(rdsFull)
glimpse(parFull)
summary(rdsFull)
summary(parFull)

####____TABLA CIC____####
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
SectorEcon <- read_excel("D:/!bso/bases/excel/SectorEcon.xlsx", sheet="Sheet1") %>% 
  select(CodActividadEconomica, DivEcon)
ventasMayor <- SectorEcon$CodActividadEconomica[SectorEcon$Sector=="Venta al por mayor"]
ventasMenor <- SectorEcon$CodActividadEconomica[SectorEcon$Sector=="Venta al por menor"]

cicList <- list()
year <- c(2018:2023)
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
my <- as.vector(sapply(year, function(x){paste0(mes,x)}))

my <- c("Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Ene2023","Feb2023","Mar2023","Abr2023",
        "May2023","Jun2023","Jul2023","Ago2023","Sep2023")
ColsDif <- c(SaldoDiferido = 0, MontoInteresDiferido = 0, MontCapitDiferido = 0, PrevCiclica = 0)
my <- c("Oct2023")

i <- 1
for (i in 1:length(my)) {
  tryCatch({
    print(paste0(my[i]))
    bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', my[i],'.rds')) %>% 
      dplyr::filter(!ctaCont %in% c('865', '623')) %>% 
      left_join(codMod, by=c("MODULO")) %>% 
      select(CTACLIENTE, OPERACION, NOMBRE_MODULO, Sucursal, OPERACION_ORI_REF)
    
    cic <- readRDS(paste0('D:/!bso/CIC/rds/cic_', my[i],'.rds')) %>% 
      dplyr::filter(!CuentaContable.y %in% c('865', '623')) %>% 
      # dplyr::filter(!(CuentaContable.y=='865' & (is.na(SaldoCastigado) | SaldoCastigado==0))) %>% 
      mutate(across(c(CodActEconDest, CodActEconOri), ~as.character(.x))) %>% 
      mutate(across(c(CodActEconDest, CodActEconOri), ~str_pad(.x, width=5, side = "left", pad=0))) %>% 
      dplyr::filter(!is.na(CuentaContable.y)) %>% 
      left_join(bdc, by=c("CTACLIENTE","OPERACION"))
    
    cic <- cic %>% add_column(!!!ColsDif[setdiff(names(ColsDif), names(cic))])  
    
    cicProc <- cic %>%
      mutate(SaldoRefin = ifelse(!is.na(OPERACION_ORI_REF) & OPERACION_ORI_REF!=0, SaldoBruto, 0)) %>% 
      mutate(SaldoRefinMora = ifelse(OPERACION_ORI_REF!=0 & CuentaContable.y %in% c('133','134','136','137'), SaldoBruto, 0)) %>% 
      mutate(SaldoRefinPar0 = ifelse(OPERACION_ORI_REF!=0 & DiasMora>0, SaldoBruto, 0)) %>% 
      mutate(SaldoDifMora = ifelse(SaldoDiferido>0 & CuentaContable.y %in% c('133','134','136','137'), SaldoDiferido, 0)) %>% 
      mutate(SaldoDifPar0 = ifelse(SaldoDiferido>0 & DiasMora>0, SaldoDiferido, 0)) %>% 
      # mutate(SaldoTotal = SaldoBruto+SaldoContingente) %>% 
      mutate(SaldoVigente = ifelse(CuentaContable.y %in% c('131','135'), SaldoBruto, 0)) %>% 
      mutate(SaldoPar0 = ifelse(DiasMora>0, SaldoBruto, 0)) %>% 
      mutate(SaldoRepro = ifelse(CuentaContable.y %in% c('135','136','137'), SaldoBruto, 0)) %>% 
      mutate(SaldoReproVigente = ifelse(CuentaContable.y %in% c('135'), SaldoBruto, 0)) %>% 
      mutate(SaldoReproMora = ifelse(CuentaContable.y %in% c('136','137'), SaldoBruto, 0)) %>% 
      mutate(SaldoReproPar0 = ifelse(DiasMora>0 & CuentaContable.y %in% c('135','136','137'), SaldoBruto, 0)) %>% 
      mutate(OpsBruta = ifelse(CuentaContable.y %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
      # mutate(OpsContingente = ifelse(CuentaContable.y %in% c('623'), 1, 0)) %>% 
      mutate(OpsVigente = ifelse(CuentaContable.y %in% c('131','135'), 1, 0)) %>% 
      mutate(OpsMora = ifelse(CuentaContable.y %in% c('133','134','136','137'), 1, 0)) %>% 
      mutate(OpsPar0 = ifelse(OpsBruta==1 & DiasMora>0, 1, 0)) %>% 
      mutate(OpsRepro = ifelse(CuentaContable.y %in% c('135','136','137'), 1, 0)) %>% 
      mutate(OpsReproVigente = ifelse(CuentaContable.y %in% c('135'), 1, 0)) %>% 
      mutate(OpsReproMora = ifelse(CuentaContable.y %in% c('136','137'), 1, 0)) %>% 
      mutate(OpsReproPar0 = ifelse(OpsRepro==1 & DiasMora>0, 1, 0)) %>%
      mutate(OpsRefin = ifelse(!is.na(OPERACION_ORI_REF) & OPERACION_ORI_REF!=0, 1, 0)) %>% 
      mutate(OpsRefinMora = ifelse(OpsRefin==1 & CuentaContable.y %in% c('133','134','136','137'), 1, 0)) %>% 
      mutate(OpsRefinPar0 = ifelse(OpsRefin==1 & DiasMora>0, 1, 0)) %>% 
      mutate(OpsDiferida = ifelse(SaldoDiferido>0, 1, 0)) %>% 
      mutate(OpsDifMora = ifelse(OpsDiferida==1 & CuentaContable.y %in% c('133','134','136','137'), 1, 0)) %>% 
      mutate(OpsDifPar0 = ifelse(OpsDiferida==1 & DiasMora>0, 1, 0)) %>% 
      # mutate(OpsCastigada = ifelse(CuentaContable.y=='865', 1, 0)) %>% 
      mutate(Objeto = case_when(ObjetoCredito %in% c('CAP.INVERSION')~"INVERSION",
                                ObjetoCredito %in% c('CAP.OPERACION')~"OPERACION",
                                ObjetoCredito %in% c('LIBRE DISPO.')~"LIBRE DISPONIBILIDAD",
                                ObjetoCredito %in% c('CONST.VIVIENDA','CONST.VIV.CASA','CONST.VIV.DEPTO','REF.REM.AM.MEJ')~"CONST/REPAR VIVIENDA",
                                ObjetoCredito %in% c('COMPRA BIENES')~"COMPRA BIENES MUEBLES",
                                str_detect(ObjetoCredito, "COMP|CONT|ADQ") ~ "COMPRA/ADQ/CONTRATO VIVIENDA",
                                TRUE~"OTROS")) %>% 
      mutate(TipoCreditoRed = case_when(substr(CodTipoCredito,1,1) =='P' ~ 'Pyme',
                                      substr(CodTipoCredito,1,1) =='M' ~ 'Micro',
                                      substr(CodTipoCredito,1,1) =='N' ~ 'Consumo',
                                      CodTipoCredito %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                      CodTipoCredito %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
      mutate(Edad = as.numeric(FechaCorte-FechaNacimiento)/365) %>%
      mutate(GrupoEdad = cut(Edad,breaks=c(0,25,30,35,40,45,50,55,Inf),
                             labels = c('25 o menos','(25-30]','(30-35]','(35-40]','(40-45]','(45-50]','(50-55]','Mayor a 55'),
                             right=T, include.lowest=T)) %>%
      mutate(GrupoEdad = as.character(GrupoEdad)) %>% 
      mutate(TimeSinceDisb = as.numeric(FechaCorte-FechaInicio)/365) %>% 
      mutate(TiempoDesdeDesembolso = case_when(TimeSinceDisb < 1 ~ 'Menos de 1 año',
                                               TimeSinceDisb < 6 ~ as.character(floor(TimeSinceDisb)),
                                               TimeSinceDisb >= 6 ~ '6 años o más',)) %>% 
      mutate(PlazoAnios = case_when(floor(CantidadDiasPlazo/365) <= 2 ~ '2 o menos', 
                                    floor(CantidadDiasPlazo/365) < 7 ~ as.character(floor(CantidadDiasPlazo/365)),
                                    TRUE ~ '7 o más')) %>% 
      mutate(RangoSaldo = case_when(SaldoBruto < 1000 ~ '< 1k USD',
                                    SaldoBruto < 5000 ~ '1k - 5k USD',
                                    SaldoBruto < 10000 ~ '5k - 10k USD',
                                    SaldoBruto < 15000 ~ '10k - 15k USD',
                                    SaldoBruto < 20000 ~ '15k - 20k USD',
                                    SaldoBruto >= 20000 ~ '>= 20k USD',)) %>% 
      mutate(RangoMonto = case_when(MontoContratado < 1000 ~ '< 1k USD',
                                    MontoContratado < 5000 ~ '1k - 5k USD',
                                    MontoContratado < 10000 ~ '5k - 10k USD',
                                    MontoContratado < 15000 ~ '10k - 15k USD',
                                    MontoContratado < 20000 ~ '15k - 20k USD',
                                    MontoContratado >= 20000 ~ '>= 20k USD',)) %>% 
      left_join(SectorEcon, by = c("CodActEconDest"="CodActividadEconomica")) %>% 
      rename(LabActEconDest = DivEcon) %>% 
      left_join(SectorEcon, by = c("CodActEconOri"="CodActividadEconomica")) %>% 
      rename(LabActEconOri = DivEcon) %>% 
      # mutate(NivelAutorizacionL = case_when(str_detect(NivelAutorizacion, coll("asesor", TRUE))~"ASESOR DE NEGOCIOS",
      #                                       str_detect(NivelAutorizacion, coll("encarg", TRUE))~"ENCARGADO DE NEGOCIOS",
      #                                       str_detect(NivelAutorizacion, fixed("SUBGERENTE", TRUE))~"SUBGERENTE REGIONAL PRODUCTIVO/ DE CREDITO",
      #                                       str_detect(NivelAutorizacion, coll("GERENTE DE AGENCIA", TRUE))~"GERENTE DE AGENCIA",
      #                                       str_detect(NivelAutorizacion, coll("GERENTE NACIONAL", TRUE))~"GERENTE NACIONAL/REGIONAL",
      #                                       str_detect(NivelAutorizacion, coll("GERENTE REGIONAL", TRUE))~"GERENTE NACIONAL/REGIONAL",
      #                                       str_detect(NivelAutorizacion, coll("Gerente", TRUE))~"GERENTE NACIONAL/REGIONAL",
      #                                       str_detect(NivelAutorizacion, coll("JEFE REGIONAL AGROPECUARIO", TRUE))~"JEFE REGIONAL AGROPECUARIO",
      #                                       str_detect(NivelAutorizacion, coll("RIESGO", TRUE))~"MIGRACION",
      #                                       TRUE ~ "ENCARGADO DE NEGOCIOS")) %>% 
      # mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
      #                                 ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
      #                                 OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
      mutate(FormaPagoRed = case_when(FormaPago =="MENSUAL" ~ "Mensual",
                                      FormaPago %in% c('QUINCENAL','BISEMANAL','SEMANAL') ~ "Intra Mensual",
                                      FormaPago %in% c('BIMESTRAL','TRIMESTRAL','SEMESTRAL','ANUAL') ~ "Inter Mensual",
                                      FormaPago %in% c('A VENCMTO.','OTROS') ~ "Otros")) %>% 
      mutate(ConGarantes = ifelse(Garantes>0, 1, 0)) %>% 
      mutate(ConCodeudores = ifelse(Codeudores>0, 1, 0)) %>%       
      select(FechaCorte, Sucursal, Genero, GrupoEdad, LabActEconOri, LabActEconDest, 
             PlazoAnios, any_of("NivelAutorizacionL"), TiempoDesdeDesembolso,
             TipoCreditoRed, Objeto, Producto = NOMBRE_MODULO, 
             RangoSaldo, RangoMonto, ConGarantes, ConCodeudores, 
             SaldoBruto, SaldoVigente, SaldoMora, SaldoPar0, SaldoRepro,
             SaldoReproMora, SaldoReproPar0, SaldoDiferido, SaldoDifMora, SaldoDifPar0, 
             SaldoRefin, SaldoRefinMora, SaldoRefinPar0, 
             PrevEspecifica, PrevEspecificaDif, PrevCiclica, Devengado, DevengadoDif,
             OpsBruta, OpsVigente, OpsMora, OpsPar0, OpsRepro, OpsReproMora, OpsReproPar0,
             OpsRefin, OpsRefinMora, OpsRefinPar0, OpsDiferida, OpsDifMora, OpsDifPar0) %>% #SaldoContingente, SaldoCastigado, OpsCastigada, OpsContingente
      group_by(FechaCorte, Sucursal, Genero, GrupoEdad, LabActEconDest, LabActEconOri, 
               PlazoAnios,  TiempoDesdeDesembolso, TipoCreditoRed, Objeto, Producto, 
               RangoSaldo, RangoMonto, ConGarantes, ConCodeudores) %>% #FormaPagoRed, PlanPagos
      summarise_all(sum) %>% 
      ungroup()
    
    cicList[[i]] <- cicProc
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

cicFull <- rbindlist(cicList)
# cicFull <- cicFull %>% 
#   mutate(RangoMonto = ifelse(is.na(RangoMonto),'>= 20k USD',RangoMonto))
cicFull <- cicFull %>% 
  mutate(Genero = ifelse(Genero=="0", "J", Genero)) %>% 
  mutate(GrupoEdad = ifelse(is.na(GrupoEdad), "No Aplica", GrupoEdad)) %>% 
  mutate(ConCodeudores = ifelse(ConCodeudores==1, "SI","NO")) %>% 
  mutate(ConGarantes = ifelse(ConGarantes==1, "SI","NO"))

write_parquet(cicFull, "D:/!bso/tablaCIC.parquet",compression = "GZIP")

cicFull <- read_parquet("D:/!bso/tablaCIC.parquet")

nrow(x %>% dplyr::filter(FechaCorte==as.Date("2018-12-31")))

####____UPDATE A SINGLE MONTH____####
cicFull <- read_parquet("D:/!bso/tablaCIC.parquet")

#Correr el loop anterior para el mes correspondiente
cicProc <- cicProc %>% 
  mutate(Genero = ifelse(Genero=="0", "J", Genero)) %>% 
  mutate(GrupoEdad = ifelse(is.na(GrupoEdad), "No Aplica", GrupoEdad)) %>% 
  mutate(ConCodeudores = ifelse(ConCodeudores==1, "SI","NO")) %>% 
  mutate(ConGarantes = ifelse(ConGarantes==1, "SI","NO"))
cicFull <- cicFull %>% 
  bind_rows(cicProc)

table(cicFull$FechaCorte)

write_parquet(cicFull, "D:/!bso/tablaCIC.parquet",compression = "GZIP")

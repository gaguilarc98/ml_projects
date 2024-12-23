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
####____SEGUIMIENTO A ORD____####
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018:2023)

monyear <- as.vector(sapply(year,function(x){paste0(month,x)}))
monyear <- monyear[-c(1:which(monyear=="Nov2018"))]

sheets <- excel_sheets("D:/!bso/shared/CosechasMuestrasORD2023.xlsx")
mexcel <- str_to_lower(str_replace(monyear,"20","-"))
i <- 4
# k <- 1
# cicFollow <- list()
# cicNew <- NULL
ordList <- list()
for (i in 1: length(sheets)){
  print(sheets[i])
  muestraORD <- read_excel("D:/!bso/shared/CosechasMuestrasORD2023.xlsx", sheet = sheets[i]) %>% 
    select(Corte, Agencia, CTACLIENTE=`Cta. Cliente`, OPERACION=`Nro. Op`, Observacion = `Observación`,
           EtapaProceso = `Etapa Proceso`, FactorAgrupador = `Factor Agrupador`)  
  ordList[[i]] <- muestraORD
}
ordFull <- rbindlist(ordList)

ordUnique <- ordFull %>% select(CTACLIENTE, OPERACION) %>% distinct_all()

cicList <- list()
for (i in 1:length(monyear)) {
  tryCatch({
    print(monyear[i])
    cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",monyear[i],".rds"))
    cicORD <- cic %>% 
      select(FechaCorte, CTACLIENTE, OPERACION, FechaIncumplimiento, CuentaContable.x, CuentaContable.y,
             FechaCancelacion, TipoCancelacion, IdOperacionOrigen) %>% 
      semi_join(ordUnique, by=c("CTACLIENTE", "OPERACION")) 
    
    cicList[[i]] <- cicORD
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

cicFull <- rbindlist(cicList)
saveRDS(cicFull, "D:/!bso/CIC/Seguimineto_ORD.rds")

####____PARA CADA CORTE DE ORD____####
i <- 1
histList <- list()
for (i in 1: length(sheets)){
  print(sheets[i])
  muestraORD <- read_excel("D:/!bso/shared/CosechasMuestrasORD2023.xlsx", sheet = sheets[i]) %>% 
    select(Corte, Agencia, CTACLIENTE=`Cta. Cliente`, OPERACION=`Nro. Op`, Observacion = `Observación`,
           EtapaProceso = `Etapa Proceso`, FactorAgrupador = `Factor Agrupador`)  
  cicHist <- muestraORD %>% 
    inner_join(cicFull, by = c("CTACLIENTE", "OPERACION")) %>% 
    dplyr::filter(FechaCorte >= Corte) %>% 
    select(-CuentaContable.y, -IdOperacionOrigen) %>% 
    dplyr::rename(CuentaContable = CuentaContable.x) %>% 
    pivot_wider(names_from = FechaCorte, values_from = c("FechaIncumplimiento","CuentaContable","FechaCancelacion", "TipoCancelacion"))
  
  ordList[[i]] <- cicHist
}

names(ordList) <- sheets
write_xlsx(ordList, "D:/!bso/requests/HistoriaMuestrasORD.xlsx")
####____OLD APPROACH____####  
for (i in 1:length(monyear)) {
  tryCatch({
    print(monyear[i])
    cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",monyear[i],".rds"))
    cicORD <- cic %>% 
      select(CTACLIENTE, OPERACION, FechaIncumplimiento, CuentaContable.x, CuentaContable.y,
             FechaCancelacion, TipoCancelacion, IdOperacionOrigen)
    if(sheets[k]==mexcel[i]){
      muestraORD <- read_excel("D:/!bso/shared/CosechasMuestrasORD2023.xlsx", sheet = sheets[k]) %>% 
        select(Corte, CTACLIENTE=`Cta. Cliente`, OPERACION=`Nro. Op`) 
      cicNew <- cicORD %>% 
        inner_join(muestraORD, by=c("CTACLIENTE","OPERACION"))
      print(paste("Muestra",nrow(muestraORD),"Join",nrow(cicNew)))
      k <- k+1
    }
    if(i==1){
      cicFollow <- cicNew
    }else{
      cicTemp <- cicFollow %>% 
        semi_join(cicORD, by=c("CTACLIENTE","OPERACION"))
      cicFollow <- cicFollow %>% 
        bind_rows(cicNew) %>% 
        bind_rows(cicTemp)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

####____FOR ALL ORD IN BDC____####
for (i in 1:length(monyear)) {
  tryCatch({
    print(monyear[i])
    bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",monyear[i],".rds"))
    bdcORD <- bdc %>% 
      dplyr::filter(monDate==cosechaM) %>% 
      select(CTACLIENTE, OPERACION, ctaCont, ESTADO, FDESEMBOLSO, MODULO, 
             TIPO_OPER, monDate, cosechaM, OPERACION_ORI_REF)
    
    cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",monyear[i],".rds"))
    cicORD <- cic %>% 
      select(CTACLIENTE, OPERACION,)
    
    ORD <- bdcORD %>% 
      anti_join(cicORD, by=c("CTACLIENTE","OPERACION"))
    
    print(paste("DIFERENCIA OBS.",nrow(ORD)))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

cicORD <- cic %>% 
  mutate(cosechaM = as.yearmon(FechaInicio)) %>% 
  mutate(cosechaM2 = as.yearmon(FechaReprogramacion)) %>% 
  mutate(cosechaM3 = as.yearmon(FechaRefinanciamiento)) %>% 
  mutate(cosechaM4 = as.yearmon(FechaPrimerPagoCap)) %>% 
  dplyr::filter(monDate==cosechaM | monDate==cosechaM2 
                | monDate==cosechaM3 ) %>% 
  select(CTACLIENTE, OPERACION, FechaInicio, FechaReprogramacion,
         CuentaContable.y, monDate, cosechaM, cosechaM2,
         FechaIncumplimiento, FechaCancelacion, OpsOrigen)
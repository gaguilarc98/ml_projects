####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
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
library(forcats)    # Working with factors/categorical data
library(openxlsx)
library(scales)
library(janitor)
library(ggrepel)
remove(list = ls())
options("encoding" = "UTF-8")
options(scipen = 999)

paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.5)
####____FUNCIONES____####
#Las funciones deben ser cargadas para ejecutar el resto del código

#grupoCosecha() requiere de una bdc con: rangom, montous, cosechaY y cosechaM
#IMPORTANTE: Solo incluir las operaciones desembolsadas (o aquellas que se consideran
#como nuevo desembolso, i.e. sin reprogramaciones)
grupoCosecha <- function(x){
  group_by(cosechaY,cosechaM) %>% 
    mutate(MontoDesSA = sum(montous)) %>% 
    mutate(OpsDesSA = n()) %>% 
    group_by(cosechaY,cosechaM,rangom) %>% 
    summarise(MontoDesRank = sum(montous),OpsDesRank = n(),
              MontoDesSA = max(MontoDesSA), OpsDesSA = max(OpsDesSA)) %>% 
    ungroup()
}
####____RANGOS____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015:2023)
myrds <- as.vector(sapply(year, function(x){paste0(mes,x)}))
i <- 99
DesemSAList <- list()
DesemCAList <- list()
for (i in 1:length(myrds)) {
  tryCatch({
    print(myrds[i])
    bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
      # dplyr::filter(MODULO!=29) %>% 
      select(CTACLIENTE,OPERACION,OPERACION_ORI_REF,monDate,fdes,montous,saldous,MONTO,MONEDA,
             par0,opDes,cosechaY,cosechaM,ctaCont,ESTADO) %>% 
      mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                                montous <= 3000 ~'b. 1k-3k',
                                montous <= 5000 ~'c. 3k-5k',
                                montous <= 8000 ~'d. 5k-8k',
                                montous <= 10000 ~'e. 8k-10k',
                                montous <= 20000 ~'f. 10k-20k',
                                montous > 20000 ~'g. Mayor a 20k')) %>%
      dplyr::filter(opDes==1)
    
    DesemRank <- bdc %>% 
      grupoCosecha()
      
    DesemRankSinRep <- bdc %>% 
      dplyr::filter(!ctaCont %in% c('135','136','137')) %>% 
      grupoCosecha()
    
    DesemSAList[[i]] <- DesemRank
    DesemCAList[[i]] <- DesemRankSinRep
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

DesemSAFull <- rbindlist(DesemSAList)
DesemCAFull <- rbindlist(DesemCAList)

write_rds(DesemSAFull,"D:/!bso/cosechas/rangos/DesemSAFull.rds")
write_rds(DesemCAFull,"D:/!bso/cosechas/rangos/DesemCAFull.rds")

####____AGREGAR UN MES A DESEMBOLSOS____####
#Leer las últimas bases agregadas por rango
DesemSAFull <- readRDS("D:/!bso/cosechas/rangos/DesemSAFull.rds")
DesemCAFull <- readRDS("D:/!bso/cosechas/rangos/DesemCAFull.rds")

myrds <- "Abr2023" #Actualizar para agregar un mes
bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds,'.rds')) %>% 
  # dplyr::filter(MODULO!=29) %>% 
  select(CTACLIENTE,OPERACION,OPERACION_ORI_REF,monDate,fdes,montous,saldous,MONTO,MONEDA,
         par0,opDes,cosechaY,cosechaM,ctaCont,ESTADO) %>% 
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous <= 3000 ~'b. 1k-3k',
                            montous <= 5000 ~'c. 3k-5k',
                            montous <= 8000 ~'d. 5k-8k',
                            montous <= 10000 ~'e. 8k-10k',
                            montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k')) %>%
  dplyr::filter(opDes==1)

#Aplicar la función grupocosecha() a los desembolsos corrientes y genuinos
DesemRank <- bdc %>% 
  grupoCosecha()

DesemRankSinRep <- bdc %>% 
  dplyr::filter(!ctaCont %in% c('135','136','137')) %>% 
  grupoCosecha()

#Actualizar las bases agregadas por rango
DesemCAFull <- DesemCAFull %>% 
  bind_rows(DesemRank)

DesemSAFull <- DesemSAFull %>% 
  bind_rows(DesemRankSinRep)

write_rds(DesemSAFull,"D:/!bso/cosechas/rangos/DesemSAFull.rds")
write_rds(DesemCAFull,"D:/!bso/cosechas/rangos/DesemCAFull.rds")

####____AGRUPANDO POR AÑO DE DESEMBOLSO____####
DesemSA <- DesemSAFull %>% 
  group_by(cosechaY,rangom) %>% 
  summarise(MontoDesRank = sum(MontoDesRank),OpsDesRank = sum(OpsDesRank),
            MontoDesSA = sum(MontoDesSA), OpsDesSA = sum(OpsDesSA)) %>% 
  mutate(pctMonto = MontoDesRank/MontoDesSA*100) %>% 
  mutate(pctOps = OpsDesRank/OpsDesSA*100) %>% 
  mutate(MontoProm = MontoDesRank/OpsDesRank)

DesemCA <- DesemCAFull %>% 
  group_by(cosechaY,rangom) %>% 
  summarise(MontoDesRank = sum(MontoDesRank),OpsDesRank = sum(OpsDesRank),
            MontoDesSA = sum(MontoDesSA), OpsDesSA = sum(OpsDesSA)) %>% 
  mutate(pctMonto = MontoDesRank/MontoDesSA*100) %>% 
  mutate(pctOps = OpsDesRank/OpsDesSA*100) %>% 
  mutate(MontoProm = MontoDesRank/OpsDesRank)

Rangos <- list(rangosSA=DesemSA,rangosCA=DesemCA)
write.xlsx(Rangos,paste0("D:/!bso/cosechas/rangos/Rangos_",myrds,".rds"))
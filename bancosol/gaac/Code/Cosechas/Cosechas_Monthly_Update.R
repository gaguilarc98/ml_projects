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

#getCosechas() requiere de una bdc con las variables de select con los mismos
#nombres.
getCosechas <- function(x){
  x %>% 
    mutate(TotalMontoDes = sum(montous)) %>% 
    mutate(TotalOpsDes = sum(opDes)) %>% 
    select(cosechaY, cosechaM, monDate, rangom, tipoCred, Sucursal, labGrupoD, TotalMontoDes, TotalOpsDes, saldous, saldoCast, 
           saldoReprog, saldoPar0, saldoPar0Reprog, saldoPar0Ref, OpsPar0, OpsTot, montous, opDes) %>% 
    group_by(cosechaY, cosechaM, monDate, rangom, tipoCred, Sucursal, labGrupoD) %>% 
    summarise(across(c(saldous:opDes),~sum(.x,na.rm = T)),
              across(c(TotalMontoDes:TotalOpsDes),~max(.x,na.rm=T))) %>% 
    ungroup()
}
####____LISTA DE CLIENTES____####
dfTotal <- readRDS("D:/!bso/features/Clientes_Ene15Jun23.rds")
# dfTotal <- readRDS("D:/!bso/features/Clientes_AjusteRef_Ene15Jun23.rds") %>% 
#   select(-fdes) %>% 
#   rename(fdes = fdes_original)
####____COSECHAS____####
length(which(dfTotal$OPERACION==dfTotal$OPERACION_ORI_REF))#Esto debe ser cero para que la reasignacion de fdes funciones

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015:2023)
year <- c(2021)
myrds <- as.vector(sapply(year, function(x){paste0(mes,x)}))
i <- 100
cosechaSAList <- list()
cosechaCAList <- list()

# bdcPrev <- readRDS(paste0('D:/!bso/girCartera/rds/ec_Feb2023.rds'))
for (i in 1:length(myrds)) {
  tryCatch({
    print(myrds[i])
    bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
      mutate(OpsPar0 = ifelse(par0>0,1,0)) %>% 
      dplyr::filter(MODULO!=131) %>% #Eliminar líneas de crédito ctaCont == 623
      dplyr::filter(MODULO!=29) %>% #Eliminar boletas de garantía ctaCont == 623
      select(CTACLIENTE, OPERACION, OPERACION_ORI_REF, monDate, fdes, montous, MONTO,
             MONEDA, opDes, ctaCont, ESTADO, tipoCred, Sucursal, labGrupoD,
             cosechaY, cosechaM, monDate, saldous, saldoCast, saldoReprog, saldoPar0 = par0,
             saldoPar0Reprog = par0Reprog, saldoPar0Ref = par0Ref, OpsPar0, OpsTot=opTot) %>% 
      mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                                montous <= 3000 ~'b. 1k-3k',
                                montous <= 5000 ~'c. 3k-5k',
                                montous <= 8000 ~'d. 5k-8k',
                                montous <= 10000 ~'e. 8k-10k',
                                montous <= 20000 ~'f. 10k-20k',
                                montous > 20000 ~'g. Mayor a 20k'))
    #Without date adjustment
    Cosecha <- bdc %>%
      getCosechas()
    
    CosechaAjustada <- bdc %>% 
      left_join(select(dfTotal,CTACLIENTE,OPERACION,fdes),
                by=c("CTACLIENTE","OPERACION"), suffix=c("_raw","_ori")) %>% #Unir con la base historica de operaciones
      # mutate(fdes = if_else(opDes==1 & ctaCont %in% c('135','136','137') & !is.na(fdes_ori), fdes_ori, fdes_raw)) %>% #reasignar fdes a los reprogramados
      # mutate(fdes = if_else(opDes==1 & OPERACION_ORI_REF!=0 & !is.na(fdes_ori), fdes_ori, fdes_raw)) %>% #reasignar fdes a los reprogramados
      # mutate(fdes = if_else(!is.na(fdes_ori),fdes_ori,fdes_raw)) %>% 
      mutate(fdes = if_else(!is.na(fdes_ori),fdes_ori,fdes_raw)) %>% 
      mutate(cosechaY = year(fdes)) %>% 
      mutate(cosechaM = as.yearmon(fdes)) %>% 
      mutate(montous = if_else(opDes==1 & ctaCont %in% c('135','136','137'),0,montous)) %>%
      mutate(opDes = if_else(opDes==1 & ctaCont %in% c('135','136','137'),0,opDes)) %>%
      # mutate(montous = if_else(cosechaM != monDate,0,montous)) %>% 
      # mutate(opDes = if_else(cosechaM != monDate,0,1)) %>% 
      getCosechas()
    
    cosechaSAList[[i]] <- Cosecha
    cosechaCAList[[i]] <- CosechaAjustada
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

cosechaSAFull <- rbindlist(cosechaSAList)
cosechaCAFull <- rbindlist(cosechaCAList)

saveRDS(cosechaSAFull,"D:/!bso/cosechas/cosechaSAFull_Final.rds")
saveRDS(cosechaCAFull,"D:/!bso/cosechas/cosechaCAFull_Final.rds")

####____AGREGAR UN MES____####
#Leer las últimas bases agregadas por rango
cosechaSAFull <- readRDS("D:/!bso/cosechas/cosechaSAFull_Final.rds")
cosechaCAFull <- readRDS("D:/!bso/cosechas/cosechaCAFull_Final.rds")

myrds <- "Abr2023"#Actualizar para agregar un mes

bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds,'.rds')) %>% 
  mutate(OpsPar0 = ifelse(par0>0,1,0)) %>% 
  dplyr::filter(MODULO!=131) %>% #Eliminar líneas de crédito ctaCont == 623
  dplyr::filter(MODULO!=29) %>% #Eliminar boletas de garantía ctaCont == 623
  select(CTACLIENTE, OPERACION, OPERACION_ORI_REF, monDate, fdes, montous, MONTO,
         MONEDA, opDes, ctaCont, ESTADO, tipoCred, Sucursal, labGrupoD,
         cosechaY, cosechaM, monDate, saldous, saldoCast, saldoReprog, saldoPar0 = par0,
         saldoPar0Reprog = par0Reprog, saldoPar0Ref = par0Ref, OpsPar0, OpsTot=opTot) %>% 
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous <= 3000 ~'b. 1k-3k',
                            montous <= 5000 ~'c. 3k-5k',
                            montous <= 8000 ~'d. 5k-8k',
                            montous <= 10000 ~'e. 8k-10k',
                            montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k'))
#Aplicar la función getCosecha() a las bases con y sin ajuste de fecha por reprogramación

cosechaSA <- bdc %>% 
  getCosechas()

cosechaCA <- bdc %>% 
  left_join(select(dfTotal,CTACLIENTE,OPERACION,fdes),
            by=c("CTACLIENTE","OPERACION"), suffix=c("_raw","_ori")) %>% #Unir con la base historica de operaciones
  # mutate(fdes = if_else(opDes==1 & ctaCont %in% c('135','136','137') & !is.na(fdes_ori),fdes_ori,fdes_raw)) %>% #reasignar fdes a los reprogramados
  mutate(fdes = if_else(!is.na(fdes_ori),fdes_ori,fdes_raw)) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(cosechaM = as.yearmon(fdes)) %>% 
  mutate(montous = if_else(opDes==1 & ctaCont %in% c('135','136','137'),0,montous)) %>% 
  mutate(opDes = if_else(opDes==1 & ctaCont %in% c('135','136','137'),0,opDes)) %>% 
  getCosechas()

#Actualizar las bases agregadas por rango
cosechaSAFull <- cosechaSAFull %>% 
  bind_rows(cosechaSA)

cosechaCAFull <- cosechaCAFull %>% 
  bind_rows(cosechaCA)

tail(cosechaSAFull %>% #Check de integridad debe cuadrar con el balance (Cartera Bruta)
       group_by(monDate) %>% summarise(Saldo=sum(saldous), Nops=sum(OpsTot)))

tail(cosechaCAFull %>% #Check de integridad debe cuadrar con el balance (Cartera Bruta)
       group_by(monDate) %>% summarise(Saldo=sum(saldous), Nops=sum(OpsTot)))

saveRDS(cosechaSAFull,"D:/!bso/cosechas/cosechaSAFull_Final.rds")
saveRDS(cosechaCAFull,"D:/!bso/cosechas/cosechaCAFull_Final.rds")

####____AGRUPANDO POR AÑO DE DESEMBOLSO____####
#The main source of difference in monto desembolsado with previous versions 
#Is the exclusion of MODULO 29 (Boletas de garantía) in the accounting
cosechaSAFull <- readRDS("D:/!bso/cosechas/cosechaSAFull_Final.rds")
DesemSA <- cosechaSAFull %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  group_by(monDate, rangom) %>% 
  summarise(MontoDesRank = sum(montous),OpsDesRank = sum(opDes),
            TotalMontoDes = max(TotalMontoDes), TotalOpsDes = max(TotalOpsDes)) %>% 
  mutate(pctMonto = MontoDesRank/TotalMontoDes*100) %>% 
  mutate(pctOps = OpsDesRank/TotalOpsDes*100) %>% 
  mutate(MontoProm = MontoDesRank/OpsDesRank)

cosechaCAFull <- readRDS("D:/!bso/cosechas/cosechaCAFull_Final.rds")
DesemCA <- cosechaCAFull %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  group_by(monDate, rangom) %>% 
  summarise(MontoDesRank = sum(montous),OpsDesRank = sum(opDes),
            TotalMontoDes = max(TotalMontoDes), TotalOpsDes = max(TotalOpsDes)) %>% 
  mutate(pctMonto = MontoDesRank/TotalMontoDes*100) %>% 
  mutate(pctOps = OpsDesRank/TotalOpsDes*100) %>% 
  mutate(MontoProm = MontoDesRank/OpsDesRank) %>% 
  mutate(monDate = as.Date(monDate,frac=1))

Rangos <- list(rangosSA = DesemSA, rangosCA = DesemCA)
# write_xlsx(Rangos, "D:/!bso/cosechas/rangos/RangosDesembolso.xlsx")

####____COSECHAS____####
#Cálculo de Meses post desembolso por año consolidado de desembolso
grupoCosecha <- function(x){
  x %>% 
    dplyr::filter(cosechaY >= 2015) %>% 
    select(-rangom, -cosechaM) %>%
    group_by(cosechaY) %>% 
    mutate(TotalMontoDesYear = sum(montous),
           TotalOpsDesYear = sum(opDes)) %>% 
    # group_by(cosechaY) %>% 
    # mutate(TotalMontoDes = sum(unique(TotalMontoDes)),
    #        TotalOpsDes = sum(unique(TotalOpsDes))) %>% 
    group_by(cosechaY,monDate) %>% 
    summarise(across(c(saldous:opDes),~sum(.x,na.rm = T)),
              across(c(TotalMontoDesYear:TotalOpsDesYear),~max(.x,na.rm=T))) %>%
    ungroup() %>% 
    mutate(monDate_ini = as.yearmon(paste0("Ene. ",cosechaY))) %>% #Para el recuento de meses se empieza desde diciembre
    mutate(mesPos = (monDate-monDate_ini)*12) %>% #Cálculo de meses post-desembolso
    mutate(saldoPar0Rel = saldoPar0/TotalMontoDesYear*100,
           opsPar0Rel = OpsPar0/TotalOpsDesYear*100)
}
#Sin ajuste por reprogramaciones
moraSA <- cosechaSAFull %>% 
  grupoCosecha() %>% 
  group_by(cosechaY) %>% 
  mutate(mesPos = round(mesPos,0)) %>% 
  mutate(label = if_else(saldoPar0Rel == max(saldoPar0Rel), 
                         paste0(as.character(cosechaY),': ',round(saldoPar0Rel,2), '%, ', round(mesPos), ' meses'),
                         NA_character_)) %>% 
  mutate(labelo = if_else(opsPar0Rel == max(opsPar0Rel), 
                          paste0(as.character(cosechaY),': ',round(opsPar0Rel,2), '%, ', round(mesPos), ' meses'),
                          NA_character_))

ggplot(moraSA[moraSA$cosechaY <= 2022,], aes(x = mesPos, y = saldoPar0Rel, color = as.factor(cosechaY))) + 
  geom_line(aes(size = as.factor(cosechaY))) + theme_minimal() + 
  labs(x="Meses post-desembolso",y="Mora 0 días/Desembolso (%)") +
  scale_y_continuous(breaks = seq(0,0.9,0.1), limits=c(0,0.9))+
  scale_x_continuous(breaks = seq(0,96,12)) +
  scale_color_manual(values = paleta(12)[c(3:5,10,9,8,6,7)]) + 
  theme_minimal() + 
  theme(text = element_text(size=12),
        legend.position = 'none',
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2, title="Año desemboloso")) + 
  geom_text_repel(aes(label=label), force=1, point.padding=unit(1,'lines'), 
                  vjust=-4.5, 
                  direction='y',
                  hjust=-3.0, 
                  segment.size=0.5,
                  size=4.61)+
  scale_size_manual(values = c(1, 1, 1, 1, 1, 1, 3, 5, 1 ))

ggsave('D:/!bso/cosechas/img/cosechasAnuales_SinAjuste_Jun23.png',width = 10,height = 6,units = 'in')
#Con ajuste por reprogramaciones
moraCA <- cosechaCAFull %>% 
  grupoCosecha() %>% 
  group_by(cosechaY) %>% 
  mutate(label = if_else(saldoPar0Rel == max(saldoPar0Rel), 
                         paste0(as.character(cosechaY),': ',round(saldoPar0Rel,2), '%, ', round(mesPos), ' meses'),
                         NA_character_)) %>% 
  mutate(labelo = if_else(opsPar0Rel == max(opsPar0Rel), 
                       paste0(as.character(cosechaY),': ',round(opsPar0Rel,2), '%, ', round(mesPos), ' meses'),
                       NA_character_))

ggplot(moraCA[moraCA$cosechaY <= 2022,], aes(x = mesPos, y = saldoPar0Rel, color = as.factor(cosechaY))) + 
  geom_line(aes(size = as.factor(cosechaY))) + theme_minimal() + 
  labs(x="Meses post-desembolso",y="Mora 0 días/Desembolso (%)") +
  scale_y_continuous(breaks = seq(0,0.9,0.1), limits=c(0,0.9))+
  scale_x_continuous(breaks = seq(0,96,12)) +
  scale_color_manual(values = paleta(12)[c(3:5,10,9,8,6,7)]) + 
  theme_minimal() + 
  theme(text = element_text(size=12),
        legend.position = 'none',
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2, title="Año desemboloso")) + 
  geom_text_repel(aes(label=label), force=1, point.padding=unit(1,'lines'), 
                  vjust=-4.5, 
                  direction='y',
                  hjust=-2.5, 
                  segment.size=0.5,
                  size=4.61)+
  scale_size_manual(values = c(1, 1, 1, 1, 1, 1, 3, 5, 1 ))

ggsave('D:/!bso/cosechas/img/cosechasAnuales_ConAjuste_Jun23.png',width = 10,height = 6,units = 'in')
####____ESTRUCTURA DE MORA____####
#Sin ajuste por reprogramaciones
moraSA_total <- cosechaSAFull %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  group_by(monDate, cosechaY) %>% 
  summarise(Saldo = sum(saldous), SaldoPar0 = sum(saldoPar0), OpTot=sum(OpsTot),
            OpsMora = sum(OpsPar0)) %>% 
  ungroup()
moraCA_total <- cosechaCAFull %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  group_by(monDate, cosechaY) %>% 
  summarise(Saldo = sum(saldous), SaldoPar0 = sum(saldoPar0), OpTot=sum(OpsTot),
            OpsMora = sum(OpsPar0)) %>% 
  ungroup()
CompMora <- c(list(moraSA = moraSA_total, moraCA=moraCA_total), Rangos)
write_xlsx(CompMora, "D:/!bso/cosechas/rangos/RangosDesembolso.xlsx")
####____FORECAST MORA____####
lastMoraSA <- moraSA %>% 
  select(mesPos, cosechaY, monDate, saldoPar0Rel)

tasasSA <- moraSA %>% 
  group_by(cosechaY) %>% 
  arrange(monDate) %>% 
  mutate(tasa_crecimiento = ifelse(dplyr::lag(saldoPar0Rel,1)>0,saldoPar0Rel/dplyr::lag(saldoPar0Rel,1)-1,0)) %>% 
  ungroup() %>% 
  group_by(mesPos) %>% 
  mutate(tasa_promedio = mean(tasa_crecimiento[cosechaY %in% c(2015:2018)])) %>% 
  select(mesPos,tasa_promedio) %>% 
  distinct_all()

for (i in 1:12) {
  Aux <- lastMoraSA %>% 
    group_by(cosechaY) %>% 
    dplyr::filter(monDate==max(monDate)) %>% 
    ungroup() %>% 
    left_join(tasasSA, by='mesPos') %>% 
    mutate(mesPos = mesPos+1) %>% 
    mutate(monDate = monDate+1/12) %>% 
    mutate(saldoPar0Rel = (tasa_promedio+1)*saldoPar0Rel) %>% 
    select(-tasa_promedio)
  lastMoraSA <- lastMoraSA %>% 
    bind_rows(Aux)
}

forecastSA <- lastMoraSA %>% 
  arrange(cosechaY,mesPos,monDate) %>% 
  mutate(monDate=as.Date(monDate,frac=1))
  
Cartera <- cosechaSAFull %>% 
  mutate(cosechaY = ifelse(cosechaY<2014,2014,cosechaY)) %>% 
  select(cosechaY, monDate, saldous,saldoPar0, montous) %>% 
  group_by(cosechaY, monDate) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(monDate=as.Date(monDate,frac=1))

Listas <- list(Cartera=Cartera, forecastSA=forecastSA)
write_xlsx(Listas, "D:/!bso/cosechas/forecast_Jun2023.xlsx")

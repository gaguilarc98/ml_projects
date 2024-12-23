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
library(openxlsx)
library(scales)
library(janitor)
library(data.table)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____CONSTRUCCION HISTORICA DE SAPOS____####
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Sep23.rds")
checkpt <- ptFull %>% 
  group_by(myPago) %>% 
  summarise(NObs= n(),NOps=n_distinct(Operacion), check=NObs==NOps)

condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Sep2023.rds')
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"),~sum(.x))) %>% 
  ungroup()
####____CONSTRUCCION DE TABLA GIR____####
year <- c(2022:2023)
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
my <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mys <- as.vector(sapply(year, function(x){paste0(mes,". ",x)}))
i <- 20
bdcList <- list()
for(i in 1:length(my)) {
  tryCatch({
    pt <- ptFull %>% 
      dplyr::filter(myPago<= as.yearmon(mys[i]) & myPago>(as.yearmon(mys[i])-1)) %>% 
      mutate(Corte = as.yearmon(mys[i])) %>% 
      group_by(Operacion) %>% 
      mutate(CantPagosTardios12m = n()) %>% 
      ungroup() %>% 
      group_by(Corte, Cuenta, Operacion) %>% 
      summarise(CantPagosTardios12m = max(CantPagosTardios12m)) %>% 
      ungroup()
    
    ct <- cond_clean %>% 
      dplyr::filter(myCond<= as.yearmon(mys[i]) & myCond>(as.yearmon(mys[i])-1)) %>% 
      mutate(Corte = as.yearmon(mys[i])) %>% 
      mutate(CantCond12m = 1) %>% 
      group_by(Corte, Cuenta, Operacion) %>% 
      summarise(CantCond12m = sum(CantCond12m)) %>% 
      ungroup()
    
    print(paste0(my[i]))
    bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', my[i],'.rds')) 
    
    bdcProc <- bdc %>%
      dplyr::filter(MODULO!=29) %>% 
      left_join(select(pt,-Corte), by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      left_join(select(ct, -Corte), by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      replace_na(list(CantCond12m=0, CantPagosTardios12m=0)) %>% 
      mutate(PTyCond = case_when(CantCond12m>=4 & CantPagosTardios12m>=4 ~ "1. Alerta 4x4",
                                 CantCond12m>0 | CantPagosTardios12m>0 ~ "2. Tiene PT o Cond",
                                 TRUE ~ "3. Sin Alerta")) %>% 
      mutate(SaldoBrutoyCast = saldous+saldoCast) %>% 
      mutate(SaldoAlerta = ifelse(PTyCond=="1. Alerta 4x4", saldous, 0)) %>% 
      mutate(SaldoAlertaMora = ifelse(par0>0, par0, SaldoAlerta)) %>% 
      mutate(SaldoMoraCast = par0+saldoCast) %>%
      mutate(OpsBrutayCast = ifelse(ctaCont %in% c('131','133','134','135','136','137','865'), 1, 0)) %>% 
      mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
      mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>%
      mutate(OpsAlerta = ifelse(PTyCond=="1. Alerta 4x4", 1, 0)) %>%
      mutate(OpsAlertaMora = ifelse(SaldoAlertaMora>0, 1, 0)) %>%
      mutate(OpsMoraCast = ifelse(SaldoMoraCast>0, 1, 0)) %>%
      mutate(OpsCastigada = ifelse(saldoCast>0, 1, 0)) %>% 
      mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
      mutate(MONTOUS = ifelse(MONEDA==0, MONTO/6.86, MONTO)) %>% 
      mutate(Controlada = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada",
                                                       "3.C2.Sector Turismo","4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib",
                                                       "6.Vivienda Controlada"), "TC","TNC")) %>% 
      mutate(Reprogramada = ifelse(ctaCont %in% c('135','136','137'), 'Repro','Normal')) %>% 
      mutate(RangoMontoDes = ifelse(MONTOUS>=20000, "Mayor o igual a 20K", "Menor a 20k")) %>% 
      mutate(SectorAgropecuario = ifelse(Sector_Destino =="Agropecuario","Agropecuario","No Agropecuario")) %>% 
      mutate(SectorEconomico = case_when(Sector_Actividad=='H. Comercio' & divCaedecC %in% c('50','51')~'H1. Ventas al por mayor',
                                         Sector_Actividad=='H. Comercio' & divCaedecC %in% c('52')~'H2. Ventas al por menor',
                                         TRUE~Sector_Actividad)) %>% 
      select(Fecha, Sucursal, tipoCred, Reprogramada, RangoMontoDes, Controlada, 
             SectorAgropecuario, SectorEconomico, SaldoBruto = saldous, SaldoPar0 = par0, 
             SaldoAlerta, SaldoAlertaMora, SaldoCast=saldoCast, SaldoMoraCast, SaldoBrutoyCast,
             OpsBruta, OpsPar0, OpsAlerta, OpsAlertaMora,
             OpsCastigada, OpsMoraCast, OpsBrutayCast) %>%
      group_by(Fecha, Sucursal, tipoCred, Reprogramada, RangoMontoDes, Controlada, 
               SectorAgropecuario, SectorEconomico) %>%
      summarise_all(sum) %>% 
      ungroup()
    
    bdcList[[i]] <- bdcProc
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

bdcFull <- rbindlist(bdcList)

write_xlsx(bdcFull, "D:/!bso/limites/Limites_Sep2023.xlsx")

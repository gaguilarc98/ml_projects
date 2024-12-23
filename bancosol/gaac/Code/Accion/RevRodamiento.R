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
library(stringr)    # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ggplot2)
library(ggrepel)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____LECTURA DE MASTER KEY LIST____####
MasterKeyList <- readRDS("D:/!bso/accion/MasterKeyList.rds") %>% 
  select(CTACLIENTE, OPERACION, LLAVEPRIMARIA)

Revision <- read_xlsx("D:/!bso/accion/NoRodamientoDPD_SameBalance.xlsx") %>% 
  left_join(MasterKeyList, by=c("LoanId"="LLAVEPRIMARIA")) %>% 
  select(Date = `Observation Date`, CTACLIENTE, OPERACION, LoanId, Gender, BranchName=`Branch Name`, 
         MaturityDate=`Maturity Date`, LoanTerm = `Loan Term`, DisbDate = `Disbursement Date`, 
         DPD, LoanCycle, LoanState = `Loan State`, NewRenew, MonthDisbNo, 
         RepRefin = `Reprogramado/Refinanciado`, GRACIA =`Periodos de gracia`,
         maximumdpd = `maximum dpd`, N_OP, loancycle_finalV4)

unique(Revision$Date)
cic <- readRDS("D:/!bso/CIC/rds/cic_Feb2023.rds") 

cicShort <- cic %>% 
  select(CTACLIENTE, OPERACION, SaldoBruto, SaldoDiferido, CuotasDiferidas, 
         FormaPago, FechaIncumplimiento, FechaProxPagoCap,
         FechaReprogramacion, FechaRefinanciamiento, CantidadDiasGracia, 
         CantidadPagosConRetraso, DiasMora, CuentaContable.y)

cicMarch <- readRDS("D:/!bso/CIC/rds/cic_Mar2023.rds") %>% 
  select(CTACLIENTE, OPERACION, SaldoBruto, FormaPago, FechaIncumplimiento, FechaProxPagoCap,
         FechaReprogramacion, FechaRefinanciamiento, CantidadDiasGracia, 
         CantidadPagosConRetraso, DiasMora, CuentaContable.y)

RevFormaPago <- Revision %>% 
  left_join(cicShort, by=c("CTACLIENTE","OPERACION"))
  
table(RevFormaPago$FormaPago)

RevMensual <- RevFormaPago %>% 
  dplyr::filter(FormaPago=="MENSUAL") %>% 
  mutate(monDatePago = as.yearmon(FechaProxPagoCap))

table(RevMensual$monDatePago)

RevMensualNextMonth <- RevMensual %>% 
  dplyr::filter(monDatePago<="Mar. 2023") 

###ABRIL Y AGOSTO

cicAbr <- readRDS("D:/!bso/CIC/rds/cic_Abr2023.rds") %>% 
  select(CTACLIENTE, OPERACION, SaldoBruto, SaldoDiferido, CuotasDiferidas, 
         FormaPago, FechaIncumplimiento, FechaProxPagoCap,
         FechaReprogramacion, FechaRefinanciamiento, CantidadDiasGracia, 
         CantidadPagosConRetraso, DiasMora, CuentaContable.y)
cicAgo <- readRDS("D:/!bso/CIC/rds/cic_Ago2023.rds") %>% 
  select(CTACLIENTE, OPERACION, SaldoBruto, SaldoDiferido, CuotasDiferidas, 
         FormaPago, FechaIncumplimiento, FechaProxPagoCap,
         FechaReprogramacion, FechaRefinanciamiento, CantidadDiasGracia, 
         CantidadPagosConRetraso, DiasMora, CuentaContable.y)


OP <- c(3449233, 3729918, 2865035)
my <- c("Sep2022","Oct2022","Nov2022","Dic2022","Ene2023","Feb2023","Mar2023","Abr2023",
        "May2023","Jun2023","Ago2023")
cicList <- list()
for (i in 1:length(my)) {
  print(my[i])
  cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",my[i],".rds")) %>% 
    dplyr::filter(OPERACION %in% OP) %>% 
    select(FechaCorte, CTACLIENTE, OPERACION, FechaInicio, FechaProxPagoCap, 
           SaldoBruto, CuentaContable = CuentaContable.y, FechaIncumplimiento, 
           DiasMora, CuotasDiferidas, FechaReprogramacion, FechaRefinanciamiento, 
           FechaCancelacion)
  cicList[[i]] <- cic
}

cicFull <- rbindlist(cicList) %>% 
  arrange(CTACLIENTE, OPERACION, FechaCorte)
write_xlsx(cicFull, "D:/!bso/CIC/CIC_Casos.xlsx")

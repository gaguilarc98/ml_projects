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
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
cosecha <- 'Abr. 2023' #cambiar para muestra
mes <- str_replace(cosecha,". ","")
bdc <- read_xlsx('D:/!bso/califClientes/BSO_CARNET_FILTRADO2.xlsx',sheet = "Hoja2")

m <- mean(bdc$`MONDO DESEMBOLSADO`)
s <- var(bdc$`MONDO DESEMBOLSADO`)
N <- nrow(bdc)
E <- c(0.1,0.05,0.05,0.025,0.01)*m
c_lev <- qnorm(1-c(0.1,0.05,0.025,0.025,0.01)/2)

n <- ceiling(N*s/((N*E^2/c_lev^2)+s))

####____TAMAÑO DE MUESTRA____####
set.seed(123456)
muestra10_90 <- bdc %>% 
  sample_n(n[1])
set.seed(123456)
muestra5_95 <- bdc %>% 
  sample_n(n[2])
set.seed(123456)
muestra5_975 <- bdc %>% 
  sample_n(n[3])
set.seed(123456)
muestra25_975 <- bdc %>% 
  sample_n(n[4])

Muestras <- list(`muestra10_90`=muestra10_90, muestra5_95=muestra5_95,
                 `muestra5_97.5`=muestra5_975, `muestra2.5_97.5`=muestra25_975)
write_xlsx(Muestras,"D:/!bso/califClientes/Muestras.xlsx")

####____MUESTRA BSO ASFI____####
BSO_ASFI <- read_xlsx('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/99. Varios/1. DD BFS/BSO - ASFI 20230508.xlsx',sheet = "BSO",)

BSO_sample <- BSO_ASFI %>% 
  dplyr::filter(Estado!="Rechazada") %>% 
  select(DESC_DEPARTAMENTO,DESC_LOCALIDAD,DESCRIPCION_COD_AGENCIA,IDENTIFICACION,
         TIPO_IDENTIFICACION,NRO_ID_INTERNO,NRO_OPERACION,NOMBRE_COMPLETO,
         CRED_TIPO_ASFI,MONTO_DES_BS,Estado) %>% 
  mutate(MONTO_DES_BS = as.numeric(MONTO_DES_BS))

m <- mean(BSO_sample$MONTO_DES_BS)
s <- var(BSO_sample$MONTO_DES_BS)
N <- nrow(BSO_sample)
E <- c(0.05)*m
c_lev <- qnorm(1-c(0.05)/2)

n <- ceiling(N*s/((N*E^2/c_lev^2)+s))  

set.seed(123456)

muestra5_95 <- BSO_sample %>% 
  sample_n(n)
Muestra <- list(muestra5_95 = muestra5_95)
write.xlsx(Muestra,'D:/!bso/Muestra_BSO_ASFI.xlsx')

####____OTRAS MUESTRAS____####
Tot <- bdcFull %>% 
  group_by(TIPO_CREDITO) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup()


Tot <- bdcFull %>% 
  group_by(REGIONAL) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup()

tam <- bdcFull %>% 
  mutate(Nops = sum(opTot)) %>%
  group_by(sucursal) %>% 
  summarise(Nh = sum(opTot),Wh= sum(opTot)/max(Nops),
            Ph = 1-0.136)%>%
  ungroup() %>% 
  mutate(pph = Wh*Ph) %>% 
  mutate(Sh = sqrt(Nh*Ph*(1-Ph)/(Nh-1))) %>% 
  summarise(Nops=sum(Nh), sumSh= sum(Wh*Sh), sumSh2=sum(Wh*Sh^2),
            z = qnorm(1-0.05/2), e = 0.05*sum(pph)) %>% 
  mutate(n=sumSh^2/(e^2/z^2+sumSh2/Nops))
#SEED 1234 PARA PRINCIPAL y 12345 PARA REEMPLAZO
set.seed(1234)
muestra <- bdcFull %>%
  mutate(Nops = sum(opTot)) %>%
  group_by(sucursal) %>%
  mutate(Wh=sum(opTot)/max(Nops)) %>% 
  sample_n(round(max(Wh)*tam$n,0)) %>%
  mutate(x=1) %>% 
  ungroup() %>% 
  select(-Wh,-x)


agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
muestra <- muestra %>% 
  left_join(agen,by="AGENCIA") %>% 
  rename(SALDO_USD=saldous, SUCURSAL=sucursal) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,
         CI,ASESOR,NOMBRE_AGENCIA,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,ESTADO,CIU,
         PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,REGIONAL,SUCURSAL)
write_excel_csv(muestra,file="D:/!bso/girCartera/samples/Muestra_Principal_1122_RME_VIVIENDA.csv",delim = ",")
write_excel_csv(muestra,file="D:/!bso/girCartera/samples/Muestra_Reemplazo_1122_RME_VIVIENDA.csv")
muestra <- fread("D:/!bso/girCartera/samples/Muestra_Principal_1122_RME_VIVIENDA.csv",
                 encoding = "UTF-8",fill=TRUE)
muestra <- fread("D:/!bso/girCartera/samples/Muestra_Reemplazo_1122_RME_VIVIENDA.csv",
                 encoding = "UTF-8",fill=TRUE)

M <- muestra %>% 
  group_by(REGIONAL) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup() 
write_xlsx(bind_cols(Tot,M),"D:/!bso/girCartera/samples/Replica2.xlsx")
t <- muestra %>% 
  select(sucursal,opTot,x) %>% 
  mutate(Nops = sum(opTot)) %>%
  group_by(sucursal) %>% 
  summarise(Wh=sum(opTot)/max(Nops),x=sum(x))

sum(muestra$x)
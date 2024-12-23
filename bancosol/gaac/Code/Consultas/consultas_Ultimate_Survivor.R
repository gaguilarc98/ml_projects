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
library(openxlsx)
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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
#####____CONSOLIDADO DE CONSULTAS_____####
nmes <- c("12","11","10","09","08","07","06","05","04","03","02","01")
year <- c(23,22,21,20,19,18,17)
#year <- c(22,21,20,19,18,17)
mycon <- as.vector(sapply(year,function(x){paste0(nmes,x)}))
mycon <- mycon[-1]
conList <- list()
k <- 1
for (my in 1:length(mycon)) {
  arch <- list.files("D:/!bso/Consultas/conxls/",
                     pattern = paste0("^VAR7.*.",mycon[my],".xls$"))
  arch <- sort(arch)
  for (i in 1:length(arch)) {
    tryCatch({
      print(arch[i])
      con <- read_excel(paste0("D:/!bso/Consultas/conxls/",arch[i]),
                        sheet="inf",col_names=c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"),
                        col_types = c("text","text","text","text","text","text","date")) %>% 
        select(-FNAC) %>% 
        mutate(FCON=as.Date(FCON))
      k <- k+1
      print(k)
      conList[[k]] <- con
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

conFull <- rbindlist(conList) %>% 
  dplyr::filter(TIPO=="CI") %>%
  mutate(myCon = as.yearmon(FCON)) %>% 
  rename(CI = NDOC) %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", "")) %>% 
  mutate(myCon= as.yearmon(FCON)) %>% 
  group_by(CI, NDOC, myCon) %>% 
  mutate(NroDiasConsultados = length(unique(as.Date(FCON)))) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(ENTIDAD = str_replace_all(ENTIDAD,'"|,|\\.','')) %>% 
  mutate(GrupoEntidad= case_when(str_detect(ENTIDAD,'BANCO')~'Banco',
                                  str_detect(ENTIDAD,'COOPERATIVA')~'Cooperativa',
                                  str_detect(ENTIDAD,'IFD')~'IFD',
                                  str_detect(ENTIDAD,'FONDO')~'Fondo',
                                  TRUE~'Otros')) %>% 
  select(CI, NDOC, CLIENTE, myCon, FCON, ENTIDAD, GrupoEntidad, NroDiasConsultados)

saveRDS(conFull, "D:/!bso/Consultas/Consultas_Ene17Jun23.rds")
#####____ADD A MONTH TO CONSULTAS_____#####
conFull <- readRDS("D:/!bso/Consultas/Consultas_Ene17Jun23.rds")
mycon <- "0723" #MM/YY
conList <- list()
k <- 1
arch <- list.files("D:/!bso/Consultas/conxls/",
                   pattern = paste0("^VAR7.*.",mycon,".xls$"))
arch <- sort(arch)
for (i in 1:length(arch)) {
  tryCatch({
    print(arch[i])
    con <- read_excel(paste0("D:/!bso/Consultas/conxls/",arch[i]),
                      sheet="inf",col_names=c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"),
                      col_types = c("text","text","text","text","text","text","date")) %>% 
      select(-EXT,-FNAC) %>% 
      mutate(FCON=as.Date(FCON))
    k <- k+1
    print(k)
    conList[[k]] <- con
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

conAux <- rbindlist(conList) %>% 
  dplyr::filter(TIPO=="CI") %>%
  mutate(myCon = as.yearmon(FCON)) %>% 
  rename(CI = NDOC) %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", "")) %>% 
  mutate(myCon= as.yearmon(FCON)) %>% 
  group_by(CI, NDOC, myCon) %>% 
  mutate(NroDiasConsultados = length(unique(as.Date(FCON)))) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(ENTIDAD = str_replace_all(ENTIDAD,'"|,|\\.','')) %>% 
  mutate(GrupoEntidad= case_when(str_detect(ENTIDAD,'BANCO')~'Banco',
                                 str_detect(ENTIDAD,'COOPERATIVA')~'Cooperativa',
                                 str_detect(ENTIDAD,'IFD')~'IFD',
                                 str_detect(ENTIDAD,'FONDO')~'Fondo',
                                 TRUE~'Otros')) %>% 
  select(CI, NDOC, CLIENTE, myCon, FCON, ENTIDAD, GrupoEntidad, NroDiasConsultados)

conFull <- conFull %>% 
  bind_rows(conAux)
saveRDS(conFull, "D:/!bso/Consultas/Consultas_Ene17Jun23.rds")
####____TRANSICIONES CON LAG VARIABLE____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
lag <- 1
cancelList <- list()
# dlist <- list()
# codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
i <- 1
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(i)
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      # mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, MONTO/6.86, MONTO)) %>% 
      mutate(MONTOUS = ifelse(MONTOUS < saldous, saldous, MONTOUS)) %>% 
      select(OPERACION, CI, CTACLIENTE, monDate, CALIFICACION, MONTOUS, saldous,
             OPERACION_ORI_REF, fdes, FFINALIZA, ctaCont, PLAZODIAS, DIASMORA)
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>% 
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      # mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, MONTO/6.86, MONTO)) %>% 
      mutate(MONTOUS = ifelse(MONTOUS < saldous, saldous, MONTOUS)) %>% 
      select(OPERACION, CI, CTACLIENTE, monDate, CALIFICACION, MONTOUS, saldous,
             OPERACION_ORI_REF, fdes, FFINALIZA, ctaCont, PLAZODIAS, DIASMORA)
    
    dfCancel <- df1 %>% 
      anti_join(df2, by = c("CTACLIENTE","OPERACION")) %>% 
      mutate(FueRefin = ifelse(OPERACION %in% df2$OPERACION_ORI_REF, 1, 0)) %>% 
      mutate(monDate = monDate+1/12) %>% 
      mutate(CALIFICACION = "Z") %>% 
      mutate(saldous = 0) %>% 
      mutate(previus = 0) %>% 
      mutate(TipoReduccion = case_when(as.yearmon(FFINALIZA)==monDate ~ '1. Regular',
                                       as.yearmon(FFINALIZA)>monDate ~ '2. Anticipado',
                                       as.yearmon(FFINALIZA)<monDate ~ '3. Intempestivo')) %>% 
      bind_rows(df2) %>% 
      select(-OPERACION_ORI_REF)
    
    dfTrans <- df1 %>% 
      left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
      # mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      # mutate(difPrev = previus_fin - previus_ini) %>% 
      mutate(FueRefin = ifelse(is.na(FueRefin), 0,FueRefin)) %>% 
      group_by(CTACLIENTE, OPERACION) %>% 
      mutate(MONTOUS = max(c(MONTOUS_ini, MONTOUS_fin))) %>% 
      ungroup() %>% 
      dplyr::filter(CALIFICACION_fin=='Z' | (saldous_ini/MONTOUS>0.5 & saldous_fin/saldous_ini<0.5)) %>% 
      mutate(ratioSaldo = ifelse(saldous_ini>0, saldous_fin/saldous_ini,1)) %>% 
      mutate(TipoReduccion = ifelse(is.na(TipoReduccion), '4. Amortización',TipoReduccion)) %>% 
      # dplyr::filter(saldous_ini/saldous_fin>0.5) %>% 
      # mutate(difSaldo = saldous_fin - saldous_ini) %>% 
      # dplyr::filter(ratioSaldo>0.5) %>% #Filtro saldo amort/saldo prev amort <0.5
      select(myAmort = monDate_fin, CTACLIENTE, OPERACION, CI = CI_ini, CALIFICACION_ini, 
             MONTOUS, saldous_ini, saldous_fin, fdes_ini, FFINALIZA_ini, FFINALIZA_fin, FueRefin,
             TipoReduccion, ctaCont_ini, PLAZODIAS_ini, ratioSaldo)
    
    cancelList[[i]] <- dfTrans
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
cancelFull <- rbindlist(cancelList)
saveRDS(cancelFull, "D:/!bso/Consultas/Amortizados_Ene2015Jun2023.rds")
check <- cancelFull %>% group_by(myAmort, TipoReduccion) %>% 
  summarise(Nops=n()) %>% 
  pivot_wider(names_from = TipoReduccion, values_from = Nops, values_fill = 0)

####____ADDING CI TO CLIENT_____####
codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx", sheet = "Old")
cancelFull <- readRDS("D:/!bso/Consultas/Amortizados_Ene2015Jun2023.rds")
dfTotal <- readRDS("D:/!bso/features/Clientes_Ene15Jun23.rds") %>% 
  left_join(codAge, by = "AGENCIA") %>% 
  select(CTACLIENTE, CI, OPERACION, Sucursal)
  

dfTotal_CI <- dfTotal %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA","")) %>%
  glimpse()

conFull_old <- conFull_old %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA","")) %>%
  glimpse()
cancelFull_CI <- cancelFull %>% 
  select(-CI) %>% 
  # mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA","")) %>% 
  left_join(dfTotal_CI, by = c("CTACLIENTE","OPERACION")) %>% 
  relocate(CI, NDOC, .after = OPERACION)
remove(dfTotal)
####____JOINING CANCELLED WITH CONSULTED____####
conFull <- readRDS("D:/!bso/Consultas/Consultas_Ene17Jun23.rds")
checkUnicity_old <- conFull_old %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA","")) %>%
  group_by(myCon) %>% 
  summarise(Nobs=n(),CI=n_distinct(CI),DOC=n_distinct(NDOC))
conCancelList <- list()

for (i in 1:7) {
  print(i)
  conFull_aux <- conFull %>% 
    mutate(myCon = myCon+(i-1)/12) %>% 
    mutate(MesesAntesCancel = i)
  bdc_wExt <- cancelFull_CI %>% 
    inner_join(select(conFull_aux,-NDOC), by = c("myAmort"="myCon", "CI"))
  bdc_wNDoc <- cancelFull_CI %>% 
    anti_join(conFull_aux, by = c("myAmort"="myCon", "CI")) %>% 
    left_join(select(conFull_aux,-CI), by = c("myAmort"="myCon","NDOC")) 
  
  # bdc_wName <- cancelFull_CI %>% 
  #   anti_join(conFull, by = c("myAmort"="myCon", "CI")) %>% 
  #   anti_join(conFull, by = c("myAmort"="myCon","NDOC")) %>% 
  #   left_join(conFull, by= c("myAmort"="myCon","NOMBRE"="CLIENTE"))
  
  bdcAll <- bdc_wExt %>% 
    bind_rows(bdc_wNDoc)
  conCancelList[[i]] <- bdcAll 
}

conCancelFull <- rbindlist(conCancelList) %>% 
  group_by(CTACLIENTE, OPERACION, myAmort) %>% 
  arrange(MesesAntesCancel) %>% 
  dplyr::filter(row_number()==1)

conCancelExp <- conCancelFull %>% 
  ungroup() %>% 
  dplyr::filter(myAmort >= "Ene. 2019" & myAmort <="Jun. 2023") %>% 
  mutate(myAmort = as.Date(myAmort, frac = 1)) %>% 
  mutate(rangom = case_when(MONTOUS <= 1000 ~ '1. Menor a 1k USD',
                            MONTOUS <= 2000 ~ '2. 1k - 2k USD',
                            MONTOUS <= 5000 ~ '3. 2k - 5k USD',
                            MONTOUS <= 10000 ~ '4. 5k - 10k USD',
                            MONTOUS > 10000 ~ '5. Mayor a 10k USD',)) %>% 
  mutate(rangos = case_when(saldous_ini <= 500 ~ '1. Menor a 500 USD',
                            saldous_ini <= 1000 ~ '2. 500 - 1k USD',
                            saldous_ini <= 2000 ~ '3. 1k - 2k USD',
                            saldous_ini <= 5000 ~ '4. 2k - 5k USD',
                            saldous_ini > 5000 ~ '5. Mayor a 5k USD',)) %>% 
  mutate(ratioSaldoMonto = saldous_ini/MONTOUS) %>% 
  mutate(ratioSM_bin = case_when(ratioSaldoMonto <= 0.10 ~ '1. < 10%',
                              ratioSaldoMonto <= 0.25 ~ '2. 10% - 25%',
                              ratioSaldoMonto <= 0.50 ~ '3. 25% - 50%',
                              ratioSaldoMonto <= 0.75 ~ '4. 50% - 75%',
                              ratioSaldoMonto > 0.75 ~ '5. > 75%',)) %>% 
  mutate(plazoRemanente = floor((as.yearmon(FFINALIZA_fin)-as.yearmon(myAmort))*12))
  
write_xlsx(conCancelExp, "D:/!bso/Consultas/ConsultadosCancelados_Ene2019Jun2023.xlsx")

x <- conCancelFull %>% group_by(myAmort,MesesAntesCancel) %>% 
  summarise(Nobs=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = MesesAntesCancel,values_from = Nobs,values_fill = 0)
  
y2 <- conCancelFull %>% 
  mutate(FueConsultado = ifelse(!is.na(MesesAntesCancel),1,0)) %>% 
  group_by(myAmort) %>% 
  summarise(saldo=sum(saldous_ini), saldoComp = sum(saldous_ini*FueConsultado))

saveRDS(conCancelFull, "D:/!bso/Consultas/CanceladosConsultados_Ene2017Jun2023.rds")

####____CRUCE CON BASES DE CARTERA AL CIERRE____####

con1XMes <- conL %>% 
  group_by(myCon,CI) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

conMes <- con1XMes %>% 
  dplyr::filter(myCon=="Mar. 2023")

cicp_Join <- conMes %>% 
  left_join(cicp, by = c("CI"="IdObligado"))

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Mar2023.rds")

bdcJoin <- conMes %>% 
  left_join(bdc, by = c("CI"))

cicp_deudores <- cicp %>% 
  dplyr::filter(str_detect(CodTipoRelacion,'A'))

cicp_deudoresJoin<- conMes %>% 
  left_join(cicp, by = c("CI"="IdObligado"))


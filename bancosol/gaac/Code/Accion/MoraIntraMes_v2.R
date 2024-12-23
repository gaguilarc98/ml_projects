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
# require(XLConnect)
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
####____MORA INTRA-MES____####
my <- "202306"
dAux <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_",
                       my,".rds")) %>% 
  select(CTACLIENTE, OPERACION, DIASMORA) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
  ungroup()
####____MORA INTRA-MES____####
year <- c(2018:2023)
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
n <- length(mes)
i <- 6
k <- 3
moraList <- list()
for(i in 1:length(year)) {
  for (k in 1:length(mes)) {
    tryCatch({
      print(paste0(mes[k],year[i]))
      
      bdcMora <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', 
                              mes[k], year[i],'.rds')) %>%
        # mutate(monDate = as.yearmon(paste0(mes[k],'. ',year[i]))) %>% 
        # mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),~dmy(.x))) %>% 
        dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
        select(monDate,CTACLIENTE, OPERACION, MODULO, ctaCont, FVEN_PROXPAGO, 
               FVEN_ULTPAGO, FULT_PAGO, DIASMORA) %>%
        mutate(ESTADO_fixed = case_when(ctaCont %in% c('131','135') ~ 'VIGENTE',
                                     ctaCont %in% c('133','136') ~ 'VENCIDA',
                                     ctaCont %in% c('134','137') ~ 'EJECUCION',
                                     ctaCont == '865' ~ 'CASTIGADA',)) %>% 
        mutate(PrimerDia = as.Date(monDate)) %>% 
        mutate(UltimoDia = as.Date(monDate,frac=1)) %>% 
        mutate(MoraIntraMes = ifelse(!is.na(FULT_PAGO) & !is.na(FVEN_ULTPAGO) & FULT_PAGO>FVEN_ULTPAGO &
                                       FVEN_ULTPAGO>=PrimerDia & FVEN_PROXPAGO>UltimoDia,FULT_PAGO-FVEN_ULTPAGO,0)) %>% 
        mutate(maximaDPD = 0) %>% 
        mutate(maximaDPD = case_when(ESTADO_fixed %in% c("VENCIDA","EJECUCION","CASTIGADA") & DIASMORA>0 ~ DIASMORA,
                                     ESTADO_fixed %in% c("VENCIDA","EJECUCION","CASTIGADA") & DIASMORA==0 
                                     & !is.na(FVEN_ULTPAGO) ~ as.numeric(UltimoDia-FVEN_ULTPAGO),
                                     ESTADO_fixed == "VIGENTE" & DIASMORA==0 & !is.na(MoraIntraMes) ~ MoraIntraMes,
                                     TRUE~maximaDPD)) %>% 
        mutate(maximaDPD_2 = ifelse(maximaDPD>(UltimoDia-PrimerDia),UltimoDia-PrimerDia,maximaDPD)) %>% 
        mutate(tuvoDPD_1 = ifelse(maximaDPD_2>0,1,0)) %>% 
        mutate(tuvoDPD_5 = ifelse(maximaDPD_2>=5,1,0)) %>% 
        mutate(tuvoDPD_10 = ifelse(maximaDPD_2>=10,1,0)) %>% 
        mutate(tuvoDPD_15 = ifelse(maximaDPD_2>=15,1,0)) %>% 
        mutate(tuvoDPD_20 = ifelse(maximaDPD_2>=20,1,0)) %>% 
        mutate(tuvoDPD_25 = ifelse(maximaDPD_2>=25,1,0)) %>% 
        mutate(tuvoDPD_30 = ifelse(maximaDPD_2>=30,1,0))
      
      moraList[[k+(i-1)*n]] <- select(bdcMora,-PrimerDia, -UltimoDia, -MoraIntraMes)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
moraFull <- rbindlist(moraList)
saveRDS(moraFull, "D:/!bso/firstTimes/moraIntraMes_Ene2018Jul2023.rds")

moraFull %>% group_by(monDate) %>% summarise(n_distinct(OPERACION))
tail(moraFull %>% group_by(monDate) %>% summarise(n_distinct(OPERACION)))

moraFull %>% group_by(monDate) %>% summarise(length(which(maximaDPD!=0)))
tail(moraFull %>% group_by(monDate) %>% summarise(length(which(maximaDPD!=0))))

moraFull <- readRDS("D:/!bso/accion/moraIntraMes_Ene2018May2023.rds")
moraEne <- moraFull %>% 
  dplyr::filter(monDate=="Jun. 2023")
####____ADDING A MONTH___####
#RUN the loop for the specific month and then come back
moraFull <- readRDS("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")
moraFull <- moraFull %>% 
  bind_rows(bdcMora)
saveRDS(moraFull, "D:/!bso/accion/moraIntraMes_Ene2018Jul2023.rds")
####____ENMASCARANDO____####
MasterKeyList <- readRDS('D:/!bso/accion/MasterKeyList.rds')
moraFull_masked <- moraFull %>% 
  left_join(MasterKeyList, by=c("CTACLIENTE","OPERACION")) %>% 
  select(-CTACLIENTE, -OPERACION, -MODULO, -ESTADO)

moraFull_masked <- moraFull_masked %>% 
  dplyr::filter(monDate!="May. 2023") %>% 
  mutate(Fecha = as.Date(monDate, frac=1)) %>% 
  select(`Fecha de Corte` = Fecha,LLAVEPRIMARIA, MASCARA_CUENTA, MASCARA_OPERACION, `Máxima DPD` = maximaDPD)

saveRDS(moraFull_masked, "D:/!bso/accion/moraIntraMes_masked_Ene2018May2023.rds")
fwrite(moraFull_masked,"D:/!bso/accion/moraIntraMes_masked_Ene2018Abr2023.txt", 
       sep = ",", row.names = F)
####____SOLVE SIZE____####
moraFull <- fread("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/moraIntraMes_masked_Ene2018Abr2023.txt",encoding = "UTF-8")
mora2018_2020 <- moraFull %>% 
  dplyr::filter(`Fecha de Corte`<as.Date('2021-01-01'))
fwrite(mora2018_2020,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/moraIntraMes_masked_Ene2018Dic2020.txt", 
       sep = ",", row.names = F)
mora2021_2023 <- moraFull %>% 
  dplyr::filter(`Fecha de Corte`>=as.Date('2021-01-01'))
fwrite(mora2021_2023,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/moraIntraMes_masked_Ene2021Abr2023.txt", 
       sep = ",", row.names = F)
##########################
moraFull_masked <- readRDS("D:/!bso/accion/moraIntraMes_masked_Ene2018May2023.rds")
moraFull_masked %>% group_by(monDate) %>% summarise(length(which(is.na(LLAVEPRIMARIA))))
tail(moraFull_masked %>% group_by(monDate) %>% summarise(length(which(is.na(LLAVEPRIMARIA)))))

moraJoin <- dAux %>% 
  left_join(moraEne,by=c("CTACLIENTE","OPERACION"))

length(which(is.na(moraJoin$maximaDPD)))
####____CON LA CIC____####
workdir <- "C:/CIC/202301/"
namesk <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-K") %>% 
  select(1:2)

cick <- fread(paste0(workdir,"CR20230131K.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesk$NAME) %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x)))

namesa <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CC-A") %>% 
  select(1:2)

cica <- fread(paste0(workdir,"CC20230131A.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesa$NAME) %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  select(FechaCorte, CTACLIENTE, OPERACION, NombreRazonSocial, NumeroCuota, FechaProgramada, 
         FechaPago, DiasRetraso) %>% 
  group_by(CTACLIENTE,OPERACION) %>% 
  summarise(maxCIC = max(DiasRetraso,na.rm = T)) %>% 
  ungroup()

cick_mora <- cick %>% 
  left_join(cica, by=c("CTACLIENTE","OPERACION")) %>% 
  select(CTACLIENTE,OPERACION,maxCIC) %>% 
  mutate(maxCIC = ifelse(is.na(maxCIC),0,maxCIC))

####____MULTIPLE JOIN____####
moraJoinJoin <- moraJoin %>% 
  left_join(cick_mora,by=c("CTACLIENTE","OPERACION")) %>% 
  relocate(maxCIC, .after = maximaDPD)

length(which(is.na(moraJoinJoin$maxCIC)))
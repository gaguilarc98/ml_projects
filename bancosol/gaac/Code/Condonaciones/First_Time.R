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
####___CONDONACIONES FULL____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jun2023.rds')

process <- function(x, vstep){
  x %>% 
    select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
           CondInt_USD = Cond_Int,
           CondCap_USD = Cond_Cap,
           Cod_Asesor = Asesor,
           Nombre_Asesor = NombreAsesor,
           AGENCIA = Sucursal_operacion,
           {{vstep}}) %>%
    left_join(codAge,by="AGENCIA") %>% 
    select(-Fecha, -Regional) %>% 
    rename(Cod_Agencia = AGENCIA) %>% 
    group_by({{vstep}}, Sucursal, Cod_Agencia, Nombre_Agencia, NOMBRE_AGENCIA, 
             Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}

gph <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int,
         CondCap_USD = Cond_Cap,
         Cod_Asesor = Asesor,
         Nombre_Asesor = NombreAsesor,
         AGENCIA = Sucursal_operacion) %>%
  left_join(codAge,by="AGENCIA") %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  group_by(Cuenta,Operacion) %>% 
  mutate(FechaFirstCond = min(Fecha)) %>% 
  ungroup() %>% 
  select(-Fecha, -Regional) %>% 
  rename(Cod_Agencia = AGENCIA) %>%
  group_by(monDate, Sucursal, Cod_Agencia, NOMBRE_AGENCIA, 
           Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
  summarise(across(CondCapInt_USD:CondCap_USD,~sum(.x)),FechaFirstCond = min(FechaFirstCond)) %>% 
  ungroup() %>% 
  mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
  mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
  mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
  mutate(Key = paste(Cuenta,Operacion,sep="-")) 

CheckOpsUnicas <- gph %>% group_by(monDate) %>% 
  summarise(NObs=n(),NOps=n_distinct(Operacion),check=NObs==NOps)
####____LISTA FEATURES____####
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Jun23.rds') %>% 
  glimpse()

dfTotal_2019 <- dfTotal %>% 
  dplyr::filter(fdes>=as.Date("2019-01-01"))

condFull_2019 <- gph %>% 
  select(CTACLIENTE=Cuenta,OPERACION=Operacion,monDate,FechaFirstCond) %>% 
  group_by(CTACLIENTE,OPERACION) %>% 
  summarise(myFirstCond=min(monDate),FechaFirstCond = min(FechaFirstCond),NMES_CONDONADOS = n_distinct(monDate)) %>% 
  ungroup()
####____LISTA DE PAGOS TARDIOS____####
ptTotal<- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Jul23.rds")

ptardioFull_2019 <- ptTotal %>% 
  dplyr::filter(myPago>"dic. 2018") %>% 
  group_by(Cuenta,Operacion) %>% 
  summarise(FechaFirstTardio = min(FechaPago),
            NMES_TARDIOS = n_distinct(myPago)) %>% 
  ungroup() %>% 
  mutate(myFirstTardio = as.yearmon(FechaFirstTardio)) %>% 
  rename(CTACLIENTE=Cuenta,OPERACION=Operacion)
  
  
dfJoin <- dfTotal_2019 %>% 
  left_join(condFull_2019,by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(ptardioFull_2019,by=c("CTACLIENTE","OPERACION"))

# dfJoinFull <- dfTotal %>%
#   left_join(condFull_2019,by=c("CTACLIENTE","OPERACION")) %>% 
#   left_join(ptardioFull_2019,by=c("CTACLIENTE","OPERACION"))

# condFull_2019$OPERACION[!condFull_2019$OPERACION %in% dfTotal$OPERACION]
# ptardioFull_2019$OPERACION[!ptardioFull_2019$OPERACION %in% dfTotal$OPERACION]
####____CALCULO DE DIFERENCIAS A LA PRIMERA FECHA____####
dfCalc <- dfJoin %>% 
  mutate(cosechaM = as.yearmon(fdes)) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(FechaFirstCond = as.Date(FechaFirstCond)) %>% 
  group_by(cosechaY) %>% 
  mutate(MONTODES = sum(MONTOUS,na.rm = T)) %>% 
  mutate(OPDES = n_distinct(OPERACION)) %>% 
  # dplyr::filter(!is.na(myfirstcond)) %>% 
  mutate(Days_to_first_cond = as.numeric(FechaFirstCond-fdes)) %>% 
  mutate(Months_to_first_cond = as.integer((myFirstCond-cosechaM)*12)) %>% 
  mutate(Days_to_first_tardio = as.numeric(FechaFirstTardio-fdes)) %>% 
  mutate(Months_to_first_tardio = as.integer((myFirstTardio-cosechaM)*12))

dfEvol <- dfCalc %>% 
  mutate(fueCond = ifelse(is.na(NMES_CONDONADOS),0,1)) %>% 
  mutate(fueTardio = ifelse(is.na(NMES_TARDIOS),0,1)) %>% 
  group_by(cosechaY,Months_to_first_cond) %>% 
  summarise(cond_rel = sum(fueCond)/max(OPDES)*100) %>% 
  dplyr::filter(!is.na(Months_to_first_cond))

ggplot(dfEvol,aes(x=Months_to_first_cond, y=cond_rel,color=factor(cosechaY)))+
  geom_line(size=1.25)+
  labs(x="Meses hasta la primera condonación",y="Operaciones relativas",
       color="Año de desembolso")+
  scale_y_continuous(labels = percent)+
  theme_light()

dfEvol <- dfCalc %>% 
  mutate(fueCond = ifelse(is.na(NMES_CONDONADOS),0,1)) %>% 
  mutate(fueTardio = ifelse(is.na(NMES_TARDIOS),0,1)) %>% 
  group_by(cosechaY,Months_to_first_tardio) %>% 
  summarise(tard_rel = sum(fueTardio)/max(OPDES)*100,nOps=sum(fueTardio)) %>% 
  dplyr::filter(!is.na(Months_to_first_tardio))

ggplot(dfEvol,aes(x=Months_to_first_tardio, y=tard_rel,color=factor(cosechaY)))+
  geom_line(size=1.25)+
  labs(x="Meses hasta el primer pago tardío",y="Operaciones",
       color="Año de desembolso")+
  scale_y_continuous(labels = percent)+
  theme_light()

ggplot(dfEvol, aes(x=Months_to_first_tardio, y=nOps, fill=as.factor(cosechaY)))+
  geom_bar(stat = "identity", alpha=0.5)+
  labs(x="Meses al primer pago tardío",y="Operaciones",
       fill="Año de desembolso")+
  theme_light()

dfMetrics <- dfCalc %>% 
  mutate(fue_cond = ifelse(!is.na(myFirstCond),1/OPDES,0)) %>% 
  mutate(fue_tardio = ifelse(!is.na(myFirstTardio),1/OPDES,0)) %>%
  group_by(cosechaM) %>% 
  summarise(Cond_rel = sum(fue_cond)*100, Tardio_rel = sum(fue_tardio)*100)

dfCalc %>% 
  mutate(cond_bin = cut(Months_to_first_cond,breaks=seq(-1,60,5),labels=seq(5,60,5))) %>% 
  mutate(tardio_bin = cut(Months_to_first_tardio,breaks=seq(-1,60,5),labels=seq(5,60,5))) %>% 
  group_by(cond_bin,tardio_bin) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=cond_bin,y=tardio_bin,fill=n))+
  geom_tile()+
  theme_light()
####____PLOTS____####
ggplot(dfCalc,aes(x=Days_to_first_cond,fill=as.factor(cosechaY)))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values = paleta(5))+
  scale_y_continuous(labels = scales::percent)+
  theme_light()

ggplot(dfCalc,aes(x=Days_to_first_tardio,fill=as.factor(cosechaY)))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values = paleta(5))+
  scale_y_continuous(labels = scales::percent)+
  theme_light()

ggplot(dfCalc,aes(x=Months_to_first_cond,fill=as.factor(cosechaY)))+
  geom_histogram(aes(y = after_stat(count / sum(count))),bins=30,alpha=0.5)+
  scale_fill_manual(values = paleta(12))+
  scale_y_continuous(labels = scales::percent)+
  theme_light()

ggplot(dfCalc,aes(x=Months_to_first_tardio,fill=as.factor(cosechaY)))+
  geom_histogram(aes(y = after_stat(count / sum(count))),bins=30,alpha=0.5)+
  scale_fill_manual(values = paleta(12))+
  scale_y_continuous(labels = scales::percent)+
  theme_light()

plt <- dfJoin %>%
  mutate(cosechaM = as.yearmon(fdes)) %>% 
  mutate(last_time = as.Date(max(monDate),frac=1)) %>% 
  mutate(last_month = as.yearmon(max(monDate))) %>% 
  mutate(from_des = as.numeric(last_month-cosechaM)*12) %>% 
  mutate(from_first = as.numeric(last_month-myfirstcond)*12) %>% 
  mutate(from_rel = ifelse(from_des!=0,from_first/from_des,1)) %>% 
  mutate(ncond_des = ifelse(from_des!=0,NMES_CONDONADOS/from_des,0)) %>%
  mutate(ncond_first = ifelse(from_first!=0,NMES_CONDONADOS/from_first,0))

ggplot(plt,aes(x=from_des,y=from_first,color=as.factor(cosechaY)))+
  geom_point(size=2,alpha=0.25)

#####____FIRST DEFAULT ON PAYMENT____####
daily <- readRDS(paste0("D:/!bso/vipCartera/dailyMM/dailyMM_",shortmonth,".rds"))

VIPJoin <- VIPJoin %>% 
  left_join(daily, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días',)) %>% 
  glimpse()
#CON ESTA VARIABLE SE CAPTURA LA MORA MÁS RECIENTE DE MANERA MÁS FIDEDIGNA QUE LA APROXIMACIÓN
####____MORA_IM_2018____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")) %>% 
  # dplyr::filter(monDate!="jun. 2023") %>% #Filtro para reproducir meses anteriores
  dplyr::filter(!is.na(maximaDPD)) %>% 
  group_by(CTACLIENTE) %>% 
  # mutate(mesDiasMora = if_else(maximaDPD==0, 2014, year(monDate))) %>%
  mutate(tuvoMora = ifelse(maximaDPD>0, 1, 0)) %>% 
  summarise(meanMoraIM_cl = mean(maximaDPD),
            tuvoMora_pct = sum(tuvoMora)/n()) %>% #lastMoraIM_cl = max(mesDiasMora), maxMoraIM_cl = max(maximaDPD)
  ungroup() %>% 
  mutate(meanMoraIMclBin = case_when(meanMoraIM_cl == 0 ~ '1. 0 días',
                                     meanMoraIM_cl <= 1 ~ '2. <= 1 día',
                                     meanMoraIM_cl <= 5 ~ '3. 1 - 5 días',
                                     meanMoraIM_cl <= 10 ~ '4. 5 - 10 días',
                                     meanMoraIM_cl <= 30 ~ '5. 10 - 30 días',
                                     meanMoraIM_cl > 30 ~ '6. >30 días',)) %>% 
  mutate(tuvoMoraIMclBin = case_when(tuvoMora_pct == 0 ~ '1. 0 %',
                                     tuvoMora_pct <= 0.05 ~ '2. 1-5 %',
                                     tuvoMora_pct <= 0.10 ~ '3. 5-10 %',
                                     tuvoMora_pct <= 0.15 ~ '4. 10-15 %',
                                     tuvoMora_pct <= 0.20 ~ '5. 15-20 %',
                                     tuvoMora_pct > 0.20 ~ '6. >20%')) %>% 
  glimpse()

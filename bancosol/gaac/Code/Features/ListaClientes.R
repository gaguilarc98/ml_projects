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
library(stringr)
library(forcats) 
library(scales)
library(janitor)
library(fastDummies)
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
####____OBJETIVO____####
#Obtener las variables descriptivas al momento de desembolso de cada crédito.
#En caso de datos faltantes buscar la primera vez que se corrigió el dato.
#Obtener la primera fecha de desembolso registrada, en caso de que la operación
#Ya venga reprogramada en enero 2015, colocar la primera fecha disponible.
####____HISTORIAL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022,2023)
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
# agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
# file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')

i <- 106
for (i in 1:104) {
  tryCatch({
    print(myrds[i])
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        # mutate(NOMBRE = str_trim(paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT))) %>% 
        mutate(MontoDes = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        mutate(rangom = cut(montous,breaks=c(0,500,1000,5000,10000,15000,20000,Inf), include.lowest=T,
                            labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
        mutate(Objeto = case_when(str_detect(DESC_OBJCRED,"INVERSION")~"INVERSION",
                                  str_detect(DESC_OBJCRED,"OPERACION")~"OPERACION",
                                  TRUE~"OTROS")) %>% 
        mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)),
                                   "> 7")) %>% 
        mutate(esFSL = if_else(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
        mutate(fdes = if_else(esFSL==1, as.Date("2023-05-31"), fdes)) %>% 
        select(CTACLIENTE, OPERACION, CI, GENERO, ASESOR, NOMBRE_ASESOR, fdes, MONEDA, MontoDes,
               Sucursal, AGENCIA, MODULO, Objeto, TIPO_CREDITO, PlazoAnios, 
               Sector_Actividad, Sector_Destino, cosechaY, cosechaM, rangom, 
               OPERACION_ORI_REF, ctaCont, monDate, esFSL) #CAEDEC_DEST, CIU
      dfTotal <- df1 %>% 
        mutate(fueRefin = if_else(OPERACION_ORI_REF!=0, 1, 0)) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0)) %>%
        mutate(fueCast = if_else(ctaCont %in% c('865'),1,0)) %>% 
        mutate(NReprog = fueReprog) %>% 
        mutate(fdesLast = fdes) %>% 
        mutate(FechaRefin = if_else(fueRefin==1,fdes,as.Date(NA))) %>% 
        mutate(FechaReprog = if_else(fueReprog==1,fdes,as.Date(NA))) %>% 
        mutate(FechaCast = if_else(fueCast==1,monDate-1/12,as.yearmon(NA))) 
    }else{
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        # mutate(NOMBRE = str_trim(paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT))) %>% 
        mutate(MontoDes = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        mutate(rangom = cut(montous,breaks=c(0,500,1000,5000,10000,15000,20000,Inf), include.lowest=T,
                            labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
        mutate(Objeto = case_when(str_detect(DESC_OBJCRED,"INVERSION")~"INVERSION",
                                  str_detect(DESC_OBJCRED,"OPERACION")~"OPERACION",
                                  TRUE~"OTROS")) %>% 
        mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)),
                                   "> 7")) %>% 
        mutate(esFSL = if_else(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
        mutate(fdes = if_else(esFSL==1, as.Date("2023-05-31"), fdes)) %>% 
        select(CTACLIENTE, OPERACION, CI, GENERO, ASESOR, NOMBRE_ASESOR, fdes, MONEDA, MontoDes,
               Sucursal, AGENCIA, MODULO, Objeto, TIPO_CREDITO, PlazoAnios, 
               Sector_Actividad, Sector_Destino, cosechaY, cosechaM, rangom, 
               OPERACION_ORI_REF, ctaCont, monDate, esFSL) 
      dfNew <- df1 %>% 
        anti_join(dfTotal, by = c("CTACLIENTE","OPERACION")) %>% 
        mutate(fueRefin = if_else(OPERACION_ORI_REF!=0,1,0)) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0)) %>%
        mutate(fueCast = if_else(ctaCont %in% c('865'),1,0)) %>% 
        mutate(NReprog = fueReprog) %>% 
        mutate(fdesLast = fdes) %>% 
        mutate(FechaRefin = if_else(fueRefin==1,fdes,as.Date(NA))) %>% 
        mutate(FechaReprog = if_else(fueReprog==1,fdes,as.Date(NA))) %>% 
        mutate(FechaCast = if_else(fueCast==1,monDate-1/12,as.yearmon(NA))) 
      dfUpdate <- df1 %>% 
        select(-CI, -GENERO, -ASESOR, -NOMBRE_ASESOR, -MONEDA, -MontoDes, -Sucursal, -AGENCIA, -MODULO,
               -cosechaY, -cosechaM, -rangom, -esFSL)
      dfOld <- dfTotal %>% 
        inner_join(dfUpdate,by=c("CTACLIENTE","OPERACION"), suffix = c('_old','_new')) %>% 
        mutate(cambio = case_when(fdesLast==fdes_new ~ 0,
                                  fdesLast!=fdes_new & ctaCont_new %in% c('135','136','137') ~ 1,
                                  fdesLast!=fdes_new & !(ctaCont_new %in% c('135','136','137')) ~ 2, 
                                  TRUE ~ 0)) %>% #1 means Reprog 2 means Refin, 0 is no change in fdes
        mutate(FechaReprog = if_else(cambio==1, fdes_new, FechaReprog)) %>% 
        mutate(fueReprog = if_else(fueReprog==0 & cambio==1, 1, fueReprog)) %>% 
        mutate(NReprog = if_else(cambio==1, NReprog+1, NReprog)) %>% 
        mutate(FechaRefin = if_else(cambio==2, fdes_new, FechaRefin)) %>% 
        mutate(fueRefin = if_else(fueRefin==0 & cambio==2, 1, fueRefin)) %>% 
        mutate(OPERACION_ORI_REF = if_else(cambio==2 & OPERACION_ORI_REF_old==0,
                                           OPERACION_ORI_REF_new, OPERACION_ORI_REF_old)) %>% 
        mutate(FechaCast = if_else(fueCast==0 & ctaCont_new =='865', monDate_new, FechaCast)) %>%
        mutate(fueCast = if_else(fueCast==0 & ctaCont_new =='865', 1, fueCast)) %>%
        mutate(fdesLast = fdes_new) %>% 
        # mutate(NOMBRE_old = ifelse(is.na(NOMBRE_old) | NOMBRE_old=="", NOMBRE_new, NOMBRE_old)) %>% 
        mutate(Objeto_old = ifelse(is.na(Objeto_old) | Objeto_old=="", Objeto_new, Objeto_old)) %>% 
        mutate(TIPO_CREDITO_old = ifelse(is.na(TIPO_CREDITO_old) | TIPO_CREDITO_old=="", TIPO_CREDITO_new, TIPO_CREDITO_old)) %>% 
        mutate(PlazoAnios_old = ifelse(is.na(PlazoAnios_old), PlazoAnios_new, PlazoAnios_old)) %>% 
        mutate(Sector_Actividad_old = ifelse(is.na(Sector_Actividad_old) | Sector_Actividad_old=="", Sector_Actividad_new, Sector_Actividad_old)) %>% 
        mutate(Sector_Destino_old = ifelse(is.na(Sector_Destino_old) | Sector_Destino_old=="", Sector_Destino_new, Sector_Destino_old)) %>% 
        select(CTACLIENTE, OPERACION, CI, GENERO, ASESOR, NOMBRE_ASESOR, MONEDA, MontoDes, Sucursal, AGENCIA, MODULO,
               cosechaY, cosechaM, rangom, fueRefin, FechaRefin, fueReprog, FechaReprog, NReprog,
               fueCast, FechaCast, fdesLast,
               ctaCont = ctaCont_new,
               monDate = monDate_new,
               esFSL,
               OPERACION_ORI_REF,
               fdes = fdes_old,
               Objeto = Objeto_old, TIPO_CREDITO = TIPO_CREDITO_old,
               PlazoAnios = PlazoAnios_old, Sector_Actividad = Sector_Actividad_old,
               Sector_Destino = Sector_Destino_old) #NOMBRE = NOMBRE_old,
      dfCancel <- dfTotal %>% 
        anti_join(df1,by=c("CTACLIENTE","OPERACION"))
      
      dfTotal <- dfCancel %>% 
        bind_rows(dfOld) %>% 
        bind_rows(dfNew)
    }
    if(i>=96){
      saveRDS(dfTotal,paste0('D:/!bso/features/ClientesTTF_Ene2015',myrds[i],'.rds'))
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

saveRDS(dfTotal,'D:/!bso/features/ClientesTTF_Ene2015Dic2022.rds')

dfTotal <- readRDS('D:/!bso/features/ClientesTTF_Ene2015Ago2023.rds')

dfTotal <- readRDS('D:/!bso/features/ClientesTTF_Ene2015Oct2023.rds')

dfTotal <- dfTotal %>% 
  group_by(CTACLIENTE) %>%
  arrange(fdes) %>%
  mutate(Ciclo = row_number()) %>%
  ungroup()

saveRDS(dfTotal, 'D:/!bso/features/ClientesTTF_Ene2015Oct2023.rds')

clientes <- readRDS('D:/!bso/features/Clientes_Ene15Ago23.rds')
#Check de fdes antiguos
# CLJoin <- clientes %>% 
#   left_join(dfTotal, by=c("CTACLIENTE","OPERACION"), suffix=c("_old","_new")) %>% 
#   dplyr::filter(fdes_old!=fdes_new)

table(dfTotal$TIPO_CREDITO[year(dfTotal$fdes)>2014], useNA = "always")


####____CAMBIOS DE PREV REQUERIDOS____####
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  glimpse()

dfTotal <- readRDS("D:/!bso/features/ClientesTTF_Ene2015Jul2023.rds")
  
dfSegment <- dfTotal %>% 
  dplyr::filter(fdes>= as.Date("2021-08-02") & fdes<=as.Date("2022-07-29")) %>% 
  dplyr::filter(str_sub(TIPO_CREDITO,1,1) %in% c('C','M','P','H') & MONEDA==0)

dfCalif <- dfOps %>% 
  semi_join(dfSegment, by=c("CTACLIENTE","OPERACION")) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(PeorCalif = max(CALIFICACION))

dfSegment <- dfSegment %>% 
  left_join(dfCalif, by=c("CTACLIENTE","OPERACION")) %>% 
  dplyr::filter(PeorCalif !="A")
####____Lectura Base de Cartera Jul 2023___####
bdcJul <- readRDS("D:/!bso/girCartera/rds/ec_Jul2023.rds")
bdc <- fread("D:/!bso/bases/txt/BaseCarteraJul2023.txt",sep='|', encoding = "Latin-1",
             fill = T) %>% 
  dplyr::filter(MODULO !=131) %>% 
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  # dplyr::filter(ctaCont!="865") %>% 
  mutate(saldous = ifelse(MONEDA==0, SALDO/6.86, SALDO)) %>% 
  mutate(saldous = ifelse(ctaCont=="865",0,saldous))
sum(bdc$saldous)
sum(bdcJul$saldous)

bdc %>% group_by(ctaCont) %>% summarise(Saldo=sum(saldous))
bdcJul %>% group_by(ctaCont) %>% summarise(Saldo=sum(saldous))
#BaseObservaciones
Obs <- read_xlsx("D:/!bso/prevCorrecciones/Obs_codigo_ONC.xlsx")

bdcClean <- bdc %>% 
  dplyr::filter(!MODULO %in% c(29, 131)) %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  mutate(saldous = ifelse(MONEDA==0, SALDO/6.86, SALDO)) %>% 
  left_join(select(dfSegment, CTACLIENTE, OPERACION, IncumpleExcepcion), 
            by = c("CTACLIENTE","OPERACION")) %>% 
  mutate(productivo = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo","4.C3.Prod Intelectual",
                                                   "5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada","8.Otra Prod.No Controlada"), 1, 2)) %>% 
  replace_na(list(IncumpleExcepcion=0))

bdcIncumple <- bdcClean %>% 
  mutate(PorNormaZero = ifelse(str_sub(TIPO_CREDITO,1,1) %in% c('P','M') & productivo==1 & CALIFICACION=="A",1,0)) %>% 
  dplyr::filter(PorNormaZero==1) %>% 
  dplyr::filter(IncumpleExcepcion==1)

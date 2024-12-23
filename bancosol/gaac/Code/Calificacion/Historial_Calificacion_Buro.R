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
library(openxlsx)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cases <- function(quant,levs,values, default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)

####____GETTING HISTORIC STATE IN BURO FROM CLIENT____####
year <- c("2018","2019","2020","2021","2022","2023")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
myburo <- as.vector(sapply(year, function(x){paste0(month,x)}))
i <- 69
califList <- list()
for (i in 1:length(myburo)) {
  print(myburo[i])
  infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',myburo[i],'.rds'))
  
  infoClientes <- infoCheck %>% 
    dplyr::filter(REGULADO=="SBEF") %>% 
    dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
    mutate(esBSO = ifelse(SIGLA=='BSO',1,0)) %>%
    mutate(noesBSO = ifelse(SIGLA!='BSO',1,0)) %>%
    group_by(CI) %>%
    dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
    mutate(CTACLIENTE = max(CTACLIENTE[esBSO==1])) %>% #Le asignamos la CTACLIENTE dentro de BSO a los créditos fuera de BSO
    ungroup() %>% 
    mutate(FECHA = as.yearmon(paste0(substr(myburo[i],1,3),'. ',substr(myburo[i],4,7)))) %>% 
    mutate(EIF = ifelse(esBSO==1,'BSO','SF')) %>% 
    mutate(ESTADO_LAST = substr(histLast16,1,1)) %>% 
    mutate(ESTADO_HIST = case_when(str_count(histLast16,"4")>0 ~ 4,
                                   str_count(histLast16,'3')>0 ~ 3,
                                   str_count(histLast16,'2')>0 ~ 2,
                                   str_count(histLast16,'1')>0 ~ 1,
                                   TRUE ~ -1)) %>% 
    select(FECHA, CTACLIENTE, EIF, CALIFICACION, ESTADO, ESTADO_LAST, ESTADO_HIST) %>% 
    mutate(CALIFICACION = cases(CALIFICACION, levs = c('_','A','B','C','D','E','F'),
                                values = c(0,1,2,3,4,5,6), default = 0)) %>%
    mutate(ESTADO = ifelse(is.na(ESTADO),'0. OTROS', ESTADO)) %>% 
    mutate(ESTADO = cases(ESTADO, levs = c("0. CONTINGENTE","0. OTROS","1. VIGENTE","2. VENCIDA","3. EJECUCION","4. CASTIGADA"),
                          values = c(0,-1,1,2,3,4), default = -1)) %>% 
    mutate(ESTADO_LAST = cases(ESTADO_LAST, levs = c("1","2","3","4","5","0"),
                            values = c(1,2,3,4,-1,-1))) %>% 
    group_by(FECHA, CTACLIENTE, EIF) %>%
    summarise_all(max, na.rm=T) %>% 
    ungroup() %>% 
    pivot_wider(names_from = EIF,values_from = c(CALIFICACION, ESTADO, ESTADO_LAST, ESTADO_HIST))
  
  califList[[i]] <- infoClientes
}
#Codificacion de ESTADOS:
#0: OTROS -> -1 OTROS/SIN DATOS
#0: CONTINGENTE -> 0
#1: VIGENTE -> 1
#2: VENCIDA -> 2
#3: EJECUCION -> 3
#4: CASTIGADA -> 4

#Codificación de Calificación:
#0: _ (SIN DATOS)
#1: A
#2: B
#3: C
#4: D
#5: E
#6: F
califFull <- rbindlist(califList) 

saveRDS(califFull, "D:/!bso/vipCartera/historic/HistCalif_Jun2023.rds")

####____AGREGANDO UN MES____####
califFull <- readRDS("D:/!bso/vipCartera/historic/HistCalif_Ago2023.rds")
tail(califFull %>% 
  group_by(FECHA) %>% 
  summarise(OPS = n()))

#Realizar el loop anterior para el mes a agregar y luego retomar desde aquí
tail(infoClientes %>% 
       group_by(FECHA) %>% 
       summarise(OPS = n()))
califFull <- califFull %>% 
  bind_rows(infoClientes)
saveRDS(califFull, "D:/!bso/vipCartera/historic/HistCalif_Sep2023.rds")
####____DECODING ESTADOS AND CALIFICACION____####
califExp <- califFull
  mutate(across(starts_with("CALIFICACION")~case_when(0~'_',
                                                     1~'A',
                                                     2~'B',
                                                     3~'C',
                                                     4~'D',
                                                     5~'E',
                                                     6~'F',))) %>% 
  mutate(across(starts_with("ESTADO"),~case_when(-1~'0. SIN DATOS',
                                                 0~'CONTINGENTE',
                                                 1~'VIGENTE',
                                                 2~'VENCIDA',
                                                 3~'EJECUCION',
                                                 4~'CASTIGADA')))


#Se obtienen medidas de rendimiento del cliente con bases en la historia de 
#sus créditos fuera del banco

print(myburo[i])
infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',myburo[i],'.rds'))


system.time(infoClientes <- infoCheck %>% 
              dplyr::filter(REGULADO=="SBEF") %>% 
              dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
              mutate(esBSO = ifelse(SIGLA=='BSO',1,0)) %>%
              mutate(noesBSO = ifelse(SIGLA!='BSO',1,0)) %>%
              mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
              group_by(CI) %>%
              dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
              mutate(CTACLIENTE = max(CTACLIENTE[esBSO==1])) %>% #Le asignamos la CTACLIENTE dentro de BSO a los créditos fuera de BSO
              ungroup() %>% 
              mutate(FECHA = as.yearmon(paste0(substr(myburo[i],1,3),'. ',substr(myburo[i],4,7)))) %>% 
              mutate(EIF = ifelse(esBSO==1,'BSO','SF')) %>% 
              select(FECHA, CTACLIENTE, EIF, ESTADO, HISTORICO, CALIFICACION) %>% 
              group_by(FECHA, CTACLIENTE, EIF) %>%
              mutate(ESTADO = ifelse(is.na(ESTADO),'0. OTROS',ESTADO)) %>% 
              mutate(lastHist = substr(HISTORICO,1,1)) %>% 
              mutate(Desc_lastHist = case_when(lastHist=='1'~'1. VIGENTE',
                                               lastHist=='2'~'2. VENCIDO',
                                               lastHist=='3'~'3. EJECUCION',
                                               lastHist=='4'~'4. CASTIGADA',
                                               lastHist=='5'~'0. SIN DATOS',
                                               TRUE~'0. OTROS')) %>% 
              select(-lastHist,-HISTORICO) %>% 
              summarise_all(max, na.rm=T) %>% 
              ungroup() %>% 
              pivot_wider(names_from = EIF,values_from = c(ESTADO, CALIFICACION, Desc_lastHist))
)
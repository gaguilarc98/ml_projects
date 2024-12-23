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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____CRUCE PAGOS TARDIOS CON PREVISION____####
bdc <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Nov2022.rds') %>% 
  rename(Operacion=OPERACION) %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  select(Operacion,CI,cosechaY)
  
lastCierre <- fread("D:/!bso/mph/Oreports/lastCierrreUR_Dic2022.csv",
                    encoding="UTF-8",fill=T,sep=",") %>% 
  # left_join(bdc,"Operacion") %>% 
  select(CI, cosechaY, Instancias_UR, Instancias_AR, Saldo2M, Saldo3M, Saldo4M, Saldo5M,
         Saldo6M,Saldo_USD) %>% 
  rename(saldoPT=Saldo_USD)
dummy <- readRDS('D:/!bso/shared/FullJoin_Ene23.rds') %>% 
  select(Operacion,CI,Nro_Pagos_Tardios,Ultimo_mes,cosechaY,Tipo_Credito,Reprogramado,Refinanciado,Diferido)

infoPerf_ASFIPrev <- readRDS('D:/!bso/Previsiones/infoPerf_ASFIPrev.rds') %>% 
  dplyr::filter(fbase==202212)

tardioPerf <- infoPerf_ASFIPrev %>% 
  left_join(lastCierre,by=c("CI")) %>% 
  replace_na(list(Instancias_AR=0,Instancias_UR=0)) %>% 
  group_by(CI) %>% 
  arrange(desc(Instancias_UR)) %>% 
  dplyr::filter(row_number()==1)

write.xlsx(tardioPerf,'D:/!bso/Previsiones/prevtardios.xlsx')

####____CON INFOPERF RAW____####
infoPerf <- fread('D:/!bso/bases/csv/infoPerf_SF_dic2022.csv',
                  encoding = 'UTF-8',sep=',',fill = T) 
infoPerf %>% 
  group_by(fbase) %>% 
  summarise(saldo=sum(saldous,na.rm = T))

infoPerf <- infoPerf %>% 
  dplyr::filter(fbase==202212) 

infoPerf2 <- infoPerf %>% 
  group_by(CI) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  arrange(CI,pos)

tardioPerf <- infoPerf %>% 
  left_join(lastCierre,by=c("CI")) %>% 
  replace_na(list(Instancias_AR=0,Instancias_UR=0)) %>% 
  group_by(CI) %>% 
  arrange(desc(Instancias_UR)) %>% 
  dplyr::filter(row_number()==1)

write.xlsx(tardioPerf,'D:/!bso/Previsiones/prevtardios.xlsx')

####____OTRAS VAINAS INNECESARIAS____####
tp <- tardioPerf %>% 
  group_by(CALIFICACION,Instancias_AR) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(names_from = Instancias_AR,values_from = n) %>% 
  adorn_totals(c("row","col"))

infoPerf_ASFIPrev %>% 
  dplyr::filter(fbase==202212) %>% 
  tally()
tp <- tardioPerf %>% 
  group_by(CALIFICACION,califPot) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(CALIFICACION,califPot)

library(knitr)
library(kableExtra)
kable(tp[,-c(1)],format = "latex") %>% 
  pack_rows(index = table(tp$CALIFICACION))

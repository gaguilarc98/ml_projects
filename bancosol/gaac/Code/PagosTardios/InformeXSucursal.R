options("encoding" = "UTF-8")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(scales)
library(ggplot2)
library(gt)
library(knitr)
library(kableExtra)
library(xlsx)
library(openxlsx)
cases <- function(quant,levs,values,default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  #new <- vector(mode = 'character',length = length(quant))
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3","slateblue3",
                             "red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____TABLA PARA CADA SUCURSAL____####
lastCierre <- read.csv("D:/!bso/mph/Oreports/lastCierrreUR_Dic2022_3031.csv") %>% 
  mutate(Instancias_AR = ifelse(is.na(Instancias_AR),0,Instancias_AR)) %>% 
  mutate(Instancias_UR = ifelse(is.na(Instancias_UR),0,Instancias_UR)) %>% 
  select(Sucursal,Agencia,Asesor,Operacion,NOMBRE_CLIENTE,Tipo_Credito,Estado,ctaCont,
         Saldo_USD,Instancias_AR,Instancias_UR)

suc <- unique(lastCierre$Sucursal)

AsesorList <- list()
require(XLConnect)
for (i in 1:length(suc)) {
  print(suc[i])
  byAsesor <- lastCierre %>%
    #dplyr::filter(!(str_detect(Agencia,'Normalizadora')) & !(str_detect(Agencia,"Movil"))) %>%
    dplyr::filter(Sucursal==suc[i],Instancias_UR>=2)
  # wb <- loadWorkbook("D:/!bso/mph/Detalle_Pagos_Tardíos.xls",create=TRUE)
  # writeWorksheet(wb,byAsesor,sheet=suc,
  #                startRow=1,startCol=1,header=T)
  # saveWorkbook(wb)
  AsesorList[[i]] <- byAsesor
}

names(AsesorList) <- suc
write.xlsx(AsesorList,file = "D:/!bso/mph/202212_Detalle_Pagos_Tardíos.xlsx")

####___DESGLOSE POR ASESOR____####
lastCierre <- read.csv("D:/!bso/mph/Oreports/lastCierrreUR_Dic2022_3031.csv") %>% 
  mutate(Instancias_AR = ifelse(is.na(Instancias_AR),0,Instancias_AR)) %>% 
  mutate(Instancias_UR = ifelse(is.na(Instancias_UR),0,Instancias_UR)) %>% 
  mutate(`Saldo recurrente 2+`= ifelse(Instancias_UR>=2,Saldo_USD,0)) %>% 
  mutate(`Op recurrentes 2+`= ifelse(Instancias_UR>=2,1,0)) %>%
  mutate(`Saldo recurrente 4+`= ifelse(Instancias_UR>=4,Saldo_USD,0)) %>% 
  mutate(`Op recurrentes 4+`= ifelse(Instancias_UR>=4,1,0)) %>%
  select(Asesor,Sucursal,Agencia,Saldo_USD,Operaciones,`Saldo recurrente 2+`,`Op recurrentes 2+`,
         `Saldo recurrente 4+`,`Op recurrentes 4+`) %>% 
  group_by(Asesor,Sucursal,Agencia) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(`Ratio Saldo+2 SaldoUSD`=`Saldo recurrente 2+`/Saldo_USD) %>% 
  mutate(`Ratio Saldo+4 SaldoUSD`=`Saldo recurrente 4+`/Saldo_USD) %>% 
  mutate(`Ratio Op+2 Operaciones`=`Op recurrentes 2+`/Operaciones) %>% 
  mutate(`Ratio Op+4 Operaciones`=`Op recurrentes 4+`/Operaciones)

write.xlsx(lastCierre,file = "D:/!bso/mph/202212_Ranking_Asesor_Pagos Tardíos.xlsx")

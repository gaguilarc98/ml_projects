####____CARGA DE LIBRERIAS Y FUNCIONES____####
remove(list = ls())
gc()
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
library(openxlsx)
library(kableExtra)
library(RODBC)
library(DBI)
cases <- function(quant,levs,values,default=NA){
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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3","slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____ESTABLECIENDO LA CONEXION____####
CAP <- odbcConnect("CAH")

nombres <- sqlTables(CAP)$TABLE_NAME


nn <- sqlColumns(CAP,nombres[1])
sqlFetch(CAP,"BaseCaptaciones_CA_Mensual_Abr2021.txt")

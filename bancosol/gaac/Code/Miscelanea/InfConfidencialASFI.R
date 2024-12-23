####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(xtable)
library(openxlsx)
library(scales)
library(janitor)
library(data.table)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____LECTURA DE TXT____####
Rep <- "//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/InformeConfidencialASFI/"
arch <- list.files(Rep)
infList <- list()
for (i in 1:length(arch)) {
  infConf <- fread(paste0(Rep,arch[i]))
  infList[[i]] <- infConf
}
infFull <- rbindlist(infList)
fwrite(infFull, paste0(Rep,"InformeConfidencial.csv"),row.names = F)
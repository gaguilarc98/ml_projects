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
################################################################################
# Data in
N_Campos <- readxl::read_excel("D:/!bso/bases/excel/Clasificacion_Sector_Economico_Modificado.xls", sheet = "subclase") %>% 
  select(CAEDEC_DEST, cat)
ventamm <- readxl::read_excel("D:/!bso/bases/excel/ventamm.xlsx") 
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
###############################################################################
####____BDC DIARIA____####
files <- c("BaseCartera_20230319")

bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria/', 
                    files,'.txt'), 
             encoding = 'UTF-8', fill = T) %>% #colClasses=colCls
  left_join(N_Campos, by="CAEDEC_DEST") %>% 
  left_join(ventamm, by="CAEDEC_DEST") %>%
  left_join(codAge, by="AGENCIA") %>% 
  dplyr::filter(MODULO != 131) %>% 
  mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),~as.Date(.x,"%d/%m/%y")))

write_xlsx(bdc,'D:/!bso/girCartera/samples/BaseCartera_20230319.xlsx')


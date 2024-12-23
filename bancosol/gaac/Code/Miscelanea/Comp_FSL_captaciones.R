####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
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
library(forcats)    # Working with factors/categorical data
library(openxlsx)
library(scales)
library(janitor)
library(ggrepel)
remove(list = ls())
options("encoding" = "UTF-8")
options(scipen = 999)

paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.5)
####____BASES DE CIERRE MENSUAL____####
bdc <- fread("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraAbr2023.txt",
             encoding = "Latin-1",fill = T,sep='|') %>% 
  select(CI) %>% 
  mutate(TieneCred = 1) %>% 
  distinct_all()
cah <- fread("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/01_Base_Capataciones_CAH_Mensual/BaseCaptaciones_CA_Mensual_Abr2023.txt",
             encoding = "UTF-8",sep='|') %>% 
  select(CI) %>% 
  mutate(TieneCah = 1) %>% 
  distinct_all()
dpf <- fread("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/02_Base_Capataciones_DPF_Mensual/BaseCaptaciones_DPF_Mensual_Abr2023.txt",
             encoding = "UTF-8",sep='|') %>% 
  select(CI = CI_NIT_RUC) %>% 
  mutate(TieneDpf = 1) %>% 
  distinct_all()
####____READ EXCEL____####
fsl <- read.xlsx("D:/!bso/CapFSL.xlsx") %>% 
  mutate(CI = NRO_DOC_IDENTIDAD) %>% 
  separate_wider_delim(CI,names = c("CI_a","CI_b","CI_c","CI_d","CI_e","CI_f","CI_g","CI_h"),delim="/",too_few = "align_start",too_many = "drop")
has <- function(df) {
  df %>% 
    replace_na(list(TieneCred.x=0,TieneCah.x=0,TieneDpf.x=0,TieneCred.y=0,TieneCah.y=0,TieneDpf.y=0)) %>%
    rowwise() %>% 
    mutate(TieneCred=max(TieneCred.x,TieneCred.y),
           TieneCah=max(TieneCah.x,TieneCah.y),
           TieneDpf=max(TieneDpf.x,TieneDpf.y)) %>% 
    ungroup() %>% 
    select(-TieneCred.x,-TieneCah.x,-TieneDpf.x,-TieneCred.y,-TieneCah.y,-TieneDpf.y)
}
fsl_cliente <- fsl %>% 
  left_join(rename(bdc,CI_a=CI),by="CI_a") %>% 
  left_join(rename(cah,CI_a=CI),by="CI_a") %>% 
  left_join(rename(dpf,CI_a=CI),by="CI_a") %>% 
  left_join(rename(bdc,CI_b=CI),by="CI_b") %>% 
  left_join(rename(cah,CI_b=CI),by="CI_b") %>% 
  left_join(rename(dpf,CI_b=CI),by="CI_b") %>% 
  has() %>% 
  left_join(rename(bdc,CI_c=CI),by="CI_c") %>% 
  left_join(rename(cah,CI_c=CI),by="CI_c") %>% 
  left_join(rename(dpf,CI_c=CI),by="CI_c") %>% 
  has() %>% 
  left_join(rename(bdc,CI_d=CI),by="CI_d") %>% 
  left_join(rename(cah,CI_d=CI),by="CI_d") %>% 
  left_join(rename(dpf,CI_d=CI),by="CI_d") %>% 
  has() %>% 
  left_join(rename(bdc,CI_e=CI),by="CI_e") %>% 
  left_join(rename(cah,CI_e=CI),by="CI_e") %>% 
  left_join(rename(dpf,CI_e=CI),by="CI_e") %>% 
  has() %>% 
  left_join(rename(bdc,CI_f=CI),by="CI_f") %>% 
  left_join(rename(cah,CI_f=CI),by="CI_f") %>% 
  left_join(rename(dpf,CI_f=CI),by="CI_f") %>% 
  has() %>% 
  left_join(rename(bdc,CI_g=CI),by="CI_g") %>% 
  left_join(rename(cah,CI_g=CI),by="CI_g") %>% 
  left_join(rename(dpf,CI_g=CI),by="CI_g") %>% 
  has() %>% 
  left_join(rename(bdc,CI_h=CI),by="CI_h") %>% 
  left_join(rename(cah,CI_h=CI),by="CI_h") %>% 
  left_join(rename(dpf,CI_h=CI),by="CI_h") %>% 
  has()

write.xlsx(fsl_cliente,"D:/!bso/capFSL_tieneCredCahDpf.xlsx")

####____LECTURA____####
ASFI <- read.xlsx("D:/!bso/20230516 Base ASFI CONS.xlsx") 

ASFIClientes <- ASFI %>% 
  select(CI = IDENTIFICACION) %>% 
  distinct_all()


baseAbr <- readRDS("D:/!bso/girCartera/rds/ec_Abr2023.rds") %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(MODULO!=29) %>%
  dplyr::filter(ctaCont!='865') %>%
  select(CI) %>% 
  mutate(EnBDC = 1) %>% 
  distinct_all()

dummyAbr <- ASFIClientes %>% 
  left_join(baseAbr, by="CI") %>% 
  replace_na(list(EnBDC=0)) %>% 
  rename(IDENTIFICACION=CI)
  
ASFI2 <- ASFI %>% 
  left_join(dummyAbr,by="IDENTIFICACION")

write.xlsx(ASFI2,"D:/!bso/ASFI_EnBDC_Abr2023.xlsx")

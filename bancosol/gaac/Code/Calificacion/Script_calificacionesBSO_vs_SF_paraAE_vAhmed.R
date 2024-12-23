#----------------------------------------------#
#
#   Generamos la información de Calificación Sistema Banco
#
#----------------------------------------------#
# cargamos las libreria 
library(tidyverse)
library(readxl)
library(openxlsx)
library(data.table)
remove(list = ls())
#----------------------------------------------
#setwd("//vprt2srv/Riesgos/ArchivosCentral/")

#list.files()

Lista_01 <- c(
              "202211_utf8"
              )


setwd("D:/!bso")
for(i in 1:length(Lista_01)) {
  tryCatch({
    print(Lista_01[i])
    bdc <- fread(paste0("D:/!bso/BSO",
                           Lista_01[i],".txt"),
                    sep="|", fill = T) %>% 
      glimpse() %>% 
      dplyr::filter(`TIPO OBLIGADO SBEF` == "1A - DEUDOR" |
                      #TIPO.OBLIGADO.SBEF ==  "1B - CODEUDOR"|
                      `TIPO OBLIGADO SBEF` == "4A - DEUDOR SOLIDARIO"| 
                      `TIPO OBLIGADO SBEF` == "5A - DEUDOR A SOLA FIRMA"
                    ) %>% 
      dplyr::filter(`SIGLA SBEF`!="BSO") %>% 
    select(`NRO DOCUMENTO`, 
           EXT, 
           `SBEF CALIFICACION`
           ) %>% 
      mutate(CI = paste0(as.numeric(`NRO DOCUMENTO`),EXT),
             ) %>% 
      group_by(CI) %>% 
      summarise(`SBEF CALIFICACION` = max(`SBEF CALIFICACION`)) %>% 
    mutate(Periodo = Lista_01[i]) %>% 
      distinct()
    
    write.csv(bdc, paste0("BD_INFO_CAL_", Lista_01[i], ".csv"))
    
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#names(BSO202208)
glimpse(bdc)
# table(bdc$SIGLA.SBEF)
# table(bdc$TIPO.OBLIGADO.SBEF)

# generamos un reporte de clientes 

Lista_02 <- c(
              "202211_utf8"
)

Lista_01 <- c(
              "Nov2022"
              )

i=1
Sys.setlocale("LC_ALL", "C")
for(i in 1:length(Lista_01)) {
  tryCatch({
    bdc_01 <- fread(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera",
                           Lista_01[i],".txt"),
                    sep="|", fill = T) %>%
      dplyr::filter(MODULO != 131) %>% 
      dplyr::filter(MODULO != 29) %>% 
      dplyr::filter(ESTADO != "CASTIGADA") %>% 
      select(CTACLIENTE, CALIFICACION_BANCO = CALIFICACION, CI) %>% 
      mutate(CI = as.character(CI)) %>% 
      distinct()
    
    bdc_02 <- read.csv(paste0("D:/!bso/BD_INFO_CAL_",
                           Lista_02[i],".csv"),
                    header=T, sep=",") %>% 
      glimpse() %>% 
      select(CI, SBEF.CALIFICACION)
    
    bdc <- left_join(bdc_01, bdc_02, by = c("CI")) %>% 
      dplyr::filter(is.na(SBEF.CALIFICACION)!=T) %>% 
      select(CTACLIENTE, CI, 
             CALIFICACION_BANCO, SBEF.CALIFICACION_MAX = SBEF.CALIFICACION)
    
    write.csv(bdc, paste0("BD_CARTERA_INFO_CAL", Lista_01[i], ".csv"))
  }, 
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




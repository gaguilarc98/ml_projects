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
####___FIRST UNMASK AND GET FIELDS FROM PREVIOUSLY EMPTY LLAVEPRIMARIA____####
myfile <- c('01_Etapa1_enero2018')

keys_new <- readxl::read_excel(paste0("C:/accion/",
                                  myfile,'_new.xlsx'), sheet = 'MS81479brllaves')
brbase_new <- readxl::read_excel(paste0("C:/accion/",
                                    myfile,'_new.xlsx'), sheet = 'MS81479brbase')

brbase_old <- readxl::read_excel(paste0("C:/accion/",
                                        myfile,'.xlsx'), sheet = 'MS81479brbase')

brbase_new <- brbase_new %>% 
  anti_join(brbase_old, by="LLAVEPRIMARIA")
brbase_new_join <- brbase_new %>% 
  select(LLAVEPRIMARIA, `ID del Producto`,
         `Fecha de desembolso`, `Monto del préstamo desembolsado`, `Monto de la Cuota`)

brbase_old_nokeys <- brbase_old %>% 
  dplyr::filter(is.na(LLAVEPRIMARIA)) %>% 
  select(`ID de sucursal`, `ID del Producto`, `Identificación del oficial/gestor de crédito`,
         `Fecha de desembolso`, `Fecha de la primera cuota`, `Monto del préstamo desembolsado`, `Monto de la Cuota`)

brbase_old_nokeys %>% 
  left_join(brbase_new, by = c("ID de sucursal", "ID del Producto", "Identificación del oficial/gestor de crédito",
                               "Fecha de desembolso", "Fecha de la primera cuota", "Monto del préstamo desembolsado", "Monto de la Cuota"))
####____THEN JOIN REST OF FILES AND ADD LLAVEPRIMARIA____####
month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2018:2023)
myfile <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))
i <- 60
for (i in 1:length(myfile)) {
  tryCatch({
    print(myfile[i])
    # keys <- readxl::read_excel(paste0("C:/accion/",
    #                                   myfile[i],'.xlsx'), sheet = 'MS81479brllaves')
    brbase <- readxl::read_xlsx(paste0("C:/accion/",
                                            myfile[i],'.xlsx'), sheet = 'MS81479brbase') %>% 
      mutate(across(c(`Fecha de desembolso`, `Fecha de la primera cuota`),~as.character(.x)))
    brbase_nokeys <- brbase %>% 
      dplyr::filter(is.na(LLAVEPRIMARIA))
    
    lcheck <- nrow(brbase_nokeys)
    brbase_nokeys <- brbase_nokeys %>% 
      select(-LLAVEPRIMARIA) %>% 
      inner_join(brbase_new_join, by = c("ID del Producto", 
                                   "Fecha de desembolso", "Monto del préstamo desembolsado", "Monto de la Cuota")) %>% 
      relocate(LLAVEPRIMARIA,.after = `Fecha de Corte`)
    lrow <- nrow(brbase_nokeys)
    lna <- length(which(!is.na(brbase_nokeys$LLAVEPRIMARIA)))
    
    if(lrow!=lcheck | lna!=lcheck){
      break
    }
    
    brbase <- brbase %>% 
      dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
      bind_rows(brbase_nokeys)
    print(paste0("LLAVES NAS: ", length(which(is.na(brbase$LLAVEPRIMARIA)))))
    write_xlsx(brbase, paste0("D:/!bso/accion/entrega/",myfile[i],".xlsx"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

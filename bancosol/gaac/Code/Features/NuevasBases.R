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

####____EXPLORATORY DATA____####
condon <- read_xlsx("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/DDBB_Condonaciones/BaseCondonaciones_2015_2023.xlsx", 
                    sheet = "Base",skip = 1)

condonSep <- condon %>% 
  dplyr::filter(Fecha>as.Date("2023-08-31"))

#LECTURA BASE SEPTIEMBRE PARA COMPARAR
condMes <- readxl::read_xlsx('D:/!bso/condonaciones/condon/Condonaciones202309.xlsx',
                             sheet = "Base Form") %>% 
  select(sucursal_tran, Modulo, Transaccion, Descripcion, Sucursal, Relacion, Fecha, Sucursal_operacion,
         Moneda, Cuenta, NombreCliente, Operacion, Rubro, capital, interes, interes_cte, interes_penal,
         formularios, gastos_jud, Asesor, NombreAsesor, Saldo, Calificacion, Monto_prevision,
         Instancia, Total_Cond_Cap = `Total Condonado Capital`, 
         Total_Cond_Int_Form_Jud = `Total Cond Intereses + Form + Gastos Jud`,
         Cond_Cap = `Cond Capital En $us`, Cond_Int = `Cond Intereses En $us`,
         Total_Cond_Cap_Int = `Total Cond cap + Int En $us`, REG=REGIONAL) %>% 
  glimpse()

####____CARGA DE PAQUETES, FUNCIONES Y OTROS____####
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
library(openxlsx)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____CUOTAS____####
plan <- read_xlsx("D:/!bso/requests/300923_SEPTIEMBRE_RTI_OK.xlsx", sheet="DDBB_CARTERA_BASE",
                  skip = 1)
names(plan)
plan <- plan %>% 
  select(1:27) %>% 
  mutate(ID=row_number()) %>% 
  relocate(ID, .before = "FECHA") %>% 
  mutate(TC = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                           "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib","6.Vivienda Controlada"), "TC","TNC")) 
names(plan)

planSinCuotas <- plan[plan$plazo==0,]

planX <- as_tibble(lapply(plan, rep, plan$plazo))
planFull <- planX %>% 
  mutate(MontoCuota = cuota) %>% 
  group_by(ID) %>% 
  mutate(Cuota = paste0("Cuota_",row_number())) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Cuota, values_from = MontoCuota, values_fill = 0) 

planFull2 <- planFull %>% 
  bind_rows(planSinCuotas)

names(planFull2)
planFull2 <- planFull2 %>% 
  mutate(across(29:272, ~ifelse(is.na(.x),0,.x)))

write_xlsx(planFull2, "D:/!bso/requests/DDBB_cuotas_230930_v2.xlsx")
# write.xlsx(planFull, "D:/!bso/requests/DDBB_cuotas_230930.xlsx")
# write_excel(planFull, "D:/!bso/requests/DDBB_cuotas_230930.xlsx")
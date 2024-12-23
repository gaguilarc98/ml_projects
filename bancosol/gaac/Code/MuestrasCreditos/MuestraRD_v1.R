#-------------------------
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
#--------------------------------
bdc <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraSep2022.txt', 
             encoding = 'Latin-1', fill = T)
base <- bdc %>% 
  mutate(fdes = lubridate::dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(fdes >= '2022-09-01' & fdes <= '2022-09-30') %>%
  dplyr::filter(MODULO != 131) %>% 
  select(AGENCIA, CTACLIENTE) %>%
  group_by(AGENCIA) %>% 
  tally() %>% 
  glimpse()
write.xlsx(base, 'D:/!bso/Muestra_sep2022.xlsx')

dfdraw <- bdc %>% 
  mutate(fdes = lubridate::dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(fdes >= '2022-09-01' & fdes <= '2022-09-30') %>% 
  dplyr::filter(MODULO != 131)
popExp <- dfdraw %>% 
  dplyr::filter(AGENCIA == 274 | AGENCIA == 267 | 
                  AGENCIA == 272 | AGENCIA == 601 |
                  AGENCIA == 221 | AGENCIA == 605 |
                  AGENCIA == 218 | AGENCIA == 101 |
                  AGENCIA == 222 | AGENCIA == 103 |
                  AGENCIA == 220 | AGENCIA == 107 |
                  AGENCIA == 219 | AGENCIA == 105 |
                  AGENCIA == 275 | AGENCIA == 106 |
                  AGENCIA == 608 | AGENCIA == 410 |
                  AGENCIA == 603 | AGENCIA == 409 |
                  AGENCIA == 902) %>% 
  arrange(AGENCIA)
write.xlsx(popExp, 'D:/!bso/Poblacion_RD_Sep2022.xlsx')
  
set.seed(1234)
lista <- bdc %>% 
  mutate(fdes = lubridate::dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(fdes >= '2022-09-01' & fdes <= '2022-09-30') %>% 
  dplyr::filter(MODULO != 131) %>% 
  left_join(base, by = 'AGENCIA') %>% 
  mutate(draw = runif(nrow(dfdraw), min = 0, max = 1)) %>% 
  group_by(AGENCIA) %>% 
  arrange(AGENCIA, desc(draw)) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos <= 31) %>% 
  dplyr::filter(AGENCIA == 274 | AGENCIA == 267 | 
                  AGENCIA == 272 | AGENCIA == 601 |
                  AGENCIA == 221 | AGENCIA == 605 |
                  AGENCIA == 218 | AGENCIA == 101 |
                  AGENCIA == 222 | AGENCIA == 103 |
                  AGENCIA == 220 | AGENCIA == 107 |
                  AGENCIA == 219 | AGENCIA == 105 |
                  AGENCIA == 275 | AGENCIA == 106 |
                  AGENCIA == 608 | AGENCIA == 410 |
                  AGENCIA == 603 | AGENCIA == 409 |
                  AGENCIA == 902) %>% 
  glimpse()
write.xlsx(lista, 'D:/!bso/Muestra_Sep2022_consolidada.xlsx')

spl <- group_split(lista, .keep = T)
spl[19]

for (i in 1:length(spl)) {
  l1 <- spl[[i]]
  sname <- as.character(l1$AGENCIA[1])
  print(sname)
  #write.xlsx(l1, 'D:/!bso/Listado_MuestrasRD_Ago2022.xlsx', 
  #          sheetName= sname, row.names=FALSE, append=TRUE)
}

dataset_names <- list('211' = spl[[1]], 
                      '216' = spl[[2]], 
                      '217' = spl[[3]],
                      '266' = spl[[4]],
                      '269' = spl[[5]],
                      '272' = spl[[6]],
                      '320' = spl[[7]],
                      '321' = spl[[8]],
                      '323' = spl[[9]],
                      '324' = spl[[10]],
                      '329' = spl[[11]],
                      '401' = spl[[12]],
                      '411' = spl[[13]],
                      '501' = spl[[14]],
                      '503' = spl[[15]],
                      '607' = spl[[16]],
                      '721' = spl[[17]],
                      '727' = spl[[18]],
                      '802' = spl[[19]])
openxlsx::write.xlsx(dataset_names, file = 'D:/!bso/Listado_MuestrasRD_Ago2022.xlsx') 

# Draw correction. This happened only in Ago 2022, sep 2022. This corrections seem 
# regular
set.seed(12345) # seed changes to create new sample
lista <- bdc %>% 
  mutate(fdes = lubridate::dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(fdes >= '2022-09-01' & fdes <= '2022-09-30') %>% 
  dplyr::filter(MODULO != 131) %>% 
  left_join(base, by = 'AGENCIA') %>% 
  mutate(draw = runif(nrow(dfdraw), min = 0, max = 1)) %>% 
  group_by(AGENCIA) %>% 
  arrange(AGENCIA, desc(draw)) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos <= 31) %>% 
  dplyr::filter(AGENCIA == 274 | AGENCIA == 267 | 
                  AGENCIA == 272 | AGENCIA == 275 ) %>% 
  glimpse()
write.xlsx(lista, 'D:/!bso/Muestra_Sep2022_consolidada_Correccion.xlsx')

popExp <- dfdraw %>% 
  dplyr::filter(AGENCIA == 274 | AGENCIA == 267 | 
                  AGENCIA == 272 | AGENCIA == 275 )
write.xlsx(popExp, 'D:/!bso/Poblacion_Sep2022_Correccion.xlsx')

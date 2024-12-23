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
library(skimr)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)


cod <- read.xlsx('C:/!bso/descripcion_caedec.xlsx') %>% 
  mutate(caedec2= as.integer(caedec2))


long_list <- c('ec_Ene2021.rds', 'ec_Feb2021.rds', 'ec_Mar2021.rds', 
               'ec_Abr2021.rds', 'ec_May2021.rds', 'ec_Jun2021.rds', 
               'ec_Jul2021.rds', 'ec_Ago2021.rds', 'ec_Sep2021.rds',
               'ec_Oct2021.rds', 'ec_Nov2021.rds', 'ec_Dic2021.rds', 
               'ec_Ene2022.rds', 'ec_Feb2022.rds', 'ec_Mar2022.rds',
               'ec_Abr2022.rds', 'ec_May2022.rds', 'ec_Jun2022.rds',
               'ec_Jul2022.rds', 'ec_Ago2022.rds', 'ec_Sep2022.rds',
               'ec_Oct2022.rds', 'ec_Nov2022.rds', 'ec_Dic2022.rds', 
               'ec_Ene2023.rds', 'ec_Feb2023.rds', 'ec_Mar2023.rds')
cae_list<-c()
for(i in 1:length(long_list)) {
  print(long_list[[i]])
  bdc_caedec <- readRDS(paste0('D:/!bso/girCartera/rds_v3/', long_list[i]))
  sumCae <-bdc_caedec %>%
  dplyr::filter(MODULO != 131) %>%
  # dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(rubDif = substr(RUBRO_CAPITAL_DIFERIDO,1,3))%>% 
  mutate(nops = ifelse(saldoCast > 0, 0, 1),
         nopsCast = ifelse(saldoCast > 0, 1, 0),
         nopsPar1 = ifelse(par1 > 0, 1, 0)) %>% 
  mutate(saldousdif = ifelse((rubDif == '131' | rubDif == '133' |
                                  rubDif == '134' | rubDif == '135' |
                                  rubDif == '136' | rubDif == '137') & MONEDA == 0 , 
                               as.numeric(SALDO_CAPITAL_DIFERIDO)/6.86, 
                               0)) %>% 
   mutate(saldousdif = ifelse((rubDif == '131' | rubDif == '133' |
                                  rubDif == '134' | rubDif == '135' |
                                  rubDif == '136' | rubDif == '137') & MONEDA != 0 , 
                               as.numeric(SALDO_CAPITAL_DIFERIDO), 
                               saldousdif)) %>% 
  select(saldous, saldoCast, nops, nopsCast, nopsPar1,
         par1, sucursal, CAEDEC_DEST, labGrupoD, monDate,  saldoRepPaR0,
         saldoMora, saldoReprog, saldousdif, DIASMORA) %>%
  mutate(saldousdifMora = ifelse(DIASMORA > 0, saldousdif, 0),
         saldousdifMora30 = ifelse(DIASMORA > 30, saldousdif, 0)) %>%
  group_by(labGrupoD, CAEDEC_DEST, sucursal, monDate) %>%
  summarise(saldous = sum(saldous, na.rm = T),
            saldoCast = sum(saldoCast, na.rm = T),
            par1 = sum(par1, na.rm = T),
            saldoMora = sum(saldoMora, na.rm =T),
            saldoReprog = sum(saldoReprog, na.rm =T),
            saldoRepPaR0 = sum(saldoRepPaR0, na.rm =T),
            nops = sum(nops, na.rm = T),
            nopsCast = sum(nopsCast, na.rm = T),
            nopsPar1 = sum(nopsPar1, na.rm = T),
            saldousdif = sum(saldousdif, na.rm =T),
            saldousdifMora = sum(saldousdifMora, na.rm =T),
            saldousdifMora30 = sum(saldousdifMora30, na.rm =T)) %>% 
  ungroup() %>%
  group_by(sucursal, monDate) %>%
  arrange(sucursal, desc(saldous)) %>%
  mutate(caedec2 = ifelse(row_number() > 50, 'Otros', as.character(CAEDEC_DEST)),
         lab2 = ifelse(row_number() > 50, 'Otros', labGrupoD)) %>%
  ungroup() %>%
  select(-CAEDEC_DEST, -labGrupoD) %>%
  group_by(sucursal, caedec2, lab2, monDate) %>%
  summarise(saldous = round(sum(saldous, na.rm = T)),
            saldoCast = round(sum(saldoCast, na.rm = T)),
            par1 = round(sum(par1, na.rm = T)),
            nops = round(sum(nops, na.rm = T)),
            nopsCast = round(sum(nopsCast, na.rm = T)),
            nopsPar1 = round(sum(nopsPar1, na.rm = T)),
            saldoMora = round(sum(saldoMora, na.rm =T)),
            saldoReprog = round(sum(saldoReprog, na.rm = T)),
            saldoRepPaR0 = round(sum(saldoRepPaR0, na.rm = T)),
            saldousdif= round(sum(saldousdif, na.rm = T)),
            saldousdifMora = round(sum(saldousdifMora, na.rm = T)),
            saldousdifMora30 = round(sum(saldousdifMora30, na.rm = T))) %>%
  mutate(par0Rel = round(par1/saldous*100, 2),
         `Mora 1 día + Saldo Castigado/Cartera Bruta (%)` = round((par1 + saldoCast)/saldous*100, 2),
         par30Rel= round(saldoMora/saldous*100, 2),
         par0reprog= round(saldoRepPaR0/saldoReprog*100, 2),
         parDif30 = round(saldousdifMora30/saldousdif, 2),
         parDif0 = round(saldousdifMora/saldousdif, 2),
         `Cartera reprogramada/Cartera Bruta (%)` = round(saldoReprog/saldous,2),
         `Cartera diferida/Cartera Bruta (%)` = round(saldousdif/saldous,2)) %>% 
  mutate(sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                                sucursal == '10' ~ 'El Alto',
                                sucursal == '2' ~ 'La Paz',
                                sucursal == '3' ~ 'Cochabamba',
                                sucursal == '4' ~ 'Oruro',
                                sucursal == '5' ~ 'Potosí',
                                sucursal == '6' ~ 'Tarija',
                                sucursal == '7' ~ 'Santa Cruz',
                                sucursal == '8' ~ 'Beni',
                                sucursal == '9' ~ 'Pando',)) 

  cae_list[[i]] <- sumCae
}

caeFinal <- bind_rows(cae_list) 
  
#################################################################################
# Long table
tabG <- caeFinal %>% 
  dplyr::rename(`Cartera Bruta (USD)` = saldous,
                `Mora 1 día (%)`= par0Rel,
                `Mora 30 días (%)`= par30Rel,
                `Mora 1 día Reprogramada (%)` = par0reprog,
                `Cartera en mora a 30 días (USD)`= saldoMora,
                `Cartera Reprogramada (USD)` = saldoReprog,
                `Cartera en mora 1 día (USD)` = par1,
                `Operaciones` = nops,
                `Operaciones Castigadas` = nopsCast,
                `Operaciones Mora 0 días` = nopsPar1,
                `Cartera Diferida (USD)` = saldousdif,
                `Cartera Diferida en Mora 1 día (USD)` = saldousdifMora,
                `Cartera Diferida en Mora 30 días (USD)` = saldousdifMora30,
                `Mora Cartera Diferida 30 días (%)`= parDif30,
                `Mora Cartera Diferida 1 día (%)`= parDif0,
                `Cartera Reprog. en Mora 1 día (USD)`= saldoRepPaR0,
                `Saldo Castigado (USD)` = saldoCast) %>% 
  # pivot_longer(!c(monDate, sucursal, caedec2, lab2)) %>%
  arrange(sucursal, caedec2, lab2, monDate) %>% 
  distinct_all() %>% 
  # pivot_wider(names_from =monDate, values_from =value) %>% 
  arrange(sucursal, lab2) %>% 
  ungroup() %>% 
  mutate(caedec2 = as.integer(caedec2)) %>% 
  left_join(cod, by = 'caedec2') %>% 
  replace_na(list(Descripción = 'Otros')) %>% 
  relocate(Descripción) %>% 
  arrange(sucursal, caedec2) %>% 
  # select(-ends_with('(%)')) %>% 
  dplyr::rename(CAEDEC = caedec2,
                Sucursal = sucursal,
                Grupo_CAEDEC = lab2) %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  relocate(Fecha)
saveRDS(tabG, 'C:/!bso/evolutivo_caedec/evolutivo_caedec.rds')
#==============================================================================
# Resultados gabo
gabo <- readRDS('C:/!bso/evolutivo_caedec/caedec_score.rds')
write.xlsx(gabo, 'C:/!bso/evolutivo_caedec/caedec_score.xlsx')
gabo_last <- gabo %>% 
  dplyr::filter(monDate == 'mar. 2023')

gph <- gabo_last %>% 
  ungroup() %>% 
  select(CAEDEC, `Cartera Bruta (USD)`, Score, 
         `Cartera en mora 1 día (USD)`, `Saldo Castigado (USD)`) %>% 
  group_by(CAEDEC) %>% 
  summarise(`Cartera Bruta (USD)` = sum(`Cartera Bruta (USD)`, na.rm = T),
            `Cartera en mora 1 día (USD)` = sum( `Cartera en mora 1 día (USD)`, na.rm = T),
            `Saldo Castigado (USD)` = sum(`Saldo Castigado (USD)`, na.rm = T),
            Score = mean(Score, na.rm = T)) %>% 
  mutate(`MMC (USD)` = `Cartera en mora 1 día (USD)` + `Saldo Castigado (USD)`,
         flag = ifelse(`MMC (USD)` < 1500000, 0, 1)) %>% 
  dplyr::filter(`MMC (USD)` < 1500000)

ggplot(gph, aes(x = round(`Cartera Bruta (USD)`/1000), 
                y =  round(`MMC (USD)`/1))) + 
  geom_point(aes(color = CAEDEC))

summary(gabo_last$Score)

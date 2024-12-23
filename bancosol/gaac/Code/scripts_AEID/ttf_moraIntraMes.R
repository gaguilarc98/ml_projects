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

#===========================================================
bdaily <- readRDS('C:/!bso/vipCartera/ddaily_raw.rds')

dailyMMop <- bdaily %>% 
  dplyr::filter(year >= 2022) %>% 
  select(DIASMORA, OPERACION, CTACLIENTE, dayDate) %>% 
  mutate(monDate = as.yearmon(dayDate)) %>%
  group_by(CTACLIENTE, OPERACION, monDate) %>%
  select(-dayDate) %>% 
  summarise_all(max, na.rm = T) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(maxMM = max(DIASMORA, na.rm = T)) %>% 
  dplyr::filter(maxMM > 0)
write_rds(dailyMMop, 'C:/!bso/vipCartera/ttf_im.rds')  

dailyMMop <- readRDS('C:/!bso/vipCartera/ttf_im.rds')
ttf <- dailyMMop %>%
  ungroup() %>% 
  group_by(CTACLIENTE, OPERACION) %>%
  arrange(CTACLIENTE, OPERACION, monDate) %>% 
  mutate(first = ifelse(DIASMORA > 0, 1, 0)) %>%
  slice(match(1, first)) %>% 
  dplyr::rename(first_occ = monDate,
                first_del = DIASMORA)
#==============================================================================
# keeping current cohort
bdcFull <- readRDS('C:/!bso/cosechas/bdcFullCosechas_mar2023.rds')

coseY_ori <- bdcFull %>% 
  mutate(par0 = ifelse(DIASMORA > 0, saldous, 0),
       opsPar0 = ifelse(par0 > 0, 1, 0)) %>% 
  select(montous, starts_with('saldo'), par0, monDate, opDes, opsPar0, cosechaM,
         OPERACION, OPERACION_ORI_REF, CTACLIENTE, fdes) %>% 
  ungroup() %>% 
  mutate(cosechaY = as.Date(cosechaM, frac = 1),
         cosechaY = year(cosechaY)) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% 
  mutate(hasRefin = sum(OPERACION_ORI_REF),
         yearRefin = ifelse(OPERACION_ORI_REF > 0, year(fdes), 0),
         maxRefin = max(yearRefin)) %>% # identificación de refinanciados
  #dplyr::filter(maxRefin == 2021) %>% 
  mutate(OPERACION_hist = ifelse(OPERACION_ORI_REF > 0, OPERACION_ORI_REF, OPERACION)) %>% # Identificación de # de operación original
  ungroup() %>% 
  group_by(OPERACION_hist) %>% 
  mutate(cosechaY_ori = min(cosechaY), 
         fdes_ori = min(fdes)) %>% # La cosecha original es la menor de las dos fechas (desembolso original y refinanciamiento)
  ungroup() %>% 
  select(fdes, fdes_ori, OPERACION, CTACLIENTE,  OPERACION_ORI_REF,
        hasRefin, maxRefin, yearRefin, OPERACION_hist, cosechaY,
        cosechaY_ori, monDate) %>% 
group_by(OPERACION, CTACLIENTE) %>% 
  summarise_all(min, na.rm = T)
write_rds(coseY_ori, 'C:/!bso/cosechas/coseY_ori_detailed_mar2023.rds')

coseY_ori <- readRDS('C:/!bso/cosechas/coseY_ori_detailed_mar2023.rds')
coseY_ori_22 <- coseY_ori %>% 
  dplyr::filter(cosechaY_ori >= 2022)
#==============================================================================
# current descriptives
bdc <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% 
  select(CTACLIENTE, OPERACION, saldous, CALIFICACION,
         previus, MODULO, ESTADO, ctaCont, sucursal, tipoCred,
         MONEDA, MONTO) %>%
  mutate(montous = ifelse(MONEDA == 0, MONTO/6.86, MONTO)) %>% 
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous > 1000 & montous <= 3000 ~'b. 1k-3k',
                            montous > 3000 & montous <= 5000 ~'c. 3k-5k',
                            montous > 5000 & montous <= 8000 ~'d. 5k-8k',
                            montous > 8000 & montous <= 10000 ~'e. 8k-10k',
                            montous > 10000 & montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k')) %>%
  mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                              sucursal == '10' ~ 'El Alto',
                              sucursal == '2' ~ 'La Paz',
                              sucursal == '3' ~ 'Cochabamba',
                              sucursal == '4' ~ 'Oruro',
                              sucursal == '5' ~ 'Potosí',
                              sucursal == '6' ~ 'Tarija',
                              sucursal == '7' ~ 'Santa Cruz',
                              sucursal == '8' ~ 'Beni',
                              sucursal == '9' ~ 'Pando',))

bdc_state <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% 
  select(CTACLIENTE, OPERACION, MODULO, ESTADO, ctaCont)

ttf_current <- coseY_ori_22 %>% 
  left_join(ttf, by = c('CTACLIENTE', 'OPERACION')) %>% 
  #dplyr::filter(maxMM > 0) %>% 
  mutate(dateOcc = as.Date(first_occ, frac = 1),
         ttf_im_months = as.integer(round(dateOcc-fdes_ori)/30),
         ttf_im_days = as.integer(dateOcc-fdes_ori)) %>% 
  left_join(bdc_state, by = c('CTACLIENTE', 'OPERACION'))

ttf_current_2022 <- ttf_current %>% 
  dplyr::filter(cosechaY_ori == 2022) %>% 
  left_join(bdc, by = c('CTACLIENTE', 'OPERACION', 'ESTADO', 'MODULO', 'ctaCont'))

summary(ttf_current_2022$ttf_im_months) 
summary(ttf_current_2022$ttf_im_days) 
length(which(is.na(ttf_current$ESTADO)))
length(which(is.na(ttf_current_2022$ESTADO)))

gph_all <- ttf_current_2022 %>% 
  dplyr::filter(!is.na(ESTADO)) %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(cosechaY_ori == 2022) %>% 
  mutate(ttf_im_months = ifelse(is.na(ttf_im_months), 0, ttf_im_months),
         ttf_im_days = ifelse(is.na(ttf_im_days), 0, ttf_im_days),
         bin_ttf_im_days = case_when(ttf_im_days == 0 ~ '1. O días',
                                     ttf_im_days >= 1 ~ '2. 1+ días',
                                     TRUE ~ 'NA') )
table(gph_all$ESTADO)
table(gph_all$ESTADO, gph_all$bin_ttf_im_days)

gph_conMM <- gph_all %>% 
  dplyr::filter(maxMM > 0)

ggplot(gph, aes(x = ttf_im_days)) + 
  geom_density()

avg <- mean(gph_conMM$ttf_im_days)
p10 <-quantile(gph_conMM$ttf_im_days, c(0.1))
p25 <-quantile(gph_conMM$ttf_im_days, c(0.25))
ggplot(gph_conMM, aes(x = ttf_im_days)) + 
  geom_density(fill = 'navy', alpha = 0.25) + theme_minimal() + 
  ylab('Densidad') + xlab('Días desde el desembolso hasta el imcumplimiento') + 
  geom_vline(xintercept = avg, size = 1.25) + 
  geom_vline(xintercept = p10, color = 'red', size = 1.25) + 
  geom_vline(xintercept = p25, color = 'darkorange', size = 1.25) +
  annotate("text", x = 210, y = 0.006, label = "Promedio: 160 días", size = 5) + 
  annotate("text", x = 125, y = 0.007, label = "p25: 96 días", size = 5) +
  annotate("text", x = 40, y = 0.006, label = "p10: 68 días", size = 5)
ggsave('C:/!bso/ttf/densidad_cose22.png')


tab_tipoCred_1 <- gph_conMM %>% 
  ungroup() %>% 
  select(tipoCred, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(tipoCred, pct) %>% 
  summarise(saldous = sum(saldous, na.rm = T)) %>% 
  # avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
  # med_ttf_im_days = median(ttf_im_days, na.rm = T),
  # p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
  # p10_ttf_im_days = quantile(ttf_im_days, c(0.10)),
  # nops = n()) %>% 
  pivot_wider(names_from = pct, values_from = saldous) %>% 
  adorn_totals('row')

tab_tipoCred_2 <- gph_conMM %>% 
  ungroup() %>% 
  select(tipoCred, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(tipoCred, pct) %>% 
  summarise(nops = n()) %>% 
  # avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
  # med_ttf_im_days = median(ttf_im_days, na.rm = T),
  # p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
  # p10_ttf_im_days = quantile(ttf_im_days, c(0.10)),
  # nops = n()) %>% 
  pivot_wider(names_from = pct, values_from = nops) %>% 
  adorn_totals('row')

tab_tipoCred <- tab_tipoCred_1 %>% 
  bind_cols(tab_tipoCred_2)

tab_tipoCred_3 <- gph_conMM %>% 
  ungroup() %>% 
  select(tipoCred, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(tipoCred) %>% 
  summarise(avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
            p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
            p10_ttf_im_days = quantile(ttf_im_days, c(0.10)))

tab_sucursal_1 <- gph_conMM %>% 
  ungroup() %>% 
  select(Sucursal, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(Sucursal, pct) %>% 
  summarise(saldous = sum(saldous, na.rm = T)) %>% 
  # avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
  # med_ttf_im_days = median(ttf_im_days, na.rm = T),
  # p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
  # p10_ttf_im_days = quantile(ttf_im_days, c(0.10)),
  # nops = n()) %>% 
  pivot_wider(names_from = pct, values_from = saldous) %>% 
  adorn_totals('row')

tab_sucursal_2 <- gph_conMM %>% 
  ungroup() %>% 
  select(Sucursal, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(Sucursal, pct) %>% 
  summarise(nops = n()) %>% 
  # avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
  # med_ttf_im_days = median(ttf_im_days, na.rm = T),
  # p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
  # p10_ttf_im_days = quantile(ttf_im_days, c(0.10)),
  # nops = n()) %>% 
  pivot_wider(names_from = pct, values_from = nops) %>% 
  adorn_totals('row')

tab_sucursal <- tab_sucursal_1 %>% 
  bind_cols(tab_sucursal_2)

tab_sucursal_3 <- gph_conMM %>% 
  ungroup() %>% 
  select(Sucursal, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(Sucursal) %>% 
  summarise(avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
            p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
            p10_ttf_im_days = quantile(ttf_im_days, c(0.10)))

# tab_rangom <- gph_conMM %>% 
#   ungroup() %>% 
#   select(rangom, saldous, previus, ttf_im_days) %>% 
#   group_by(rangom) %>% 
#   summarise(saldous = sum(saldous, na.rm = T),
#             previus = sum(previus, na.rm = T),
#             avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
#             med_ttf_im_days = median(ttf_im_days, na.rm = T),
#             p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
#             p10_ttf_im_days = quantile(ttf_im_days, c(0.10)),
#             nops = n())

tab_rangom_1 <- gph_conMM %>% 
  ungroup() %>% 
  select(rangom, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(rangom, pct) %>% 
  summarise(saldous = sum(saldous, na.rm = T)) %>% 
            # avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
            # med_ttf_im_days = median(ttf_im_days, na.rm = T),
            # p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
            # p10_ttf_im_days = quantile(ttf_im_days, c(0.10)),
            # nops = n()) %>% 
  pivot_wider(names_from = pct, values_from = saldous) %>% 
  adorn_totals('row')

tab_rangom_2 <- gph_conMM %>% 
  ungroup() %>% 
  select(rangom, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(rangom, pct) %>% 
  summarise(nops = n()) %>% 
  # avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
  # med_ttf_im_days = median(ttf_im_days, na.rm = T),
  # p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
  # p10_ttf_im_days = quantile(ttf_im_days, c(0.10)),
  # nops = n()) %>% 
  pivot_wider(names_from = pct, values_from = nops) %>% 
  adorn_totals('row')

tab_rangom_3 <- gph_conMM %>% 
  ungroup() %>% 
  select(rangom, saldous, previus, ttf_im_days) %>% 
  mutate(pct = case_when(ttf_im_days <= 68 ~ '1. Pct 10',
                         ttf_im_days > 68 & ttf_im_days <= 96 ~ '2. Pct 25',
                         ttf_im_days > 96 ~ '3. Ops. Restantes',
                         TRUE ~ 'NA')) %>% 
  group_by(rangom) %>% 
  summarise(avg_ttf_im_days = mean(ttf_im_days, na.rm = T),
  p25_ttf_im_days = quantile(ttf_im_days, c(0.25)),
  p10_ttf_im_days = quantile(ttf_im_days, c(0.10)))

tab_rangom <- tab_rangom_1 %>% 
 bind_cols(tab_rangom_2)

tabList = list(tab_sucursal = tab_sucursal, tab_rangom = tab_rangom, 
               tab_tipoCred = tab_tipoCred)
write.xlsx(tabList, 'C:/!bso/ttf/ppt_ttf_mar23.xlsx')

tabList = list(tab_sucursal = tab_sucursal_3, tab_rangom = tab_rangom_3, 
               tab_tipoCred = tab_tipoCred_3)
write.xlsx(tabList, 'C:/!bso/ttf/ppt_ttf_mar23_pt2.xlsx')

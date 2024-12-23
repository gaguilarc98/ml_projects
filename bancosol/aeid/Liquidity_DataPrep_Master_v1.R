remove(list = ls())
gc()
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
library(readstata13)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RcppRoll)
remove(list = ls())
gc()

data_fsh_3 <- read.csv('D:/!bso/datax/eeff_n3_jul22.csv')
df2 <- data_fsh_3

data_fsh_5 <- read.csv('D:/!bso/datax/eeff_n5_jul22.csv')
df5 <- data_fsh_5

#sapply(seq(1,207,by=8),function(i) rowSums(df[,i:(i+1)]))

for (j in seq(1,6, by=1)){
  for (i in seq(1,9, by=1)){
    ac0 = paste0('cta_', j)
    ac1 = paste0(ac0, i)
    vna = paste0(ac1, 0)
    print(ac0)
    print(ac1)
    print(vna)
    
    
    df2 <- df2 %>%
      mutate(!!vna := rowSums(select(., starts_with(ac1))))
  }
}

for (i in seq(1,6, by=1)){
  acc = paste0('cta_', i)
  vna = paste0(acc, '00')
  print(acc)
  print(vna)
  
  df2 <- df2 %>%
    mutate(!!vna := rowSums(select(., starts_with(acc) & ends_with('0'))))
}

liq_3 <- df2 %>%
  select(monyear, tipo, entidad, moneda,
         cta_117, cta_121, cta_122, cta_123, cta_124, cta_126, cta_211, 
         cta_212, cta_281, cta_282, cta_210, cta_280, cta_212, cta_282,
         cta_213, cta_215, cta_283, cta_285, cta_235)

liq_5 <- df5 %>%
  select(monyear, tipo, entidad, moneda,
         cta_111.01 , cta_111.04 , cta_112.01 , cta_112.05 , cta_113.01,
         cta_114.01 , cta_115.01, cta_127.11, cta_127.17 , cta_232.01, 
         cta_235.01 , cta_235.07 , cta_235.08, cta_235.13, cta_211.01,
         cta_211.02, cta_281.01, cta_281.02, cta_231.03, cta_231.04,
         cta_231.05, cta_231.06)

liq_m <- liq_5 %>% 
  left_join(liq_3, by = c('monyear', 'tipo', 'entidad', 'moneda'))

write.csv(liq_m, 'D:/!bso/Liquidity/Liquidity_SF_monthly.csv', row.names = F)
#----------------------------------------------------------------
# final monthly sets by currency
liq_m_e_co <- liq_m %>% 
  select(-moneda, -tipo) %>% 
  mutate(date = ymd(monyear)) %>% 
  mutate(monyear = as.yearmon(date)) %>% 
  mutate(date = as.Date(monyear, frac = 1)) %>% 
  select(-monyear) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  group_by(date, entidad) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01 + cta_281.02) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  #select(date, entidad, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
  #      r2, r23, r3, r7, r4) %>% 
  rename_at(vars(-date, -entidad), function(x) paste0(x, '_m_e_co')) %>% 
  pivot_longer(!c(date, entidad)) %>% 
  dplyr::rename(id = entidad) %>% 
  arrange(id, name, date) %>% 
  glimpse()

write.csv(liq_m_e_co, 'D:/!bso/Liquidity/liq_m_e_co.csv', row.names = F)

liq_m_t_co <- liq_m %>% 
  select(-moneda, -tipo) %>% 
  mutate(date = ymd(monyear)) %>% 
  mutate(monyear = as.yearmon(date)) %>% 
  mutate(date = as.Date(monyear, frac = 1)) %>% 
  select(-monyear) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  mutate(id = case_when(entidad == 'BSO'~'BancoSol',
                           entidad == 'BPR' | entidad == 'BIE' |
                             entidad == 'PEF' | entidad == 'PCO' |
                             entidad == 'BFO' ~ 'Bancos MF',
                           TRUE ~ 'Bancos Múltiples')) %>% 
  select(-entidad) %>% 
  group_by(date, id) %>% 
  summarise_all(sum, na.rm = T) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01 + cta_281.02) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, id, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_m_t_co')) %>% 
  pivot_longer(!c(date, id)) %>% 
  arrange(id, name, date) %>% 
  glimpse()
write.csv(liq_m_t_co, 'D:/!bso/Liquidity/liq_m_t_co.csv', row.names = F)

#---
liq_m_e_mn <- liq_m %>% 
  dplyr::filter(moneda == 'MN'| moneda == 'UFV') %>% 
  select(-moneda, -tipo) %>% 
  mutate(date = ymd(monyear)) %>% 
  mutate(monyear = as.yearmon(date)) %>% 
  mutate(date = as.Date(monyear, frac = 1)) %>% 
  select(-monyear) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  group_by(date, entidad) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01 + cta_281.02) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, entidad, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  dplyr::rename(id = entidad) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_m_e_mn')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_m_e_mn, 'D:/!bso/Liquidity/liq_m_e_mn.csv', row.names = F)


liq_m_t_mn <- liq_m %>% 
  dplyr::filter(moneda == 'MN'| moneda == 'UFV') %>% 
  select(-moneda, -tipo) %>% 
  mutate(date = ymd(monyear)) %>% 
  mutate(monyear = as.yearmon(date)) %>% 
  mutate(date = as.Date(monyear, frac = 1)) %>% 
  select(-monyear) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  mutate(sectorSF = case_when(entidad == 'BSO'~'BancoSol',
                              entidad == 'BPR' | entidad == 'BIE' |
                                entidad == 'PEF' | entidad == 'PCO' |
                                entidad == 'BFO' ~ 'Bancos MF',
                              TRUE ~ 'Bancos Múltiples')) %>% 
  select(-entidad) %>% 
  group_by(date, sectorSF) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01 + cta_281.02) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, sectorSF, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  dplyr::rename(id = sectorSF) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_m_t_mn')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_m_t_mn, 'D:/!bso/Liquidity/liq_m_t_mn.csv', row.names = F)

#---
liq_m_e_me <- liq_m %>% 
  dplyr::filter(moneda == 'ME'| moneda == 'MNMV') %>% 
  select(-moneda, -tipo) %>% 
  mutate(date = ymd(monyear)) %>% 
  mutate(monyear = as.yearmon(date)) %>% 
  mutate(date = as.Date(monyear, frac = 1)) %>% 
  select(-monyear) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  group_by(date, entidad) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01 + cta_281.02) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, entidad, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  dplyr::rename(id = entidad) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_m_e_me')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_m_e_me, 'D:/!bso/Liquidity/liq_m_e_me.csv', row.names = F)


liq_m_t_me <- liq_m %>% 
  dplyr::filter(moneda == 'ME'| moneda == 'MNMV') %>%
  select(-moneda, -tipo) %>% 
  mutate(date = ymd(monyear)) %>% 
  mutate(monyear = as.yearmon(date)) %>% 
  mutate(date = as.Date(monyear, frac = 1)) %>% 
  select(-monyear) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  mutate(sectorSF = case_when(entidad == 'BSO'~'BancoSol',
                              entidad == 'BPR' | entidad == 'BIE' |
                                entidad == 'PEF' | entidad == 'PCO' |
                                entidad == 'BFO' ~ 'Bancos MF',
                              TRUE ~ 'Bancos Múltiples')) %>% 
  select(-entidad) %>% 
  group_by(date, sectorSF) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01 + cta_281.02) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, sectorSF, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  dplyr::rename(id = sectorSF) %>%
  rename_at(vars(-date, -id), function(x) paste0(x, '_m_t_me')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_m_t_me, 'D:/!bso/Liquidity/liq_m_t_me.csv', row.names = F)

final_data_m <- liq_m_e_co %>% 
  bind_rows(liq_m_e_me) %>%
  bind_rows(liq_m_e_mn) %>%
  bind_rows(liq_m_t_co) %>%
  bind_rows(liq_m_t_me) %>%
  bind_rows(liq_m_t_mn) %>% 
  distinct(date, id, name, value)
write.csv(final_data_m, 'D:/!bso/Liquidity/liquidity_monthly_data.csv', row.names = F)
#-----------------------------------------
# daily sets
# level 3 daily data
eeff3 <- read.csv('D:/!bso/datax/eeff_n3_daily_all.csv')
df2 <- eeff3
for (j in seq(1,6, by=1)){
  for (i in seq(1,9, by=1)){
    ac0 = paste0('cta_', j)
    ac1 = paste0(ac0, i)
    vna = paste0(ac1, 0)
    print(ac0)
    print(ac1)
    print(vna)
    
    
    df2 <- df2 %>%
      mutate(!!vna := rowSums(select(., starts_with(ac1))))
  }
}

for (i in seq(1,6, by=1)){
  acc = paste0('cta_', i)
  vna = paste0(acc, '00')
  print(acc)
  print(vna)
  
  df2 <- df2 %>%
    mutate(!!vna := rowSums(select(., starts_with(acc) & ends_with('0'))))
}
#-------------------------------
# level 5 data
eeff5_bso <- read.csv('D:/!bso/datax/EEFF_bso_n5_daily_all.csv')
eeff5_bmi <- read.csv('D:/!bso/datax/EEFF_BMI_n5_daily_all.csv')
eeff5_bmu <- read.csv('D:/!bso/datax/EEFF_BMU_n5_daily_all.csv')

eeff5_all <- eeff5_bso %>% 
  bind_rows(eeff5_bmi) %>% 
  bind_rows(eeff5_bmu)

# No 114.01 or 231.06
liq_3d <- df2 %>%
  select(dayDate, tipo, entidad, moneda,
         cta_117, cta_121, cta_122, cta_123, cta_124, cta_126, cta_211, 
         cta_212, cta_281, cta_282, cta_210, cta_280, cta_212, cta_282,
         cta_213, cta_215, cta_283, cta_285, cta_235)

liq_5d <- eeff5_all %>%
  select(dayDate, tipo, entidad, moneda,
         cta_111.01 , cta_111.04 , cta_112.01  , cta_113.01,
         cta_115.01, cta_127.11, cta_127.17 , cta_232.01, 
         cta_235.01 , cta_235.07 , cta_235.08, cta_235.13, cta_211.01,
         cta_211.02, cta_281.01, cta_231.03, cta_231.04,
         cta_231.05)

liq_d <- liq_3d %>% 
  left_join(liq_5d, by = c('dayDate', 'tipo', 'entidad', 'moneda')) %>% 
  mutate(date = ymd(dayDate)) %>%
  select(-dayDate) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::filter(date >= '2012-01-01') %>% 
  glimpse()

write.csv(liq_d, 'D:/!bso/Liquidity/Liquidity_SF_daily.csv', row.names = F)

#-------------------------------------------------------------------------------
# final daily sets by currency
# no 281.02
liq_d_e_co <- liq_d %>% 
  select(-moneda, -tipo) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  group_by(date, entidad) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01  + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, entidad, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  rename_at(vars(-date, -entidad), function(x) paste0(x, '_d_e_co')) %>% 
  pivot_longer(!c(date, entidad)) %>% 
  dplyr::rename(id = entidad) %>% 
  glimpse()

write.csv(liq_d_e_co, 'D:/!bso/Liquidity/liq_d_e_co.csv', row.names = F)

liq_d_t_co <- liq_d %>% 
  select(-moneda, -tipo) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  mutate(id = case_when(entidad == 'BSO'~'BancoSol',
                        entidad == 'BPR' | entidad == 'BIE' |
                          entidad == 'PEF' | entidad == 'PCO' |
                          entidad == 'BFO' ~ 'Bancos MF',
                        TRUE ~ 'Bancos Múltiples')) %>% 
  select(-entidad) %>% 
  group_by(date, id) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01  + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, id, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_d_t_co')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_d_t_co, 'D:/!bso/Liquidity/liq_d_t_co.csv', row.names = F)

#---
liq_d_e_mn <- liq_d %>% 
  dplyr::filter(moneda == 'MN'| moneda == 'UFV') %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  select(-moneda, -tipo) %>% 
  group_by(date, entidad) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01  + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, entidad, actliq, totalDP, totalCA, totalPF, totalEI) %>% 
  dplyr::rename(id = entidad) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_d_e_mn')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_d_e_mn, 'D:/!bso/Liquidity/liq_d_e_mn.csv', row.names = F)


liq_d_t_mn <- liq_d %>% 
  dplyr::filter(moneda == 'MN'| moneda == 'UFV') %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  mutate(sectorSF = case_when(entidad == 'BSO'~'BancoSol',
                              entidad == 'BPR' | entidad == 'BIE' |
                                entidad == 'PEF' | entidad == 'PCO' |
                                entidad == 'BFO' ~ 'Bancos MF',
                              TRUE ~ 'Bancos Múltiples')) %>% 
  select(-entidad, -moneda, -tipo) %>% 
  group_by(date, sectorSF) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01  + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, sectorSF, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  dplyr::rename(id = sectorSF) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_d_t_mn')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_d_t_mn, 'D:/!bso/Liquidity/liq_d_t_mn.csv', row.names = F)

#---
liq_d_e_me <- liq_d %>% 
  dplyr::filter(moneda == 'ME'| moneda == 'MNMV') %>% 
  select(-moneda, -tipo) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  group_by(date, entidad) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01  + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, entidad, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  dplyr::rename(id = entidad) %>% 
  rename_at(vars(-date, -id), function(x) paste0(x, '_d_e_me')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_d_e_me, 'D:/!bso/Liquidity/liq_d_e_me.csv', row.names = F)


liq_d_t_me <- liq_d %>% 
  dplyr::filter(moneda == 'ME'| moneda == 'MNMV') %>%
  select(-moneda, -tipo) %>% 
  dplyr::filter(entidad != 'BNA' & entidad != 'BDB') %>% 
  mutate(sectorSF = case_when(entidad == 'BSO'~'BancoSol',
                              entidad == 'BPR' | entidad == 'BIE' |
                                entidad == 'PEF' | entidad == 'PCO' |
                                entidad == 'BFO' ~ 'Bancos MF',
                              TRUE ~ 'Bancos Múltiples')) %>% 
  select(-entidad) %>% 
  group_by(date, sectorSF) %>% 
  summarise_all(sum) %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01  + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  mutate(totalCC = cta_211.01 + cta_211.02 + cta_281.01) %>% 
  mutate(r2 = totalCC/totalDP) %>% 
  mutate(r3 = totalCA/totalDP) %>% 
  mutate(r4 = totalPF/ totalDP) %>% 
  mutate(r23 = (totalCC + totalCA)/totalDP) %>% 
  mutate(r7 = totalEI/totalDP) %>% 
  select(date, sectorSF, actliq, totalDP, totalCA, totalPF, totalEI, totalCC,
         r2, r23, r3, r7, r4) %>% 
  dplyr::rename(id = sectorSF) %>%
  rename_at(vars(-date, -id), function(x) paste0(x, '_d_t_me')) %>% 
  pivot_longer(!c(date, id)) %>% 
  glimpse()
write.csv(liq_d_t_me, 'D:/!bso/Liquidity/liq_d_t_me.csv', row.names = F)

final_data_d <- liq_d_e_co %>% 
  bind_rows(liq_d_e_me) %>%
  bind_rows(liq_d_e_mn) %>%
  bind_rows(liq_d_t_co) %>%
  bind_rows(liq_d_t_me) %>%
  bind_rows(liq_d_t_mn) %>% 
  distinct(date, id, name, value)
write.csv(final_data_d, 'D:/!bso/Liquidity/liquidity_daily_data.csv', row.names = F)
################################################################################
#-------------------------------
# data reading
lia <- read.csv('D:/!bso/Liquidity/liabilities_ST.csv') %>% 
  mutate(date = ymd(date))
ass_co <- read.csv('D:/!bso/Liquidity/eeff_liquidity_daily_bso_co.csv') %>% 
  mutate(date = ymd(date)) %>% 
  select(act, date) %>% 
  dplyr::rename(actLiq = act)
ass_mn <- read.csv('D:/!bso/Liquidity/eeff_liquidity_daily_bso_mn.csv') %>% 
  mutate(date = ymd(date)) %>% 
  select(act, date) %>% 
  dplyr::rename(actLiq = act)
ass_me <- read.csv('D:/!bso/Liquidity/eeff_liquidity_daily_bso_me.csv') %>% 
  mutate(date = ymd(date)) %>% 
  select(act, date) %>% 
  dplyr::rename(actLiq = act)
#-------------------------------
# Aggregate ratio
l = 30
liqAgg <- lia %>% 
  select(date, dpf, cah) %>% 
  group_by(date) %>% 
  summarise_all(sum) %>% 
  mutate(liqLia = dpf + cah) %>% 
  left_join(ass_co, by = 'date') %>%
  mutate(ratio = actLiq/liqLia) %>% 
  dplyr::filter(!is.na(ratio)) %>% 
  #ungroup() %>% 
  arrange(date) %>% 
  mutate(dif = ratio - dplyr::lag(ratio)) %>% 
  tq_mutate(select  = cah, mutate_fun = lag.xts, k = -l) %>%  
  dplyr::rename(l_cah = cah..1) %>% 
  mutate(d_cah = l_cah - cah) %>% 
  glimpse()

gph <- liqAgg %>% 
  select(date, ratio) %>% 
  dplyr::filter(!is.na(ratio)) %>% 
  dplyr::rename(value = ratio) %>% 
  #dplyr::filter(date <= '2022-07-31') %>% 
  #dplyr::filter(value < 25000000) %>% # this is the outlier filtering
  glimpse()
gph %>% 
  plot_time_series(date, value, .interactive = TRUE)
write.csv(gph, 'D:/!bso/Liquidity/ratio1_daily_co.csv', row.names = F)

liqAgg <- lia %>% 
  dplyr::filter(cur == 0) %>% 
  select(date, dpf, cah) %>% 
  group_by(date) %>% 
  summarise_all(sum) %>% 
  mutate(liqLia = dpf + cah) %>% 
  left_join(ass_mn, by = 'date') %>%
  mutate(ratio = actLiq/liqLia) %>% 
  dplyr::filter(!is.na(ratio)) %>% 
  #ungroup() %>% 
  arrange(date) %>% 
  mutate(dif = ratio - dplyr::lag(ratio)) %>% 
  tq_mutate(select  = cah, mutate_fun = lag.xts, k = -l) %>%  
  dplyr::rename(l_cah = cah..1) %>% 
  mutate(d_cah = l_cah - cah) %>% 
  glimpse()

gph <- liqAgg %>% 
  select(date, ratio) %>% 
  dplyr::filter(!is.na(ratio)) %>% 
  dplyr::rename(value = ratio) %>% 
  #dplyr::filter(date <= '2022-07-31') %>% 
  #dplyr::filter(value < 25000000) %>% # this is the outlier filtering
  glimpse()
gph %>% 
  plot_time_series(date, value, .interactive = TRUE)
write.csv(gph, 'D:/!bso/Liquidity/ratio1_daily_mn.csv', row.names = F)

liqAgg <- lia %>% 
  dplyr::filter(cur == 101) %>% 
  select(date, dpf, cah) %>% 
  group_by(date) %>% 
  summarise_all(sum) %>% 
  mutate(liqLia = dpf + cah) %>% 
  left_join(ass_me, by = 'date') %>%
  mutate(ratio = actLiq/liqLia) %>% 
  dplyr::filter(!is.na(ratio)) %>% 
  #ungroup() %>% 
  arrange(date) %>% 
  mutate(dif = ratio - dplyr::lag(ratio)) %>% 
  tq_mutate(select  = cah, mutate_fun = lag.xts, k = -l) %>%  
  dplyr::rename(l_cah = cah..1) %>% 
  mutate(d_cah = l_cah - cah) %>% 
  glimpse()

gph <- liqAgg %>% 
  select(date, ratio) %>% 
  dplyr::filter(!is.na(ratio)) %>% 
  dplyr::rename(value = ratio) %>% 
  #dplyr::filter(date <= '2022-07-31') %>% 
  #dplyr::filter(value < 25000000) %>% # this is the outlier filtering
  glimpse()
gph %>% 
  plot_time_series(date, value, .interactive = TRUE)
write.csv(gph, 'D:/!bso/Liquidity/ratio1_daily_me.csv', row.names = F)


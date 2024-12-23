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

cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#bdc <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraEne2021.txt', encoding = 'Latin-1')

#-------------------------------------
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
# eeff5_bso <- read.csv('D:/!bso/datax/EEFF_BSO_n5_daily_all.csv')
# eeff5_bmi <- read.csv('D:/!bso/datax/EEFF_BMI_n5_daily_all.csv')
# eeff5_bmu <- read.csv('D:/!bso/datax/EEFF_BMU_n5_daily_all.csv')

# eeff5_all <- eeff5_bso %>% 
#   bind_rows(eeff5_bmi) %>% 
#   bind_rows(eeff5_bmu)
eeff5_all <- read.csv('D:/!bso/datax/eeff_n5_daily_all.csv')
#-------------------------------
# final join for levels 3 and 5
eeff_daily_all <- eeff5_all %>% 
  left_join(df2, by = c('dayDate', 'tipo', 'entidad', 'moneda', 'month', 'year')) %>% 
  mutate(date = ymd(dayDate)) %>%
  select(-dayDate) %>% 
  replace(is.na(.), 0) %>% 
  glimpse()

# No 114.01 or 231.06
liq <- eeff_daily_all %>% 
  select(date, tipo, entidad, moneda, cta_117, cta_121, cta_122, cta_123, 
         cta_124, cta_126, cta_211, cta_210, cta_280, cta_212, cta_282,
         cta_212, cta_281, cta_282, cta_213, cta_215, cta_283, cta_285,
         cta_111.01 , cta_111.04 , cta_112.01 , cta_112.05 , cta_113.01,
         cta_114.01, cta_115.01, cta_127.11, cta_127.17 , cta_232.01, 
         cta_235.01 , cta_235.07 , cta_235.08, cta_235.13, 
         cta_231.03 , cta_231.04 , cta_231.05 , cta_235)

act_co <- liq %>% 
  select(-entidad, -moneda) %>% 
  group_by(date, tipo) %>% 
  summarise_all(sum) %>% 
  dplyr::filter(tipo == 'BancoSol') %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  select(date, actliq, totalDP, totalCA, totalPF, totalEI) %>% 
  ungroup() %>% 
  dplyr::rename(act = actliq) %>%
  select(date, act, totalDP, totalCA, totalPF, totalEI) %>% 
  glimpse()
write.csv(act_co, 'D:/!bso/Liquidity/eeff_liquidity_daily_bso_co.csv', row.names = F)

act_mn <- liq %>% 
  dplyr::filter(moneda == 'MN' | moneda == 'UFV') %>% 
  select(-entidad, -moneda) %>% 
  group_by(date, tipo) %>% 
  summarise_all(sum) %>% 
  dplyr::filter(tipo == 'BancoSol') %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  select(date, actliq, totalDP, totalCA, totalPF, totalEI) %>% 
  ungroup() %>% 
  dplyr::rename(act = actliq) %>%
  select(date, act, totalDP, totalCA, totalPF, totalEI) %>% 
  glimpse()
write.csv(act_mn, 'D:/!bso/Liquidity/eeff_liquidity_daily_bso_mn.csv', row.names = F)

act_me <- liq %>% 
  dplyr::filter(moneda == 'ME' | moneda == 'MNMV') %>% 
  select(-entidad, -moneda) %>% 
  group_by(date, tipo) %>% 
  summarise_all(sum) %>% 
  dplyr::filter(tipo == 'BancoSol') %>% 
  mutate(actliq =cta_111.01 +cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(totalDP = cta_210 + cta_280) %>% 
  mutate(totalCA = cta_212 + cta_282) %>%
  mutate(totalPF = cta_213 + cta_215 + cta_283 + cta_285) %>%
  mutate(totalEI = cta_231.03 + cta_231.04 + cta_231.05 + cta_235) %>% 
  select(date, actliq, totalDP, totalCA, totalPF, totalEI) %>% 
  ungroup() %>% 
  dplyr::rename(act = actliq) %>%
  select(date, act, totalDP, totalCA, totalPF, totalEI) %>% 
  glimpse()
write.csv(act_me, 'D:/!bso/Liquidity/eeff_liquidity_daily_bso_me.csv', row.names = F)



ggplot(act_m, aes(x = date, y = totalDP)) + geom_line(group = 1)
#-------------------------------------
# liquid assets from daily EEFF, only for years in which we have daily data for 
# This csv file comes from python

act_final <- eeff_daily_all %>% 
  dplyr::filter(entidad == 'BSO') %>% 
  #mutate(date = ymd(dayDate)) %>% 
  mutate(cta_117 = cta_117.01 + cta_117.02 + cta_117.03 + cta_117.04) %>% 
  mutate(cta_121 = cta_121.99) %>% 
  mutate(cta_122 = cta_122.01 + cta_122.02 + cta_122.03 + cta_122.06 + 
           cta_122.99) %>% 
  mutate(cta_123 = cta_123.01 + cta_123.98) %>% 
  mutate(cta_124 = cta_124.01 + cta_124.02 + cta_124.03 + cta_124.05 +
           cta_124.05.1 + cta_124.99) %>% 
  mutate(cta_126 = cta_126.01 + cta_126.02 + cta_126.03) %>% 
  mutate(act = cta_111.01 + cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_114.01 + cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(actE = cta_111.01 + cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_114.01 + cta_115.01 + cta_117 + cta_121 + (cta_122*0.5) +
           cta_123 + cta_124 + (cta_126*0.7) + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  select(date, act, actE) %>% 
  dplyr::filter(date >= '2021-11-01') %>% 
  group_by(date) %>% 
  summarise_all(sum) %>% 
  mutate(pctLass = actE/act) %>% 
  glimpse()
write.csv(act_final, 'D:/!bso/Liquidity/assets_ST_co.csv', row.names = F)

gph <- act_final %>% 
  select(act, actE, date) %>% 
  pivot_longer(!date)
ggplot(gph, aes(x = date, y = value, color = name)) + geom_line(size =1.25) +
  theme_minimal() + ylab('Activo CP') + 
  theme(legend.position = 'NONE')
act_final <- dfass %>% 
  dplyr::filter(entidad == 'BSO') %>% 
  mutate(date = ymd(dayDate)) %>% 
  mutate(cta_117 = cta_117.01 + cta_117.02 + cta_117.03 + cta_117.04) %>% 
  mutate(cta_121 = cta_121.99) %>% 
  mutate(cta_122 = cta_122.01 + cta_122.02 + cta_122.03 + cta_122.06 + 
           cta_122.99) %>% 
  mutate(cta_123 = cta_123.01 + cta_123.98) %>% 
  mutate(cta_124 = cta_124.01 + cta_124.02 + cta_124.03 + cta_124.05 +
           cta_124.05.1 + cta_124.99) %>% 
  mutate(cta_126 = cta_126.01 + cta_126.02 + cta_126.03) %>% 
  mutate(act = cta_111.01 + cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_114.01 + cta_115.01 + cta_117 + cta_121 + cta_122 +
           cta_123 + cta_124 + cta_126 + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  mutate(actE = cta_111.01 + cta_111.04 + cta_112.01 + cta_112.05 + cta_113.01 +
           cta_114.01 + cta_115.01 + cta_117 + cta_121 + (cta_122*0.5) +
           cta_123 + cta_124 + (cta_126*0.7) + cta_127.11 + cta_127.17 - cta_232.01) %>% 
  select(date, act, actE) %>% 
  dplyr::filter(date >= '2021-11-01') %>% 
  group_by(date) %>% 
  summarise_all(sum) %>% 
  mutate(pctLass = actE/act) %>% 
  glimpse()
write.csv(act_final, 'D:/!bso/Liquidity/assets_ST_me.csv', row.names = F)
#-------------------------------------
# Data in final form
cah_final <- cah %>% 
  mutate(fecha = ymd(fecha)) %>% 
  dplyr::filter(fecha >= '2021-11-01') %>% 
  dplyr::rename(cah = SALDO_SUS,
                date = fecha,
                cur = MONEDA) %>% 
  glimpse()
dpf_final <- read.csv('D:/!bso/Liquidity/dpf30daily_seg.csv') %>% 
  select(dateB, MONEDA, seg, SALDO_SUS) %>% 
  mutate(dateB = ymd(dateB)) %>% 
  dplyr::rename(date = dateB, 
                dpf = SALDO_SUS,
                cur = MONEDA) %>%
  dplyr::filter(date >= '2021-11-01') %>% 
  glimpse()
cap <- dpf_final %>% 
  left_join(cah_final, by = c('date', 'seg', 'cur')) %>% 
  glimpse()
write.csv(cap, 'D:/!bso/Liquidity/liabilities_ST.csv', row.names = F)

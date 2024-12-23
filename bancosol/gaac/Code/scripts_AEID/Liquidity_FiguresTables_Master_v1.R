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
library(RcppRoll)
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(kableExtra)
remove(list = ls())
gc()
options("encoding" = "UTF-8")

cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
cbp1 <- c("#4198B5", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#-------------------------------
rdsm <- 'D:/!bso/Liquidity/rdsFCST_m/'

glist <- c('r2_m_t_co', 'r2_m_t_mn', 'r2_m_t_me', 
           'r23_m_t_co', 'r23_m_t_mn', 'r23_m_t_me', 
           'r3_m_t_co', 'r3_m_t_mn', 'r3_m_t_me', 
           'r4_m_t_co', 'r4_m_t_mn', 'r4_m_t_me', 
           'r7_m_t_co', 'r7_m_t_mn', 'r7_m_t_me')

for(i in 1:length(glist)) {
  # Comparison
  bmf <- readRDS(paste0(rdsm, 'm_Bancos MF_',  glist[i], '.rds'))
  bmu <- readRDS(paste0(rdsm, 'm_Bancos Múltiples_',  glist[i], '.rds'))
  bso <- readRDS(paste0(rdsm, 'm_BancoSol_',  glist[i], '.rds'))
  print(glist[i])
  
  if(glist[i] == 'r3_m_t_co' ){
    limit <- 0.4
    }  else  if  (glist[i] == 'r3_m_t_mn' ) {
    limit <- 0.4
    }  else  if  (glist[i] == 'r3_m_t_me'  ) {
    limit <- 0.83
    } else if(glist[i] == 'r4_m_t_co' ){
    limit <- 0.70
    }  else if (glist[i] == 'r4_m_t_mn' ){
    limit <- 0.765
    }  else if(glist[i] == 'r4_m_t_me' ){
    limit <- 0.46
    }  else if(glist[i] == 'r7_m_t_co' ){
    limit <- 0.32
    }  else  if(glist[i] == 'r7_m_t_mn' ){
    limit <- 0.338
    }  else if(glist[i] == 'r7_m_t_me' ){
    limit <- 0.117
    }  else if(glist[i] == 'r23_m_t_co' ){
      limit <- 0.4
    }  else  if(glist[i] == 'r23_m_t_mn' ){
      limit <- 0.4
    }  else if(glist[i] == 'r23_m_t_me' ){
      limit <- 0.83
    }  else {limit <- 0}
  print(limit)
  gph <- bso %>% bind_rows(bmf) %>% bind_rows(bmu) %>% 
    mutate(key = ifelse(date <= '2022-07-31', 'Observado', 'Predicho')) %>% 
    dplyr::rename(Sector = id) %>% 
    mutate(limite = limit)
    limiteg = mean(gph$limite)
  ggplot(gph, aes(date, value*100, color = Sector)) + geom_line(size = 1.25) +
    theme_minimal() + scale_color_manual(values = cbp1) + 
    geom_vline(xintercept = as.Date('2022-07-31'), linetype="dashed") + 
    xlab('') + ylab('%') + ylim(0,100) + 
    geom_hline(yintercept = limiteg*100, linetype="dashed", color = 'red', size = 1.25) + 
    theme(legend.position = 'bottom')
  ggsave(paste0('D:/!bso/Liquidity/gph/comp_', glist[i], '.png'))

}

# Comparison
glist <- c('totalDP_m_t_co', 'totalDP_m_t_mn', 'totalDP_m_t_me',
           'totalCC_m_t_co', 'totalCC_m_t_mn', 'totalCC_m_t_me',
           'totalPF_m_t_co', 'totalPF_m_t_mn', 'totalPF_m_t_me',
           'totalCA_m_t_co', 'totalCA_m_t_mn', 'totalCA_m_t_me',
           'actliq_m_t_co', 'actliq_m_t_mn', 'actliq_m_t_me')
for(i in 1:length(glist)) {

  bmf <- readRDS(paste0(rdsm, 'm_Bancos MF_',  glist[i], '.rds'))
  bmu <- readRDS(paste0(rdsm, 'm_Bancos Múltiples_',  glist[i], '.rds'))
  bso <- readRDS(paste0(rdsm, 'm_BancoSol_',  glist[i], '.rds'))
  
  gph <- bso %>% bind_rows(bmf) %>% bind_rows(bmu) %>% 
    mutate(key = ifelse(date <= '2022-07-31', 'Observado', 'Predicho')) %>% 
    dplyr::rename(Sector = id)
  
  ggplot(gph, aes(date, value, color = Sector)) + geom_line(size = 1.25) +
    theme_minimal() + scale_color_manual(values = cbp1) + 
    geom_vline(xintercept = as.Date('2022-07-31'), linetype="dashed") + 
    xlab('') + ylab('%') + scale_y_continuous(labels = comma) +
    theme(legend.position = 'bottom')
  ggsave(paste0('D:/!bso/Liquidity/gph/comp_', glist[i], '.png'))
}

# Confidence intervals

glist <- c('m_BancoSol_r2_m_t_co', 'm_BancoSol_r2_m_t_mn', 'm_BancoSol_r2_m_t_me', 
           'm_BancoSol_r23_m_t_co', 'm_BancoSol_r23_m_t_mn', 'm_BancoSol_r23_m_t_me', 
           'm_BancoSol_r3_m_t_co', 'm_BancoSol_r3_m_t_mn', 'm_BancoSol_r3_m_t_me', 
           'm_BancoSol_r4_m_t_co', 'm_BancoSol_r4_m_t_mn', 'm_BancoSol_r4_m_t_me', 
           'm_BancoSol_r7_m_t_co', 'm_BancoSol_r7_m_t_mn', 'm_BancoSol_r7_m_t_me')
for(i in 1:length(glist)) {
  bso <- readRDS(paste0(rdsm, glist[i], '.rds'))
  gph <- bso %>% 
    select(date, value, `Hi 95`, `Hi 99`, `Lo 95`, `Lo 99`)
  print(glist[i])
  
  if(glist[i] == 'm_BancoSol_r3_m_t_co' ){
    limit <- 0.4
  }  else  if  (glist[i] == 'm_BancoSol_r3_m_t_mn' ) {
    limit <- 0.4
  }  else  if  (glist[i] == 'm_BancoSol_r3_m_t_me'  ) {
    limit <- 0.83
  } else if(glist[i] == 'm_BancoSol_r4_m_t_co' ){
    limit <- 0.70
  }  else if (glist[i] == 'm_BancoSol_r4_m_t_mn' ){
    limit <- 0.765
  }  else if(glist[i] == 'm_BancoSol_r4_m_t_me' ){
    limit <- 0.46
  }  else if(glist[i] == 'm_BancoSol_r7_m_t_co' ){
    limit <- 0.32
  }  else  if(glist[i] == 'm_BancoSol_r7_m_t_mn' ){
    limit <- 0.338
  }  else if(glist[i] == 'm_BancoSol_r7_m_t_me' ){
    limit <- 0.117
  }  else if(glist[i] == 'm_BancoSol_r23_m_t_co' ){
    limit <- 0.4
  }  else  if(glist[i] == 'm_BancoSol_r23_m_t_mn' ){
    limit <- 0.4
  }  else if(glist[i] == 'm_BancoSol_r23_m_t_me' ){
    limit <- 0.83
  }  else {limit <- 0}
  print(limit)
  
  gph <- gph %>% 
    mutate(limite = limit)
  limiteg = mean(gph$limite)
  lim <- bso %>% 
    select(date, value, starts_with('Hi'), starts_with('Lo')) %>% 
    pivot_longer(!date) %>% 
    mutate(limite = limiteg) %>% 
    dplyr::filter(!is.na(value)) %>% 
    mutate(flag = ifelse(value > limite, 1, 0)) %>% 
    dplyr::filter(name != 'value') %>% 
    mutate(ci = substr(name, 1, 2)) %>% 
    mutate(pctile = as.numeric(substr(name, 4, 5))) %>% 
    dplyr::filter(flag == 1) %>% 
    arrange(pctile) %>% 
    dplyr::filter(row_number() == 1) %>% 
    mutate(id = glist[i]) %>% 
    select(-ci, -flag, -name) %>% 
    mutate(prob = 100 - pctile) %>% 
    write_rds(paste0('D:/!bso/Liquidity/gph/lim_', glist[i], '.rds'))
  
  ggplot(gph, aes(date, value*100)) + 
    geom_ribbon(aes(ymin = `Lo 95`*100, ymax = `Hi 95`*100, fill="CI 80"), fill = "orange") + 
    geom_ribbon(aes(ymin = `Lo 99`*100, ymax = `Hi 99`*100, fill="CI 95"), fill = "orange4", alpha = 0.35) +
    geom_line(size = 1.25, color = "navy")  + theme_minimal() + 
    geom_vline(xintercept = as.Date('2022-07-31', linetype="dashed")) + 
    geom_hline(yintercept = limiteg*100, linetype="dashed", color = 'red', size = 1.25) + 
    xlab('') + ylab('%') + ylim(0,100) 
  ggsave(paste0('D:/!bso/Liquidity/gph/', glist[i], '.png'))
  
  
  # finding limit blow off
  # lim <- gph %>% 
  #   select(date, value, limite)
  # pivot_longer(!c(date, ))
}

glist <- c('m_BancoSol_totalDP_m_t_co', 'm_BancoSol_totalDP_m_t_mn', 'm_BancoSol_totalDP_m_t_me',
           'm_BancoSol_totalCC_m_t_co', 'm_BancoSol_totalCC_m_t_mn', 'm_BancoSol_totalCC_m_t_me',
           'm_BancoSol_totalCA_m_t_co', 'm_BancoSol_totalCA_m_t_mn', 'm_BancoSol_totalCA_m_t_me',
           'm_BancoSol_totalPF_m_t_co', 'm_BancoSol_totalPF_m_t_mn', 'm_BancoSol_totalPF_m_t_me',
           'm_BancoSol_actliq_m_t_co', 'm_BancoSol_actliq_m_t_mn', 'm_BancoSol_actliq_m_t_me')
for(i in 1:length(glist)) {
  bso <- readRDS(paste0(rdsm, glist[i], '.rds'))
  gph <- bso %>% 
    select(date, value, `Hi 95`, `Hi 99`, `Lo 95`, `Lo 99`)
  
  ggplot(gph, aes(date, value)) + 
    geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`, fill="CI 80"), fill = "orange") + 
    geom_ribbon(aes(ymin = `Lo 99`, ymax = `Hi 99`, fill="CI 95"), fill = "orange4", alpha = 0.35) +
    geom_line(size = 1.25, color = "navy")  + theme_minimal() + 
    geom_vline(xintercept = as.Date('2022-07-31'), linetype="dashed") + 
    scale_y_continuous(labels = comma) +
    xlab('') + ylab('%') 
  ggsave(paste0('D:/!bso/Liquidity/gph/', glist[i], '.png'))
}


################################################################################
# Ratio 1
bso <- readRDS('D:/!bso/Liquidity/rdsFCST_d/ratio_1_co.rds')
gph <- bso %>% 
  select(date, value,  `Lo 99`,  `Lo 95`,  `Hi 99`,  `Hi 95`) %>% 
  #dplyr::filter(!(date >= '2022-08-01' & date <= '2022-08-18' & !is.na(lo80))) %>% 
  dplyr::filter(date <= '2023-12-31')

ggplot(gph, aes(date, value)) + 
  geom_ribbon(aes(ymin =  `Lo 95`, ymax =  `Hi 95`, fill="CI 80"), fill = "orange") + 
  geom_ribbon(aes(ymin =  `Lo 99`, ymax =  `Hi 99`, fill="CI 95"), fill = "orange4", alpha = 0.35) +
  geom_line(size = 1.25, color = "navy")  + theme_minimal() + 
  geom_vline(xintercept = as.Date('2022-08-19'), linetype="dashed") +
  geom_hline(yintercept = 0.49, linetype="dashed", color = 'red', size = 1.25) +
  xlab('') + ylab('%') 
ggsave('D:/!bso/Liquidity/gph/ratio_1_co.png')

lim <- bso %>% 
  select(date, value, starts_with('Hi'), starts_with('Lo')) %>% 
  pivot_longer(!date) %>% 
  mutate(limite = 0.49) %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(flag = ifelse(value < limite, 1, 0)) %>% 
  dplyr::filter(name != 'value') %>% 
  mutate(ci = substr(name, 1, 2)) %>% 
  mutate(pctile = as.numeric(substr(name, 4, 5))) %>% 
  dplyr::filter(flag == 1) %>% 
  arrange(desc(pctile)) %>% 
  dplyr::filter(pctile <= 99) %>% 
  group_by(pctile) %>% 
  arrange(desc(value)) %>% 
  mutate(keep = row_number()) %>% 
  dplyr::filter(keep == 1 ) %>% 
  mutate(id = 'r1_co') %>% 
  select(-ci, -flag, -name) %>% 
  mutate(prob = 100 - pctile) %>% 
  arrange(date) %>% 
  dplyr::filter(date >= '2022-09-15') %>% 
  ungroup() %>% 
  group_by(date) %>% 
  dplyr::filter(prob == min(prob)) %>% 
  write_rds(paste0('D:/!bso/Liquidity/gph/lim_r1_co.rds'))
write.xlsx(lim, 'D:/!bso/Liquidity/gph/lim_r1_co.xlsx')
#-------

bso <- readRDS('D:/!bso/Liquidity/rdsFCST_d/ratio_1_mn.rds')
gph <- bso %>% 
  select(date, value,  `Lo 99`,  `Lo 95`,  `Hi 99`,  `Hi 95`) %>% 
  #dplyr::filter(!(date >= '2022-08-01' & date <= '2022-08-18' & !is.na(lo80))) %>% 
  dplyr::filter(date <= '2023-12-31')

ggplot(gph, aes(date, value)) + 
  geom_ribbon(aes(ymin =  `Lo 95`, ymax =  `Hi 95`, fill="CI 80"), fill = "orange") + 
  geom_ribbon(aes(ymin =  `Lo 99`, ymax =  `Hi 99`, fill="CI 95"), fill = "orange4", alpha = 0.35) +
  geom_line(size = 1.25, color = "navy")  + theme_minimal() + 
  geom_vline(xintercept = as.Date('2022-08-19'), linetype="dashed") + 
  geom_hline(yintercept = 0.48, linetype="dashed", color = 'red', size = 1.25) +
  xlab('') + ylab('%') 
ggsave('D:/!bso/Liquidity/gph/ratio_1_mn.png')
lim <- bso %>% 
  select(date, value, starts_with('Hi'), starts_with('Lo')) %>% 
  pivot_longer(!date) %>% 
  mutate(limite = 0.48) %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(flag = ifelse(value < limite, 1, 0)) %>% 
  dplyr::filter(name != 'value') %>% 
  mutate(ci = substr(name, 1, 2)) %>% 
  mutate(pctile = as.numeric(substr(name, 4, 5))) %>% 
  dplyr::filter(flag == 1) %>% 
  arrange(desc(pctile)) %>% 
  dplyr::filter(pctile <= 95) %>% 
  group_by(pctile) %>% 
  arrange(desc(value)) %>% 
  mutate(keep = row_number()) %>% 
  dplyr::filter(keep == 1 ) %>% 
  mutate(id = 'r1_mn') %>% 
  select(-ci, -flag, -name) %>% 
  mutate(prob = 100 - pctile) %>% 
  arrange(date) %>% 
  dplyr::filter(date >= '2022-09-15') %>% 
  ungroup() %>% 
  group_by(date) %>% 
  dplyr::filter(prob == min(prob)) %>% 
  write_rds('D:/!bso/Liquidity/gph/lim_r1_mn.rds')


bso <- readRDS('D:/!bso/Liquidity/rdsFCST_d/ratio_1_me.rds')

gph <- bso %>% 
  select(date, value,  `Lo 99`,  `Lo 95`,  `Hi 99`,  `Hi 95`) 
  #dplyr::filter(!(date >= '2022-08-01' & date <= '2022-08-18' & !is.na(lo80)))

ggplot(gph, aes(date, value)) + 
  geom_ribbon(aes(ymin =  `Lo 95`, ymax =  `Hi 95`, fill="CI 80"), fill = "orange") + 
  geom_ribbon(aes(ymin =  `Lo 99`, ymax =  `Hi 99`, fill="CI 95"), fill = "orange4", alpha = 0.35) +
  geom_line(size = 1.25, color = "navy")  + theme_minimal() + 
  geom_vline(xintercept = as.Date('2022-08-19'), linetype="dashed") +
  geom_hline(yintercept = 0.64, linetype="dashed", color = 'red', size = 1.25) +
  xlab('') + ylab('%') 
ggsave('D:/!bso/Liquidity/gph/ratio_1_me.png')

lim <- bso %>% 
  select(date, value, starts_with('Hi'), starts_with('Lo')) %>% 
  pivot_longer(!date) %>% 
  mutate(limite = 0.64) %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(flag = ifelse(value < limite, 1, 0)) %>% 
  dplyr::filter(name != 'value') %>% 
  mutate(ci = substr(name, 1, 2)) %>% 
  mutate(pctile = as.numeric(substr(name, 4, 5))) %>% 
  dplyr::filter(flag == 1) %>% 
  arrange(desc(pctile)) %>% 
  dplyr::filter(pctile <= 95) %>% 
  group_by(pctile) %>% 
  arrange(desc(value)) %>% 
  mutate(keep = row_number()) %>% 
  dplyr::filter(keep == 1 ) %>% 
  mutate(id = 'r1_me') %>% 
  select(-ci, -flag, -name) %>% 
  mutate(prob = 100 - pctile) %>% 
  arrange(date) %>% 
  dplyr::filter(date >= '2022-09-15') %>% 
  ungroup() %>% 
  group_by(date) %>% 
  dplyr::filter(prob == min(prob)) %>%  
  write_rds(paste0('D:/!bso/Liquidity/gph/lim_r1_me.rds'))

###############################################################################
d1 <- read.csv('D:/!bso/Liquidity/lead30_noOutliers.csv')
d1 <- read.csv('D:/!bso/Liquidity/lead30_noOutliers.csv')
d1 <- read.csv('D:/!bso/Liquidity/lead30_noOutliers.csv')
d1 <- read.csv('D:/!bso/Liquidity/lead30_noOutliers.csv')
###############################################################################
# limit blow-off table
llist <- list.files('D:/!bso/Liquidity/gph/', pattern = "\\.rds$")
limList <- list()
for(i in 1:length(llist)) {
  ll <- readRDS(paste0('D:/!bso/Liquidity/gph/', llist[i]))
  limList[[i]] <- ll

}

limTable_fp <- bind_rows(limList) %>% 
  arrange(desc(id)) %>% 
  mutate(id = case_when(id == 'r1_co' ~ 'Act. Líquido/Pasivo CP CO',
                        id == 'r1_me' ~ 'Act. Líquido/Pasivo CP ME',
                        id == 'r1_mn' ~ 'Act. Líquido/Pasivo CP MN',
                        id == 'm_BancoSol_r23_m_t_me' ~ '(CC + CA)/Total DP ME',
                        id == 'm_BancoSol_r3_m_t_me' ~ 'CA/Total DP ME',
                        id == 'm_BancoSol_r23_m_t_co' ~ '(CC + CA)/Total DP CO',
                        id == 'm_BancoSol_r3_m_t_co' ~ 'CA/Total DP CO',
                        id == 'm_BancoSol_r23_m_t_mn' ~ '(CC + CA)/Total DP MN',
                        id == 'm_BancoSol_r3_m_t_mn' ~ 'CA/Total DP MN',
                        id == 'm_BancoSol_r4_m_t_mn' ~ 'DPF/Total DP MN',
                        id == 'm_BancoSol_r7_m_t_me' ~ 'Oblig. EIF/Total DP ME',)) 
write.xlsx(limTable_fp, 'D:/!bso/Liquidity/gph/lim_lastFC_table.xlsx')

limTable <- bind_rows(limList) %>% 
  arrange(desc(prob)) %>% 
  mutate(id = case_when(id == 'r1_co' ~ 'Act. Líquido/Pasivo CP CO',
                        id == 'r1_me' ~ 'Act. Líquido/Pasivo CP ME',
                        id == 'r1_mn' ~ 'Act. Líquido/Pasivo CP MN',
                        id == 'm_BancoSol_r23_m_t_me' ~ '(CC + CA)/Total DP ME',
                        id == 'm_BancoSol_r3_m_t_me' ~ 'CA/Total DP ME',
                        id == 'm_BancoSol_r23_m_t_co' ~ '(CC + CA)/Total DP CO',
                        id == 'm_BancoSol_r3_m_t_co' ~ 'CA/Total DP CO',
                        id == 'm_BancoSol_r23_m_t_mn' ~ '(CC + CA)/Total DP MN',
                        id == 'm_BancoSol_r3_m_t_mn' ~ 'CA/Total DP MN',
                        id == 'm_BancoSol_r4_m_t_mn' ~ 'DPF/Total DP MN',
                        id == 'm_BancoSol_r7_m_t_me' ~ 'Oblig. EIF/Total DP ME',)) %>% 
  select(-keep, -pctile) %>% 
  mutate(limite = round(limite*100, 2)) %>% 
  mutate(value = round(value*100, 2)) %>% 
  arrange(date) %>% 
  dplyr::rename(`Fecha proyectada` = date,
                `Valor pronosticado (%)` = value,
                `Limite (%)` = limite,
                Indicador = id,
                `Probabilidad de incumplimiento (%)` = prob) %>% 
  dplyr::filter(`Fecha proyectada` >= '2022-09-15') %>% 
  ungroup() %>% 
  group_by(Indicador) %>% 
  arrange(`Fecha proyectada`) %>% 
  #dplyr::filter(row_number() == 1) %>% 
  ungroup() %>% 
  arrange(`Fecha proyectada`) %>% 
  glimpse() 

write.xlsx(limTable, 'D:/!bso/Liquidity/gph/lim_blowoff_table.xlsx')

limTable %>% 
  arrange(`Fecha proyectada`) %>% 
  relocate(Indicador, `Fecha proyectada` , `Valor pronosticado (%)`,
           `Limite (%)`, `Probabilidad de incumplimiento (%)`) %>% 
  kbl(caption="Pronóstico de exceso de límites",
    format="latex",
    booktabs = T,
    align="r",
    valign = "H") %>%
  kable_minimal(full_width = F) %>% 
  save_kable('D:/!bso/Liquidity/gph/lim_blowoff_table.tex')
#==============================================================================
#-----------------------------------------------------------------------------
# the densities
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

l30no <- read.csv('D:/!bso/Liquidity/lead30_noOutliers.csv') %>% 
  mutate(date = ymd(.index)) %>%
  group_by(.key) %>% 
  mutate(tile = ntile(lead30_noOutliers, n = 100)) %>% 
  arrange(.key, lead30_noOutliers, tile)
glimpse()
l30fu <- read.csv('D:/!bso/Liquidity/lead30_Full.csv')%>% 
  mutate(date = ymd(.index))%>% 
  group_by(.key) %>% 
  mutate(tile = ntile(lead30_full, n = 100)) %>% 
  arrange(.key, lead30_full, tile) %>% 
  glimpse()
l1no <- read.csv('D:/!bso/Liquidity/lead1_noOutliers.csv')%>% 
  mutate(date = ymd(.index))%>% 
  glimpse()
l1fu <- read.csv('D:/!bso/Liquidity/lead1_full.csv')%>% 
  mutate(date = ymd(.index))%>% 
  glimpse()

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
  # mutate(dif = ratio - dplyr::lag(ratio)) %>% 
  # tq_mutate(select  = cah, mutate_fun = lag.xts, k = -l) %>%  
  # dplyr::rename(l_cah = cah..1) %>% 
  # mutate(d_cah = l_cah - cah) %>% 
  mutate(dfpntile = ntile(dpf, n = 100)) %>% 
  glimpse()

df <- l30no %>% 
  #  full_join(l30no, by = c('date', '.key', '.model_desc')) %>% 
  full_join(l30fu, by =  c('date', '.key', '.model_desc')) %>% 
  full_join(l1no, by = c('date', '.key', '.model_desc')) %>% 
  full_join(l1fu, by =  c('date', '.key', '.model_desc'))

gph <- df %>% 
  ungroup() %>% 
  #select(date, ends_with('cumD')) %>% 
  mutate(p95_30f = ntile(lead30_full_cumD, n = 100)) %>% 
  mutate(p95_30n = ntile(lead30_noOutliers_cumD, n = 100)) %>% 
  mutate(p95_1f  = ntile(lead1_full_cumD, n = 100)) %>% 
  mutate(p95_1n  = ntile(lead1_noOut_cumD, n = 100)) %>% 
  select(date, lead1_full_cumD, lead30_full, lead1_noOut_cumD,
         lead30_noOutliers) %>% 
  pivot_longer(!date) %>% 
  dplyr::filter(value < 0)

qlist_l1f <- quantile(gph[gph$name == 'lead1_full_cumD',]$value, c(.01))
qlist_l1o <- quantile(gph[gph$name == 'lead1_noOut_cumD',]$value, c(.01))
qlist_l30f <- quantile(gph[gph$name == 'lead30_full',]$value, c(.01))
qlist_l30o <- quantile(gph[gph$name == 'lead30_noOutliers',]$value, c(.01))

gph <- gph %>% 
  mutate(name = case_when(name == 'lead1_full_cumD' ~ 'Dif. 1 día Acum. MT',
                          name == 'lead1_noOut_cumD' ~ 'Dif. 1 día Acum. MsO',
                          name == 'lead30_full' ~ 'Dif. 30 días. MT',
                          name == 'lead30_noOutliers' ~ 'Dif. 30 días. MsO',)) %>% 
  dplyr::rename(Estimación = name)
ggplot(gph, aes(x = value, fill = Estimación)) + geom_density(alpha = 0.35) +
  theme_minimal() + scale_fill_manual(values = cbp1) + 
  geom_vline(xintercept = qlist_l1f[1], color = 'blue') +
  geom_vline(xintercept = qlist_l1o[1], color = 'blue') +
  geom_vline(xintercept = qlist_l30f[1], color = 'red') +
  geom_vline(xintercept = qlist_l30o[1], color = 'red') +
  ylab('Densidad') + xlab('Diferencias negativas a 30 días en cajas de ahorro (USD)') +
  scale_x_continuous(labels = comma) +
  theme(legend.position = 'bottom')+guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave('D:/!bso/Liquidity/gph/cah_dif30_densities.png')

# Ratio estimations
lass <- liqAgg %>% 
  dplyr::filter(date == '2022-07-31') %>% 
  mutate(q_l1f = -qlist_l1f,
         q_l1o = -qlist_l1o,
         q_l30f = -qlist_l30f,
         q_l30o = -qlist_l30o,
         pasct_1 = dpf + q_l1f + cah,
         opt_1 = actLiq/pasct_1*100,
         pasct_2 = dpf + q_l1o + cah,
         opt_2 = actLiq/pasct_2*100,
         pasct_3 = dpf + q_l30f + cah,
         opt_3 = actLiq/pasct_3*100,
         pasct_4 = dpf + q_l30o + cah,
         opt_4 = actLiq/pasct_4*100,) %>% 
  glimpse()


write.xlsx(lass, 'D:/!bso/Liquidity/ActuarialDiciendo.xlsx')

p99_dpf <- quantile(liqAgg$dpf, 0.99)
p99_dpf

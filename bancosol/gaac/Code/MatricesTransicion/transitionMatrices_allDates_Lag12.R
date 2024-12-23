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
library(plotly)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#---------------------------------------------------------
bdcFull <- fread('D:/!bso/transMat/bdcFull.csv')
bdcFull <- bdcFull %>% 
  arrange(OPERACION, FDESEMBOLSO)
#---------------------------------------------------------

# plots
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate) %>% 
  mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 12)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 == 'A' & cmt == 'A' , 'AA',
                        ifelse(cm1 == 'A' & cmt == 'B' , 'AB',
                               ifelse(cm1 == 'A' & cmt == 'C' , 'AC',
                                      ifelse(cm1 == 'A' & cmt == 'D' , 'AD',
                                             ifelse(cm1 == 'A' & cmt == 'E' , 'AE',
                                                    ifelse(cm1 == 'A' & cmt == 'F' , 'AF',
                                                           ifelse(cm1 == 'A' & cmt == 'S' , 'AS',
                                                                  ifelse(cm1 == 'B' & cmt == 'A' , 'BA',
                                                                         ifelse(cm1 == 'B' & cmt == 'B' , 'BB',
                                                                                ifelse(cm1 == 'B' & cmt == 'C' , 'BC',
                                                                                       ifelse(cm1 == 'B' & cmt == 'D' , 'BD',
                                                                                              ifelse(cm1 == 'B' & cmt == 'E' , 'BE',
                                                                                                     ifelse(cm1 == 'B' & cmt == 'F' , 'BF',
                                                                                                            ifelse(cm1 == 'B' & cmt == 'S' , 'BS',
                                                                                                                   ifelse(cm1 == 'C' & cmt == 'A' , 'CA',
                                                                                                                          ifelse(cm1 == 'C' & cmt == 'B' , 'CB',
                                                                                                                                 ifelse(cm1 == 'C' & cmt == 'C' , 'CC',
                                                                                                                                        ifelse(cm1 == 'C' & cmt == 'D' , 'CD',
                                                                                                                                               ifelse(cm1 == 'C' & cmt == 'E' , 'CE',
                                                                                                                                                      ifelse(cm1 == 'C' & cmt == 'F' , 'CF',
                                                                                                                                                             ifelse(cm1 == 'C' & cmt == 'S' , 'CS',
                                                                                                                                                                    ifelse(cm1 == 'D' & cmt == 'A' , 'DA',
                                                                                                                                                                           ifelse(cm1 == 'D' & cmt == 'B' , 'DB',
                                                                                                                                                                                  ifelse(cm1 == 'D' & cmt == 'C' , 'DC',
                                                                                                                                                                                         ifelse(cm1 == 'D' & cmt == 'D' , 'DD',
                                                                                                                                                                                                ifelse(cm1 == 'D' & cmt == 'E' , 'DE',
                                                                                                                                                                                                       ifelse(cm1 == 'D' & cmt == 'F' , 'DF',
                                                                                                                                                                                                              ifelse(cm1 == 'D' & cmt == 'S' , 'DS',
                                                                                                                                                                                                                     ifelse(cm1 == 'E' & cmt == 'A' , 'EA',
                                                                                                                                                                                                                            ifelse(cm1 == 'E' & cmt == 'B' , 'EB',
                                                                                                                                                                                                                                   ifelse(cm1 == 'E' & cmt == 'C' , 'EC',
                                                                                                                                                                                                                                          ifelse(cm1 == 'E' & cmt == 'D' , 'ED',
                                                                                                                                                                                                                                                 ifelse(cm1 == 'E' & cmt == 'E' , 'EE',
                                                                                                                                                                                                                                                        ifelse(cm1 == 'E' & cmt == 'F' , 'EF',
                                                                                                                                                                                                                                                               ifelse(cm1 == 'E' & cmt == 'S' , 'ES',
                                                                                                                                                                                                                                                                      ifelse(cm1 == 'F' & cmt == 'A' , 'FA',
                                                                                                                                                                                                                                                                             ifelse(cm1 == 'F' & cmt == 'B' , 'FB',
                                                                                                                                                                                                                                                                                    ifelse(cm1 == 'F' & cmt == 'C' , 'FC',
                                                                                                                                                                                                                                                                                           ifelse(cm1 == 'F' & cmt == 'D' , 'FD',
                                                                                                                                                                                                                                                                                                  ifelse(cm1 == 'F' & cmt == 'E' , 'FE',
                                                                                                                                                                                                                                                                                                         ifelse(cm1 == 'F' & cmt == 'F' , 'FF',
                                                                                                                                                                                                                                                                                                                ifelse(cm1 == 'F' & cmt == 'S' , 'FS',
                                                                                                                                                                                                                                                                                                                       ifelse(cm1 == 'S' & cmt == 'S' , 'SS',
                                                                                                                                                                                                                                                                                                                              NA)))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>% 
  group_by(OPERACION) %>% 
  arrange(OPERACION, monDate) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()
table(bdcTrans[bdcTrans$monDate == 'ago. 2022',]$trans)
write.csv(bdcTrans, 'D:/!bso/transMat/bdcTrans_Lag12.csv')
########
# Aggregate transition matrices
bdcTrans <- fread('D:/!bso/transMat/bdcTrans_Lag12.csv')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds') 
tm <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(prob[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ 0,
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(prob[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

gphT <- tm %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  mutate(eom = as.Date(monDate, frac = 1)) %>% 
  glimpse()


gph <- tm %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  dplyr::filter(trans != 'BF' & trans != 'BE') %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  glimpse()

ggplot(gph, aes(x= Fecha, y = prob, color = trans)) + 
  geom_line(size = 1.25) + theme_minimal() +
  facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.5, "lines")) +
  scale_color_manual(values = cbp1) +
  geom_vline(xintercept = as.Date('2020-04-01'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")

gph <- gphT %>% 
  ungroup() %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  select(monDate, Recuperacion, Permanencia, Deterioro, panel) %>%
  group_by(monDate, panel) %>% 
  summarise_all(mean) %>% 
  pivot_longer(!c('monDate', 'panel'))  %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>%  
  dplyr::rename(Destino = name) %>%
  ungroup() %>%
  group_by(monDate) %>% 
  mutate(ptot = sum(value)) %>% 
  glimpse()

cbp3 <- c( "#D43B1B","#4198B5", "#246D94",
           "#E96732", "#FB9263")
filename = 'D:/!bso/transMat/transicion_desde_B_2_Lag 12.png'
ggplot(gph, aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.5, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)') +
  ggtitle('Evolucion de la probabilidad mensual de transición desde B') +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename)
ggplotly(p)
###################
bdcExp <- gphT %>% 
  select(-monDate) %>% 
  glimpse()
write.csv(bdcExp, 'D:/!bso/transMat/tm_pbi.csv', row.names = F)

##################
# Last transition matrix
gph <- tm %>% 
  #dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate == 'ago. 2022') %>% 
  arrange(trans, monDate) %>% 
  glimpse()
tmExp <- gph %>% 
  ungroup() %>% 
  select(cm1, cmt, prob) %>% 
  group_by(cm1, cmt) %>% 
  summarise_all(mean) %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  ungroup() %>% 
  group_by(cm1) %>% 
  arrange(cm1) %>% 
  mutate(cumProb = cumsum(prob))
write.csv(tmExp, 'D:/!bso/transMat/tmExp.csv', row.names = F)
#----------
# monthly transitions
library(expm)

tm1 <- read.csv('D:/!bso/transMat/tmExp.csv') %>%
  select(cm1, cmt, prob) %>%
  mutate(prob = prob/100) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  glimpse()
M <-data.matrix(tm1)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'Z')

z <- as_tibble(M %^% 12) %>% 
  bind_cols(cm1v) %>% 
  dplyr::rename(cm1 = `...8`) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) 
z2 <- as.data.frame(t(z)) %>% 
  row_to_names(row_number = 1)
write.csv(z2, 'D:/!bso/transMat/tmExpZ_12.csv', row.names = F)
#----------
# 3 month transition
z <- as_tibble(M %^% 3) %>% 
  bind_cols(cm1v) %>% 
  dplyr::rename(cm1 = `...7`) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) 
z3 <- as.data.frame(t(z)) %>% 
  row_to_names(row_number = 1)
write.csv(z3, 'D:/!bso/transMat/tmExpZ_3.csv', row.names = F)
#---------
# average transition
gph <- tm %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise_all(mean) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1) %>% 
  mutate(ptot = sum(prob)) %>% 
  mutate(excess = 100-ptot) %>% 
  mutate(pct = prob/ptot) %>% 
  mutate(prob = prob + (excess*pct)) %>% 
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmExp <- gph %>% 
  ungroup() %>% 
  select(cm1, cmt, prob) %>% 
  group_by(cm1, cmt) %>% 
  summarise_all(mean) %>%
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  ungroup() %>% 
  #group_by(cm1) %>% 
  arrange(cm1) %>% 
  #mutate(cumProb = cumsum(prob))  %>%
  select(cm1, cmt, prob) %>%
  mutate(prob = prob/100) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()
M <-data.matrix(tmExp)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
M
z <- as_tibble(M %^% 12) %>% 
  bind_cols(cm1v) %>% 
  dplyr::rename(cm1 = `...9`) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) %>% 
  glimpse()
zavg <- as.data.frame(t(z)) %>% 
  row_to_names(row_number = 1) %>% 
  glimpse()
write.csv(zavg, 'D:/!bso/transMat/tmExpZ_12avg.csv', row.names = F)
M12 <- as_tibble(M %^% 12)
M12
#===============================================================================
# Sensitivity
# improving b to a and b to c

gph <- tm %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise_all(mean) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1) %>% 
  mutate(ptot = sum(prob)) %>% 
  mutate(excess = 100-ptot) %>% 
  mutate(pct = prob/ptot) %>% 
  mutate(prob = prob + (excess*pct)) %>% 
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmSens <- gph %>% 
  ungroup() %>% 
  select(cm1, cmt, prob) %>% 
  group_by(cm1, cmt) %>% 
  summarise_all(mean) %>%
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  ungroup() %>% 
  #group_by(cm1) %>% 
  arrange(cm1) %>% 
  #mutate(cumProb = cumsum(prob))  %>%
  select(cm1, cmt, prob) %>%
  mutate(prob = prob/100) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()
tmSens$A[1] <- tmSens$A[1] - 0.0005
tmSens$B[1] <- tmSens$B[1] + 0.0005
MSens <-data.matrix(tmSens)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
MSens
# improving transitions
zSens <- as_tibble(MSens %^% 12) %>% 
  bind_cols(cm1v) %>% 
  dplyr::rename(cm1 = `...9`) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) %>% 
  glimpse()
zSens <- as.data.frame(t(zSens)) %>% 
  row_to_names(row_number = 1) %>% 
  glimpse()
write.csv(zSens, 'D:/!bso/transMat/tmExpZ_12sensWab.csv', row.names = F)
#----------
# sensitivity Yearly transitions

gph <- tm %>% 
  #dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate == 'ago. 2022') %>% 
  arrange(trans, monDate) %>% 
  glimpse()
tmExp <- gph %>% 
  ungroup() %>% 
  select(cm1, cmt, prob) %>% 
  group_by(cm1, cmt) %>% 
  summarise_all(mean) %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  ungroup() %>% 
  group_by(cm1) %>% 
  arrange(cm1) %>% 
  mutate(cumProb = cumsum(prob))
write.csv(tmExp, 'D:/!bso/transMat/tmExp.csv', row.names = F)
tm1 <- read.csv('D:/!bso/transMat/tmExp.csv') %>%
  select(cm1, cmt, prob) %>%
  mutate(prob = prob/100) %>%
  mutate(prob = ifelse(cm1 == 'B' & cmt == 'A', prob + 0.20, prob)) %>% 
  mutate(prob = ifelse(cm1 == 'B' & cmt == 'C', prob - 0.20, prob)) %>% 
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  glimpse()
M <-data.matrix(tm1)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F')

z <- as_tibble(M %^% 12) %>% 
  bind_cols(cm1v) %>% 
  dplyr::rename(cm1 = `...7`) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) 
z2 <- as.data.frame(t(z)) %>% 
  row_to_names(row_number = 1)
write.csv(z2, 'D:/!bso/transMat/tmExpZ_12better.csv', row.names = F)
#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
#library(xlsx)
library(dplyr)
#library(foreign)
#library(reshape)
#library(reshape2)
#library(stringr)
library(lubridate)
#library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
#library(forecast)
library(quantmod)

#library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
library(tseries)
#library(xtable)
library(openxlsx)
#library(hrbrthemes)
#library(viridis)
#library(scales)
#library(janitor)
#library(RColorBrewer)
#library(paletteer)
#library(plotly)
library(ggplot2)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
#                    mos[2],'.txt'), encoding = 'Latin-1')
#-------------------------------
# Data in
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds')
for (i in 1:length(file_list)) {
  
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/',
                        file_list[i])) %>% 
    #select(-MODULO) %>% 
    #dplyr::filter(substr(AGENCIA, 1, 1) == '6') %>% 
    # mutate(RUBRO = as.character(RUBRO)) %>% 
    # mutate(CALIFICACION = as.character(CALIFICACION)) %>% 
    # mutate(SALDO = as.double(SALDO)) %>% 
    # mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
    # mutate(fbase = substr(file_list[i], 4, 10)) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
           CALIFICACION, fbase, montous, saldous, previus, saldoCast) %>% 
    mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION))
  bdcList[[i]] <- bdc
}
gc()

#-------------------------------
bdcFull <- bind_rows(bdcList) %>% 
  mutate(mon = substr(fbase,1,3)) %>% 
  mutate(year = substr(fbase,4,7)) %>% 
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>% 
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
  mutate(monDate = as.yearmon(dayDate)) %>% 
  select(-dayDate, -mon, -year, -mes) %>% 
  arrange(CI, CTACLIENTE, OPERACION, monDate) %>% 
  glimpse()
gc()
bdcList <- NULL
write_rds(bdcFull, 'D:/!bso/transMat/bdcFull.rds')

#---------------------------------------------------------
bdcFull <- readRDS('D:/!bso/transMat/bdcFull.rds')
bdcFull <- bdcFull %>% 
  arrange(OPERACION, FDESEMBOLSO)
#---------------------------------------------------------
# counting loans
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate, saldous, previus, saldoCast) %>% 
  #mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  mutate(saldous = ifelse(CALIFICACION == 'S', saldoCast, saldous)) %>% 
  select(-saldoCast) %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  mutate(difPrev = previus - dplyr::lag(previus)) %>% 
  ungroup() 
bdcTrans2 <- bdcTrans %>% 
  # mutate(trans = ifelse(cm1 == 'A' & cmt == 'A' , 'AA',
  #                ifelse(cm1 == 'A' & cmt == 'B' , 'AB',
  #                ifelse(cm1 == 'A' & cmt == 'C' , 'AC',
  #                ifelse(cm1 == 'A' & cmt == 'D' , 'AD',
  #                ifelse(cm1 == 'A' & cmt == 'E' , 'AE',
  #                ifelse(cm1 == 'A' & cmt == 'F' , 'AF',
  #                ifelse(cm1 == 'A' & cmt == 'S' , 'AS',
  #                ifelse(cm1 == 'B' & cmt == 'A' , 'BA',
  #                ifelse(cm1 == 'B' & cmt == 'B' , 'BB',
  #                ifelse(cm1 == 'B' & cmt == 'C' , 'BC',
  #                ifelse(cm1 == 'B' & cmt == 'D' , 'BD',
  #                ifelse(cm1 == 'B' & cmt == 'E' , 'BE',
  #                ifelse(cm1 == 'B' & cmt == 'F' , 'BF',
  #                ifelse(cm1 == 'B' & cmt == 'S' , 'BS',
  #                ifelse(cm1 == 'C' & cmt == 'A' , 'CA',
  #                ifelse(cm1 == 'C' & cmt == 'B' , 'CB',
  #                ifelse(cm1 == 'C' & cmt == 'C' , 'CC',
  #                ifelse(cm1 == 'C' & cmt == 'D' , 'CD',
  #                ifelse(cm1 == 'C' & cmt == 'E' , 'CE',
  #                ifelse(cm1 == 'C' & cmt == 'F' , 'CF',
  #                ifelse(cm1 == 'C' & cmt == 'S' , 'CS',
  #                ifelse(cm1 == 'D' & cmt == 'A' , 'DA',
  #                ifelse(cm1 == 'D' & cmt == 'B' , 'DB',
  #                ifelse(cm1 == 'D' & cmt == 'C' , 'DC',
  #                ifelse(cm1 == 'D' & cmt == 'D' , 'DD',
  #                ifelse(cm1 == 'D' & cmt == 'E' , 'DE',
  #                ifelse(cm1 == 'D' & cmt == 'F' , 'DF',
  #                ifelse(cm1 == 'D' & cmt == 'S' , 'DS',
  #                ifelse(cm1 == 'E' & cmt == 'A' , 'EA',
  #                ifelse(cm1 == 'E' & cmt == 'B' , 'EB',
  #                ifelse(cm1 == 'E' & cmt == 'C' , 'EC',
  #                ifelse(cm1 == 'E' & cmt == 'D' , 'ED',
  #                ifelse(cm1 == 'E' & cmt == 'E' , 'EE',
  #                ifelse(cm1 == 'E' & cmt == 'F' , 'EF',
  #                ifelse(cm1 == 'E' & cmt == 'S' , 'ES',
  #                ifelse(cm1 == 'F' & cmt == 'A' , 'FA',
  #                ifelse(cm1 == 'F' & cmt == 'B' , 'FB',
  #                ifelse(cm1 == 'F' & cmt == 'C' , 'FC',
  #                ifelse(cm1 == 'F' & cmt == 'D' , 'FD',
  #                ifelse(cm1 == 'F' & cmt == 'E' , 'FE',
  #                ifelse(cm1 == 'F' & cmt == 'F' , 'FF',
  #                ifelse(cm1 == 'F' & cmt == 'S' , 'FS',
  #                ifelse(cm1 == 'S' & cmt == 'S' , 'SS',
  #                NA)))))))))))))))))))))))))))))))))))))))))))) %>% 
  mutate(trans2 = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                         cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans2 = ifelse(cm1=='S'&cmt=='S',paste0(cm1,cmt),trans2)) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>% 
  group_by(OPERACION) %>% 
  arrange(OPERACION, monDate) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()
table(bdcTrans[bdcTrans$monDate == 'sep. 2022',]$trans)
write_rds(bdcTrans, 'D:/!bso/transMat/bdcTrans.rds')

bdcTrans <- readRDS("D:/!bso/transMat/bdcTrans.rds")

########
# Aggregate transition matrices
bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds') 
tm_ops <- bdcTrans %>% 
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
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(prob[cmt == 'Z']),
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
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'dic. 2021') %>% 
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
filename = 'D:/!bso/transMat/transicion_desde_B_2.png'
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
  geom_vline(xintercept = as.Date('2022-01-31'), linetype="dashed", color = "black")
ggsave(filename)
ggplotly(p)
###################
bdcExp <- gphT %>% 
  select(-monDate) %>% 
  glimpse()
write.csv(bdcExp, 'D:/!bso/transMat/tm_pbi.csv', row.names = F)

#-------------------------------------------------------------------------------
# exporting monthly for 2022

tm2022_prob <- tm_ops %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(prob, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = prob) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0)) %>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write_xlsx(tm2022_prob, 'D:/!bso/transMat/tmprob_2022.xlsx')

tm2022_ops <- tm_ops %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(one, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = one) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write_xlsx(tm2022_ops, 'D:/!bso/transMat/tmops_2022.xlsx')

#===============================================================================
# Creating portfolio transitions

tm_all <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate, saldous, previus, difPrev) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum, na.rm = T) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(probN = round(one/rowTot*100,2)) %>% 
  mutate(saldoTot = sum(saldous, na.rm = T)) %>% 
  mutate(probS = round(saldous/saldoTot*100,2)) %>% 
  mutate(previTot = sum(previus, na.rm = T)) %>% 
  mutate(probP = round(previus/previTot*100,2)) %>% 
  mutate(difPrevTot = sum(difPrev, na.rm = T)) %>% 
  mutate(probDP = round(difPrev/difPrevTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(probN[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(probN[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(probN[cmt == 'Z']),
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(probN[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(probN[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(probN[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(probN[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(probN[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(probN[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(probN[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(probN[cmt == 'B']),
                                 cm1 == 'C' ~ sum(probN[cmt == 'C']),
                                 cm1 == 'D' ~ sum(probN[cmt == 'D']),
                                 cm1 == 'E' ~ sum(probN[cmt == 'E']),
                                 cm1 == 'F' ~ sum(probN[cmt == 'F']),
                                 cm1 == 'S' ~ sum(probN[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(probN[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

tm2022_saldo <- tm_all %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(saldous, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = saldous) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write_xlsx(tm2022_saldo, 'D:/!bso/transMat/tmsaldo_2022.xlsx')

# tm2022_previ <- tm_all %>%
#   dplyr::filter(monDate > 'dic. 2021') %>%
#   select(previus, cm1, cmt, monDate) %>%
#   pivot_wider(names_from = cmt, values_from = previus) %>%
#   arrange(monDate, cm1) %>%
#   dplyr::filter(!is.na(cm1)) %>%
#   #select(-`NA`) %>%
#   replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
#   mutate(monDate=as.character(monDate)) %>% 
#   relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
# write_xlsx(tm2022_previ, 'D:/!bso/transMat/tmprevi_2022.xlsx')

tm2022_difPrev <- tm_all %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(difPrev, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = difPrev) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write.xlsx(tm2022_difPrev, 'D:/!bso/transMat/tmdifprev_2022.xlsx')

#-------------------------------------------------------------------------------
# MWE
mwe <- P2full %>%
  dplyr::filter(Operacion == 3267334)
write.xlsx(mwe, 'D:/!bso/mph/3267334.xlsx')
mwe <- P2full %>%
  dplyr::filter(Operacion == 2967255)
write.xlsx(mwe, 'D:/!bso/mph/2967255.xlsx')

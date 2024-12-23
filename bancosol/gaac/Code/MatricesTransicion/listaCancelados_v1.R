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
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#################################################################
# list of finished credits
dlist <- c('Mar2015', 'Abr2015', 'May2015', 'Jun2015', 'Jul2015', 'Ago2015', 'Sep2015', 'Oct2015', 'Nov2015', 'Dic2015',
           'Ene2016', 'Feb2016', 'Mar2016', 'Abr2016', 'May2016', 'Jun2016', 'Jul2016', 'Ago2016', 'Sep2016', 'Oct2016', 'Nov2016', 'Dic2016',
           'Ene2017', 'Feb2017', 'Mar2017', 'Abr2017', 'May2017', 'Jun2017', 'Jul2017', 'Ago2017', 'Sep2017', 'Oct2017', 'Nov2017', 'Dic2017',
           'Ene2018', 'Feb2018', 'Mar2018', 'Abr2018', 'May2018', 'Jun2018', 'Jul2018', 'Ago2018', 'Sep2018', 'Oct2018', 'Nov2018', 'Dic2018',
           'Ene2019', 'Feb2019', 'Mar2019', 'Abr2019', 'May2019', 'Jun2019', 'Jul2019', 'Ago2019', 'Sep2019', 'Oct2019', 'Nov2019', 'Dic2019',
           'Ene2020', 'Feb2020', 'Mar2020', 'Abr2020', 'May2020', 'Jun2020', 'Jul2020', 'Ago2020', 'Sep2020', 'Oct2020', 'Nov2020', 'Dic2020',
           'Ene2021', 'Feb2021', 'Mar2021', 'Abr2021', 'May2021', 'Jun2021', 'Jul2021', 'Ago2021', 'Sep2021', 'Oct2021', 'Nov2021', 'Dic2021',
           'Ene2022', 'Feb2022', 'Mar2022', 'Abr2022', 'May2022', 'Jun2022', 'Jul2022', 'Ago2022', 'Sep2022')
# dlist <- c('Ene2021', 'Feb2021', 'Mar2021', 'Abr2021', 'May2021', 'Jun2021', 'Jul2021', 'Ago2021', 'Sep2021', 'Oct2021', 'Nov2021', 'Dic2021',
#            'Ene2022', 'Feb2022', 'Mar2022', 'Abr2022', 'May2022', 'Jun2022', 'Jul2022', 'Ago2022')

clist <- list()

for(i in 1:length(dlist)) {
  tryCatch({
    print(i)
    k = i + 1
    print(k)
    print(dlist[i])
    
    df1 <- readRDS(paste0('D:/!bso/output/rds/ec_', dlist[i], '.rds'))
    
    df2 <- readRDS(paste0('D:/!bso/output/rds/ec_', dlist[k], '.rds'))
    
    df3 <- df1[!(df1$OPERACION %in% df2$OPERACION),] %>% 
      select(OPERACION, CI, CTACLIENTE, saldous, CALIFICACION, ESTADO,
             tipoCred, montous, sucursal, caedec3d) %>% 
      mutate(rangom = ifelse(montous > 20000, '20k+', 'under20k')) %>% 
      mutate(mCancel = dlist[k]) %>% 
      mutate(mSearch = dlist[i])
    clist[[i]] <- df3
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

dfCancel <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  #dplyr::filter(mCancel != 'Abr2015') %>%
  mutate(mSearch = str_replace(mSearch, 'Ene', 'Jan')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Abr', 'Apr')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Ago', 'Aug')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Dic', 'Dec')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ene', 'Jan')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Abr', 'Apr')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ago', 'Aug')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Dic', 'Dec')) %>% 
  mutate(dCancel = as.yearmon(dmy(paste0('01',mCancel)))) %>% 
  mutate(dSearch = as.yearmon(dmy(paste0('01',mSearch)))) %>%
  mutate(cmt = 'Z') %>%
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = dCancel) %>%
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate) %>%
  group_by(cm1, cmt, trans, monDate) %>% 
  tally() %>% 
  dplyr::rename(one = n) %>% 
  glimpse()
table(dfCancel$mCancel)
table(dfCancel$mSearch)
table(dfCancel$trans)
write_rds(dfCancel, 'D:/!bso/transMat/matCancel.rds')

dfCancel_tipoCred <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  #dplyr::filter(mCancel != 'Abr2015') %>%
  mutate(mSearch = str_replace(mSearch, 'Ene', 'Jan')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Abr', 'Apr')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Ago', 'Aug')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Dic', 'Dec')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ene', 'Jan')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Abr', 'Apr')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ago', 'Aug')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Dic', 'Dec')) %>% 
  mutate(dCancel = as.yearmon(dmy(paste0('01',mCancel)))) %>% 
  mutate(dSearch = as.yearmon(dmy(paste0('01',mSearch)))) %>%
  mutate(cmt = 'Z') %>%
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = dCancel) %>%
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate, tipoCred) %>%
  group_by(cm1, cmt, trans, monDate, tipoCred) %>% 
  tally() %>% 
  dplyr::rename(one = n) %>% 
  glimpse()
table(dfCancel_tipoCred$mCancel)
table(dfCancel_tipoCred$mSearch)
table(dfCancel_tipoCred$trans)
write_rds(dfCancel_tipoCred, 'D:/!bso/transMat/matCancel_tipoCred.rds')

dfCancel_sucursal <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  #dplyr::filter(mCancel != 'Abr2015') %>%
  mutate(mSearch = str_replace(mSearch, 'Ene', 'Jan')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Abr', 'Apr')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Ago', 'Aug')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Dic', 'Dec')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ene', 'Jan')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Abr', 'Apr')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ago', 'Aug')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Dic', 'Dec')) %>% 
  mutate(dCancel = as.yearmon(dmy(paste0('01',mCancel)))) %>% 
  mutate(dSearch = as.yearmon(dmy(paste0('01',mSearch)))) %>%
  mutate(cmt = 'Z') %>%
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = dCancel) %>%
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate, sucursal) %>%
  group_by(cm1, cmt, trans, monDate, sucursal) %>% 
  tally() %>% 
  dplyr::rename(one = n) %>% 
  glimpse()
table(dfCancel_tipoCred$mCancel)
table(dfCancel_tipoCred$mSearch)
table(dfCancel_tipoCred$trans)
write_rds(dfCancel_sucursal, 'D:/!bso/transMat/matCancel_sucursal.rds')

dfCancel_caedec3d <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  #dplyr::filter(mCancel != 'Abr2015') %>%
  mutate(mSearch = str_replace(mSearch, 'Ene', 'Jan')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Abr', 'Apr')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Ago', 'Aug')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Dic', 'Dec')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ene', 'Jan')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Abr', 'Apr')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ago', 'Aug')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Dic', 'Dec')) %>% 
  mutate(dCancel = as.yearmon(dmy(paste0('01',mCancel)))) %>% 
  mutate(dSearch = as.yearmon(dmy(paste0('01',mSearch)))) %>%
  mutate(cmt = 'Z') %>%
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = dCancel) %>%
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate, caedec3d) %>%
  group_by(cm1, cmt, trans, monDate, caedec3d) %>% 
  tally() %>% 
  dplyr::rename(one = n) %>% 
  glimpse()
table(dfCancel_tipoCred$mCancel)
table(dfCancel_tipoCred$mSearch)
table(dfCancel_tipoCred$trans)
write_rds(dfCancel_caedec3d, 'D:/!bso/transMat/matCancel_caedec3d.rds')

dfCancel_caedec3d <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  #dplyr::filter(mCancel != 'Abr2015') %>%
  mutate(mSearch = str_replace(mSearch, 'Ene', 'Jan')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Abr', 'Apr')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Ago', 'Aug')) %>% 
  mutate(mSearch = str_replace(mSearch, 'Dic', 'Dec')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ene', 'Jan')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Abr', 'Apr')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Ago', 'Aug')) %>% 
  mutate(mCancel = str_replace(mCancel, 'Dic', 'Dec')) %>% 
  mutate(dCancel = as.yearmon(dmy(paste0('01',mCancel)))) %>% 
  mutate(dSearch = as.yearmon(dmy(paste0('01',mSearch)))) %>%
  mutate(cmt = 'Z') %>%
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = dCancel) %>%
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate, caedec3d) %>%
  group_by(cm1, cmt, trans, monDate, caedec3d) %>% 
  tally() %>% 
  dplyr::rename(one = n) %>% 
  glimpse()
table(dfCancel_tipoCred$mCancel)
table(dfCancel_tipoCred$mSearch)
table(dfCancel_tipoCred$trans)
write_rds(dfCancel_caedec3d, 'D:/!bso/transMat/matCancel_caedec3d.rds')

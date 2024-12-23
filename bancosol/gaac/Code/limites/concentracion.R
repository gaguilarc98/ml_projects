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

gc()
limList <- list()
short_list<-c()
short_list <- list.files('C:/!bso/girCartera/rds')

for(i in 1:length(short_list)){
  bdcLim<- read_rds(paste0('C:/!bso/girCartera/rds/', short_list[i]))
  lim<-bdcLim %>% 
    select(monDate, saldous, sucursal, AGENCIA) %>%
    mutate(sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                                sucursal == '10' ~ 'El Alto',
                                sucursal == '2' ~ 'La Paz',
                                sucursal == '3' ~ 'Cochabamba',
                                sucursal == '4' ~ 'Oruro',
                                sucursal == '5' ~ 'Potosí',
                                sucursal == '6' ~ 'Tarija',
                                sucursal == '7' ~ 'Santa Cruz',
                                sucursal == '8' ~ 'Beni',
                                sucursal == '9' ~ 'Pando',)) %>%
    group_by(monDate, sucursal) %>% 
    summarise(carteraBso=sum(saldous))%>%
    ungroup() %>% 
    group_by(monDate) %>% 
    mutate(pct_saldo=carteraBso/sum(carteraBso)*100,
           pct_rel=sum(pct_saldo),
           cartera_rel=sum(carteraBso)) %>% 
    ungroup() 
  limList[[i]] <- lim
  
}

base <- bind_rows(limList)
write.csv(base, 'C:/!bso/concentracion/output/baseLimites.csv')

################################################################################
#Tabla de Estadisticos

tabla_resumen<-base %>% 
  ungroup() %>% 
  dplyr::filter(sucursal!= is.na(sucursal)) %>% 
  select(sucursal, monDate, carteraBso, pct_saldo) %>% 
  group_by(sucursal) %>% 
  summarise(minCartera=min(carteraBso, na.rm = T),
         maxCartera=max(carteraBso, na.rm = T),
         promCartera=mean(carteraBso, na.rm = T),
         sdCartera= sd(carteraBso, na.rm = T),
         minpct=min(pct_saldo, na.rm = T),
         maxpct=max(pct_saldo, na.rm = T),
         prompct=mean(pct_saldo, na.rm = T),
         sdPct=sd(pct_saldo, na.rm = T)) %>% 
  mutate(rangoCartera=maxCartera-minCartera,
         rangoPct=maxpct-minpct,
         cvCartera=sdCartera/promCartera*100,
         cvPct=sdPct/prompct*100)

summary(tabla_resumen$cvCartera)



################################################################################
#Gráficos
ggplot(base, aes(x = monDate, y = pct_rel, color=sucursal)) + 
  geom_line(size = 1.25) + theme_minimal() 



ggplot(base, aes(x = monDate, y = pct_rel, fill=sucursal)) + 
  geom_bar(stat = 'identity', position = 'stack') + theme_minimal() 





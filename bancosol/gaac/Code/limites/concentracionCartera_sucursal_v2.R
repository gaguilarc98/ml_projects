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
library(ggrepel)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

gc()
limList <- list()
short_list<-c()
short_list <- list.files('C:/!bso/girCartera/rds/')

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
write_rds(base, 'D:/!bso/limites/baseLimites.rds')

base<-readRDS('D:/!bso/limites/baseLimites.rds')
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
#Pronosticos
 baseFcast<-base %>% 
  dplyr::filter(!is.na(sucursal)) %>% 
  dplyr::filter(monDate>'feb. 2015') %>% 
  select(monDate, sucursal, carteraBso) %>% 
  dplyr::rename(saldo=carteraBso) %>% 
  mutate(sucursal=str_replace(sucursal, ' ', '_')) %>%
  group_by(sucursal) %>% 
  arrange(monDate) %>% 
  mutate(delta=(saldo-dplyr::lag(saldo))/dplyr::lag(saldo)) %>% 
  dplyr::filter(!is.na(delta)) %>% 
  #pivot_wider(names_from = sucursal, values_from = c(saldo, delta)) %>%
  pivot_longer(!c(monDate, sucursal)) %>% 
  arrange(sucursal, monDate)
  

for(i in 1:length(unique(baseFcast$sucursal))) {
  for(k in 1:length(unique(baseFcast$name))) {
    tryCatch({
      print(paste0(i, ', ', k))
      
      gph <- baseFcast %>% 
        dplyr::filter(sucursal == unique(baseFcast$sucursal)[i] & name == unique(baseFcast$name)[k]) %>% 
        ungroup()
      
      #--------------------------------------------------------------
      # good auto arima
      
      # 12 month seasonality adjusted forecasts
      dport <- gph %>%
        select(monDate, value)
      dport$value <- as.numeric(dport$value)
      dport <- dport[complete.cases(dport$value),]
      dport <- dport[complete.cases(dport$monDate),]
      dport$date <- as.Date(dport$monDate, frac=1)
      
      count_ma = ts(na.omit(dport$value), frequency=12)
      decomp = stl(count_ma, s.window="periodic")
      deseasonal_cnt <- forecast::seasadj(decomp)
      plot(decomp)
      
      # deseasonal_cnt %>%
      #   auto.arima(D = 1, trace = T, num.cores = 8) %>%
      #   forecast(h=41) %>%
      #   autoplot()
      
      fit <- forecast::auto.arima(deseasonal_cnt, seasonal=T, D=1, num.cores = 8, trace = T)
      fcast <- forecast::forecast(fit, h = 9, level = c(seq(1,99, 1)))
      fcast <- as.data.frame(fcast)
      colnames(fcast)[colnames(fcast) == "Point Forecast"] <- "value"
      # colnames(fcast)[colnames(fcast) == "Lo 80"] <- "lo80"
      # colnames(fcast)[colnames(fcast) == "Hi 80"] <- "hi80"
      # colnames(fcast)[colnames(fcast) == "Lo 95"] <- "lo95"
      # colnames(fcast)[colnames(fcast) == "Hi 95"] <- "hi95"
      
      fcast <- fcast %>%
        mutate(date = seq(as.Date("2023-04-30"), by = "month", length.out = 9)) %>% 
        relocate(date)
      
      exp <- gph %>%
        mutate(date = as.Date(monDate, frac = 1)) %>%
        bind_rows(fcast) %>%
        mutate(bic = fit$bic) %>%
        mutate(aicc = fit$aicc) %>%
        mutate(aic = fit$aic) %>%
        mutate(p = fit$arma[1]) %>%
        mutate(d = fit$arma[6]) %>%
        mutate(q = fit$arma[2]) %>%
        mutate(P = fit$arma[3]) %>%
        mutate(D = fit$arma[7]) %>%
        mutate(Q = fit$arma[4]) %>%
        fill(sucursal, name, .direction = 'updown') %>% 
        relocate(date) %>% 
        mutate(monDate = as.yearmon(date))
      
      write_rds(exp, paste0("C:/!bso/concentracion/rdsFCST_m/m_", unique(baseFcast$sucursal)[i], '_',
                            unique(baseFcast$name)[k], ".rds"))
      
      
      # # ets forecasts
      # deseasonal_cnt %>%
      #   ets() %>%
      #   forecast(h = 17) %>%
      #   autoplot()
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
 
################################################################################
lim<-read.xlsx('D:/!bso/limites/limites.xlsx') %>% 
  mutate(tipoCred = case_when(tipoCred=="Microcredito"~"Micro",
                              tipoCred==""))
gc()
cc_list <- list.files('D:/!bso/limites/rdsFCST_m')
cc_list_2<-list()

for(i in 1:length(cc_list)){
  df<-readRDS(paste0('D:/!bso/limites/rdsFCST_m/',
                     cc_list[[i]]))
  cc_list_2[[i]]<-df
}
################################################################################
#Agregado
allExp_2<-bind_rows(cc_list_2) %>% 
  # select(monDate, sucursal, value, name,  `Hi 90`,  `Hi 91`,  `Hi 92`,  `Hi 93`,
  #        `Hi 94`, `Hi 95`,  `Hi 96`,  `Hi 97`, `Hi 98`,`Hi 99`, `Hi 40` ) %>% 
  select(monDate, sucursal, value, name, starts_with('Hi ') ) %>%
  dplyr::filter(name=='saldo') %>% 
  dplyr::filter(month(monDate)> 2 & year(monDate)==2023) %>% 
  left_join(lim, by='sucursal') %>% 
  select(-name) %>% 
  dplyr::rename(saldo=value) %>%
  mutate(LimiteInicial=ifelse(sucursal=='Cochabamba', 25, LimiteInicial)) %>% 
  mutate(LimiteFinal=ifelse(sucursal=='Cochabamba', 26, LimiteFinal)) %>% 
  pivot_longer(!c(monDate, sucursal, LimiteInicial, LimiteFinal)) %>% 
  arrange(monDate, name, sucursal) %>% 
  group_by(monDate, name) %>% 
  mutate(carteraTotal=sum(value)) %>% 
  ungroup() %>% 
  group_by(monDate, sucursal) %>% 
  arrange(monDate, sucursal, desc(name)) %>% 
  mutate(pos=row_number(),
         value_0_=ifelse(pos==1, value, 0),
         value_0=max(value_0_),
         cb_0_=ifelse(pos==1, carteraTotal, 0),
         cb_0=max(cb_0_),
         dif=value-value_0,    
         new_cartera=cb_0+dif,
         pct=value/new_cartera*100,
         flag_Inicial=ifelse(pct>LimiteInicial, 1,0),
         flag_Final=ifelse(pct>LimiteFinal, 1,0),
         total_Flag_Inicial=sum(flag_Inicial),
         total_Flag_Final=sum(flag_Final)) %>% 
  arrange(monDate, sucursal, name) %>% 
  mutate(first_flag_inicial = flag_Inicial == 1 & !duplicated(flag_Inicial == 1)) %>% 
  dplyr::filter(name=='saldo' | first_flag_inicial==TRUE) %>% 
  ungroup() %>% 
  group_by(sucursal) %>% 
  #dplyr::filter(monDate=='dic. 2023' | flag_Inicial == 1) %>% 
  arrange(sucursal, monDate, desc(name))

write_rds(allExp_2, 'C:/!bso/concentracion/proySucursal.rds')

allExp_2 <- readRDS('D:/!bso/limites/proySucursal.rds')
i <- 3
for(i in 1:length(unique(allExp_2$sucursal))) {
    
all_gph <- bind_rows(cc_list_2) %>% 
  # select(monDate, sucursal, value, name, `Hi 99`) %>% 
  dplyr::filter(name=='saldo') %>% 
  left_join(lim, by='sucursal') %>% 
  mutate(LimiteInicial=ifelse(sucursal=='Cochabamba', 25, LimiteInicial)) %>% 
  mutate(LimiteFinal=ifelse(sucursal=='Cochabamba', 26, LimiteFinal)) %>% 
  dplyr::filter(sucursal==unique(allExp_2$sucursal)[[i]])

flags <- allExp_2 %>%
  ungroup() %>% 
  dplyr::filter(sucursal==unique(allExp_2$sucursal)[[i]]) %>% 
  select(monDate, value, name, dif) %>% 
  dplyr::filter(name!='saldo') %>% 
  dplyr::rename(perc_flag=name,
                valor_flag=value,
                dif_flag = dif)

gph <- all_gph %>% 
  select(sucursal, monDate, value,  `Lo 99`,  `Lo 95`,  `Hi 99`,  `Hi 95`) %>% 
  dplyr::filter(monDate>='ene. 2021') %>% 
  dplyr::filter(sucursal==unique(allExp_2$sucursal)[[i]]) %>% 
  left_join(flags) 

ggplot(gph, aes(monDate, value/1000000)) + 
  geom_ribbon(aes(ymin =  `Lo 95`/1000000, ymax =  `Hi 95`/1000000, fill="CI 80"), fill = "orange", alpha = 0.35) + 
  geom_ribbon(aes(ymin =  `Lo 99`/1000000, ymax =  `Hi 99`/1000000, fill="CI 95"), fill = "orange4", alpha = 0.15) +
  geom_line(size = 1.25, color = "navy")  + theme_minimal() + 
  # geom_vline(xintercept = as.Date('2022-08-19'), linetype="dashed") +
  # geom_hline(yintercept = lim1, linetype="dashed", color = 'red', size = 1.25) +
  xlab('') + ylab('MM de USD') +  
  geom_line(aes(y=valor_flag/1000000), color='red', size=1.15)+
  geom_label_repel(aes(label=paste0(monDate, '-Cap: ',format(round(dif_flag/1000000,1), big.mark = ','), '$'), 
                       y=valor_flag/1000000))

ggsave(paste0('D:/!bso/limites/sucursal_',unique(allExp_2$sucursal)[[i]],
              '.png'))

}

################################################################################
#Gráficos Porcentuales
i <- 1
for(i in 1:length(unique(allExp_2$sucursal))) {
  
  all_gph <- bind_rows(cc_list_2) %>% 
    # select(monDate, sucursal, value, name, `Hi 99`) %>% 
    dplyr::filter(name=='saldo') %>% 
    group_by(monDate) %>% 
    mutate(Cartera=sum(value),
           pct=value/Cartera*100) %>% 
    left_join(lim, by='sucursal') %>% 
    mutate(LimiteInicial=ifelse(sucursal=='Cochabamba', 25, LimiteInicial)) %>% 
    mutate(LimiteFinal=ifelse(sucursal=='Cochabamba', 26, LimiteFinal)) %>% 
    dplyr::filter(sucursal==unique(allExp_2$sucursal)[[i]])
  
  flags <- allExp_2 %>%
    ungroup() %>% 
    dplyr::filter(sucursal==unique(allExp_2$sucursal)[[i]]) %>% 
    select(monDate, value, name, pct, cb_0, dif) %>% 
    dplyr::filter(name!='saldo') %>% 
    dplyr::rename(perc_flag=name,
                  valor_flag=value,
                  pct_flag=pct,
                  dif_flag=dif)
  
  gph <- all_gph %>% 
    select(sucursal, monDate, value,  `Lo 99`,  `Lo 95`,  `Hi 99`, 
           `Hi 95`, pct, Cartera, LimiteInicial, LimiteFinal) %>% 
    dplyr::filter(monDate>='ene. 2021') %>% 
    dplyr::filter(sucursal==unique(allExp_2$sucursal)[[i]]) %>% 
    left_join(flags) 

lim1<-mean(gph$LimiteInicial)
lim2<-mean(gph$LimiteFinal)
  ggplot(gph, aes(monDate, pct)) + 
    geom_ribbon(aes(ymin =  pct, ymax =  pct_flag, fill="CI 80"), fill = "orange", alpha = 0.35) +
    # geom_ribbon(aes(ymin =  `Lo 99`, ymax =  `Hi 99`, fill="CI 95"), fill = "orange4", alpha = 0.15) +
    geom_line(size = 1.25, color = "navy")  + theme_minimal() + 
    # geom_vline(xintercept = as.Date('2022-08-19'), linetype="dashed") +
    geom_hline(yintercept = lim1, linetype="dashed", color = 'red', size = 1.25) +
    geom_hline(yintercept = lim2, linetype="dashed", color = 'orange', size = 1.25) +
    xlab('') + ylab('%') +
    # scale_y_continuous(limits=c(2.6,3)) +
    geom_line(aes(y=pct_flag), color='orange', size=1.15)+
    scale_x_continuous(breaks = as.numeric(gph$monDate),labels = format(gph$monDate,"%m/%y"))+
    theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5),
          panel.grid.minor.x = element_blank())
    # geom_label_repel(aes(label=paste0(monDate,'-',as.character(round(pct_flag, 4)),
    #                                   '%'),
    #                      y=pct_flag), size=2.75, nudge_y = 0.045,nudge_x = 0.0475)
  
  ggsave(paste0('D:/!bso/limites/sucursal_pct_',unique(allExp_2$sucursal)[[i]],
                '.png'),width = 9,height = 4,units = "in")

}

################################################################################
#exportar ppt
allexp_ppt <- readRDS('C:/!bso/concentracion/proySucursal.rds') %>%  
  select(monDate, sucursal, LimiteInicial, LimiteFinal, pct, value, total_Flag_Inicial,
         total_Flag_Final, first_flag_inicial) %>% 
  #dplyr::filter(monDate=='dic. 2023')%>% 
  dplyr::filter(first_flag_inicial=='TRUE')

write.xlsx(allexp_ppt, 'C:/!bso/concentracion/output/tablaSuc.xlsx')

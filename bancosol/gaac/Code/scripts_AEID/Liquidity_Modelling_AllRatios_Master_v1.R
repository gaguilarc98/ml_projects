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
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#-------------------------------
# monthly data reading
df <- read.csv('D:/!bso/Liquidity/liquidity_monthly_data.csv') %>% 
  mutate(date = ymd(date)) %>% 
  arrange(id, name, date) %>% 
  dplyr::filter(id == 'BancoSol' |id == 'Bancos MF' |id == 'Bancos Múltiples' ) %>% 
  dplyr::filter(date >= '2010-01-01')

for(i in 1:length(unique(df$id))) {
  for(k in 1:length(unique(df$name))) {
    tryCatch({
  
    gph <- df %>% 
      dplyr::filter(id == unique(df$id)[i] & name == unique(df$name)[k]) 
 
  #--------------------------------------------------------------
  # good auto arima
  
  # 12 month seasonality adjusted forecasts
  dport <- gph %>%
    select(date, value)
  dport$value <- as.numeric(dport$value)
  dport <- dport[complete.cases(dport$value),]
  dport <- dport[complete.cases(dport$date),]
  dport$date <- as.Date(dport$date)

  count_ma = ts(na.omit(dport$value), frequency=12)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  plot(decomp)

  # deseasonal_cnt %>%
  #   auto.arima(D = 1, trace = T, num.cores = 8) %>%
  #   forecast(h=41) %>%
  #   autoplot()

  fit<-auto.arima(deseasonal_cnt, seasonal=T, D=1, num.cores = 8, trace = T)
  fcast <- forecast(fit, h = 17, level = c(seq(1,99, 1)))
  fcast <- as.data.frame(fcast)
  colnames(fcast)[colnames(fcast) == "Point Forecast"] <- "value"
  # colnames(fcast)[colnames(fcast) == "Lo 80"] <- "lo80"
  # colnames(fcast)[colnames(fcast) == "Hi 80"] <- "hi80"
  # colnames(fcast)[colnames(fcast) == "Lo 95"] <- "lo95"
  # colnames(fcast)[colnames(fcast) == "Hi 95"] <- "hi95"

  fcast <- fcast %>%
    mutate(date = seq(as.Date("2022-08-01"), by = "month", length.out = 17))

  exp <- gph %>%
    bind_rows(fcast) %>%
    mutate(date = as.yearmon(date)) %>%
    mutate(date = as.Date(date, frac = 1)) %>%
    mutate(bic = fit$bic) %>%
    mutate(aicc = fit$aicc) %>%
    mutate(aic = fit$aic) %>%
    mutate(p = fit$arma[1]) %>%
    mutate(d = fit$arma[6]) %>%
    mutate(q = fit$arma[2]) %>%
    mutate(P = fit$arma[3]) %>%
    mutate(D = fit$arma[7]) %>%
    mutate(Q = fit$arma[4]) %>%
    fill(id, name, .direction = 'updown')

  write_rds(exp, paste0("D:/!bso/Liquidity/rdsFCST_m/m_", unique(df$id)[i], '_',
                          unique(df$name)[k], ".rds"))

  
  # # ets forecasts
  # deseasonal_cnt %>%
  #   ets() %>%
  #   forecast(h = 17) %>%
  #   autoplot()
  
  
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

#-------------------------------
# daily data reading
dfd <- read.csv('D:/!bso/Liquidity/liquidity_daily_data.csv') %>% 
  mutate(date = ymd(date)) %>% 
  arrange(id, name, date) %>% 
  dplyr::filter(id == 'BancoSol' |id == 'Bancos MF' |id == 'Bancos MU' )

for(i in 1:length(unique(dfd$id))) {
  for(k in 1:length(unique(dfd$name))) {
    tryCatch({
      
      gph <- dfd %>% 
        dplyr::filter(id == unique(dfd$id)[i] & name == unique(dfd$name)[k])
      
      #--------------------------------------------------------------
      # good auto arima
      
      # 12 month seasonality adjusted forecasts
      dport <- gph %>% 
        select(date, value)
      dport$value <- as.numeric(dport$value)
      dport <- dport[complete.cases(dport$value),]
      dport <- dport[complete.cases(dport$date),]
      dport$date <- as.Date(dport$date)
      
      count_ma = ts(na.omit(dport$value), frequency=365)
      decomp = stl(count_ma, s.window="periodic")
      deseasonal_cnt <- seasadj(decomp)
      plot(decomp)
      
      # deseasonal_cnt %>%
      #   auto.arima(D = 1, trace = T, num.cores = 8) %>%
      #   forecast(h=1080) %>%
      #   autoplot()
      
      fit<-auto.arima(deseasonal_cnt, seasonal=T, D=1,
                      num.cores = 8, trace = T)
      fcast <- forecast(fit, h = 1249)
      fcast <- as.data.frame(fcast)
      colnames(fcast)[colnames(fcast) == "Point Forecast"] <- "value"
      colnames(fcast)[colnames(fcast) == "Lo 80"] <- "lo80"
      colnames(fcast)[colnames(fcast) == "Hi 80"] <- "hi80"
      colnames(fcast)[colnames(fcast) == "Lo 95"] <- "lo95"
      colnames(fcast)[colnames(fcast) == "Hi 95"] <- "hi95"
      
      fcast <- fcast %>% 
        mutate(date = seq(as.Date("2022-08-01"), by = "day", length.out = 1249))
      
      exp <- gph %>% 
        bind_rows(fcast) %>% 
        mutate(bic = fit$bic) %>% 
        mutate(aicc = fit$aicc) %>% 
        mutate(p = fit$arma[1]) %>% 
        mutate(d = fit$arma[6]) %>% 
        mutate(q = fit$arma[2]) %>% 
        mutate(P = fit$arma[3]) %>% 
        mutate(D = fit$arma[7]) %>% 
        mutate(Q = fit$arma[4]) %>% 
        fill(id, name, .direction = 'updown')
      
      write_rds(exp, paste0("D:/!bso/Liquidity/rdsFCST_d/d_", unique(dfd$id)[i], '_', 
                            unique(dfd$name)[k], ".rds"))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

#############################################################
# ratio 1
gph <- read.csv('D:/!bso/Liquidity/ratio1_daily_co.csv') 
dport <- gph %>% 
  mutate(date = ymd(date))

dport$value <- as.numeric(dport$value)
dport <- dport[complete.cases(dport$value),]
dport <- dport[complete.cases(dport$date),]
dport$date <- as.Date(dport$date)

count_ma = ts(na.omit(dport$value), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# deseasonal_cnt %>%
#   auto.arima(D = 1, trace = T) %>%
#   forecast(h=99) %>%
#   autoplot()

fit<-auto.arima(deseasonal_cnt, seasonal=T, D=1,
                num.cores = 8, trace = T)
fcast <- forecast(fit, h = 135, level = c(seq(1, 99, 1)))
fcast <- as.data.frame(fcast)
colnames(fcast)[colnames(fcast) == "Point Forecast"] <- "value"
# colnames(fcast)[colnames(fcast) == "Lo 80"] <- "lo80"
# colnames(fcast)[colnames(fcast) == "Hi 80"] <- "hi80"
# colnames(fcast)[colnames(fcast) == "Lo 95"] <- "lo95"
# colnames(fcast)[colnames(fcast) == "Hi 95"] <- "hi95"

fcast <- fcast %>% 
  mutate(date = seq(as.Date("2022-08-19"), by = "day", length.out = 135))

exp <- gph %>% 
  mutate(date = ymd(date)) %>% 
  bind_rows(fcast) %>% 
  mutate(bic = fit$bic) %>% 
  mutate(aicc = fit$aicc) %>% 
  mutate(p = fit$arma[1]) %>% 
  mutate(d = fit$arma[6]) %>% 
  mutate(q = fit$arma[2]) %>% 
  mutate(P = fit$arma[3]) %>% 
  mutate(D = fit$arma[7]) %>% 
  mutate(Q = fit$arma[4]) 

write_rds(exp, "D:/!bso/Liquidity/rdsFCST_d/ratio_1_co.rds")


gph <- read.csv('D:/!bso/Liquidity/ratio1_daily_mn.csv') 
dport <- gph %>% 
  mutate(date = ymd(date))

dport$value <- as.numeric(dport$value)
dport <- dport[complete.cases(dport$value),]
dport <- dport[complete.cases(dport$date),]
dport$date <- as.Date(dport$date)

count_ma = ts(na.omit(dport$value), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# deseasonal_cnt %>%
#   auto.arima(D = 1, trace = T) %>%
#   forecast(h=99) %>%
#   autoplot()

fit<-auto.arima(deseasonal_cnt, seasonal=T, D=1,
                num.cores = 8, trace = T)
fcast <- forecast(fit, h = 135, level = c(seq(1, 99, 1)))
fcast <- as.data.frame(fcast)
colnames(fcast)[colnames(fcast) == "Point Forecast"] <- "value"
# colnames(fcast)[colnames(fcast) == "Lo 80"] <- "lo80"
# colnames(fcast)[colnames(fcast) == "Hi 80"] <- "hi80"
# colnames(fcast)[colnames(fcast) == "Lo 95"] <- "lo95"
# colnames(fcast)[colnames(fcast) == "Hi 95"] <- "hi95"

fcast <- fcast %>% 
  mutate(date = seq(as.Date("2022-08-19"), by = "day", length.out = 135))

exp <- gph %>% 
  mutate(date = ymd(date)) %>% 
  bind_rows(fcast) %>% 
  
  mutate(bic = fit$bic) %>% 
  mutate(aicc = fit$aicc) %>% 
  mutate(p = fit$arma[1]) %>% 
  mutate(d = fit$arma[6]) %>% 
  mutate(q = fit$arma[2]) %>% 
  mutate(P = fit$arma[3]) %>% 
  mutate(D = fit$arma[7]) %>% 
  mutate(Q = fit$arma[4]) 

write_rds(exp, "D:/!bso/Liquidity/rdsFCST_d/ratio_1_mn.rds")

gph <- read.csv('D:/!bso/Liquidity/ratio1_daily_me.csv') 
dport <- gph %>% 
  mutate(date = ymd(date))

dport$value <- as.numeric(dport$value)
dport <- dport[complete.cases(dport$value),]
dport <- dport[complete.cases(dport$date),]
dport$date <- as.Date(dport$date)

count_ma = ts(na.omit(dport$value), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# deseasonal_cnt %>%
#   auto.arima(D = 1, trace = T) %>%
#   forecast(h=99) %>%
#   autoplot()

fit<-auto.arima(deseasonal_cnt, seasonal=T, D=1,
                num.cores = 8, trace = T)
fcast <- forecast(fit, h = 135, level = c(seq(1, 99, 1)))
fcast <- as.data.frame(fcast)
colnames(fcast)[colnames(fcast) == "Point Forecast"] <- "value"
# colnames(fcast)[colnames(fcast) == "Lo 80"] <- "lo80"
# colnames(fcast)[colnames(fcast) == "Hi 80"] <- "hi80"
# colnames(fcast)[colnames(fcast) == "Lo 95"] <- "lo95"
# colnames(fcast)[colnames(fcast) == "Hi 95"] <- "hi95"

fcast <- fcast %>% 
  mutate(date = seq(as.Date("2022-08-19"), by = "day", length.out = 135))

exp <- gph %>% 
  mutate(date = ymd(date)) %>% 
  bind_rows(fcast) %>% 
  mutate(bic = fit$bic) %>% 
  mutate(aicc = fit$aicc) %>% 
  mutate(p = fit$arma[1]) %>% 
  mutate(d = fit$arma[6]) %>% 
  mutate(q = fit$arma[2]) %>% 
  mutate(P = fit$arma[3]) %>% 
  mutate(D = fit$arma[7]) %>% 
  mutate(Q = fit$arma[4]) 

write_rds(exp, "D:/!bso/Liquidity/rdsFCST_d/ratio_1_me.rds")

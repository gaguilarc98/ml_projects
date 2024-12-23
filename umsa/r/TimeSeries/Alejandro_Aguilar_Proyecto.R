setwd("/home/acga/Documents/Statistics/Time Series")
####BOLIVIA LAND TEMPERATURE####
library(readxl)
library(lubridate)
library(forecast)
library(urca)
library(tseries)
library(fUnitRoots)
library(ggplot2)
library(ggfortify)
library(TSA)
#####_____IMPORTANDO LA BASE DE DATOS_____#####
dat <-  read.table("BoliviaLandTemp.txt",header=T,sep = "\t")
dat <- read_excel("BoliviaLandTemp.xlsx")
#####_____GRAFICO DE LA SERIE ORIGINAL_____#####
periodo <- 12
Temp.ts <- ts(dat$AveTemp,start = decimal_date(as.Date(dat$Time[1])),
              frequency = periodo)
plot(Temp.ts,xlab = "Tiempo",
     ylab="Temperatura mensual promedio (°C)",col="darkblue")
plot(Temp.ts,type="l",
     xlab = "Tiempo",ylab="Temperatura mensual promedio (°C)",
     col="darkblue",xlim=c(1990,2010))
TempMed <- rep(0,12)
for (i in 1:12) {
  pos <- which(month(dat$Time)==i)
  TempMed[i] <- mean(dat$AveTemp[pos])
  dat$AveTemp[pos] <- dat$AveTemp[pos]-TempMed[i]
}
#####_____DECLARANDO LA BASE COMO SERIE DE TIEMPO_____#####
Anomaly.ts <- ts(dat$AveTemp,start = decimal_date(as.Date(dat$Time[1])),
                 frequency = periodo)
autoplot(Anomaly.ts,ts.colour = "darkred",xlab = "Tiempo",
         ylab = "Anomalia de temperatura (C)")
#####_____FUNCIONES DE AUTOCORRELACION_____#####
par(mfrow=c(1,2))
acf(Anomaly.ts,lag.max = 20,xlab="Rezago",ylab="ACF",main="ACF")
pacf(Anomaly.ts,lag.max = 20,xlab="Rezago",ylab="PACF",main="PACF")
adf.test(Anomaly.ts)
kpss.test(Anomaly.ts)
summary(ur.df(Anomaly.ts, type = "none", selectlags = "AIC"))
arima(Anomaly.ts,order = c(1,0,1))
#####_____ESTABILIZANDO CON OPERADOR DE REZAGO_____#####
DifAnomaly <- diff(Anomaly.ts)
autoplot(DifAnomaly,ts.colour = "darkred",
         xlab = "Fecha",ylab = "1a diferencia de Anomalía")
#####_____PRUEBAS DE RAIZ UNITARIA_____#####
adf.test(DifAnomaly)
kpss.test(DifAnomaly)
summary(ur.df(DifAnomaly, type = "none", selectlags = "AIC"))
#####____FUNCIONES DE AUTOCORRELACION_____#####
par(mfrow=c(1,2))
acf(DifAnomaly,lag.max = 20,xlab="Rezago",ylab="ACF",main="ACF")
pacf(DifAnomaly,lag.max = 20,xlab="Rezago",ylab="PACF",main="PACF")
#####_____AJUSTES DE MODELO ARIMA_____#####
auto.arima(DifAnomaly,seasonal=T,ic ="aic")
auto.arima(DifAnomaly,seasonal=T,ic ="bic")
auto.arima(DifAnomaly,seasonal=F,ic ="aic")
modeloAR <- auto.arima(DifAnomaly,
                        seasonal=F, ic ="aic",
                        trace=F)
summary(modeloAR)
#####_____PRONOSTICO_____#####
par(mfrow=c(1,1))
pronos <- forecast(modeloAR,h=24,level=95)
plot(pronos,xlim=c(2000,2016),xlab="Fecha",
     ylab="Anomalía Diferenciada (C)")
lines(pronos$fitted,lty=2,col="purple",lwd=2)
#####_____INVERSION DEL MODELO_____#####
pronostico <- function(vec,low,upp,x0){
  pronos <- rep(0,length(vec))
  lower <- rep(0,length(vec))
  upper <- rep(0,length(vec))
  for (i in 1:length(vec)) {
    if(i==1)  {
      pronos[i] <- vec[i]+x0
      lower[i] <- low[i]+0
      upper[i] <- upp[i]+0
      }
    else{
      pronos[i]<- vec[i]+vec[i-1]
      lower[i] <- low[i]+low[i-1]
      upper[i] <- upp[i]+upp[i-1]
      }
  }
  result <- data.frame(pronos=pronos,lower=lower,upper=upper)
  return(result)
}
p1 <- pronostico(pronos$mean,pronos$lower,pronos$upper,
                 Anomaly.ts[length(Anomaly.ts)])
ini <- month(dat$Time[length(dat$Time)])
Temp <- c(TempMed[ini:12],TempMed[1:12],TempMed[1:(ini-1)])
p1$pronos <- p1$pronos+Temp
p1$lower <- p1$lower+Temp
p1$upper <- p1$upper+Temp
p1 <- ts(p1, start = decimal_date(as.Date(dat$Time[length(Anomaly.ts)])+31),
         frequency = periodo)
plot(Temp.ts,xlab="Fecha",ylab="Temperatura (C)",
     xlim=c(2000,2016),ylim=c(15,26))
lines(p1[,1],col="red")
lines(p1[,2],lty=2,col="orange") 
lines(p1[,3],lty=2,col="orange") 
#####_____PRONOSTICO CON HOLT-WINTERS_____#####
par(mfrow=c(1,1))
hw <- HoltWinters(Temp.ts)
hw.pred <- predict(hw,24,prediction.interval = T,level = 0.95)
hw1 <- forecast(hw,h=24,level = c(95))
plot(hw1,xlim=c(2000,2016),xlab="Fecha",ylab="Temperatura (C)")
lines(hw1$fitted,lty=2,col="purple",lwd=2)

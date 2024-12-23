#####_____INSTALACION DE PAQUETES_____#####
install.packages("TTR")
install.packages("readxl")
install.packages("lubridate")
install.packages("stats")
install.packages("lmtest", repos = "https://cran.r-project.org/")
#####_____CARGAR LIBRERIAS DE TRABAJO_____#####
library(TTR)
library(readxl)
library(lubridate)
library(lmtest)
#####_____FUNCIONES_____#####
line <- function(x){
  y <- mod$coefficients[1]+mod$coefficients[2]*x
  return(y)
}
#####_____ESTABLECER EL DIRECTORIO DE TRABAJO_____#####
workdir <- "/home/acga/Documents/Statistics/Time Series/"
setwd(workdir)
#####_____IMPORTAR LA BASE DE DATOS_____#####
expimport2 <- read_excel("expimport2.xlsx")
expimport.ts <- ts(expimport2,start=1990,frequency = 4)
expimport.ts
expimport.ts <- ts(expimport.ts[,2],start=1990,frequency = 4)
t <- 1:length(expimport2$IMPORT)
plot(expimport.ts)
#####_____AJUSTAR UN MODELO DETERMINISTICO PARA LA TENDENCIA_____#####
mod <- lm(expimport2$EXPORT~t)
y_pred <- line(t)
######_____GRAFICAR LA SERIE Y EL AJUSTE_____#####
plot(t,expimport.ts,type="o",pch=16,col="blue",xlab = "Tiempo",
     ylab = "Ventas")
lines(t,y_pred,col="red")
#####_____PRUEBAS DE AUTOCORRELACION_____#####
dwtest(mod)
bgtest(mod)


#####_____SERIES DE PRACTICA_____#####
#####_____SENAMHI TEMPERATURA MINIMA_____#####
dat <- read_excel("senamhi.xlsx")
dat.ts <- ts(dat$Temp_min,start=2017,frequency = 365)
plot(dat.ts)
d <- decompose(dat.ts)
trend <- as.vector(na.omit(d$trend+d$random))
start <- min(which(!is.na(trend)))
end <- max(which(!is.na(trend)))
#AJUSTAR UN MODELO DETERMINISTICO PARA LA TENDENCIA
t <- start:end
mod <- lm(na.omit(trend)~t)
y_pred <- line(t)
#GRAFICAR LA SERIE Y EL AJUSTE
plot(t,trend,type="o",pch=16,col="blue",xlab = "Tiempo",
     ylab = "Temperatura mínima [K]")
lines(t,y_pred,col="red",lwd=2)
#PRUEBAS DE AUTOCORRELACION
dwtest(mod)
bgtest(mod)

#####_____ELECTRICITY PRODUCTION IN AUSTRALIA____#####
dat <- read_excel("electricity.xlsx")
dat.ts <- ts(dat$Electricity,start=1956,frequency = 12)
plot(dat.ts)
#AJUSTAR UN MODELO DETERMINISTICO PARA LA TENDENCIA
t <- 1:length(dat$Month)
mod <- lm(dat$Electricity~t)
y_pred <- line(t)
#GRAFICAR LA SERIE Y EL AJUSTE
plot(as.Date(dat$Month),dat$Electricity,type="l",pch=16,col="blue",
     xlab = "Tiempo", ylab = "Producción de electricidad (Mill. de kWh)")
lines(as.Date(dat$Month),y_pred,col="red",lwd=2)
#####_____PRUEBAS DE AUTOCORRELACION_____#####
dwtest(mod)
bgtest(mod)

#####_____MLTOLLS STACK OVERFLOW_____#####
dat <- read_excel("stack.xlsx")
dat.ts <- ts(dat$r,start=2009,frequency = 12)
plot(dat.ts)
#AJUSTAR UN MODELO DETERMINISTICO PARA LA TENDENCIA
t <- 1:length(dat$month)
mod <- lm(dat$r~t)
y_pred <- line(t)
#GRAFICAR LA SERIE Y EL AJUSTE
plot(as.Date(dat$month),dat$r,type="l",pch=16,col="blue",
     xlab = "Tiempo", ylab = "R clicks")
lines(as.Date(dat$month),y_pred,col="red",lwd=2)
#####_____PRUEBAS DE AUTOCORRELACION_____#####
dwtest(mod)
bgtest(mod)

#####_____INDICE DE CARGA ECONOMICA INE_____####
dat <- read_excel("indice.xlsx")
dat.ts <- ts(dat$ICE,start=decimal_date(as.Date(dat$Date[1])),
             frequency = 12)
plot(dat.ts)
#AJUSTAR UN MODELO DETERMINISTICO PARA LA TENDENCIA
t <- 1:length(dat$Date)
mod <- lm(dat$ICE~t)
y_pred <- line(t)
#GRAFICAR LA SERIE Y EL AJUSTE
plot(as.Date(dat$Date),dat$ICE,type="l",pch=16,col="blue",
     xlab = "Tiempo", ylab = "ICE (PEI/PEA)")
lines(as.Date(dat$Date),y_pred,col="red",lwd=2)
#####_____PRUEBAS DE AUTOCORRELACION_____#####
dwtest(mod)
bgtest(mod)

#####_____YAHOO!_____####
dat <- read_excel("yahoo.xlsx")
dat.ts <- ts(dat$Close,start=decimal_date(as.Date(dat$Date[1])),
             frequency = 365)
plot(dat.ts)
#AJUSTAR UN MODELO DETERMINISTICO PARA LA TENDENCIA
t <- 1:length(dat$Date)
mod <- lm(dat$Close~t)
y_pred <- line(t)
#GRAFICAR LA SERIE Y EL AJUSTE
plot(as.Date(dat$Date),dat$Close,type="l",pch=16,col="blue",
     xlab = "Tiempo", ylab =  "Precio Yahoo")
lines(as.Date(dat$Date),y_pred,col="red",lwd=2)
#####_____PRUEBAS DE AUTOCORRELACION_____#####
dwtest(mod)
bgtest(mod)

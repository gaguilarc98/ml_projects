#####_____INSTALACION DE PAQUETES_____#####
install.packages("TTR")
install.packages("readxl")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ncdf4")
install.packages("stats")
#####_____CARGAR LIBRERIAS DE TRABAJO_____#####
library(TTR)
library(readxl)
library(lubridate)
#####_____ESTABLECER EL DIRECTORIO DE TRABAJO_____#####
workdir <- "/home/acga/Documents/Statistics/Time Series/"
setwd(workdir)
#####_____DESCOMPOSICION DE LA SERIE AIR PASSENGERS_____#####
AP <- AirPassengers
plot(AP)
periodo <- 12
Media <- mean(AP)
media_s <- SMA(AP,n = periodo)
plot(media_s,type="l")
abline(Media,0)
lines(media_s,col="blue")
#####_____IMPORTAR LA BASE DE DATOS_____#####
expimport2 <- read_excel("expimport2.xlsx")
expimport.ts <- ts(expimport2,start=1990,frequency = 4)
expimport.ts
#####_____CONVERTIR A SERIE DE TIEMPO Y GRAFICAR_____#####
expimport.ts <- ts(expimport.ts[,2],start=1990,frequency = 4)
plot(expimport.ts)
#####_____DESCOMPOSICION DE LA SERIE_____#####
d <- decompose(expimport.ts)
plot(expimport.ts,type='o',pch=20)
lines(d$trend,lwd=2,col="red")
#####_____CREACION DE SERIE DE TIEMPO EN R_____#####
importac <- ts(data=c(770,840,945,1138,831,1039,1082,
                      1206,1066,1070,1201,1233),start = 1990,frequency = 4)
importac
d <- decompose(importac)
plot(importac,type='o',pch=20)
lines(d$trend,lwd=2,col="red")


#####_____SERIES DE PRACTICA_____#####
#####_____SENAMHI TEMPERATURA MINIMA_____#####
dat <- read_excel("senamhi.xlsx")
periodo <- 30
Media <- mean(dat$Temp_min)
media_s <- SMA(dat$Temp_min,n = periodo)
plot(media_s,type="l",xlab = "Indice",ylab = "Temperatura mínima [K]")
abline(Media,0)
lines(media_s,col="blue")
#DESCOMPOSICION DE LA SERIE TEMPORAL
dat.ts <- ts(dat$Temp_min,start=2017,frequency = 365)
d <- decompose(dat.ts)
plot(dat.ts,type='l',pch=20,
     xlab = "Fecha",ylab = "Temperatura mínima [K]")
lines(d$trend,lwd=2,col="red")

#####_____ELECTRICITY PRODUCTION IN AUSTRALIA____#####
dat <- read_excel("electricity.xlsx")
periodo <- 30
Media <- mean(dat$Electricity)
media_s <- SMA(dat$Electricity,n = periodo)
plot(media_s,type="l",xlab="Indice",ylab="Producción de electricidad (Millones de kWh)")
abline(Media,0)
lines(media_s,col="blue")
#DESCOMPOSICION DE LA SERIE TEMPORAL
dat.ts <- ts(dat$Electricity,start=1956,frequency = 12)
d <- decompose(dat.ts)
plot(dat.ts,type='l',pch=20,
     xlab="Fecha",ylab="Producción de electricidad (Millones de kWh)")
lines(d$trend,lwd=2,col="red")

#####_____MLTOLLS STACK OVERFLOW_____#####
dat <- read_excel("stack.xlsx")
periodo <- 12
Media <- mean(dat$r)
media_s <- SMA(dat$r,n = periodo)
plot(media_s,type="l",xlab = "Indice",ylab = "R clicks",col="blue")
abline(Media,0)
lines(media_s,col="blue")
#DESCOMPOSICION DE LA SERIE TEMPORAL
dat.ts <- ts(dat$r,start=2009,frequency = 12)
d <- decompose(dat.ts)
plot(dat.ts,type='l',pch=20,xlab = "Fecha",ylab = "R clicks",col="blue")
lines(d$trend,lwd=2,col="red")

#####_____INDICE DE CARGA ECONOMICA INE_____####
dat <- read_excel("indice.xlsx")
periodo <- 12
Media <- mean(dat$ICE)
media_s <- SMA(dat$ICE,n = periodo)
plot(media_s,type="l",xlab = "Indice",ylab = "ICE (PEI/PEA)")
abline(Media,0)
lines(media_s,col="blue")
#DESCOMPOSICION DE LA SERIE TEMPORAL
dat.ts <- ts(dat$ICE,start=decimal_date(as.Date(dat$Date[1])),
             frequency = 12)
d <- decompose(dat.ts)
plot(dat.ts,type='l',pch=20,xlab = "Fecha",ylab = "ICE (PEI/PEA)")
lines(d$trend,lwd=2,col="red")

#####_____YAHOO!_____####
dat <- read_excel("yahoo.xlsx")
periodo <- 30
Media <- mean(dat$Close)
media_s <- SMA(dat$Close,n = periodo)
plot(media_s,type="l",xlab = "Indice",ylab = "Precio Yahoo")
abline(Media,0)
lines(media_s,col="blue")
#DESCOMPOSICION DE LA SERIE TEMPORAL
dat.ts <- ts(dat$Close,start=decimal_date(as.Date(dat$Date[1])),
             frequency = 365)
d <- decompose(dat.ts)
plot(dat.ts,type='l',pch=20)
lines(d$trend,lwd=2,col="red",xlab = "Fecha",ylab = "Precio Yahoo")


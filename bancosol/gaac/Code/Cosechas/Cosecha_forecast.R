####____CARGA DE LIBRERIAS Y FUNCIONES_____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ca)
library(openxlsx)
library(stats)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____CARGA DE BASE DE DATOS____####

cosecha <- readRDS('D:/!bso/bases/rds/coseY_ene23.rds')
cosecha <- cosecha %>% 
  dplyr::filter(cosechaY>=2015 & cosechaY<2023)
glimpse(cosecha)

ggplot(cosecha,aes(x=mesPos,y=par0Rel,color=factor(cosechaY)))+
  geom_line(size=1)+
  labs(xlab='meses transcurridos',ylab='Mora 0')+
  scale_color_manual(values = paleta(10)[9:1])+
  scale_x_continuous(breaks = seq(0,96,12),labels = seq(0,96,12))+
  theme_minimal()

####____SMOOTHING____####
cosechaSmooth <- cosecha %>% 
  group_by(cosechaY) %>% 
  arrange(mesPos) %>% 
  mutate(par0_smooth=rollmean(par0,k = 5,fill = NA)) %>% 
  mutate(par0Rel_smooth=rollmean(par0Rel,k = 5,fill = NA)) %>% 
  ungroup() %>% 
  arrange(cosechaY,mesPos)

ggplot(cosechaSmooth,aes(x=mesPos,y=par0Rel_smooth,color=factor(cosechaY)))+
  geom_line(size=1)+
  labs(xlab='meses transcurridos',ylab='Mora 0')+
  scale_color_manual(values = paleta(10)[9:1])+
  scale_x_continuous(breaks = seq(0,96,12),labels = seq(0,96,12))+
  theme_minimal()

####____CRATING DATA TRAINING____####

cosechaBase <- cosechaSmooth %>% 
  group_by(cosechaY) %>% 
  mutate(csd=cumsd(par0Rel_smooth)) %>% 
  mutate(X1=dplyr::lag(par0Rel_smooth,1),
         X2=dplyr::lag(par0Rel_smooth,2),
         X3=dplyr::lag(par0Rel_smooth,3)) %>% 
  ungroup() %>% 
  arrange(cosechaY,mesPos) %>% 
  dplyr::filter(!is.na(X1) & !is.na(X2) & !is.na(X3))

cosechaBase2 <- cosechaBase %>% 
  group_by(cosechaY) %>% 
  arrange(mesPos) %>% 
  mutate(cmean=cummean(par0Rel_smooth)) %>% 
  mutate(cmax=cummax(par0Rel_smooth)) %>% 
  ungroup() %>% 
  dplyr::select(cosechaY,par0Rel_smooth,X1,X2,X3,cmean,cmax,csd)
  

plot(cosechaBase2[-1,],col=factor(cosechaBase2[1,]),pch='*')

cosecha2015 <- cosecha %>% 
  dplyr::filter(cosechaY==2015) %>% 
  mutate(X1=dplyr::lag(par0Rel,1),
         X2=dplyr::lag(par0Rel,2),
         X3=dplyr::lag(par0Rel,3)) %>% 
  mutate(grw1=par0Rel-X1) %>% 
  mutate(grw2=par0Rel-X2) %>% 
  mutate(grw3=par0Rel-X3) %>% 
  dplyr::select(par0Rel,X1,X2,X3,grw1,grw2,grw3)
plot(cosecha2015)

####____BY MONTO____####
cosecha2 <- cosecha %>%
  group_by(cosechaY) %>% 
  mutate(prom=totalMdes/totalOdes) %>% 
  ungroup() %>% 
  mutate(pp=prom/mean(prom)) %>% 
  mutate(par0New=par0Rel/pp)

ggplot(cosecha2,aes(x=mesPos,y=par0New,color=factor(cosechaY)))+
  geom_line(size=1.15)+
  labs(xlab='meses transcurridos',ylab='Mora 0')+
  scale_color_manual(values = paleta(10)[9:1])+
  scale_x_continuous(breaks = seq(0,96,12),labels = seq(0,96,12))+
  theme_minimal()
####___NLS TO DATA___####
cosecha2 <- cosecha2 %>% 
  arrange(desc(prom))

cos2015 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2015)
model1 <- nls(par0New ~ a*mesPos^5/(exp(a*mesPos/b-1)),data = cos2015,start = list(a=0.1,b=1.05))

cor(cos2015$par0New,predict(model1))
plot(cos2015$mesPos,cos2015$par0New)
lines(cos2015$mesPos,predict(model1),col="red",lwd=3)

cos2016 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2016)
model2 <- nls(par0New ~ a*mesPos^5/(exp(a*mesPos/b-1)),
             data = cos2016,start = list(a=0.1,b=1.05))

cor(cos2016$par0New,predict(model2))
plot(cos2016$mesPos,cos2016$par0New)
lines(cos2016$mesPos,predict(model2),col="red",lwd=3)

cos2017 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2017)
model3 <- nls(par0New ~ a*mesPos^5/(exp(a*mesPos/b-1)),
             data = cos2017,start = list(a=0.1,b=1.05))

cor(cos2017$par0Rel,predict(model3))
plot(cos2017$mesPos,cos2017$par0New)
lines(cos2017$mesPos,predict(model3),col="red",lwd=3)

cos2018 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2018)
model <- nls(par0Rel ~ a*mesPos^5/(exp(a*mesPos/b-1)),
             data = cos2018,start = list(a=0.1,b=1.05))
model

cos2021 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2021)
model <- nls(par0New ~ a*mesPos^5/(exp(a*mesPos/b-1)),
             data = cos2021,start = list(a=0.1,b=1.05))

cor(cos2021$par0New,predict(model))
plot(cos2021$mesPos,cos2021$par0New)
lines(cos2021$mesPos,predict(model),col="red",lwd=3)

harvest <- function(a,b,x){
  fx <- a*x^5/(exp(a*x/b-1))
  return(fx)
}

x <- 1:80
y <- harvest(a=model$m$getPars()[1],b=model$m$getPars()[2],x)
y1 <- harvest(a=model1$m$getPars()[1],b=model1$m$getPars()[2],x)
y2 <- harvest(a=model2$m$getPars()[1],b=model2$m$getPars()[2],x)
y3 <- harvest(a=model3$m$getPars()[1],b=model3$m$getPars()[2],x)

plot(x,y,type = "l",col="red")
points(cos2021$mesPos,cos2021$par0New,col="blue",lwd=3)
lines(x,y1,col="green",size=1.5)
points(cos)
lines(x,y2,col="orange",size=1.5)
lines(x,y3,col="gray",size=1.5)
####____Models with par0New____####
cosecha2 <- cosecha2 %>% 
  arrange(desc(prom))

cos2015 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2015)
model1 <- nls(par0Rel ~ a*mesPos^5/(exp(a*mesPos/b-1)),data = cos2015,start = list(a=0.1,b=1.05))
model

cor(cos2015$par0Rel,predict(model))
plot(cos2015$mesPos,cos2015$par0Rel)
lines(cos2015$mesPos,predict(model),col="red",lwd=3)

cos2016 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2016)
model2 <- nls(par0Rel ~ a*mesPos^5/(exp(a*mesPos/b-1)),
              data = cos2016,start = list(a=0.1,b=1.05))
model

cor(cos2016$par0Rel,predict(model))
plot(cos2016$mesPos,cos2016$par0Rel)
lines(cos2016$mesPos,predict(model),col="red",lwd=3)

cos2017 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2017)
model3 <- nls(par0Rel ~ a*mesPos^5/(exp(a*mesPos/b-1)),
              data = cos2017,start = list(a=0.1,b=1.05))
model

cor(cos2017$par0Rel,predict(model))
plot(cos2017$mesPos,cos2017$par0Rel)
lines(cos2017$mesPos,predict(model),col="red",lwd=3)

cos2018 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2018)
model <- nls(par0Rel ~ a*mesPos^5/(exp(a*mesPos/b-1)),
             data = cos2018,start = list(a=0.1,b=1.05))
model

cor(cos2018$par0Rel,predict(model))
plot(cos2018$mesPos,cos2018$par0Rel)
lines(cos2018$mesPos,predict(model),col="red",lwd=3)

cos2019 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2019)
model <- nls(par0Rel ~ a*mesPos^5/(exp(a*mesPos/b-1)),
             data = cos2019,start = list(a=0.1,b=1.05))
model

cor(cos2019$par0Rel,predict(model))
plot(cos2019$mesPos,cos2019$par0Rel)
lines(cos2019$mesPos,predict(model),col="red",lwd=3)


cos2021 <- cosecha2 %>% 
  dplyr::filter(cosecha2$cosechaY==2021)
model <- nls(par0Rel ~ a*mesPos^5/(exp(a*mesPos/b-1)),
             data = cos2021,start = list(a=0.1,b=1.05))
model

cor(cos2021$par0Rel,predict(model))
plot(cos2021$mesPos,cos2021$par0Rel)
lines(cos2021$mesPos,predict(model),col="red",lwd=3)

harvest <- function(a,b,x){
  fx <- a*x^5/(exp(a*x/b-1))
  return(fx)
}

x <- 1:80
y <- harvest(a=model$m$getPars()[1],b=model$m$getPars()[2],x)
y1 <- harvest(a=model1$m$getPars()[1],b=model1$m$getPars()[2],x)
y2 <- harvest(a=model2$m$getPars()[1],b=model2$m$getPars()[2],x)
y3 <- harvest(a=model3$m$getPars()[1],b=model3$m$getPars()[2],x)

plot(x,y,type = "l",col="red",lwd="1.5")
points(cos2021$mesPos,cos2021$par0Rel,col="blue",pch=20)
lines(x,y1,col="green",lwd=1.5)
points(cos2015$mesPos,cos2015$par0New,col="green",pch=20)
lines(x,y2,col="orange",lwd=1.5)
points(cos2016$mesPos,cos2016$par0New,col="orange",pch=20)
lines(x,y3,col="purple",lwd=1.5)
points(cos2017$mesPos,cos2017$par0New,col="purple",pch=20)
legend("topleft",legend = c("2015","2016","2017","2021"),
       fill = c("green","orange","purple","blue"))

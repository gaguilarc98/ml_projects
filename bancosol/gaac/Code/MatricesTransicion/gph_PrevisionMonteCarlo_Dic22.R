####____CARGA DE LIBRERÍAS____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(expm)
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(janitor)
library(openxlsx)
require(XLConnect)
library(scales)
library(RColorBrewer)
library(paletteer)
library(plotly)
library(kableExtra)
library(glmnet)
options("encoding" = "UTF-8")
#Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
#======================================

dpprev_avg <- read.csv("D:/!bso/transMat/prev/dpprev_avg12_dic22.csv", skip = 2)
names(dpprev_avg) <- c("CALIFICACION","califPot","previus_Sum","previus_Nro",
                   "newPrev_Sum","newPrev_Nro","difPrev_Sum","difPrev_Nro")

dens_avg <- dpprev_avg %>% 
    group_by(CALIFICACION,califPot) %>%
    mutate(pos=row_number()) %>% 
    ungroup() %>% 
    select(-CALIFICACION,-califPot) %>% 
    group_by(pos) %>%
    summarise_all(sum) %>% 
  mutate(tipo = 'Promedio')

dpprev_ba <- read.csv("D:/!bso/transMat/dpprev_avg12_dic22_sens_ba.csv", skip = 2)
names(dpprev_ba) <- c("CALIFICACION","califPot","previus_Sum","previus_Nro",
                       "newPrev_Sum","newPrev_Nro","difPrev_Sum","difPrev_Nro")

dens_ba <- dpprev_ba %>% 
  group_by(CALIFICACION,califPot) %>%
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  select(-CALIFICACION,-califPot) %>% 
  group_by(pos) %>%
  summarise_all(sum) %>% 
  mutate(tipo = 'B-A +0.5%')

dpprev_ab <- read.csv("D:/!bso/transMat/dpprev_avg12_dic22_sens_ab.csv", skip = 2)
names(dpprev_ab) <- c("CALIFICACION","califPot","previus_Sum","previus_Nro",
                      "newPrev_Sum","newPrev_Nro","difPrev_Sum","difPrev_Nro")

dens_ab <- dpprev_ab %>% 
  group_by(CALIFICACION,califPot) %>%
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  select(-CALIFICACION,-califPot) %>% 
  group_by(pos) %>%
  summarise_all(sum) %>% 
  mutate(tipo = 'A-B +0.5%')

dpprev_pct <- read.csv("D:/!bso/transMat/dpprev_avg12_dic22_sens_PCT_v2.csv", skip = 2)
names(dpprev_pct) <- c("CALIFICACION","califPot","previus_Sum","previus_Nro",
                      "newPrev_Sum","newPrev_Nro","difPrev_Sum","difPrev_Nro")

dens_pct <- dpprev_pct %>% 
  group_by(CALIFICACION,califPot) %>%
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  select(-CALIFICACION,-califPot) %>% 
  group_by(pos) %>%
  summarise_all(sum) %>% 
  mutate(tipo = 'Percentiles extremos')

dens_all <- dens_avg %>% 
  bind_rows(dens_ba) %>% 
  bind_rows(dens_ab) %>% 
  #bind_rows(dens_pct) %>% 
  dplyr::rename(`Matriz de transiciòn` = tipo)

####____EN LOOP____####
dp <- paste0('D:/!bso/transMat/prev/dpprev_',
             c('avg12','12sensP1P99','12sensP5P95','12sensP10P90','12sensP25P75',
               'avg12_Lag12','12sensab','12sensba'),'.csv')
tip <- c('Promedio','Perc 1 y Perc 99','Perc 5 y Perc 95','Perc 10 y Perc 90',
         'Perc 25 y Perc 75','Promedio Lag 12','A-B +0.05%', 'B-A +5%')
densList <- list()
for (i in 1:length(dp)) {
  dpprev_pct <- read.csv(dp[i],skip=2)
  names(dpprev_pct) <- c("CALIFICACION","califPot","previus_Sum","previus_Nro",
                         "newPrev_Sum","newPrev_Nro","difPrev_Sum","difPrev_Nro")
  dens <- dpprev_pct %>% 
    group_by(CALIFICACION,califPot) %>%
    mutate(pos=row_number()) %>% 
    ungroup() %>% 
    select(-CALIFICACION,-califPot) %>% 
    group_by(pos) %>%
    summarise_all(sum) %>% 
    mutate(tipo = tip[i])
  densList[[i]] <- dens
}

dens_all <- bind_rows(densList) %>% 
  dplyr::rename(`Matriz de transición` = tipo)

m <- NULL  
for (t in tip) {
  m <- c(m,mean(dens_all[dens_all$`Matriz de transición`==t,]$newPrev_Sum, na.rm=T))
}
mpromedio <- mean(dens_all[dens_all$`Matriz de transiciòn` == tip[1], ]$newPrev_Sum, na.rm=T)
mab <- mean(dens_all[dens_all$`Matriz de transiciòn` == tip[2], ]$newPrev_Sum, na.rm=T)
mba <- mean(dens_all[dens_all$`Matriz de transiciòn` == tip[3], ]$newPrev_Sum, na.rm=T)
mpct <- mean(dens_all[dens_all$`Matriz de transiciòn` == tip[4], ]$newPrev_Sum, na.rm=T)


ggplot(dens_all,aes(x=newPrev_Sum, fill = `Matriz de transición`))+
  geom_density(alpha=0.5)+
  geom_vline(xintercept = m,color="black")+
  # geom_vline(xintercept = mba,color="black")+
  # geom_vline(xintercept = mab,color="black")+
  # geom_vline(xintercept = mpct,color="black")+
  annotate(geom="text",x=m,y=seq(9e-7,6e-7,length.out=8),label=comma(m),size=3)+
  # geom_text(aes(x=m[1],y=1.1e-6,label=comma(m[1])),size=3,color="black")+
  # geom_text(aes(x=m[2],y=9e-7,label=comma(m[2])),size=3,color="black")+
  # geom_text(aes(x=m[3],y=1.1e-6,label=comma(m[3])),size=3,color="black")+
  # geom_text(aes(x=mpct,y=1.1e-6,label=comma(mpct)),size=3,color="black")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_x_continuous(labels = scales::comma, breaks = seq(10e6,80e6,10e6))+
  labs(x="Dif. Previsión USD", y="Densidad")+
  theme_minimal()+ scale_fill_brewer(palette = 'Set1') +
  theme(plot.title = element_text(size = 10,face="bold"),
        legend.position = 'bottom')
ggsave('D:/!bso/transMat/dens_all_dic22.png')


mpromedio <- mean(dens_avg$newPrev_Sum, na.rm=T)
q05 <- quantile(dens_avg$newPrev_Sum,0.05,na.rm=T)
q50 <- quantile(dens_avg$newPrev_Sum,0.50,na.rm=T)
q99 <- quantile(dens_avg$newPrev_Sum,0.99,na.rm=T)

ggplot(dens_avg,aes(x=newPrev_Sum))+
  geom_density(fill = 'blue', alpha=0.5)+
  geom_vline(xintercept = mpromedio,color="black")+
  geom_vline(xintercept = q05,color="blue")+
  geom_vline(xintercept = q99,color="red")+
  #geom_vline(xintercept = mpct,color="black")+
  geom_text(aes(x=mpromedio,y=1.1e-6,label=comma(mpromedio)),size=3,color="black")+
  geom_text(aes(x=q05,y=9e-7,label=comma(q05)),size=3,color="blue")+
  geom_text(aes(x=q99,y=1.1e-6,label=comma(q99)),size=3,color="red")+
  #geom_text(aes(x=mpct,y=1.1e-6,label=comma(mpct)),size=3,color="black")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_x_continuous(labels = scales::comma, breaks = seq(30500000, 32000000, 500000))+
  labs(x="Dif. Previsión USD", y="Densidad")+
  theme_minimal()+ scale_fill_brewer(palette = 'Set1') +
  theme(plot.title = element_text(size = 10,face="bold"),
        legend.position = 'bottom',
        axis.text.x = element_text(size = 10))
ggsave('D:/!bso/transMat/dens_avg_dic22.png')


#-----
dens_all <- dens_avg %>% 
  bind_rows(dens_ba) %>% 
  bind_rows(dens_ab) %>% 
  bind_rows(dens_pct) %>% 
  dplyr::rename(`Matriz de transiciòn` = tipo)

mpromedio <- mean(dens_all[dens_all$`Matriz de transiciòn` == 'Promedio', ]$newPrev_Sum, na.rm=T)
mab <- mean(dens_all[dens_all$`Matriz de transiciòn` == 'A-B +0.5%', ]$newPrev_Sum, na.rm=T)
mba <- mean(dens_all[dens_all$`Matriz de transiciòn` == 'B-A +0.5%', ]$newPrev_Sum, na.rm=T)
mpct <- mean(dens_all[dens_all$`Matriz de transiciòn` == 'Percentiles extremos', ]$newPrev_Sum, na.rm=T)


ggplot(dens_all,aes(x=newPrev_Sum, fill = `Matriz de transición`))+
  geom_density(alpha=0.5)+
  geom_vline(xintercept = m,color="black")+
  # geom_vline(xintercept = mpromedio,color="black")+
  # geom_vline(xintercept = mba,color="black")+
  # geom_vline(xintercept = mab,color="black")+
  # geom_vline(xintercept = mpct,color="black")+
  annotate(geom="text",x=m,y=1.1e-6,label=comma(m),size=3,color="black")+
  # geom_text(aes(x=mpromedio,y=1.1e-6,label=comma(mpromedio)),size=3,color="black")+
  # geom_text(aes(x=mba,y=9e-7,label=comma(mba)),size=3,color="black")+
  # geom_text(aes(x=mab,y=1.1e-6,label=comma(mab)),size=3,color="black")+
  # geom_text(aes(x=mpct,y=1.1e-6,label=comma(mpct)),size=3,color="black")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_x_continuous(labels = scales::comma, breaks = seq(30e6,80e6,10e6))+
  labs(x="Dif. Previsión USD", y="Densidad")+
  theme_minimal()+ 
  scale_fill_manual(values = paleta(5))+
  #scale_fill_brewer(palette = 'Set1') +
  theme(plot.title = element_text(size = 10,face="bold"),
        legend.position = 'bottom')
ggsave('D:/!bso/transMat/dens_all_dic22_pct.png')

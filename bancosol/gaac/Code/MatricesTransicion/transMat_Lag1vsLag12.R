####____CARGA DE PAQUETES y FUNCIONES____####
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
library(quantmod)
library(scales)
library(stringr)
library(forcats)
library(tseries)
library(scales)
library(janitor)
library(expm)
library(ggplot2)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)

cases <- function(quant,levs,values,default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____CREATING BDC CANCEL LAG 12____####
myrds <- c('Ene2022','Ene2023') #Colocar nombre MonthYYYY del mes previo y mes actual
tryCatch({
  print(myrds[1])
  print(myrds[2])
  
  df1 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', myrds[1], '.rds')) #se abre mes anterior
  df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', myrds[2], '.rds')) #se abre mes posterior
  
  #Se obtienen los cancelados como aquellos que están en el cierre del mes anterior pero no en el posterior
  df3 <- df1[!(df1$OPERACION %in% df2$OPERACION),] %>% 
    select(OPERACION, CI, CTACLIENTE, MONTO, MONEDA, saldous, CALIFICACION, ESTADO,
           tipoCred, sucursal) %>% 
    dplyr::filter(CALIFICACION %in% c("A","B","C","D","E","F","S")) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(mCancel = as.yearmon(paste0(substr(myrds[2],1,3),". ",substr(myrds[2],4,7)))) %>% 
    mutate(mSearch = as.yearmon(paste0(substr(myrds[1],1,3),". ",substr(myrds[1],4,7))))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

df3 <- df3 %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = mCancel) %>%
  mutate(cmt='Z') %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate) %>%
  group_by(cm1, cmt, trans, monDate) %>% 
  tally() %>% 
  dplyr::rename(one = n) %>% 
  glimpse()
####____READING BDC FULL____####
bdcFull <- readRDS('D:/!bso/transMat/bdcFull.rds')
####____BDC TRANS LAG 12___####
bdcTrans <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, DIASMORA, saldous, previus) %>% 
  dplyr::filter(monDate >= 'mar. 2015') %>%
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(OPERACION, CTACLIENTE, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 12)) %>% 
  mutate(difPrev = previus - dplyr::lag(previus)) %>% 
  mutate(DiasMoraMes= DIASMORA - dplyr::lag(DIASMORA,12)) %>% 
  ungroup() 
bdcTrans <- bdcTrans %>% 
  mutate(trans = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                          cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans = ifelse(cm1=='S'&cmt=='S',paste0(cm1,cmt),trans)) %>% 
  dplyr::filter(!is.na(trans)) %>%
  #dplyr::filter(!is.na(DiasMoraMes) & !(DiasMoraMes>31)) %>% 
  ungroup() %>% 
  group_by(cm1, monDate) %>% #Se agrupa por calificacion inicial y por mes
  mutate(rowTot = n()) %>% 
  ungroup()

####____JOIN CANCEL Y TRANS____####
tm_ops <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate,saldous, previus, difPrev) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum, na.rm=T) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(df3) %>% #Aquí se añade la tabla resumen de cancelados
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% #Delimitación temporal
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% #Transiciones en número de operaciones
  mutate(prob = round(one/rowTot*100,2)) %>% 
  mutate(saldoTot = sum(saldous, na.rm = T)) %>% #Transiciones en saldo
  mutate(probS = round(saldous/saldoTot*100,2)) %>% 
  mutate(previTot = sum(previus, na.rm = T)) %>% #Transiciones en previsión
  mutate(probP = round(previus/previTot*100,2)) %>% 
  mutate(difPrevTot = sum(difPrev, na.rm = T)) %>% #Transiciones en diferencia de previsión
  mutate(probDP = round(difPrev/difPrevTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[!(cmt %in% c('A','Z'))]), 
                               cm1 == 'B' ~ sum(prob[!(cmt %in% c('A','B','Z'))]),
                               cm1 == 'C' ~ sum(prob[!(cmt %in% c('A','B','C','Z'))]),
                               cm1 == 'D' ~ sum(prob[!(cmt %in% c('A','B','C','D','Z'))]),
                               cm1 == 'E' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','Z'))]),
                               cm1 == 'F' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','F','Z'))]),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(prob[cmt == 'Z']),
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt %in% c('A','Z')]),
                                  cm1 == 'C' ~ sum(prob[cmt %in% c('A','B','Z')]),
                                  cm1 == 'D' ~ sum(prob[cmt %in% c('A','B','C','Z')]),
                                  cm1 == 'E' ~ sum(prob[cmt %in% c('A','B','C','D','Z')]),
                                  cm1 == 'F' ~ sum(prob[cmt %in% c('A','B','C','D','E','Z')]),
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

tm <- tm_ops %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  dplyr::filter(monDate=="ene. 2023") %>% 
  glimpse()

tmSens <- tm %>% 
  ungroup() %>% 
  arrange(cm1) %>% 
  group_by(cm1) %>% 
  mutate(prob=prob/sum(prob)) %>% 
  ungroup() %>% 
  select(cm1, cmt, prob) %>%
  # mutate(prob = prob/100) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, `F`=0, S=0, Z=1) %>% 
  glimpse()
M12 <- data.matrix(tmSens)
M12
####____LAG 1^12____####
tm <- read.csv('D:/!bso/transMat/Oreports/tmAll_Dic22.csv') %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  rename(prob=probN) %>% 
  dplyr::filter(monDate < 'mar. 2020' | monDate > 'dic. 2021') %>% 
  glimpse()

gph <- tm %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise_all(mean) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1) %>% 
  mutate(prob=prob/sum(prob)) %>% 
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmExp <- gph %>% 
  ungroup() %>% 
  arrange(cm1) %>% 
  select(cm1, cmt, prob) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()

MSens <- data.matrix(tmExp)

M12_v2 <- as_tibble(MSens %^% 12)
M12_v2

Matrices <- list(MLag1=MSens,MLag1_12=M12_v2,MLag12=M12)
write.xlsx(Matrices,file = "D:/!bso/transMat/MLag1_vs_MLag12.xlsx")

####____READING MAT STANDARD & POOR's____####
mat <-read_excel('D:/Files/Presentations/mat.xlsx')
m <- data.matrix(mat)
q <- matrix(0,ncol = 8,nrow = 8)
q <- m*log(diag(m))/(diag(m)-1)
diag(q) <-log(diag(m))
q[8,] <- rep(0,8)
q

m2 <- expm((1/30)*q)
m2
rowSums(m2)
m-m2
####____NEW_STATE____####
mat <- read_excel('D:/Files/Presentations/mat.xlsx',sheet = "mat")
m <- data.matrix(mat)
lam <- read_excel('D:/Files/Presentations/mat.xlsx',sheet = "lam")
l <- data.matrix(lam)
pi <- c(0.84,0.05,0,0,0,0.01,0.07,0.03)
round(pi %*% m * 100,2)
eigen(m)
expm((15/30)*m)
round(pi %*% l * 100,0)

#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
#library(xlsx)
library(dplyr)
#library(foreign)
#library(reshape)
#library(reshape2)
#library(stringr)
library(lubridate)
#library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
#library(forecast)
library(quantmod)
library(scales)
#library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
library(tseries)
#library(xtable)
library(openxlsx)
#library(hrbrthemes)
#library(viridis)
#library(scales)
#library(janitor)
#library(RColorBrewer)
#library(paletteer)
#library(plotly)
library(ggplot2)
library(gt)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
#                    mos[2],'.txt'), encoding = 'Latin-1')
cases <- function(quant,levs,values,default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  #new <- vector(mode = 'character',length = length(quant))
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
#_______________________________________________________________________________
# Data in
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
for (i in 1:length(file_list)) {
  
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/',
                        file_list[i])) %>% 
    #select(-MODULO) %>% 
    #dplyr::filter(substr(AGENCIA, 1, 1) == '6') %>% 
    # mutate(RUBRO = as.character(RUBRO)) %>% 
    # mutate(CALIFICACION = as.character(CALIFICACION)) %>% 
    # mutate(SALDO = as.double(SALDO)) %>% 
    # mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
    # mutate(fbase = substr(file_list[i], 4, 10)) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
           CALIFICACION, fbase, montous, saldous, previus, saldoCast, tipoCred, SECTOR_CARTERA) %>% 
    mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION))
  bdcList[[i]] <- bdc
}
gc()


#_______________________________________________________________________________

bdcFull <- bind_rows(bdcList) %>% 
  mutate(mon = substr(fbase,1,3)) %>% 
  mutate(year = substr(fbase,4,7)) %>% 
  mutate(mes = cases(mon, c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic'),
                     c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))) %>% 
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
  mutate(monDate = as.yearmon(dayDate)) %>% 
  select(-dayDate, -mon, -year, -mes) %>% 
  arrange(CI, CTACLIENTE, OPERACION, monDate) %>% 
  glimpse()
gc()
bdcList <- NULL
write_rds(bdcFull, 'D:/!bso/transMat/bdcFull.rds')

#_______________________________________________________________________________
bdcFull <- readRDS('D:/!bso/transMat/bdcFull2.rds')
bdcFull <- bdcFull %>% 
  arrange(OPERACION, FDESEMBOLSO)


####____COUNTING LOANS____####
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate, saldous, previus, saldoCast) %>% 
  #mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  mutate(saldous = ifelse(CALIFICACION == 'S', saldoCast, saldous)) %>% 
  select(-saldoCast) %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  mutate(difPrev = previus - dplyr::lag(previus)) %>% 
  ungroup() 
bdcTrans <- bdcTrans %>% 
  mutate(trans = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                         cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans = ifelse(cm1=='S'&cmt=='S',paste0(cm1,cmt),trans)) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>%
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()

table(bdcTrans[bdcTrans$monDate == 'nov. 2022',]$trans)
write_rds(bdcTrans, 'D:/!bso/transMat/bdcTrans2.rds')

bdcTrans <- readRDS("D:/!bso/transMat/bdcTrans2.rds")

BA <- bdcTrans %>% 
  dplyr::filter((monDate=="dic. 2022" | monDate=="nov.2022" )& trans=="BA") %>% 
  arrange(desc(saldous)) %>% 
  group_by(monDate) %>% 
  mutate(avgsaldo=sum(saldous)/n()) %>% 
  mutate(saldoTot=sum(saldous)) %>% 
  ungroup() %>% 
  arrange(monDate,desc(saldous))
AB <- bdcTrans %>% 
  dplyr::filter((monDate=="dic. 2022" | monDate=="nov.2022" )& trans=="AB") %>% 
  arrange(desc(saldous)) %>% 
  group_by(monDate) %>% 
  mutate(avgsaldo=sum(saldous)/n()) %>% 
  mutate(saldoTot=sum(saldous)) %>% 
  ungroup() %>% 
  arrange(monDate,desc(saldous))
####___AGGREGATE TRANSITION MATRICES____####
bdcCancel <- readRDS('D:/!bso/transMat/matCancel2.rds') 
tm_ops <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
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

write.csv(tm_ops,"D:/!bso/transMat/Oreports/tmOps_Dic22.csv",row.names = F)

gphT <- tm_ops %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'dic. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  mutate(eom = as.Date(monDate, frac = 1)) %>% 
  glimpse()


gph <- tm_ops %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  dplyr::filter(trans != 'BF' & trans != 'BE') %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  glimpse()

ggplot(gph, aes(x= Fecha, y = prob, color = trans)) + 
  geom_line(size = 1.25) + theme_minimal() +
  facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.5, "lines")) +
  scale_color_manual(values = cbp1) +
  geom_vline(xintercept = as.Date('2020-04-01'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")

gph <- gphT %>% 
  ungroup() %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  select(monDate, Recuperacion, Permanencia, Deterioro, panel) %>%
  group_by(monDate, panel) %>% 
  summarise_all(mean) %>% 
  pivot_longer(!c('monDate', 'panel'))  %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>%  
  dplyr::rename(Destino = name) %>%
  ungroup() %>%
  group_by(monDate) %>% 
  mutate(ptot = sum(value)) %>% 
  glimpse()

cbp3 <- c( "#D43B1B","#4198B5", "#246D94",
           "#E96732", "#FB9263")
filename = 'D:/!bso/transMat/transicion_desde_B_2.png'
ggplot(gph, aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.5, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)') +
  ggtitle('Evolucion de la probabilidad mensual de transición desde B') +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2022-01-31'), linetype="dashed", color = "black")
ggsave(filename)
ggplotly(p)
###################
bdcExp <- gphT %>% 
  select(-monDate) %>% 
  glimpse()
write.csv(bdcExp, 'D:/!bso/transMat/tm_pbi.csv', row.names = F)

#-------------------------------------------------------------------------------
# exporting monthly for 2022

tm2022_prob <- tm_ops %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(prob, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = prob) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0)) %>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write_xlsx(tm2022_prob, 'D:/!bso/transMat/tmprob_2022.xlsx')

####____CUMULATE PROBABILITIES____####
tm_probL <- list()
mes <- unique(tm2022_prob$monDate)
recup <- NULL
deter <- NULL
perm <- NULL
i <- 1
for (i in 1:length(mes)) {
  tm_probL[[i]] <- tm2022_prob[which(tm2022_prob==mes[i]),-c(1:2)]
  recup <- c(recup,rowSums(tm_probL[[i]][,]*lower.tri(tm_probL[[i]],diag=FALSE))+tm_probL[[i]]$Z)
  deter <- c(deter,rowSums(tm_probL[[i]][,-8]*upper.tri(tm_probL[[i]][,-8],diag = F)))
  perm <- c(perm,diag(as.matrix(tm_probL[[i]])))
}


tm_probL <- lapply(tm_probL,as.matrix)
tm_probM <- as.data.frame(Reduce('+',tm_probL)/length(tm_probL)) %>% 
  mutate(cm1 = c("A","B","C","D","E","F","S")) %>% 
  rowwise() %>% 
  mutate(Total=round(sum(c_across(where(is.numeric))),0))%>% 
  gt(rowname_col = "cm1") %>% 
  tab_spanner(label = "Destino",columns = c('A','B','C','D','E','F','S','Z')) %>% 
  opt_stylize(style = 3,color="pink")


tm2022_prob <- tm2022_prob %>% 
  group_by(monDate,cm1) %>% 
  rowwise() %>% 
  mutate(Total=round(sum(c_across(where(is.numeric))),0))%>% 
  gt(rowname_col = "cm1",groupname_col = "monDate",auto_align = T) %>% 
  tab_options(row_group.as_column = T,row_group.padding = px(1500)) %>% 
  tab_spanner(label = "Destino",columns = c('A','B','C','D','E','F','S','Z')) %>% 
  opt_stylize(style = 3,color = "pink")
tm2022_probSum <- data.frame(monDate=tm2022_prob$monDate,Recup. = recup, Deter. = deter, Perm. = perm) %>% 
  rowwise() %>% 
  mutate(Total=round(sum(c_across(where(is.numeric))),0)) %>% 
  gt(groupname_col = "monDate") %>% 
  tab_options(row_group.as_column = T,row_group.padding = px(1500)) %>% 
  opt_stylize(style=3,color="pink")

####____PLOTTING TRANSITIONS AB BA____####
plot_tmBA <- list()
plot_tmAB <- list()
for (i in 1:length(tm_probL)) {
  plot_tmBA[[i]] <- tm_probL[[i]][2,1]
  plot_tmAB[[i]] <- tm_probL[[i]][1,2]
}

plot_tm <- data.frame(x=1:length(tm_probL),BA=unlist(plot_tmBA),AB=unlist(plot_tmAB))

ggplot(plot_tm, aes(x=x,y=BA))+
  geom_bar(stat="identity",fill="slateblue3")+
  geom_smooth(method="lm",se=FALSE,color="tan3",size=1.5,linetype=2)+
  theme_minimal()

ggplot(plot_tm, aes(x=x,y=AB))+
  geom_bar(stat="identity",fill="slateblue3")+
  geom_smooth(method="lm",se=FALSE,color="tan3",size=1.5,linetype=2)+
  theme_minimal()

####____TRANSITIONS MATRICES BY OPERATION COUNTING_____####
tm2022_ops <- tm_ops %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(one, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = one) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write_xlsx(tm2022_ops, 'D:/!bso/transMat/tmops_2022.xlsx')

####____CREATING PORTFOLIO TRANSITIONS____####

tm_all <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate, saldous, previus, difPrev) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum, na.rm = T) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(probN = round(one/rowTot*100,2)) %>% 
  mutate(saldoTot = sum(saldous, na.rm = T)) %>% 
  mutate(probS = round(saldous/saldoTot*100,2)) %>% 
  mutate(previTot = sum(previus, na.rm = T)) %>% 
  mutate(probP = round(previus/previTot*100,2)) %>% 
  mutate(difPrevTot = sum(difPrev, na.rm = T)) %>% 
  mutate(probDP = round(difPrev/difPrevTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(probN[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(probN[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(probN[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(probN[cmt == 'Z']),
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(probN[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(probN[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(probN[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(probN[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(probN[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(probN[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(probN[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(probN[cmt == 'B']),
                                 cm1 == 'C' ~ sum(probN[cmt == 'C']),
                                 cm1 == 'D' ~ sum(probN[cmt == 'D']),
                                 cm1 == 'E' ~ sum(probN[cmt == 'E']),
                                 cm1 == 'F' ~ sum(probN[cmt == 'F']),
                                 cm1 == 'S' ~ sum(probN[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(probN[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

write.csv(tm_all,"D:/!bso/transMat/Oreports/tmAll_Dic22.csv",row.names = F)

tm2022_saldo <- tm_all %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(saldous, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = saldous) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write_xlsx(tm2022_saldo, 'D:/!bso/transMat/tmsaldo_2022.xlsx')

# tm2022_previ <- tm_all %>%
#   dplyr::filter(monDate > 'dic. 2021') %>%
#   select(previus, cm1, cmt, monDate) %>%
#   pivot_wider(names_from = cmt, values_from = previus) %>%
#   arrange(monDate, cm1) %>%
#   dplyr::filter(!is.na(cm1)) %>%
#   #select(-`NA`) %>%
#   replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
#   mutate(monDate=as.character(monDate)) %>% 
#   relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
# write_xlsx(tm2022_previ, 'D:/!bso/transMat/tmprevi_2022.xlsx')

tm2022_difPrev <- tm_all %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(difPrev, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = difPrev) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0))%>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z)
write_xlsx(tm2022_difPrev, 'D:/!bso/transMat/tmdifprev_2022.xlsx')

#-------------------------------------------------------------------------------
# MWE
mwe <- P2full %>%
  dplyr::filter(Operacion == 3267334)
write.xlsx(mwe, 'D:/!bso/mph/3267334.xlsx')
mwe <- P2full %>%
  dplyr::filter(Operacion == 2967255)
write.xlsx(mwe, 'D:/!bso/mph/2967255.xlsx')

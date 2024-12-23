####____UNIENDO Y SEPARANDO BASES____####
#_____________________________________________________________
####____ARCHIVOS  BIMENSUALES____####
Pagos <- fread('D:/!bso/transMat/PagosCarteraDesdeEnero2022.csv')
meses <- 1:9
P <- Pagos %>% 
  dplyr::filter(month(FechaPago)==1 | month(FechaPago)==2) %>%
  mutate(FechaPago=as.character(FechaPago)) %>%
  mutate(FechaPrevistaPago=as.character(FechaPrevistaPago)) %>%
  mutate(FechaTrx=as.character(FechaTrx)) %>%
  select(everything())
write_xlsx(P,path = "D:/!bso/PagosCartera_ene_feb_2022.xlsx",col_names = T)
P <- Pagos %>% 
  dplyr::filter(month(FechaPago)==3 | month(FechaPago)==4) %>%
  mutate(FechaPago=as.character(FechaPago)) %>%
  mutate(FechaPrevistaPago=as.character(FechaPrevistaPago)) %>%
  mutate(FechaTrx=as.character(FechaTrx)) %>%
  select(everything())
write_xlsx(P,path = "D:/!bso/PagosCartera_mar_abr_2022.xlsx",col_names = T)
P <- Pagos %>% 
  dplyr::filter(month(FechaPago)==5 | month(FechaPago)==6) %>% 
  mutate(FechaPago=as.character(FechaPago)) %>%
  mutate(FechaPrevistaPago=as.character(FechaPrevistaPago)) %>%
  mutate(FechaTrx=as.character(FechaTrx)) %>%
  select(everything())
write_xlsx(P,path = "D:/!bso/PagosCartera_may_jun_2022.xlsx",col_names = T)
P <- Pagos %>% 
  dplyr::filter(month(FechaPago)==7 | month(FechaPago)==8) %>%
  mutate(FechaPago=as.character(FechaPago)) %>%
  mutate(FechaPrevistaPago=as.character(FechaPrevistaPago)) %>%
  mutate(FechaTrx=as.character(FechaTrx)) %>%
  select(everything())
write_xlsx(P,path = "D:/!bso/PagosCartera_jul_ago_2022.xlsx",col_names = T)
P <- Pagos %>% 
  dplyr::filter(month(FechaPago)==9) %>%
  mutate(FechaPago=as.character(FechaPago)) %>%
  mutate(FechaPrevistaPago=as.character(FechaPrevistaPago)) %>%
  mutate(FechaTrx=as.character(FechaTrx)) %>%
  select(everything())
write_xlsx(P,path = "D:/!bso/PagosCartera_sep_2022.xlsx",col_names = T)

####____AÑADIENDO OCTUBRE____####
Pagos <- fread('D:/!bso/transMat/PagosCarteraDesdeEnero2022.csv')
Pagos_oct <- fread('D:/!bso/transMat/PagosCarteraOct2022.csv')
P <- Pagos %>% 
  dplyr::filter(month(FechaPago)!=10) %>% 
  bind_rows(Pagos_oct)
write.csv(P,'D:/!bso/transMat/PagosCarteraHastaOct2022.csv',row.names = F)
#_____________________________________________________________
####____COMANDOS EJEMPLO____####
horabin <- cut(diaPago$hora,breaks=bins)
unique(horabin)
glimpse(Pagos)

df1 <- data.frame(id=c('a','b','c','d','e','f'),ob=1:6,sep=sample(c(1,2),6,replace = T))
df2 <- data.frame(id=c('a','b','e','f'),sep=sample(c(1,2),4,replace = T))
df1 %>% 
  group_by(sep) %>% 
  right_join(group_by(df2,sep))

Pagos[Pagos$FechaPago>=fechad & Pagos$FechaPago<=fechad+2,]

meses <- list(
  '1'="Enero",'2'= 'Febrero','3'= 'Marzo','4'='Abril',
  '5' = 'Mayo','6' = 'Junio', '7' = 'Julio','8' = 'Agosto')
meses_labeller <- function(var,val){
  return(meses[val])
}
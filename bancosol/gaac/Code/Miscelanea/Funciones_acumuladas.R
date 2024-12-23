####____FUNCION DE DESVIACION ESTANDAR ACUMULADA____####
cumsd <- function(x){
    if(!is.numeric(x)){ return(NA)}
    sdev <- vector(mode="numeric",length=length(x))
    sdev[1] <- NA
    for(i in 2:length(x)) sdev[i] <- sd(na.omit(x[1:i]))
    return(sdev)
}
#Ejemplo de aplicación
cumsd(c(1,1,0,0,NA,1,2))
cumsd(c("1",1,0,0,NA,1,2))
#____FUNCION DE MEDIA ACUMULADA
cummed <- function(x){
    if(!is.numeric(x)){ return(NA)}
    med <- vector(mode="numeric",length=length(x))
    med[1] <- NA
    for(i in 2:length(x)) med[i] <- mean(na.omit(x[1:i]))
    return(med)
    #return(c(NA,cumsum(na.omit(x))))
}
#####____FUNCION TABLA DE AMORTIZACION____####
#A: Capital inicial
#r: Redito anual
#n: Número de pagos
#m: Descuentos por año
#fini: Fecha inicial de crédito
tab_amort <- function(A,r,n,m=12){
  i <- r/m
  C <- A*i/(1-(1+i)^(-n))
  tabla <- matrix(0,nrow=n+1,ncol=5)
  tabla[1,] <- c(0,0,0,0,A)
  for (k in 2:(n+1)) {
    interes <- tabla[k-1,5]*i
    amort <- C-tabla[k-1,5]*i
    tabla[k,] <- c(k-1,C,interes,amort,tabla[k-1,5]-amort)
  }
  tabla[,5] <- round(tabla[,5],1)
  colnames(tabla)<- c("Mes","Cuota","Interés","Amortización","Saldo")
  return(tabla)
}
tab_amort(1000000,0.24,12,12)
####____FUNCION DE CASES____####
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
####____PRUEBA FUNCION CASES____####
Mes <- c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic')
month <- c('jan','feb','mar','apr','may','jun','jul','ago','sep','oct','nov','dec')
set.seed(20221115)
ejem <- data.frame(mon=sample(Mes,size = 9000000,replace = T))
system.time(
  ejem <- ejem %>% 
    mutate(Meses =case_when(mon == 'Ene'~'jan',
                            mon == 'Feb'~'feb',
                            mon == 'Mar'~'mar',
                            mon == 'Abr'~'apr',
                            mon == 'May'~'may',
                            mon == 'Jun'~'jun',
                            mon == 'Jul'~'jul',
                            mon == 'Ago'~'aug',
                            mon == 'Sep'~'sep',
                            mon == 'Oct'~'oct',
                            mon == 'Nov'~'nov',
                            mon == 'Dic'~'dec',))
)
system.time(
  ejem <- ejem %>% 
    mutate(Meses = cases(mon,Mes,month))
)
cases <- function(quant,levs,values){
  #quant es la variable (columna) a la que se aplica la función
  #levs es un vector con los niveles que tiene la variable quant
  #values es un vector con los valores a asignar por cada valor en levs
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- vector(mode = 'character',length = length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
month(Pagos$FechaPago)
values <- 1:12
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
m <- cases(month(Pagos$FechaPago),values,meses)
####____PALETA DE COLOR____####
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3","slateblue3",
                             "red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
pie(rep(5,12),col=paleta(12),radius = 1)
####____FREAD____####
#copy the following to a txt and name it Ex.txt
# "AA",3,3,3,3
# "CC","ad",2,2,2,2,2
# "CC","ad",2,2,2,2,2
# "ZZ",2
# "AA",3,3,3,3
# "CC","ad",2,2,2,2,2
m <- fread("D:/Data/Ex.txt",encoding = "Latin-1")
m <- fread("D:/Data/Ex.txt",encoding = "Latin-1",fill = T)
#When we do not use fill=T, first row will always be used as column names
#and if the second row is longer the names will be filled authomatically
#However when it finds a row that has less elements than the second it stops reading
#In case we set fill=TRUE it will read until the end of file and will fill the
#incomplete rows with NAs it stops reading when there

####____replace_na with dplyr_____####
replace_na(list(califBSO_2="NC",califSF_2="NC"))
####____PALETA2____####
paleta2 <- colorRampPalette(c("#460087","#AA0064","#FF7D00","#FFDC00"))
pie(rep(5,12),col=paleta2(12),radius = 1)
####____AGRUPAR____####
agrupar <- function(x, vstep, vgrupo, vagre, pct=5, tms=100, last= 1){
  ult <- x %>% distinct({{vstep}}) %>% arrange(desc({{vstep}})) %>% 
    dplyr::filter(row_number()==last) %>% pull
  y <- x %>% 
    group_by({{vstep}}) %>% 
    mutate(rat = {{vagre}}/sum({{vagre}}, na.rm = T)*tms) %>% 
    #VARIANTE1
    group_by({{vstep}},{{vgrupo}}) %>%
    summarise(tot = sum({{vagre}}, na.rm = T),rat = sum(rat, na.rm = T)) %>%
    ungroup() %>%
    mutate(ORDEN = ifelse({{vgrupo}} %in% {{vgrupo}}[rat>=pct & {{vstep}} == ult],
                          {{vgrupo}}, "Otros")) %>%
    mutate(ORDEN = fct_reorder(ORDEN,rat)) %>%
    #VARIANTE2
    # group_by({{vstep}},{{vgrupo}}) %>% 
    # summarise(tot = sum({{vagre}}, na.rm = T),rat = sum(rat, na.rm = T)) %>% 
    # mutate(ORDEN = ifelse({{vgrupo}} %in% {{vgrupo}}[rat>=pct & {{vstep}} == ult],
    #                       {{vgrupo}}, "Otros")) %>% 
    # mutate(ORDEN = fct_reorder(ORDEN,rat)) %>% 
    # ungroup() %>% 
    #
    group_by({{vstep}},ORDEN) %>% 
    summarise(tot = sum(tot,na.rm = T),rat=sum(rat,na.rm = T)) %>% 
    ungroup()
  z <- y %>%
    group_by({{vstep}}) %>% 
    summarise(tot = sum(tot,na.rm = T),rat=sum(rat,na.rm = T))
  result <- list(y=y,z=z)
  return(result)
}
####____CUTS <=____####
cuts <- function(quant,levs,values,lowest = 0){
  new <- rep(NA,length(quant))
  levs <- c(lowest,levs)
  n <- length(values)
  for (i in 1:n) {
    new[which(quant>levs[i] & quant<=levs[i+1])] <- values[i]
  }
  return(new) 
}
####____COMBINAR PDFs____####
library(qpdf)
setwd('D:/Files/Norma externa')
doc <- c("ASFI-754.pdf","ASFI-755.pdf")
qpdf::pdf_combine(input = doc,
                  output = 'D:/combinado.pdf')
####____CHANGE NAMES OF FILES____####
x <- list.files()
setwd("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/process/")
file.rename(from = x, to = paste0("comp_",x))

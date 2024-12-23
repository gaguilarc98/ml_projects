install.packages("MASS")
library(MASS)
library(dplyr)
library(ggplot2)
set.seed(20221127)
dat <- as.data.frame(mvrnorm(n=1000,mu=c(500,500),
                             Sigma=matrix(c(5,3,4,4),ncol=2))) %>% 
  mutate(sep=0) %>% 
  rename(X=V1,Y=V2)
x <- rnorm(50,mean = 497,sd=2)
y <- 0.5+0.995*x+rnorm(25,mean = 0,sd=1.3)
dat3 <-data.frame(X=c(497,499,498,498.5),Y=c(501,502,501.5,501.3),sep=c(1,1,1,1))
dat2 <- data.frame(X=x,Y=y) %>% 
  mutate(sep=1) %>% 
  bind_rows(dat) %>% 
  bind_rows(dat3)

paleta <- colorRampPalette(c("slateblue3","tan3"),bias=1.5)

ggplot(dat2,aes(x=X,y=Y,color=factor(sep)))+
  geom_point()+
  scale_color_manual(values = c("darkviolet","tan1"))+
  theme_minimal()

r <- rbeta(100,2,2)
pos <- sample(1:nrow(dat),size=50,replace = F)
xx <- dat$X[pos]+r*(dat3$X-dat$X[pos])
yy <- dat$Y[pos]+r*(dat3$Y-dat$Y[pos])
dat4 <- data.frame(X=xx,Y=yy,sep=1) %>% 
  bind_rows(dat2)

ggplot(dat4,aes(x=X,y=Y,color=factor(sep)))+
  geom_point(size=1.5)+
  scale_color_manual(values = c("darkviolet","tan1"))+
  theme_minimal()

x <- rexp(5000,0.05)
x1 <- rexp(50,0.1)
x2 <- rexp(100,0.08)
y <- runif(5000,0,500)
y1 <- runif(50,0,500)
y2 <- runif(100,0,500)
cat <- c(rep(0,5000),rep(1,50),rep(0,100))
dat <- data.frame(X=c(x,x1,x2),Y=c(y,y1,y2),C=cat)

ggplot(dat,aes(x=X,y=Y,color=factor(C)))+
  geom_point()+
  labs(color="Clase",title="Antes de muestreo en clase mayoritaria",x=NULL,y=NULL)+
  scale_color_manual(values = c("slateblue3","orange1"))+
  theme_minimal()

cat <- c(rep(1,50),rep(0,100))
dat2 <- data.frame(X=c(x1,x2),Y=c(y1,y2),C=cat)

ggplot(dat2,aes(x=X,y=Y,color=factor(C)))+
  geom_point()+
  labs(color="Clase",title="Después de muestreo en clase mayoritaria",x=NULL,y=NULL)+
  scale_color_manual(values = c("slateblue3","orange1"))+
  theme_minimal()


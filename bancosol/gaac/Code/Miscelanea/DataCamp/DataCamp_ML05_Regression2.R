####____PARALLEL SLOPES LINEAR REGRESSION____####
#Parallel slope regression
#Interactions (paradoxes with categorical expansion vars)
#Multiple regression (multiple explanatory variable)
#Multiple logistic regression
library(ggplot2)
library(dplyr)
install.packages("moderndive")
library(moderndive)
df <- iris 
model <- lm(Sepal.Length~Petal.Length + Species,df)
summary(model)
#Adding +0 gives a parameter for each level of the categorical variable (but no intercept)
model2 <- lm(Sepal.Length~Petal.Length + Species+0,df)
summary(model2)
#INTERPRETATION:
# * When intercept is not zero
#Each coefficient is the expected increment of the outcome when the explanatory variable increases a unit
# * When intercept is zero
#Coefficients for numerical vars are the increment of the outcome when the num var increases (slope)
#Coefficients for categorical vars represent the mean response when all numerical var are set to zero.

#Linear fit
ggplot(df, aes(Sepal.Width,Sepal.Length))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)
#Plot for cat vars
ggplot(df, aes(Species, Sepal.Length))+
  geom_boxplot()+
  stat_summary(fun.y = mean, shape=15) #With moderndive package
#Parallel slopes regression
ggplot(df, aes(Sepal.Width, Sepal.Length, color =Species))+
  geom_point()+
  geom_parallel_slopes(se=FALSE)
#As the names suggests the names of PARALLEL SLOPE MODEL
#comes from the fact that the slope for each category is the same 
#but the lines are separated by an amount that is the mean response of each level

####____PREDICTING PARALLEL SLOPES____####
#CREATE ARTIFICIAL DATA
#expandgrid(var=seq(1,10,1), species = c('A','B')) this gives a combination of each level
exp_data <- expand.grid(Petal.Length=seq(2,4,0.25), Species = c('setosa','versicolor','virginica'))
exp_data %>% 
  mutate(SepalLpred = predict(model2, exp_data)) %>% 
  ggplot(aes(x=SepalLpred, y=Petal.Length, color=Species))+
  geom_point()

#MANUALLY REPRODUCE PREDICTIONS
coeff <- model2$coefficients
print(coeff)
df_pred <- df %>% 
  mutate(intercept = case_when(Species=='setosa'~coeff[2],
                               Species=='versicolor'~coeff[3],
                               Species=="virginica"~coeff[4],),
         pred = coeff[1]*Petal.Length+intercept)
ggplot(df_pred, aes(x=Petal.Length, y=Sepal.Length, color=Species))+
  geom_point()+
  geom_point(data=df_pred, aes(x=Petal.Length, y=pred),size=2)+
  geom_line(aes(y=pred),size=1)
  
#ASSESING MODEL PERFORMANCE
#Adding more explanatory vars increases the determination coef but this is not ideal
#Too many variables cause overfitting. For each additional expansion var we use adj. deter coef

####____MODELS FOR EACH CATEGORY____####
#We could use training model in a list with methods like nest_by() and map()in dply
#Or split() and lapply() in base R. But a naive version is to assign a name to each category

ggplot(df, aes(x=Sepal.Length,y=Petal.Length, color=Species))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)
#And then we can add the results of each model sepparately

#The model with overall data benefits for having more obs and the model for each category
#benefits from not having to deal with outliers from other groups within each group.
####____INTERACTIONS____####
#The effect of one variable changes with another expansion variables.
#NO intercation: var~ var1+ var2
#Implicit interaction: var ~ var1*var2
#Explicit interaction: var ~ var1 + var2 + var1:var2
model_int <- lm(Sepal.Length~Petal.Length*Species, data=df)
model_int
#The coefficients of the interaction terms represent the changes in slope for each level of the interaction
model_int2 <- lm(Sepal.Length~Species + Petal.Length:Species + 0, data=df) #with no global intercept
coefficients(model_int2)

#the coefficients of this las tmodel are identical to those of a model for each category taken separately
model_setosa <- lm(Sepal.Length~Petal.Length, data=df[df$Species=='setosa',])
model_versicolor <- lm(Sepal.Length~Petal.Length, data=df[df$Species=='versicolor',])
model_virginica <- lm(Sepal.Length~Petal.Length, data=df[df$Species=='virginica',])
coefficients(model_setosa)
coefficients(model_versicolor)
coefficients(model_virginica)

####____MAKING PREDICTIONS WITH INTERACTIONS____####
#Simpson's paradox
#trend given by a model in the whole dataset is different from the model in each subset
#Try to plot dataset to capture this behaviour.

####___TWO NUMERIC EXPLANATORY VARIABLES____####
#If we have two numeric variables and one categorical variable as input
#We
df <- iris
install.packages("plot3D")
library(plot3D)
library(ggplot2)
scatter3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length)
#Another approach would be to use a color code for the third dimension
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Petal.Length))+
  geom_point()+
  scale_color_viridis_c(option='inferno')#the c stands for continuous

model <- lm(Sepal.Length~Sepal.Width*Petal.Length, data=df)
summary(model)
#More than two variables
#Facetting by categorical variable
ggplot(df, aes(x=Sepal.Length,y=Sepal.Width, color=Petal.Length))+
  geom_point()+
  scale_color_viridis_c(option='plasma')+
  facet_wrap(vars(Species))
#However visualization gets harder the more variables you put into the model
#But modelling does not get much harder
model3var <- lm(Sepal.Length~Sepal.Width+Petal.Length+Species+0, data=df)
coefficients(model3var)
#2way interaction
model3_2w <- lm(Sepal.Length~Sepal.Width+Petal.Length+Sepal.Width:Petal.Length+ Sepal.Length:Species+ Petal.Length:Species+Species+0, data=df)
coefficients(model3_2w)
#3way interaction
model3_3w <- lm(Sepal.Length~Sepal.Width+Petal.Length+Sepal.Width:Petal.Length+ Sepal.Length:Species+ Petal.Length:Species+Sepal.Length:Petal.Length:Species+Species+0, data=df)
coefficients(model3_3w)
#Shortcuts
model3_3w2 <- lm(Sepal.Length~Sepal.Width*Petal.Length*Species+0,df)
coefficients(model3_3w2)
#2w but not three way interaction shortcut
model3_2w2 <- lm(Sepal.Length~(Sepal.Width+Petal.Length+Species)^2+0,df)
coefficients(model3_2w2)

#How linear regression works
#Calculate residuals=actual-pred. The best fit wants them to be the shortest possible
#Add up all residuals squared. This is the sum of squares. 
#We want to minimize the sum of squares. This can be done analitically, but it quickly gets
#complicated almost impossible to solve it that way so we perform optimization algorithms.
quadra <-function(x){
  return(x[1]^2-x[1]+10+x[2]^2-2*x[2])
}
optim(par=c(0,0), fn=quadra) #Para optimizar con una función vectorial

#MULTIPLE LOGISTIC REGRESSION
#glm(fmla, data, family="binomial")
#predict(model, data, type='reponse')
#predicted_outcome = round(predicted)
#outcomes <- table(pred, actual) then confussion <- conf_mat(outcomes)
#Finally autoplot(confussion)
ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
  geom_point(alpha=0.5)+
  scale_color_gradient2(midpoint=0.5)

df$esvirginica <- ifelse(df$Species=='virginica',1,0)
model_lr <- glm(esvirginica~Sepal.Length*Sepal.Width,df,family="binomial")
dfpred <- df
dfpred$predict <- round(predict(model_lr,df, type="response"))
outcome <- table(dfpred$predict,dfpred$esvirginica)
#library(yardstick)
#confusion = conf_mat(outcome)
####____LOGISTIC DISTRIBUTION____####
#plogis for cdf and qlogis for inverse cdf
#dlogis for likelihood value
#It works the same way as lm
#In logistic regression the metric is no longer the sum of squares
#Another metric is the likelihood y_pred*y_actual and the goal is to maximize this metric.
#Computationally is better to potimize the log-likelihood.
#maximize the -log(likelihood)

####____REGRESSION____####
#Fundamentals
#In the statistical sense regression deals with predicitng the expected value of an outcome.
#It basically means predicting numerical values rather than discrete or categories
#This differentiates it from classification.

#The objective from the statistical persepctive is to understand the data generation process
#It focuses on the relations between the variables.
#From an engineering mindset the aim is to predict numerical values accurately.

#In machine learning the mindset is mostly engineer-based.
#####____LINEAR REGRESSION____#####
#It assumes that the dependent variable is proportional to the independent variables.
datasets::trees
df <- trees
set.seed(1234)
df_train <- df[sample(1:nrow(trees),size=0.90*nrow(trees)),]
df_test <- df[-sample(1:nrow(trees),size=0.90*nrow(trees)),]
linear_model <- lm(Volume~Girth+Height,df_train)
summary(linear_model)
#Note: You can create a formula object with tilde:
form <- Volume~Girth+Height
class(form)
linear_model <- lm(form, df)
summary(linear_model)

#Or form a text:
form <- as.formula("Volume~Girth+Height")
#To get the diagnosis of the model as a DataFrame use glance from broom:
library(broom)
glance(linear_model)
#For the r squared diagnosis use sigr::wrapFTest(cmodel)
install.packages("sigr")
library(sigr)
sigr::wrapFTest(linear_model)

#To get predictions from the model call the predict function with the model and the data
#whose outcome you want to predict.
df_test$pred <- predict(linear_model, df_test)
library(ggplot2)
ggplot(df_test,aes(x=Volume,y=pred))+
  geom_point()+
  geom_abline(color='blue')

#Advantges
#* They are easy to fit plot 
#* They are smooth and less prone to overfit that is performance on new data is better
#* They are interpretable
#Cons
#* It cannot express non additive relationships
#* Collinearity when the input variables are partially correlated 
#* When variables are highly correlated the signs of the coeffcients may change 
#* This affects interpretability but not accuracy so for ML it does not pose great problem.
#* 
#* However when the coefficients or their std errors are too large it could make the model unstable
#* with unreliable predictions.
####____GRAPHICAL ASSESMENT____####
#If the data points ar off the sides of the predicted line it means that the errors
#are correlated with the actual outcomes. Systematics errors.
#It means we do not still have all the important variables or we need an algorithm that shows
#more complex relationships

#RESIDUAL PLOTS: Plot of the residuals (outcome-predicted) vs predictions
#When these are uncorrelated they appear spread evenly above and below with no pattern.
#GAIN CURVES: If we are more interested in sorting the instances than in predicting
#the actual outcomes such as in a ranking/probability or getting the more expensive clients
#We plot a gain curve

#In the gain cuve the diagonal is the outcome of a random sample
install.packages("WVPlots")
library(WVPlots)
GainCurvePlot(df_train,xvar="Height",truthVar = 'Volume',title = 'Volume vs Height')

#ROOT MEAN SQUARED ERROR
#It can be interpreted as the typical prediction error. And many models such as linear models
#Minimize squared errors.
df_train$pred <- predict(linear_model,df_train)
errors <- df_train$Volume-df_train$pred
sqrt(mean(errors^2))
sd(df_train$Volume)
sqrt(mean(errors^2)) < sd(df_train$Volume) #If this is true it means
#That the model tends to estimate the (dependent) variable better than simpli taking the average

##R SQUARED
#It is a value that measures how well a model fits to the data 0<R2<1
#R2 near zero means that the model is no better than simply predicting the average of th data
#It is the ratio of the sum of the squares due to regression divided by the total sum of squares
#R2 = SCReg/SCT = (SCT-SCRes)/SCT =1- SCRes/SCT 

#R2 = rho^2
#Where rho is the correlation between the outcome and the prediction.

#CROSS VALIDATION
#Use it if you do not have enough observations to split the data into training and testing
#in this approach we split the data into smaller data sets and pick one at a time to test
#And use the rest as training

library(vtreat)

splitPlan <- kWayCrossValidation(nRows = nrow(df), nSplits = 3, NULL, NULL)

split <- splitPlan[[1]]
model <- lm(form,df[split$train,])
df$pred[split$app] <- predict(model,df[split$app,])

rms <- sqrt(mean((df$pred-df$Volume)^2, na.rm = T))
rms
#Repeat this process with all combinations. If the model performs well enough
#Then use all data to produce one final model. However, this las one cannot be validated
#CROSS VALIDATION ONLY TESTS THE MODELLING PROCESS, while TESTING SPLITS tests
#the final model.

####____CATEGORICAL VARIABLES____####
df <- iris
plot(df)
str(iris)
#Suppose we want to model iris Sepal.Length as a function of Species and the rest of vars
#We can use model.matrix(formula, data)
model.matrix(Sepal.Length ~ Species+Petal.Length+Petal.Width, data = df)
#This converts categorical variables into dummy variables

#However be careful when a variable has too many categories as the number of vars
#approaches the number of observations it can lead to overfit.

####____INTERACTIONS____####
#Uo to now it was assumed that variables were related to the outcome linearly and 
#additively. Now we explore violations of this assumption.

#It may occur that the simultaneous effect of two variables in the outcome is not additive
#Due to the change in response between the explanatory variables.
#To express interactions in formulas we use the notation:
#* y ~ a+b +a:b #semicolon
#* y ~ a*b #asterisk is the same as  = a+b+a:b
#* y ~ I(a*b) #multplication of two variables
fmla_add <- Sepal.Length ~ Petal.Length+Petal.Width
fmla_interaction <- Sepal.Length ~ Petal.Length*Petal.Width
model_add <- lm(fmla_add, df)
model_interaction <- lm(fmla_interaction, df)
summary(model_add)
summary(model_interaction)

####____TRANSFORM____####
#Sometimes is better to transfrom the data to feed the model rather than fitting
#On the raw variables
#A useful transformation is log specially for monetary values that tend to be skewed 
#with long tail and wide range.

#regression usually predicts the expected values so for skewed data this value could
#overestimate or underestaimete the outcome for regular individuals.
#If we take the lognormal distribution it compensates for the mean median difference.
#And the dynamic range is more modest.
logmodel <- lm(log(Sepal.Length)~ Petal.Length+ Petal.Width, df)
df$pred <- exp(predict(logmodel, df))
sqrt(mean((df$pred-df$Sepal.Length)^2))

#One consequence of log transform is that the prediction errors are multiplicative .
#That is the size of the error is relative to the size of the outcome.
#This is useful when we seek to reduce relative error, rather than additive errors.

#* rms_relative=sqrt(mean((pred-otucome)/outcome))
#* 
#TRANSFORMING INPUTS
df <- iris 
fmla <- as.formula(Sepal.Length ~ I(Petal.Width^2))#Where I() treats expressions literally not as interaction
mod_lin <- lm(Sepal.Length~Petal.Width,data=df)
summary(mod_lin)

mod_sqrt <- lm(Sepal.Length~ I(Petal.Width^2),data=df)
summary(mod_sqrt)

mod_cube <- lm(Sepal.Length~ I(Petal.Width^3),data=df)
summary(mod_lin)
form <- as.formula()

####____LOGISTIC REGRESSION____####
#Predicting the ocurrence of an event is: Classification
#Predicting a numerical outcome: Regression
#  * Range of values is -Infty +Infty : Linear Regression
#  * Predicitng probabilites [0, 1]: Nonlinear regression
#Use glm(fmla, data, family)
#For logistic, family=binomial
#Recommendation: Encode the outcomes as 1/0 or True/False

#EVALUATING LOGISTIC REGRESSION MODEL
#Pseudo R^2 = 1-deviance/null.deviance  
#Use wrapChiSqTest(model) from sigr library
#Model if a leaf is Virginica
library(broom)
df$isVirginica <- ifelse(df$Species=='virginica',TRUE,FALSE)
glm_model <- glm(isVirginica~Sepal.Length+Sepal.Width+Petal.Length, df, family=binomial)
summary(glm_model)
perf <- glance(glm_model)
pseudor2 <- 1- perf$deviance/perf$null.deviance

####____POISSON AND QUASI POISSON REGRESSION TO PREDICT COUNTS____####
#Since counting prediction restricts to non negative integer outcomes,then
#it is a nonlinear process. For this we use poisson process
#family = poisson or quasipoisson. 
#If mean(count) es muy diferente a var(count) we use quasipoisson
glm(fmla,data, family=poisson)
####____GAM____####
#Generalized additive models
#This is used to discover  non linear additive transformations of the input data
#gam(fmla,data,family=gaussian, binomial, poisson, quasipoisson)
#fmla = y~s(var) #where s denotes that the relationship should be nonlinear
#HOWEVER DON'T USE s() ON CATEGORICAL VARIABLES

####___INTUITON FOR DECISSION TREES_####
#Decission trees can be used to predict numerical outcomes as well as categories 
#it does so by splitting the possible outcomes in ranges and intervals
#Because of this decission trees can:
#   * Catch non-linear relationships
#   * Catch intervals an non-montonic relationships
#The biggest con is that it only predicts coarse-grained values
#rather than continuous.
#It is also difficult to express linear relationships with these.
#It can be solved by adding more branches, but a deep tree can lead to overfitting
#WITH ENSSAMBLE OF TREES we gain finer-grained predictions and ussually better adjustmnet thatn one tree

####____RANDOM FORESTS____####
#Multiple trees averaged reduce overfit, meakes finer-grained predictions
#In random trees:
#  * We select features randomly
#  * We grow each tree independently
#  * For each tree in each node we pick the best variable to split on
#  * To score over all data we evaluate it in all tress and avearge the result
library(ranger)
library(dplyr)
library(ggplot2)
#ranger(fmla, data, num.trees,respect.unordered.factors='order')
#If the outcome var in the fmla is numeric it will perform a regression tree
#predict(model, new_data)$predictions
df <- trees
model_lm <- lm(Girth~Height+Volume,df)
model_rf <- ranger(Girth~Height+Volume,df, num.trees = 500, respect.unordered.factors = 'order')
df$pred_lm <- predict(model_lm, df)
df$pred_rf <- predict(model_rf, df)$predictions

df <- df %>% 
  mutate(res_lm = Girth-pred_lm,
         res_rf = Girth-pred_rf)
sqrt(mean(df$res_lm^2))
sqrt(mean(df$res_rf^2))

ggplot(df, aes(x=pred_rf, y = Girth))+
  geom_point(color="blue")+
  geom_abline()
ggplot(df, aes(x=pred_lm, y = Girth))+
  geom_point(color="blue")+
  geom_abline()

#ONE HOT ENCODING VARIABLES
df <- iris
library(vtreat)
#To encode a categorical variable
treatplan <- designTreatmentsZ(df,c('Species','Sepal.Length','Sepal.Width','Petal.Length'))
scoreFrame <- treatplan %>% 
  magrittr::use_series(scoreFrame) %>% 
  select(varName, origName, code)

new_vars <- scoreFrame %>% 
  filter(code %in% c('clean','lev')) %>% 
  magrittr::use_series(varName)

data.treat <- prepare(treatplan, df, varRestriction = new_vars)
#vtreat is safer that dummyVars for one-hot encoding when novel categories are present in new data
#vtreats also manages missing values in categorical and continuous data

####____GRADIENT BOOSTING MACHINES____###
#Gradients boosting machine models with xgboost
install.packages("xgboost")
library(xgboost)
cv <- xgb.cv(data = as.matrix(data.treat),
              label = df$Petal.Width,
             nrounds = 50,nfold=5,eta=0.5,
             objective='reg:squarederror',max_depth=5,
             early_stopping_rounds = 5)
elog <- cv$evaluation_log
elog

elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)
model_xg <- xgboost(data = as.matrix(data.treat),
                    label = df$Petal.Width,
                    nrounds = 50,eta=0.5,
                    objective='reg:squarederror',max_depth=5,
                    verbose = FALSE)

####____HYPERPARAMETER TUNING____####
#Hyperparameter tuning in caret
install.packages("caret")
install.packages("gbm")
library(caret)
library(gbm)
library(tidyverse)
fitControl <- trainControl(method="repeatedcv",number=3, repeats=5)
set.seed(42)
df <- iris
df_train <- train(Species~.,data=df, method="gbm",trControl=fitControl, verbose=F)
hpgrid <- expand.grid(n.trees=c(50,100,150), interaction.depth=c(1,4,6),
                      shrinkage=0.1, n.minobsinnode=10)
set.seed(42)
df_train_tune <- train(Species~.,data=df, method="gbm",trControl=fitControl,
                       verbose=F, tuneGrid=hpgrid)
plot(df_train_tune, metric="Kappa",plotType="level")
#Inspect the model tuned to look for the columns names an put it into metric
#plotType="line","scatter","level2
library(ggplot2)
ggplot(df_train_tune)
#RANDOM GRID
#We could put a random grid instead of a predefined set of values to test ranges of values.
#Randomly pick a specified number of hp combinations by chance.
#trainControl(...,search="random") #where search could be "grid", "random"
#then train(..., tuneLength=5)
#In practice evaluate at least 100.

#NEURAL_NETWORK
#train(...,method="nnet")

#IN CARET IS NOT POSSIBLE TO DEFINE A GRID AND USE SAMPLING TO SEARCH ON THAT GRID

#ADAPTIVE SAMPLING
#Grid and random search ar not so efficient, so use adaptive resampling instead,
#With this technique hp combinations that performed well are resampled around values
#close to combinations that performed better.
df_ada <- trainControl(method="adaptive_cv",number=3, repeats=5,
                       search="random",adaptive=list(min=2, alpha=0.05,
                                                     method="gls",complete=F))
#min: minimum number of resamples per hp
#alpha:significance level to remove hp.
#method="gls","BT"
#complete=T/F If full resampling is done if an optimal solution is found.

####____HP TUNING with mlr____####
#mlr is another framework for ML in R. Model training follows:
# 1. Define the task
# 2. Define the learner
# 3. Fit the model
https://mlr-org.github.io/mlr
#In mlr the tasks can be:
# * RegrTask()
# * ClassifTask()
# * MultilabelTask()
# * CostSensTask()
install.packages("mlr")
library(mlr)
#First define the task
task <- makeClassifTask(data=df,target = "Species")
#Next define the learner
listLearners()
lrn <- makeLearner("classif.gbm",
                   fix.factors.prediction = T,#If test and train have different categories
                   predict.type = "prob")#or predict.type="response"
#Fit model
model <- train(lrn, task)
#Grid and random search
# 1. Define the search space
# 2. The tuning method "grid" or "random"
# 3. The resampling scheme

# makeParamSet(makeNumericParam(),
#              makeIntegerParam(),
#              makeDiscreteParam(),
#              makeLogicalParam(),
#              makeDiscreteVectorParam())
getParamSet("classif.rpart")
params <- makeParamSet()
#Now define the tuning method
#ctrl_grid <- makeTuneControlGrid() 
#or makeTuneControlRandom()
#finally choose resampling strategy
#cv <- makeResampleDesc("RepCV",predict="both",
#                         folds=5*3)
#tuneParams
#dftune <- tuneParams(learner, task,resampling, par.set,control)
getDefaultMeasure("classif.rpart")

#EVALUATING RESULTS
#We are interested in knowing how hp affect the performance, which have the 
#weakest impact, and whether the hp combination converged.

#df_hp <- generateHyperParsEffectData(df_tune, partial.dep=T)
plotHyperParsEffect(df_hp, partial.dep.learn, x, y, z, plot.type)

makeResampleDesc(method = "Holdout")
####____HP TUNING with h2o____####
install.packages("h2o")
library(h2o)
#Designed for scalability
#h2o's ML algorithms can be trained on distributed clusters
h2o.init()
as.h2o()
h2o.splitFrame(data,ratios=c(Train,Eval),seed=42) #In ratios the ramaining percentage will go to test
h2o.gbm(), h2o.glm(), h2o.randomForest()
h2o.performance()
h2o.confussionMatrix()

#Grid and random search
#?h2o.gbm
gbm_params <- list()
h2o.grid("gbm", grid_id, x, y , training_frame, validation_frame,
         seed, hyper_params=gbm_params)
h2o.getGrid()

#For Random Search:
search_criteria <- list(strategy="RandomDiscrete",
                        max_runtime_secs=60, seed=42)
h2o.grid(..., search_criteria=search_criteria)
search_criteria <- list(strategy="RandomDiscrete",
                        stopping_metric,
                        stopping_tolerance,
                        stopping_rounds)
#Automating ML tuning
h2o.automl(x,y,training_frame,validation_frame, max_runtime_secs,
           sort_metric, seed)

automl_model@leaderboard
automl_model@leader

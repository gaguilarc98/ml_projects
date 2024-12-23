####___SUPERVISED LEARNING WITH CARET____####
#Supervised learning is machine learning when you have a "target variable" whose value 
#You want to predict.
#For quantitative prediction models we usually use the RMSE for assessing a models performance.
#However when this metric is calculated on the same data that was used to train we
#get over-optimistic estimates and leads to overfitting. This could be improved using
#out of sample estimates.
#THIS COURSE FOCUSES ON PREDICTIVE RATHER THAN EXPLANATORY MODELLING
#That is, Do the models perform well on new data?
#To answer this it is better to test the model on new data.
#Assessing models on training data guarentees some level of overfitting.
#The caret package's purpose is DO NOT OVERFIT.
#createResamples() or createFolds() in caret.
library(dplyr)
install.packages("caret")
library(caret)
df <- iris
rows <- round(nrow(df)*0.8)
set.seed(1)
trainrows <- sample(1:nrow(df),rows)
train <- df[trainrows,]
test <- df[-trainrows,]
model <- lm(Sepal.Length~., data=train)
p <- predict(model, train)
error <- p-train$Sepal.Length
print(paste0("RMSE on train: ",sqrt(mean(error^2))))
p <- predict(model, test)
error <- p-test$Sepal.Length
print(paste0("RMSE on test: ",sqrt(mean(error^2))))
####____CROSS VALIDATION____####
#The presence or absence of an outlier observation can vastly change the results of our model.
#A better approach is to use multiple test sets and averaging sample error. Which gives
#A better estimate of the TRUE out of sample error. CROSS VALIDATION to split in folds.
#So that each observation appears only in one train set.
#The assignment of observations to folds should be RANDOM.
#Cross validation is ONLY USED TO ESTIMATE THE OUT OF SAMPLE ERROR. Once we pick the best model
#We train the model on the whole dataset.

#This approach is expensive as it requires modelling k times (the number of folds) and overall.
#The train function in caret does another validation BOOTSTRAP VALIDATION but also can do CROSS VALIDATION.

#The following approach with the train function is particular or caret that permits to change the type of model 
#and type of validation to perform by just tweaking wome arguments.
set.seed(1234)
model <- train(Sepal.Length~., df, method="lm", #this could be changed to randomforest without changing the rest
               trControl = trainControl(
                 method="cv", 
                 number=10, 
                 verboseIter = TRUE
                 )
)
print(model)
#In train you provide the method for modelling 
#In trainControl you provide the method for validating "cv:crossvalidation" for example
#train also provides a parameter to repeat the whole proces it is the  "repeats" argument
set.seed(1234)
model <- train(Sepal.Length~., df, method="lm", #this could be changed to randomforest without changing the rest
               trControl = trainControl(
                 method="cv", 
                 number=10, 
                 repeats = 10,
                 verboseIter = TRUE
               )
)
print(model)

####____LOGISTIC REGRESSION____####
#For binary targets.
bin <- df %>% 
  mutate(isvirginica = ifelse(Species=="virginica",1,0)) %>% 
  select(-Species)
model <- glm(isvirginica~., bin, family="binomial")
p <- predict(model, bin, type="response")
print(sqrt(mean((p-bin$isvirginica)^2)))
####____CONFUSSION MATRIX____####
#It reveals instances when one class is confused with the other
p <- predict(model, bin, type="response")
bin$pred <- p>0.5 #choose thresshols
table(bin$pred, bin$isvirginica)
bin$isvirginica <- as.logical(bin$isvirginica)

####THE FOLLOWING FUNCTION GIVES A SUMMARY WITH METRICS FOR BINARY CLASSIFICATION
confusionMatrix(as.factor(bin$pred), as.factor(bin$isvirginica)) #available in caret 
#Sensitivity is the ratio of true positives/all positives
#Specificity is the ratio of true negatives/all negatives
#Precission is the ratio of true positives/ predicted positives
####____CHOOSING THRESHOLDS____####
#This is a balance between catching more positives at the expense of getting more false positives
#and being more precise at the expence of catching less positives.
library(broom)
glance(model)
####___ROC CURVES____####
#It plots the true positive vs. false positive (or 1 - specificity in the x-axis) rate at every possible threshold.
#library(caTools)
#colAUC(df$p, df$isvirginica, plotROC=TRUE)
####____Area Under the Curve____####
#The worst model is the diagonal box in the ROC curve.
#The best model is the perfect "box" With a 100% tru positive(y-axis) reached at 0% x-axis.
#The AUC for a perfect model is 1. And the AUC for a random guessing model (diagonal) is 0.5.

#This measure can be used to summarize the model's performance across all diferent thresholds.
#and also serves the purpose of ranking models. It ranges from 0 to 1.
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

bin <- df %>% 
  mutate(isvirginica = ifelse(Species=="virginica",1,0)) %>% 
  select(-Species)
bin$isvirginica <- as.factor(bin$isvirginica)
bin$isvirginica <- as.factor(ifelse(bin$isvirginica==1, "IS","NOT"))
table(bin$isvirginica)
model <- train(isvirginica~.,bin, method="glm", trControl=myControl)

####____RANDOM FORESTS____####
#Random forests are robust against over-fitting and usually yield accurate results
#Even for non-linear relationships.
#However, these models need to tune hyperparameters. These cannot be estimated from the data.
#They must be specified by the scientist. Default values are okay but sometimes they need adjustment.

#The random forest model uses decision trees on different bootstrap sample of the dataset.
#THIS IS CALLED BOOTSTRAP AGGREGATION OR BAGGING. However, random forests take this approach
#One step further by randomly re-sampling the columns of the dataset.

library(caret)
# library(mlbench) #For Sonar dataset but not required
set.seed(1234)
rows <- sample(1:nrow(iris),round(nrow(iris)*0.8))
df <- iris[rows,]
model <- train(
  Species~.,
  data=df,
  method="ranger"
)
plot(model)
dftest <- iris[-rows,]
dftest$pred <- predict(model, dftest)
table(dftest$pred, dftest$Species)
####____TUNING____####
#Random forests require tuning.They have hyperparameters that control how the model fits.
#One of those is mtry that is the number of variables selected at each split.
#caret saves a lot of this manual work by automating these hyperparameter selection.

#Caret not only does cross validation for us but it also automates GRID-SEARCH for selecting
#hyperparameters based on out of sample error.
#train (..., tuneLength=10) Thise tells to explore more models on top of the default
df <- iris[rows,]
model <- train(Species~., 
               data=iris,
               method="ranger",
               tuneLength=5,
               trControl = trainControl(
                 method="cv",
                 number=5,
                 verboseIter = TRUE
               )
               )

#NOTE::::::RECOMMENDED TO USE ranger method rather than randomForest() in native R.
dftest <- iris[-rows,]
dftest$pred <- predict(model, dftest)
table(dftest$pred,dftest$Species)
plot(model)
####____CUSTOM TUNING GRIDS____####
#It gives more control of the models to explore but it could increase runtime and requires 
#knowledge of the model.
df <- iris[rows,]
#We need a dataframe with the values of the tunning parameters we want to explore
myGrid <- data.frame(mtry=c(2,3,4,5,6))
set.seed(2)
model <- train(
  Species~.,
  data=df,
  mehtod="ranger",
  tuneGrid=myGrid
)
plot(model)
####____GLMNET MODELS____####
#They are an extension of generalized linear mdoels. They have built-in variable selection.
#LASSO REGRESSION. This penalizes number of non-zero coefficients.
#RIDGE REGRESSION. Penalizes absolute magnitude of coeffcients.
#It pairs well with random forests models.
install.packages("glmnet")
library(glmnet)
#Glmnet models are a combination of RIDGE and LASSO.
#####____DIFFERENCES____####
#Random forests have one hyperparameter to tune: mtry
#GMLNET models have two hyperparameters: 
# * alpha: mixing parameter between pure ridge and pure lasso (0:ridge, 1:lasso)
# * lambda: strength of the penalty on the coefficients.
model <- train(
  isvirginica~.,
  data=bin,
  mehtod="glmnet",
  trControl = myControl
)
plot(model)

####_____glmnet with custom tuning grid____####
#It tunes alpha and lambda. For each value of alpha all values of lambda are fit simultaneously.
myGrid <- expand.grid(
  alpha=0:1,
  lambda = seq(0.0001,0.1, length=10)
)
set.seed(42)
model <- train(
  isvirginica~.,
  data=bin,
  method='glmnet',
  tuneGrid=myGrid,
  trControl=myControl
)
plot(model)
plot(model$finalModel)
####____MEDIAN IMPUTATION____####
#Removing missing data is not always the best idea as could lead to bias,
#It is better to impute with the median value.

df_na <- df %>% 
  mutate(isvirginica = ifelse(Species=='virginica',1,0)) %>% 
  select(-Species)

df_na[sample(1:nrow(df_na),10),'Sepal.Length'] <- NA

Y <- as.factor(df_na$isvirginica)
X <- df_na %>% select(-isvirginica)
model <- train(X, Y) #Throws an error
#Use medianImpute in caret's preProcess option
model <- train(X, Y, preProcess = "medianImpute")
####____KNN imputation____####
#If there is a pattern in the missing values. Median imputation cannot save us.
#Other strategies:
# * K Nearest Neighbours. This is based on other non missing rows. 
model <- train(X,Y, preProcess = "knnImpute")
print(model)
#OTHER PRE PROCESS STEPS:
# * Center => scale => Median imputation => pca then fit glm
# model <- train(X,Y, method="glm", preProcess = c("center","scale","medianImpute","pca"))

####____HANDLING LOW INFORMATION PREDICTORS____####
#To remove constante values or nearly constant values (that do not improve the predictions)
#we could add the "zv" or "nzv" arguments to preProcess to remove nearly constant columns.

#PCA is an alternative to remove collinearity and use the information of low-variance variables.
model <- train(X,Y, method="glm", preProcess = c("center","scale","medianImpute","pca","zv"))
model$results
####____NOTES ON COMPARING MODELS____####
#To compare models we need to make sure that they are trained on the same split for each fold.
#we can set this in trainControl that can then be used in multiple models
df_final <- df %>% 
  mutate(isvirginica = ifelse(Species=='virginica',"IS","ISNOT")) %>% 
  mutate(isvirginica = as.factor(isvirginica)) %>% 
  select(-Species)
myFolds <- createFolds(df_final$isvirginica, k=5)
#i <- myfolds$Fold1 then table(churnTrain$churn[i])/length(i)
mycontrol <- trainControl(summaryFunction = twoClassSummary, 
                          classProbs=T, 
                          verboseIter=T, 
                          savePredictions=T, 
                          index=myFolds)
# * Advantages of glmnet
#   * Its results are just as interpretable as lm
#   * It deals with insignificant columns
# * Advantages of rf
#   * They are usually more accurate at the expense of being slower
#   * They deal with threshold values. They do not nedd much previous process.

model_glmnet <- train(
  isvirginica~.,
  data=df_final,
  metric = "ROC",
  method="glmnet",
  trControl = myControl
)
plot(model_glmnet)

#Trying a random forest
model_rf <- train(
  isvirginica~.,
  data=df_final,
  metric = "ROC",
  method="ranger",
  trControl = myControl
)
plot(model_rf)

####____COMPARING MODELS____####
#After you make sure the models u trained are based on the same split
#Compare the results among them and choose the one with highest average AUC across all folds
#And also the one with lower sd in AUC.
#resamples()collects the results from all models and helps assessing the best model.
modelList <- list(glmnet = model_glmnet, rf=model_rf)
resamps <- resamples(modelList)
resamps
summary(resamps)

bwplot(resamps, metric="ROC")
dotplot(resamps, metric="ROC") #preferred plot for dozens of models
densityplot(resamps, metric="ROC")
xyplot(resamps, metric="ROC")

####____HYPERPARAMETER TUNNING IN R____####
#Hyperparameters are different from regular parameters
#In R we use libraries such as caret, mlr and h2o
#Classification of malignant cancer

#Model parameters are fit during training they modulate the change in response
#variable with changes in the input variables.
#Hyperparameters are set before training and specify how the training is supposed
#to happen.

#Tuning hyperparameters lets us find the best combination of them to enhance training.
library(caret)
set.seed(1234)
df <- iris
index <- createDataPartition(df$Species, p=0.80, list=FALSE)
dfTrain <- df[index,]
dfTest <- df[index,]
#Training with the desired proportion with enough power. This has to be a
#representative set. 

#In REAL-WORLD situations we need to incorporate feature engineering, preprocessing
#normalization balancing classes etc. into the workfolw.

fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 5)

#HERE WE SAY MAKE 5 REPETITIONS OF 3-FOLD CROSSVALIDATIONS
#method "repeatedcv" stands for repeated cross validation.
# tic()
set.seed(1234)
rf_model <- train(Species~.,
                  data=dfTrain,
                  method="rf",
                  trControl=fitControl,
                  verbose=FALSE)
# toc()
rf_model
#Utilizar método randomForest con el control definido en fitControl.

#GRADIENT BOOSTING METHOD
gbm_model <- train(Species~.,
                  data=dfTrain,
                  method="gbm",
                  trControl=fitControl,
                  verbose=FALSE)

gbm_model

#The output spits the measures of performance (accuracy) with each combination
#of hyperparameters.

#LINK FOR CARET PACKAGE TO KNOW WHICH HYPERPARAMETERS CAN BE TUNED
#https://topepo.github.io/caret/available-models.html

#GRADIENT BOOSTING METHOD
svm_model <- train(Species~.,
                   data=dfTrain,
                   method="svmPoly",
                   trControl=fitControl,
                   verbose=FALSE)

svm_model
#We can also specify how many combinations to use for tuning
svm_model2 <- train(Species~.,
                   data=dfTrain,
                   method="svmPoly",
                   trControl=fitControl,
                   verbose=FALSE,
                   tuneLength=5)
svm_model2

#WE CAN ALSO SPECIFY HP COMBINATIONS] MANUALLY WITH EXPAND.GRID
hyperparams <- expand.grid(degree=4, scale=1, C=1)#One value for each HP
svm_model3 <- train(Species~.,
                    data=dfTrain,
                    method="svmPoly",
                    trControl=fitControl,
                    tuneGrid=hyperparams,
                    verbose=FALSE)
svm_model3

modelLookup("gbm") #To display hyperparameters of the model in argument

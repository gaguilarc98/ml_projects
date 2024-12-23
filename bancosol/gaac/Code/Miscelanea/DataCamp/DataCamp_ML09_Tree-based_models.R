####____ML WITH TREE BASED MODELS____####
#Classification and regression methods with decission trees and ensemble methods.
#Bias-variance trade-off
#Bagging and random forests
#Boosting.
#They are easy to explain, they capture non-linear relationships, they do need much
#Data pre-processing and they are robust to outliers.
#But they can get complex very quickly.
library(tidymodels)
#decision_tree %>% set_engine("rpart") #recursive partition
#set_mode("classification") #THIS IS ONLY A SKELETON IT DOES NOT TRAIN ANY MODEL
#For fitting: df_model %>% fit(fmla, data)

df <- iris
df$issetosa <- as.factor(df$Species=="setosa")

set.seed(123456)
df_split <- initial_split(df, prop=0.90, strata=issetosa)
df_train <- df_split %>% 
  training()
df_test <- df_split %>% 
  testing()
counts_train <- table(df_test$issetosa)
counts_test <- table(df_train$issetosa)
#Make sure that the proportions of true/false outcomes is similar in both test and train
#To fix this use the strata argument
counts_train["TRUE"]/sum(counts_train)
counts_test["TRUE"]/sum(counts_test)

####____CONTINUOUS OUTCOMES____####
#Performance metrics for regression trees
#In regression models there is no binary outocme, so we cannot use a metric that uses right or wrong
#Values to assess model peroformance. We need a different metric.
#How far away are the predictions from the true values: MAE RMSE.
#RMSE penalizes more thoughly larger errors
#mae(predictions, estimate=.pred, truth=outcome)
#rmse(predictions, estimate=.pred, truth=outcome)

####____CROSS VALIDATION____###
#When using a single test set is fragile. A single outlier can change our out-of-sample error.
#We reduce this by averaging results of multiple models.
#We split the training set into train and validate. Then apply leave one out to train and test in each split.
#The calculate the cross-validated rmse or mae.

#vfolds <- vfold_cv(df, v=10, strata=var)
#fits <- fit_resamples(df, fmla, resamples=vfolds, metrics=metric_set(mae, rmse)) #to train for each fold
#all_errors <- collect_metrics(fits, summarize=FALSE)
#Then plot these errors as a histogram.

####____BIAS-VARIANCE TRADE-OFF____####
#Hyperparameters are chose by the modeler.
#For example a setting the depth of the tree can enhance the performance of the model, 
#but it could lead to overfitting, and then performs poorly on new data.

#However, using few predictors or decreasing the depth of the trees we could get systematic large errors.
#They are caused because there is HIGH BIAS in the model.

####____TUNING HYPERPARAMETERS____####
#They influence model complexity
#For decision trees:: min_n, tree_depth, cost_complexity
#To make this process easier tidymodels has tune() 
#First create a grid of hyperparameters.
#Second, create dummy model specification and set the parameters for tuning.
#Third, build and evaluate performance for every combination of the grid
#Fourth, select the best performing hyperparameters.
#spec_tuned <- decision_tree(min_n=tune()) #it lets other functions know that min_n needs to be optimized.

#df_grid <- grid_regular(parameters(spec_tuned), levels=3)
#tune_results <- tune_grid(spec_untunned, fmla, resamples=df_folds, grid=df_grid, metrics = metric_set(accuracy))
#autoplot(tune_results)
#final_params <- select_best(tune_results)
#best_spec <- finalize_model(spec_untuned, final_params)

####_____MORE MODEL METRICS____####
#Sometimes accuracy is not the best metric of performance for classification
#Specially for imbalanced sets. Instead, use sensitivity= TP/(TP+FN)
#specificty = TN/(TN+FP), False positive rate=1-Specificity = FP/(TN+FP)
#ROC: predictions <- predict(model, new_data, type="prob")
#roc_curve(predicitons, truth=var, estimate=.pred_yes) %>% autoplot()
#roc_auc(predictions, truth=var, estimate=.pred_yes)

####____BAGGED TREES____####
#Sometimes small changes in data can lead to very different models and varying performance.
#The collective knowledge of many trees could be better than a single tree. These are ENSEMBLE MODELS.
#Bootstrap Aggregation aka BAGGING.
#Bootstraping means sampling rows at random from the training set (with replacement)
#Aggregation is done using the average prediction in regression (or majority vote in classification)

library(baguette)
#bag_tree() #behaves similarly to decision_tree()
#bag_tree() %>% set_mode("classification") %>% set_engine("rpart", times=100)
####____RANDOM FORESTS____####
#They are an improvement of bagged trees.
#Easy to use and out of the box performance.
#rand_forest( mtry=4, trees= 500, min_n=10)
#The improvement comes with sampling the predictor variables as well.
#This reduces the correlation of sampled trees.

#The hyperparameters are mtry (number of predictors to try), trees (number of trees for forest)
#rand_forest( mode="calssification",mtry=4, trees= 500, min_n=10) %>% 
#set_engine("ranger", importance="impurity") %>% 
#vip::vip() #This calculates the variable importance for our predictors.

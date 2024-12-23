####____THE TIDYMODEL ECOSYSTEM____####
install.packages("tidymodels")
library(tidymodels)
#recipes() and rsample() for data resampling and feature engineering
#parsnip(), tune() and dials() for model fitting and tuning
#yardstick() for model evaluation
#____WORKFLOW____#
#First split the data into training and testting sets
#For this task use the initial_split(df, prop=0.75, strata=var)
mpg_split <- initial_split(mpg, prop=0.75, strata=hwy)
#The OUTCOME VARIABLES is used as strata so that in each split they have similar range.
#Then pass the split to training and testing
mpg_training <- mpg_split %>% 
  training()
mpg_testing <- mpg_split %>% 
  testing()

#____FIRST MODEL____#
#Parsnip is used to model and calculate predictions in tidymodel ecosystem
#With parsnip syntax we need to provide the MODEL_TYPE: linear regression or other
#the ENGINE of the underlying R package and the MODE either regression or classification
lm_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')
lm_fit <- lm_model %>% 
  fit(hwy~cty, data=mpg_training)
tidy(lm_fit)
hwy_pred <- lm_fit %>% 
  predict(new_data = mpg_testing)

mpg_test_results <- mpg_testing %>% 
  select(hwy,cty) %>% 
  bind_cols(hwy_pred)

#___EVALUATE MODEL____#
#Use the yardstick package for this purpose. It requires a tibble.
#It requires the actual values and predicted outcomes in two columns.
#One metric to evaluate performance is RMSE
mpg_test_results %>% 
  rmse(truth=hwy, estimate = .pred)
#Another metric is Rsquared metric
mpg_test_results %>% 
  rsq(truth = hwy, estimate = .pred)
#Rsquared plots actual on x and truth on y axis of scatterplot and diagonal
#It helps to identify non linear relationships
ggplot(mpg_test_results, aes(x=hwy,y=.pred))+
  geom_point()+
  geom_abline(color="blue",linetype=2)+
  coord_obs_pred() #To present same range of x and y

#last_fit() takes a model specification (parsnip object), model formula and data split.
# * It creates training and test datasets
# * Fits models to the training data
# * Calculates metrics and predictions
# * Returns object with all the results
lm_last_fit <- lm_model %>% 
  last_fit(hwy~cty, split = mpg_split)

#Finally we pass this object to collect metrics to get a tibble with all metrics
lm_last_fit %>% 
  collect_metrics()

lm_pred <- lm_last_fit %>% 
  collect_predictions()

ggplot(lm_pred,aes(x=hwy,y=.pred))+
  geom_point()+
  geom_abline(color="blue")+
  coord_obs_pred()
####____CLASSIFICATION MODELS____####
#Predict categorical outcomes
#Logistics regression creates a linear separation between outcome categories.
#It forms a decision boundary.
df <- iris
df$issetosa <- as.factor(df$Species=="setosa")
df_ini_split <- initial_split(df, prop=0.75, strata=issetosa)
df_train <- df_ini_split %>% 
  training()
df_test <- df_ini_split %>% 
  testing()
#Model specification parsnip
df_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

df_fit <- df_model %>% 
  fit(issetosa~Sepal.Length, data=df_train)

df_pred <- df_fit %>% 
  predict(new_data=df_test, type="class") #Type to get classes instead of probabilities

df_pred

probs_pred <- df_fit %>% 
  predict(new_data=df_test, type="prob")
probs_pred

#Evalutate model with yardsitck
df_result <- df_test %>% 
  select(issetosa) %>% 
  bind_cols(df_pred, probs_pred)
df_result

####____ASSESSING MODEL'S PERFORMANCE____#####
#The target variable must be a factor for the model to run
#Confusion matrix. Predicted on the rows and true outcomes on the columns.
#To calculate these matrix we need a tibble with the outcomes and the predicted values.
#conf_mat(tibble, truth=outcome, estimated=pred)
df_conf_mat <- conf_mat(df_result, truth=issetosa, estimate=.pred_class)

accuracy(df_result, truth=issetosa, estimate=.pred_class)#Is not the best measure
sens(df_result, truth=issetosa, estimate=.pred_class) #First form
recall(df_result, truth=issetosa, estimate=.pred_class) #Second form
sensitivity(df_result, truth=issetosa, estimate=.pred_class) #Third form
precision(df_result, truth=issetosa, estimate=.pred_class)
specificity(df_result, truth=issetosa, estimate=.pred_class) #Proportion of negative cases correctly clasisified
spec(df_result, truth=issetosa, estimate=.pred_class)
#To create all metrics at once
custom_metrics  <- metric_set(accuracy, spec, sens)
custom_metrics(df_result, truth=issetosa, estimate=.pred_class)

#Visualize models's results
autoplot(df_conf_mat, type = "heatmap")
autoplot(df_conf_mat, type = "mosaic")
#The default threshold for classification with logit models is 0.5
#It is important to evaluate different values of the threshold, to see if the model 
#is able to consistently predict well.
#One way to do this is to get the unique() values of .pred_prob
#And to calculate the measures of performance for each one.
#ROC (receiver operating characteristic)
#This plot displays the proportion of correct among actual positives vs the proportion
#incorrect among actual negatives

#One way to summarize the ROC curve is to calculate the area under the curve AUC
roc_curve(df_result, truth=issetosa, .pred_FALSE) %>% 
  autoplot()
roc_auc(df_result, truth=issetosa, .pred_FALSE)

####____AUTOMATING THE MODELING WORKFLOW____####
#last_fit() trains the data and makes prediction on the test data
#It needs a data split, also we need the parsnip model, the formula, and the split object.
df_last_fit <- last_fit(df_model, issetosa~Sepal.Length, df_ini_split)

df_last_fit %>% 
  collect_metrics()

df_lastfit_results <- df_last_fit %>% 
  collect_predictions()
df_lastfit_results

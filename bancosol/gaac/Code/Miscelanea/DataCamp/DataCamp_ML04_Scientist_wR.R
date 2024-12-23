####____FOUNDATIONS OF TIDY MACHINE LEARNING____####
#Tidyverse builds on objects called tibbles
#Tidyr nd purr packages are required
#Tibbles store natively objects in list columns
#List column workflow (purr):
# * Make list columns: nest()
# * Work with list columns: map()
# * Simplify list columns: unnest(), map_*()
#dslabs packages
#Observations and features.
install.packages("dslabs")
library(dslabs)
library(dplyr)
library(purr)
library(tidyverse)
df <- iris
df_nest <- df %>% 
  group_by(Species) %>% 
  nest()

head(df_nest)
df_unnest <- df_nest %>% 
  unnest(data)
identical(df_unnest,df)
#map() applies a function to a vector or a list and returns a list
#map(.x= ,.f= ), .x being the list and .f being the function
#.f can be a default function name such as mean or an anonymous function
#such as ~mean(.x)

map(.x = df_nest$data,.f =~mean(.x$Sepal.Length))
#It is possible to store a tibble from map in mutate
df_all <- df %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(SepalLPop_mean = map(data,~mean(.x$Sepal.Length))) %>% 
  unnest(data)

#However you can append the calculated value as a "basic" value if you know the type
#using map_dbl(), etc
df_all <- df %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(SepalLPop_mean = map_dbl(data,~mean(.x$Sepal.Length)))
#This mapping can be used to build models for each level of the nest
df_all <- df %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(model = map(data,~lm(Sepal.Length~Sepal.Width+Petal.Width+Petal.Length,
                              data=.x)))
####____USING BROOMON TIDY MODELS____####
#Obviously step two of the list column workflow is where most of the thing happens
#Since it can store a model for each level of nest
library(broom)
# * tidy() returns statistical findings of the model
# * glance() returns a onerow summary of the model
# * augment() adds prediction columns to the data being modeled
tidy(df_all$model[[1]])
glance(df_all$model[[1]])
species_fit <- augment(df_all$model[[1]])
ggplot(species_fit, aes(x=Sepal.Width))+
  geom_point(aes(y=Sepal.Length))+
  geom_line(aes(y=.fitted), color='red')

####____EXPLORING COEFFICIENTS IN MODELS____####
#similarly to how we got models from nested data we can get the coefficients
#using the map function but .x=model instead of data 
df_all %>% 
  mutate(SWslope = map(model, ~tidy(.x))) %>% 
  unnest(SWslope) %>% 
  dplyr::filter(term=='Sepal.Width') %>% 
  ggplot(aes(x=estimate)) +
  geom_histogram()

####____EVALUATING FIT of many models____####
#One way is to get r-squared from each model
model_perf <- df_all %>% 
  mutate(R2 = map(model,~glance(.x))) %>% 
  unnest(R2)

model_perf %>% 
  slice_max(r.squared, n=1)
model_perf %>% 
  slice_min(r.squared, n=1)
#Visualize inspect the fit of models
aug_model <- df_all %>% 
  mutate(augmented = map(model,~augment(.x))) %>% 
  unnest(augmented)
library(ggplot2)
aug_model %>% 
  dplyr::filter(Species=='setosa') %>% 
  ggplot(aes(x=Sepal.Width))+
  geom_point(aes(y=Sepal.Length))+
  geom_line(aes(y = .fitted), color="red")

#Improve the fit of models
####____TRAINING AND TEST VALIDATION SPLITS____####
#Questions for ML how well a model will perform on new data and
#Did i select the best model possible
#To answer first: Train-Test Split.
#As long as the test is a fair representation of the data we can use to calculate
#the expected performance for future observations.
install.packages("rsample")
library(rsample)
library(tidyverse)
library(broom)
#sp <- initial_split(df, prop=0.75) then train <- training(sp) and test <- testing(sp)
#You must not use the train data to make decissions on the model. For that you can further
#split the training data into train and validate.
#Furthermore you can use CROSSVALIDATION to validate the selection of the model.

#vfold_cv(trainingdATA, v=3)
#cv_data <- cv_split %>% mutate(train= map(split,~training(.x)))
df <- iris
df_split <- initial_split(df, prop=0.75)
traindf <- training(df_split)
testdf <- testing(df_split)
#Cross validation from traindf
cvdf <- vfold_cv(traindf, v=3)
#with the new training folded data extract train and validate
cvdata <- cvdf %>% 
  mutate(train = map(splits,~training(.x)),
         validate = map(splits,~training(.x)))
#Then compare model's predictions for validate df with the actual values to
#validate the model selected with a selected metric

#To select the actual values df %>% mutate(actual = map(train, ~.x$actual_values))
#To predict the values we could use predict(model, df). Since this needs to inputs we use map2()
#df %>% mutate(pre = map2(.y=train, .x=model, .f=~predict(.x, .y)))
install.packages("Metrics")
library(Metrics)

cvmodel <- cvdata %>% 
  mutate(model = map(train, ~lm(formula=Sepal.Length~Sepal.Width,.x))) %>% #to get models
  mutate(actual = map(validate, ~.x$Sepal.Length)) %>% #to get actual values
  mutate(pred = map2(.x=model, .y=validate, ~predict(.x,.y)))
#Now evaluate performance
cvperf <- cvmodel %>% 
  mutate(perf = map2_dbl(.x=actual, .y=pred, ~mean(abs(.x -.y)))) %>% 
  mutate(perf2 = map2_dbl(.x=actual, .y=pred, ~mae(actual=.x,predicted=.y))) 
print(cvperf$perf)
print(cvperf$perf2)
mean(cvperf$perf)

####____TUNING RANDOM FOREST MODEL____####
#We can compare different models between them.
#A random forests natively models non-linear relationships and interactions
library(ranger)
#ranger(fmla, data, seed) then predict(model, data)$predictions
#ranger(..., mtry, num.trees) #where mtry goes from 1:number_features
#To tune the parameters we could expand mtry by using
#df %>% crossing(mtry=1:5)
cvmodel_rf <- cvdata %>% 
  mutate(model = map(train, ~ranger(Sepal.Length~Sepal.Width, .x, seed=1234, num.trees = 50))) %>% 
  mutate(pred = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions)) %>% 
  mutate(actual = map(validate, ~.x$Sepal.Length)) %>% 
  mutate(mae = map2_dbl(.x = actual, .y = pred, ~mean(abs(.x - .y))))#Now validate
print(cvmodel_rf$mae)
mean(cvmodel_rf$mae)
#Tuning parameters. MAKE SURE THAT THE FORMULA DOES NOT OVERRIDE THE NUMBER OF PARAMETERS
cvmodel_tunerf <- cvdata %>% 
  crossing(mtry=3:4) %>% 
  mutate(model = map2(train, mtry, ~ranger(formula=Sepal.Length~., data= .x, mtry=.y, seed=1234,num.trees = 50))) %>% 
  mutate(pred = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions)) %>% 
  mutate(actual = map(validate, ~.x$Sepal.Length)) %>% 
  mutate(mae = map2_dbl(.x = actual, .y = pred, ~mean(abs(.x - .y))))#Now validate
print(cvmodel_tunerf$mae)
mean(cvmodel_tunerf$mae)
cvmodel_tunerf %>% group_by(mtry) %>% summarise(MeanMAE= mean(mae))

####____TESTING PART OF THE WORKFLOW____####
#Now that we have seen which model is better: random forest with 4 vars, we proceed to test
best_model <- ranger(Sepal.Length~., data=traindf, mtry=4, num.trees = 50, seed=1234)

test_actual <- testdf$Sepal.Length
test_pred <- predict(best_model, testdf)$predictions
mae(test_actual, test_pred)

####____LOGISTIC REGRESSION____####
#This belongs to binary classification models
#glm(fmla, data, family='binomial')
#Basically everything is almost the same.
#Classes should be binary TRUE OR FALSE
#To measure performance use an appropiate metric
#predict(model, df, type='response')

#Metrics package has accuracy(actual, pred)
#Metrics: precision(actual, pred) True Positives/Predicted Positives
#Metrics: recall(actual, pred) True Positives/Actual Positives
####____FEATURE ENGINEERING____####
library(recipes)
#Transform data into format that is suitable for machine learning.
#FIRST
#Specify data types and roles: that is predictor or outcomes and numeric or categorical.
#recipe()
#SECOND
#Data imputation, scaling and transforming variables.
#Creating new variables as ratios of existing variables.
#step_*()
#THIRD
#Train on data source for centering and scaling or creating new columns
#prep()
#FOURTH
#Apply data preparation to all sources of data even for future observations 
#Use bake() for this step

#recipe(fmla, data=df) #To assign roles to columns and store data_types from a database
#step_log(total_time, base=10)
#prep(.x=df_recipe,training=df_train)
#bake(.x=df_prep,new_data=NULL)
#bake(new_data=df_test)
#EXAMPLE
#Splitting data into test and training
df <- iris %>% 
  mutate(isvirginica = ifelse(Species=="virginica","IS","ISNOT")) %>% 
  mutate(isvirginica = factor(isvirginica)) %>% 
  select(-Species)

df_split <- initial_split(df, prop=0.75, strata = isvirginica)
df_train <- training(df_split)
df_test <- testing(dF_split)

#Creating a recipe with recipe and step
df_flow <- recipe(isvirginica~.,data=df) %>% 
  step_center(where(is.numeric),na_rm = T) #Centering numeric variables 
#As you can see dplyr lazy selection can be used
#Other processing steps are step_normalize(), step_scale(), step_range(), step_log()
summary(df_flow) #This creates a tibble with column information

#Training a recipe
df_rec_prep <- df_flow %>% 
  prep(training = df_train) #Put the training dataset

df_rec_prep #This shows which steps were successfully trained on data.

#Baking a trained recipe
#bake requires a trained recipe object |or new data on which to train the recipe
bake(df_rec_prep, new_data = NULL) #with trained recipe
df_rec_prep %>% 
bake(new_data = df_test) # with new data
####____NUMERIC PREDICTORS____#####
#multicollinearity causes instability in machine learning optimization algorithms.
df %>% select_if(is.numeric) %>% cor()
df_step_corr <- recipe(isvirginica~., data=df) %>% 
  step_corr(where(is.numeric), threshold=0.9)#We can also pass the column names separated by commas
#THIS REMOVES HIGHLY CORRELATED VARIABLES ACCORDING TO THE THRESHOLD
#SPECIAL SELECTOR FUNCTIONS:
# * all_outcomes()
# * all_numeric() --including outcome if it is numeric 
#To avoid including outcome use -all_outcomes()

df_step_corr_prep <- df_step_corr %>% 
  prep(training = df_train)

df_step_corr_prep
#NORMALIZATION 
#Recommended for numeric in different units.
#step_normalize(all_numeric()) #in the sequence of steps in recipes.
df_step_corr <- recipe(isvirginica~., data=df) %>% 
  step_corr(where(is.numeric), threshold=0.9) %>% 
  step_normalize(all_numeric())

df_step_corr_prep <- df_step_corr %>% 
  prep(training = df_train)
df_step_corr_prep

df_step_corr_bake <- df_step_corr_prep %>% 
  bake(new_data = df_train)
df_step_corr_bake <- df_step_corr_prep %>% 
  bake(new_data = df_test)
####____NOMINAL PREDICTORS____####
#Unordered nominal data that splits data into categories or groups.
#These need to be transformed into numeric data for most ML algorithms to work
#One-hot encoding. Indicator variables for all categories.
#Dummy variable encoding. Indicator variables for all categories but one (default in recipes).
#step_dummy() the prep(training=df_train)
#all_nominal(), -all_outcomes()
#recipes packages standardize the processing steps.
df <- df %>% 
  mutate(size = case_when(Sepal.Length<5 & Petal.Length<1.5 ~ 'Small',
                          Sepal.Length<6 & Petal.Length<4 ~ 'Medium',
                          TRUE ~ 'Large'))
df_split <- initial_split(df, prop=0.75, strata=isvirginica)
df_train <- training(df_split)
df_test <- testing(df_split)
df_step_nom <- recipe(isvirginica~.,data=df) %>% 
  step_corr(where(is.numeric), threshold=0.9) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) #Should always be after normalize as 0's and 1's are normalized

df_nom_prep <- df_step_nom %>% prep(training=df_train)
df_nom_prep

bake(df_nom_prep, new_data=df_train)
#NOTICE THAT NORMALIZING SHOULD IN MOST CASES BE SPECIFIED FIRST BEFORE CREATING DUMMIES#CAUSE THEY WILL ALSO BE NORMALIZED IF THEY ARE CREATED BEFORE.
####____COMPLETE MODELLING WORKFLOW____####
#Decision trees segment the predictor space into rectangular regions.
#Tree diagrams are made of nodes. Terminal nodes provide the model prediction.
#decission_tree() %>% set_engine('rpart') %>% set_mode('classificcation') or 'regression'
library(workflows)
#This package provides a way to add various steps of the workflow into a single object.
#wf <- workflow() %>% add_model(df_model) %>% add_recipe(df_recipe)
#wf %>% last_fit(split=df_split) then collect_metrics()
#metric_set(roc_auc, sens, spec)
####____ESTIMATING MODELS WITH CROSS VALIDATION____#####
#A downside of the previous approach is that only gives you one estimate of model performance.
#Cross validation provides k estimates of model performance.
#df_folds <- vfold_cv(df_train, v=, strata=)

#Then df_fit <- df_wkfl %>% fit_resamples(resamples=df_folds, metrics= df_metrics), Finally collect_metrics()

####____HYPERPARAMETER TUNING____####
#We use cross validation to tune the hyperparameters. These are parameters that are fixed before the training.
#In Decision Trees we have number of min obs, depth of the trees etc.
#df_model <- decision_tree(cost_completxity=tune(), tree_depth=tune(), min_n = tune())

#parameters() from dials package can be used to identify the hyperparameters in a parsnip model.
#set.seed(1234) df_grid <-  grid_random(parameters(df_tune_model), size=5)
#The tune_grid() function performs hyperparameter tuning
#tune_grid(df_wkfl, resamples = df_folds, grid=df_grid, metrics=df_metrics)

####____SELECT BEST MODEL____####
#show_best(df_tuning, metric="roc_auc', n=5)
#df_best_model <- select_besT(df_tuning, metric="roc_auc")
#finalize_workflow(df_wkfl, df_best_model)

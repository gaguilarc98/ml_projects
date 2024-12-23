####____FEATURE ENGINEERING____####
library(recipes)
#Transfomr data into format that is suitable for machine learning.
#FIRST
#Specify data types and roles: that is predictor or outcomes and numeric or categorical.
#SECOND
#Data imputation, scaling and transforming variables.
#Creating new variables as ratios of existing variables.
#THIRD
#Train 
#Creating formulas with prep()
#FOURTH
#Apply data preparation to all sources of data even for future observations with bake()

#recipe(fmla, data=df)
#prep(.x=df_recipe,training=df_train)
#bake(.x=df_prep,new_data=NULL)
#bake(new_data=df_test)
####____NUMERIC PREDICTORS____#####
#multicollinearity causes instability in machine learning optimization algorithms.
#df %>% select_if(is.numeric) %>% cor()
#df %>% recipe(fmla, data) %>% step_corr(x,y,z,..., threshold=0.9)
#THIS REMOVES HIGHLY CORRELATED VARIABLES ACCORDING TO THE THRESHOLD
#OR df %>% recipe(fmla, data) %>% step_corr(all_outcomes(), threshold=0.9) #to select all outcome
#all_numeric() to select all numeric variables

#NORMALIZATION 
#Recommended for numeric in different units.
#step_normalize(all_numeric()) #in the sequence of steps in recipes.
####____NOMINAL PREDICTORS____####
#Unordered nominal data that splits data into categories or groups.
#These need to be transformed into numeric data for most ML algorithms to work
#One-hot encoding. Indicator variables for all categories.
#Dummy variable encoding. Indicator variables for all categories but one (default in recipes).
#step_dummy() the prep(training=df_train)
#all_nominal(), -all_outcomes()
#recipes packages standardize the processing steps.

#NOTICE THAT NORMALIZING SHOULD IN MOST CASES BE SPECIFIED FIRST BEFORE CREATING DUMMIES
#CAUSE THEY WILL ALSO BE NORMALIZED IF THEY ARE CREATED BEFORE.

####____COMPELTE MODELLING WORKFLOW____####
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
#df_best_model <- select_best(df_tuning, metric="roc_auc")
#finalize_workflow(df_wkfl, df_best_model)

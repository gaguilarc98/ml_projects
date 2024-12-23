####____BOOSTING____####
#In ensemble models, each decision tree is independent from each other.
#Because they are independent, they can be run in parallel to speed up the training.
#However, this fact could also be used to enhance the flaws of each other sequentially.

#This method is called BOOSTING. It improves each model iteratively. 
#ADABOOST
#Adaptive Boosting. It pays attention to the otucomes wrongly predicted by the previous method.
#To do this, change the weight of wrongly classified training examples. Subsequent trainings
#should improve the classification rate of these wrongly.
#Gradient descent.

#boost_tree() %>% set_mode("classification") %>% set_engine("xgboost")

####____GRADIENT BOOSTING####
#These boosting techniques handle remaining difficult observations at each step.
#Gadient Boosts improve Adaboost
#Adaboost:
# * Uses deicision stumps (one single split) as learners
# * Attaches weights to observations: high for difficult observations and los for correct ones
#Gradient boost:
# * Uses smal decission trees rather than stumps
# * Does not use weights but loss functions. 
# * Since it requires optimization of loss function it uses gradient descent.
#Pros:
# * It requires more tuning but it outperforms most models.
# * Good option for unbalanced data.
#Cons:
# * Prone to overfitting.
# * Slow since it is iteratively.
#Parameters: min_n, tree_depth, sample_size, trees, mtry, 
#learn_rate: rate at which the boosting algorithm adapts
#loss_reduction: reduction in the loss function required to split further
#stop_iter: number of iterations with no improvement before stopping

####____OPTIMIZE THE BOOSTED ENSEMBLE____####
#tune()
#df_grid <- grid_regular(parameters(spec), levels=2)
#vfold_cv(df, v=5) 
#tune_grid() then select_best()  finally finalize_model(spec,params=select_best()) then fit()
#tune_results <- tune_grid(spec, fmla, resamples, grid = df_grid, metrics=metric_set())
#autoplot(tune_results)

####____MODEL COMPARISON____####
#Compare with a metric: highest AUC, ROC curves.
#predscombined <- bind_rows(decision_tree = roc_auc(df,truth,.pred_yes),
#         decision_tree = roc_auc(df,truth,.pred_yes), .id="model)
#preds_long <- pivot_longer(preds_combined, cols=starts_with("pred_"), names_to='model', values_to='pred')
#preds_long %>% group_by(model) %>% roc_curve(truth=, estimate) %>% autoplot()



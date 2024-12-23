library(tidymodels)

table(iris$Species)
df <- iris %>% 
  mutate(isVersicolor = ifelse(Species=="versicolor",1,0)) %>% 
  arrange(desc(isVersicolor)) %>% 
  dplyr::filter(row_number()>27) %>%
  mutate(isVersicolor = as.factor(isVersicolor)) %>% 
  select(-Species)
# plot(df,col=df$isVersicolor)

#MODEL
df_model <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')
#RECIPE
set.seed(42)
df_split <- initial_split(df, prop=0.75, strata=isVersicolor)
df_train <- training(df_split)
df_test <- testing(df_split)

df_recipe <- recipe(isVersicolor~., data=df_train) %>% 
  step_normalize(all_numeric()) 

df_wf <- workflow() %>% 
  add_model(df_model) %>% 
  add_recipe(df_recipe)

df_wf_fit <- df_wf %>% 
  last_fit(split = df_split)

df_wf_fit %>% 
  collect_metrics()

df_wf_preds <- df_wf_fit %>% 
  collect_predictions()

df_wf_preds %>% roc_curve(truth = isVersicolor, .pred_1) %>% autoplot()
x <- df_wf_preds %>% roc_curve(truth = isVersicolor, .pred_1) 
x
x %>% 
  mutate(fnr = 1-specificity) %>% 
  arrange(fnr,sensitivity) %>% 
  ggplot(aes(x=fnr, y=sensitivity)) +
  geom_line()

df_preds_2 <- df_wf_preds %>% 
  mutate(.pred_class2 = ifelse(.pred_1>0.832, 1, 0)) %>% 
  mutate(.pred_class2 = as.factor(.pred_class2))

df_preds_2 %>% sens(isVersicolor, .pred_class2)
1- df_preds_2 %>% spec(isVersicolor, .pred_class2) %>% pull()

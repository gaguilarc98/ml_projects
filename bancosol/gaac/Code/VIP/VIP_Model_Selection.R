####____CARGA DE LIBRERIAS Y FUNCIONES_____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(forcats) 
library(openxlsx)
library(ca)
library(tidymodels)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

####____SELECCION DE VARIABLES E Y VERIFICACION DEL MODELO____####
longmonth <- "Jun. 2023"
shortmonth <- str_replace(longmonth,". ","")
bdcVIPScore <- fread(paste0('D:/!bso/vipCartera/dataScore_',shortmonth,'.csv'), 
                     encoding = "UTF-8",sep = ",") %>% 
  column_to_rownames("CTACLIENTE") %>% 
  mutate(target = as.factor(target))

vip_split <- initial_split(bdcVIPScore, prop=0.80, strata = target)

vip_train <- training(vip_split)
vip_test <- testing(vip_split)
table(vip_train$target)
table(vip_test$target)

vip_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") 
vip_model_fit <- vip_model %>% 
  fit(target~., data = vip_train)

set.seed(1234)
vip_fold <- vfold_cv(vip_train, v=5, strata = target)
cv_results <- fit_resamples(vip_model,
                            target~.,
                            resamples=vip_fold,
                            metrics=metric_set(roc_auc))

vip_fit <- vip_model %>% 
  predict(new_data=vip_train, type="prob") %>% 
  mutate(.pred_class = predict(vip_model, new_data = vip_train, type="class")$.pred_class) %>% 
  bind_cols(vip_train)

glimpse(vip_fit)
conf_mat(vip_fit,truth = target, estimate = .pred_class)
precision(vip_fit,truth = target, estimate = .pred_class)
recall(vip_fit,truth = target, estimate = .pred_class)

roc_curve(vip_fit, truth=target, .pred_0) %>% 
  autoplot()

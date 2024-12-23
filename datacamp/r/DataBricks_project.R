####____CARGA DE PAQUETES____####
devtools::install_github("dustinfife/flexplot")
library(readxl)
library(writexl)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidymodels)
####____CARGA DE BASE DE DATOS____####
airbnb <- fread("C:/Files/Data/airbnb.csv",sep = ",") 
glimpse(airbnb)
summary(airbnb)

airbnbCat <- airbnb %>% 
  mutate(pricelog = log(price)) %>% 
  select(host_is_superhost, cancellation_policy,instant_bookable, neighbourhood_cleansed,
         property_type, room_type, pricelog, bed_type)
airbnbNumeric <- airbnb %>%
  mutate(pricelog = log(price)) %>% 
  select(host_total_listings_count, accommodates,bathrooms, bedrooms,
         beds, minimum_nights, number_of_reviews, starts_with("review_"),-ends_with("_na"),
         pricelog)

ggplot(airbnbNumeric, aes(x=host_total_listings_count, y=pricelog))+
  geom_point()

x <- airbnbCat %>% group_by(instant_bookable) %>% 
  summarise(S=mean(pricelog), SD=sd(pricelog), N=n())

ggplot(airbnbCat, aes(x=pricelog, y=0, color=host_is_superhost))+
  geom_point()
#Variables with powerful explanatory power
# accommodates, bedrooms, beds, review_scores_rating, review_scores_cleanliness,
# review_scores_location, bathrooms, number_of_reviews, review_scores_accuracy
# Categorical: host_is_superhost, room_type, instant_bookable, cancellation_policy, property_type
cor(airbnbNumeric)
cor(airbnb)

####___PCA____####
airbnbN <- airbnbNumeric %>% 
  select(accommodates, bedrooms, beds, review_scores_rating, review_scores_cleanliness,
         review_scores_location)
airbnb.pca <- prcomp(airbnbN, scale. = T, center = T)
biplot(airbnb.pca, main='Biplot')

#SCREE PLOT
#They show the amount of variance explained
#Or the cumulative percentage of variance explained as the number of components increases
airbnb.var <- airbnb.pca$sdev^2
pve <- airbnb.var/sum(airbnb.var)
plot(pve, xlab='Number of Component', ylab='Proportion of Variance Explained',
     ylim = c(0,1), type='b')
plot(cumsum(pve), xlab='Number of Component', ylab='Proportion of Variance Explained',
     ylim = c(0,1), type='b')
screeplot(airbnb.pca)
airbnb.pca

####____LINEAR MODEL____####
airbnbFull <- airbnb %>% 
  mutate(pricelog = log(price)) %>% 
  mutate(across(c(host_is_superhost, cancellation_policy, instant_bookable, 
                  property_type, room_type),~as.factor(.x))) %>% 
  select(pricelog, host_is_superhost, cancellation_policy, instant_bookable, 
         property_type, room_type, neighbourhood_cleansed)
airbnbFull <- airbnbFull %>% bind_cols(airbnb.pca$x[,c(1:3)])
airbnblm <- lm(pricelog ~., data=airbnbFull)
y <- predict(airbnblm,airbnbFull)
sqrt(mean((airbnbFull$pricelog-y)^2))
summary(airbnblm)

#Mejores variables: todas las categóricas
#Numéricas escaladas y PCA: accommodates, bedrooms, beds, review_scores_rating, review_scores_cleanliness, review_scores_location 
#PCA with only 3 variables
airbnb_split <- initial_split(airbnbFull, prop=0.75)

airbnb_training <- airbnb_split %>% 
  training()
airbnb_testing <- airbnb_split %>% 
  testing()

####____LINEAR MODEL____####
lm_model <- linear_reg() %>% set_engine('lm') %>% set_mode('regression')

lm_fit <- lm_model %>% 
  fit(pricelog~., data=airbnb_training)
tidy(lm_fit)
pricelog_pred <- lm_fit %>% 
  predict(new_data = airbnb_testing)

airbnb_test_results <- airbnb_testing %>% 
  # select(hwy,cty) %>% 
  bind_cols(pricelog_pred)

airbnb_test_results %>% 
  rmse(truth = pricelog, estimate = .pred)
#Another metric is Rsquared metric
airbnb_test_results %>% 
  rsq(truth = pricelog, estimate = .pred)

####____RANDOM FOREST MODEL____####
install.packages("xgboost")
library(xgboost)
rf_model <- boost_tree() %>% 
  set_engine('xgboost') %>% set_mode('regression')

rf_fit <- rf_model %>% 
  fit(pricelog~., data=airbnb_training)
tidy(rf_fit)
pricelog_pred <- rf_fit %>% 
  predict(new_data = airbnb_testing)

airbnb_test_results <- airbnb_testing %>% 
  # select(hwy,cty) %>% 
  bind_cols(pricelog_pred)

airbnb_test_results %>% 
  rmse(truth = pricelog, estimate = .pred)
#Another metric is Rsquared metric
airbnb_test_results %>% 
  rsq(truth = pricelog, estimate = .pred)

library(ranger)

####____MY VERY FIRST MODEL FROM SCRATCH____####
library(rpart)
library(tidymodels)
library(readxl)
library(data.table)
library(ggplot2)
library(ranger)
install.packages("vip")
library(vip)
####____READING DATA____####
sleep <- fread("C:/Files/Data/SleepHealth/Sleep_health_and_lifestyle_dataset.csv",
               encoding = "UTF-8")
head(sleep)
sleep$HasDisorder <- as.factor(sleep$`Sleep Disorder`!="None")

####____CREATING TRAINING AND TEST SETS___####
sleepSplit <- initial_split(sleep, prop=0.8, strata=HasDisorder)

sleepTrain <- training(sleepSplit)
sleepTest <- testing(sleepSplit)

table(sleepTrain$HasDisorder)/sum(table(sleepTrain$HasDisorder))
table(sleepTest$HasDisorder)/sum(table(sleepTest$HasDisorder))
#Balanced data

####____SET MODEL ENGINE____####
sleepModel <- rand_forest(mtry = 4, trees = 20, min_n = 10) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

sleepFit <- sleepModel %>% 
  fit(HasDisorder~., data = sleepTrain)

sleepTrainPred <- sleepTrain %>% 
  mutate(predict(sleepFit))

vip(sleepFit)

ggplot(sleep)+
  geom_bar(aes(x=`Sleep Disorder`))

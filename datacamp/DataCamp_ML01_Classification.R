install.packages("class")
install.packages("naivebayes")
install.packages("rpart.plot")
install.packages("ROC")
install.packages("randomForest")
library(class)
library(naivebayes)
# library(ROC)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
####____CONSTRUCTING DATASETS____####
set.seed(1234)
df <- iris %>% 
  group_by(Species) %>% 
  mutate(muestra = ifelse(runif(n(),0,1)<0.15,1,0)) %>% 
  ungroup()
test_set <- df %>% filter(muestra==1) %>% select(-muestra)
train_set <- df %>% filter(muestra==0) %>% select(-muestra)
str(test_set)
#Clasificación por k nearest neighbors del test_set
test_pred <- knn(train = train_set[-5],test = test_set[-5],cl = train_set[["Species"]])

table(test_set$Species,test_pred) #Matriz de confusión
#To get probabilities
test_prob <- knn(train = train_set[-5],test = test_set[-5],cl = train_set[["Species"]],
                 prob=TRUE)

train_set_glm <- train_set %>% 
  ungroup() %>% 
  mutate(isVersicolor = ifelse(Species=="versicolor",1,0)) %>% 
  select(-Species)
test_set_glm <- test_set %>% 
  ungroup() %>% 
  mutate(isVersicolor = ifelse(Species=="versicolor",1,0)) %>% 
  select(-Species)

train_glm_model <- glm(isVersicolor~.,data = train_set_glm,family = 'binomial')
summary(train_glm_model)

pred <- predict(train_glm_model,test_set_glm[-5],type='response') #response to get prob values instead of log(odds)

table(pred>0.2,test_set_glm$isVersicolor)
####____DECISSION TREES____####
#This technique has the divide and conquer approach aka division partitioning
#It consist of finding ifelse statments that split the data better wrt the classification target
tree_model <- rpart(isVersicolor~.,data=train_set_glm, method = 'class')
tree_pred <- predict(tree_model,test_set_glm[-5],type = 'class')
table(tree_pred,test_set_glm$isVersicolor)
 
rpart.plot(tree_model)
rpart.plot(tree_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

#Decission trees draw parallel lines to the axis to split the data.
#This feature makes decission trees to become very complex very quickly, even for
#data with an obvious pattern that does not consist of parallel lines to the axis (e.g. a diagonal)

#Decission trees tend to overfit the data that it is trained on. A technique to test
#performance of the model is to test it against data other that the training set.

####____DIVIDE & CONQUER____####
nrow(df)
ids <- sample(1:nrow(df),nrow(df)*0.75)
train_tree <- df[ids,] %>% 
  mutate(isVersicolor=ifelse(Species=='versicolor',1,0)) %>% 
  select(-Species,-muestra)
test_tree <- df[-ids,] %>% 
  mutate(isVersicolor=ifelse(Species=='versicolor',1,0)) %>% 
  select(-Species,-muestra)

tree_model2 <- rpart(isVersicolor~.,train_tree,method = 'class',control=rpart.control(cp=0))

test_tree$pred <- predict(tree_model2,test_tree,type = 'class')

table(test_tree$isVersicolor,test_tree$pred)
mean(test_tree$isVersicolor==test_tree$pred)
#Prunning strategies (podar los árboles si se vuelven muy grandes)
#Pre-pruning. Stop dividing when the tree has arrived to a pre-defined:
#*Size or depth in branches
#*Number of observations per branch
#*#Post pruning grow a large complex tree and then prune to prevent cutting patterns
#*To identify the best point is to draw an error rate vs complexity plot.
#*Then the error rate starts to flatten as it gets more complex is a very good point to stop.
prune_control <- rpart.control(maxdepth = 30, minsplit = 20)
model_tree <- rpart(isVersicolor~.,data = train_tree,method = 'class',control = prune_control)
test_tree$pred2 <- predict(model_tree,test_tree,type='class')
table(test_tree$isVersicolor,test_tree$pred2)
mean(test_tree$isVersicolor==test_tree$pred2)

plotcp(model_tree)
#Post pruning needs an already build model,

model_tree_pruned <-prune(model_tree,cp=0.3)
test_tree$pred3 <- predict(model_tree_pruned,test_tree,type='class')
table(test_tree$isVersicolor,test_tree$pred3)
mean(test_tree$isVersicolor==test_tree$pred3)

model_tree <-rpart(isVersicolor~.,data=train_tree,method='class',control=rpart.control(cp=0))

####____Random forests____####
#Decision trees can be created in parallel to make smaller trees yet powerful as an enssemble 
#to capture the data's complexity. This is called a random forests.
#This comes from the fact that some of the trees will be very good at handling specific patterns of the data.
#And together they complement each other.
#These are referred to as ensemble methods

random_forest_model <- randomForest(isVersicolor~.,data=train_data,
                                    ntree=500)
#where ntree is the number of trees in the forest and mtry is the number of predictors per tree 
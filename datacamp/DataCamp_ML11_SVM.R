####____SUPPORT VECTOR MACHINE MODELS____####
####____INTRODUCTION____####
#Binary-classification one dimensional for clarity of concepts.
#When plotting the variables in a scatter plot and distinguishing them among classes
#Any separating boundary between classes is called a separator or decision boundary.
#Another important concept is the margin that is the distance between the decision
#boundary and the closest data point.

#THE BEST DECISSION BOUNDARY IS THE ONE THAT MAXIMIZES THE MARGIN CALLED THE
#MAXIMUM MARGIN BOUNDARY (OR SEPARATOR)

#Now we look at 2D datasets generated randomly
set.seed(1998)
n <- 200
df <- data.frame(x=runif(n), y=runif(n))
df$color <- ifelse(df$x-df$y>0, "red", "blue")
margin <- 0.05
df <- df[abs(df$x-df$y)>margin,]
library(ggplot2)
p <- ggplot(df,aes(x=x,y=y, color = color)) +
  geom_point()+
  scale_color_manual(values=c("red","blue"))+
  geom_abline(slope=1, intercept = 0)+
  geom_abline(slope=1, intercept = -margin, linetype="dashed")+
  geom_abline(slope=1, intercept = margin, linetype="dashed")
####____LINEAR KERNELS____####
#Linear support vector machines applies the previous concepts to find a linear separator
#between the groups.
library(tidymodels)
split <- initial_split(df, prop=0.8)
dfTrain <- training(split)
dfTest <- testing(split)
#DECISION BOUNDARIES CAN HAVE DIFFERENT SHAPES THESE TYPES ARE CALLED KERNEL.
install.packages("e1071")
library(e1071)
#svm(fmla, data = dftrain, type = "C-classification", kernel, cost, gamma, scale =TRUE)
dfTrain$color <- as.factor(dfTrain$color)
svmModel <- svm(color~., data=dfTrain, 
                type="C-classification", 
                kernel="linear",
                scale=FALSE)
svmModel
svmModel$index #index of rows that are used for SV
svmModel$SV #support vectors
svmModel$rho #the negative y intercept of the decision boundary
svmModel$coefs #weighting coefs of support vectors

predTrain <- predict(svmModel, dfTrain)
accureTrain <- mean(predTrain==dfTrain$color)
#Evaluate on test dataset
predTest <- predict(svmModel, dfTest)
accureTest <- mean(predTest==dfTest$color)
#Note that the number of support vectors is 50. They will be discussed later.

#VISUALIZING SUPPORT VECTORS
dfSV <- dfTrain[svmModel$index,]
p <- p+geom_point(data=dfSV,aes(x=x, y=y), color="purple", size=4, alpha=0.5)
p
#These points are used as support vectors for the decision boundary.
#To plot this boundary we need the slope and intercept but they are not stored in the model.
#First construct the weight vector:
w <- t(svmModel$coefs)  %*% svmModel$SV
w
slope_1 <- -w[1]/w[2]
intercept_1 <- svmModel$rho/w[2]

ggplot(data=dfTrain, aes(x=x, y=y, color=color))+
  geom_point()+
  scale_color_manual(values=c("red","blue"))+
  geom_abline(slope = slope_1,
              intercept = intercept_1)+
  geom_abline(slope = slope_1,
              intercept = intercept_1-1/w[2], linetype="dashed")+
  geom_abline(slope = slope_1,
              intercept = intercept_1+1/w[2], linetype="dashed")

#SUMMARY FORM
library(e1071)
svmModel <- svm(color~., data=dfTrain, kernel="linear", type="C-classification")
plot(svmModel, data=dfTrain)

#Up to now we have not tuned the parameters such as the margin and how to make them soft
#We tweak this with the cost parameter. 
svmModel <- svm(color~., data=dfTrain, kernel="linear", type="C-classification",
                cost=100)
svmModel
plot(svmModel, data=dfTrain)
#Making the margins narrow by making the cost higher is useful when the margin is known 
#to be linear, but that is rarely the case. Decreasing the margin when the margin is 
#confusing assures to get a wider area where the real margin may lay.

#MULTICLASS PROBLEMS
#Since SVM is essentially a binary classification model it can be used to multiclass problem
#with the following ONE-AGAINST-ONE strategy:
# 1. Partition the data into subsets of two classes only.
# 2. Apply the SVM algorithm to each subset
# 3. Use majority vote to assign a class to each observation.
irisSplit <- initial_split(iris, prop = 0.7, strata = Species)
irisTrain <- training(irisSplit)
irisTest <- testing(irisSplit)
speciesModel <- svm(Species~., data=irisTrain, kernel="linear", type="C-classification",
                    scale = FALSE)
#Performance in trainset
predSpecies <- predict(speciesModel, irisTrain)
mean(predSpecies==irisTrain$Species)
#Performance in testset
predSpecies <- predict(speciesModel, irisTest)
mean(predSpecies==irisTest$Species)

plot(speciesModel, data=irisTrain,formula = Sepal.Length~Petal.Length)

####____POLYNOMIAL KERNELS____####
#RADIALLY SEPARABLE DATASET
n <- 200
df <- data.frame(x1=runif(n, min=-1, max=1), 
                 x2=runif(n, min=-1, max=1))
radius <- 0.7
df$y <- factor(ifelse(df$x1^2+df$x2^2<radius^2, -1,1), levels = c(-1,1))

circle <- function(h, k, r, npoint=100){
  theta <- seq(0, 2*pi, length.out=npoint)
  x <- h+r*cos(theta)
  y <- k+r*sin(theta)
  data.frame(x1c=x, x2c=y)
}
boundary <- circle(0,0,r=radius)
p <- ggplot(data=df, aes(x=x1, y=x2, color=y))+
  geom_point()+
  scale_color_manual(values=c("red","blue"))
p
p <- p+ geom_path(data=boundary,aes(x=x1c,y=x2c), inherit.aes = FALSE)
p
#THE KERNEL TRICK
#We use a mathematical transformation to render the problem linearly separable.
#We know x1^2+x2^2=r then X1=x1^2 and X2=x2^2 so that X1+X2=r or X2 = -X1+r
#The polynomial kernel has the form:
#(gamma* (u.v)+coef0)^degree   ...where gamma and degree are tuning parameters.

circleModel <- svm(color~., data=dfTrain, kernel="polynomial", degree=2, 
                   type="C-classification",scale=FALSE)
plot(circleModel, dfTrain)

#THESE KERNEL FUNCTIONS SATISFY CERTAIN PROPERTIES:
# 1. These are generalizations of dot products.
#svm(..., kernel="polynomial", degree=2)

#TUNE PARAMETERS TO FIND THE BEST COMBINATION
#tune_out <- tune.svm(x=predictors, y=outcome, type="C-classifcation",kernel="polynomial",degree, 
#         cost=10^(-1:2), gamma= c(0.1,1,10), coef0 = c(0.1,1,10))
#tune_out$best.parameters$cost

####____RADIAL-BASED KERNELS____####
#We use the radial zone around each point to classify observations. The extent to which
#this area is used for classification shpuld decay with distance.
#For this we have the exp(-gamma*r) where gamma should be tunnable.
#svm(..., kernel="radial"
#tune_svm(, gamma, cost)

#The wortkflow is:
#Tune the model with tune.svm()
#Build model with best parameters svm()
#Calculate accuracy predict() then mean()
#plot the model with plot(model, data)


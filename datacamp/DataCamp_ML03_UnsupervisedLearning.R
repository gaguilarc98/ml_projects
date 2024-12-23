####____UNSUPERVISED LEARNING____####
#Types of machine learning
#  * Unsupervised learning. Find patterns in unlabeled data
#  * Supervised learning. Predictions on labeled data: regression/ classification
#  * Reinforcement learning. When computers learn from real feed data.

#Objectives of learning data:
#  * CLUSTERING. Finding homogeneous subgroups within larger groups.
#  * FINDING PATTERNS. This can be done by dimensionality reduction.
#   Dimensionality reduction allows to visualize high dimensional data
#   It also serves the purpose of pre-processing data for supervised learning

####____K-MEANS____####
#It is a clustering algorithm, that is finding homogeneous subgroups within a population
#Breaks the observations into pre-defined number of clusters.
#kmeans(df, centers= 5, nstart= 20)
#To improve the results run the algorithm multiple tumes to improve odds of the best model
df <- iris[as.vector(sapply(iris, function(x){is.numeric(x)}))]
str(df)
df.km <-kmeans(df, centers = 3, nstart=20)
str(df.km)

plot(df[c(1,2)],col=df.km$cluster, xlab='', ylab='', main='k-means with 3 clusters')
plot(iris[c(1,2)],col=iris$Species, xlab='', ylab='', main='original species')

#Sketch of how kmeans works:
# * Random cluster assignment at first
# * Calculate centers of each subgroup (average)
# * Reassign points to its closest center (iterate)
# * Recalculate centers and repeat assignment until no point changes assignment

#In R k means best outcome is based on total within cluster sum of squares (minimized)
#The argument n-starts specifies how many random starts to select
#This tries to find the global minima of the total within sum of squares.
####____Scree plot____####
#To choose the best number of groups try different number of groups and
#Plot the total within sum of squares as a function of number of groups
#There should be an identifiable elbow after which increasing the number of groups
#does not lower the total within SS significantly.

par(mfrow=c(2,3))
for (i in 1:6) {
  df.km <- kmeans(df,centers=3,nstart = 1)
  plot(df[c(1,2)], col = df.km$cluster, main=df.km$tot.withinss, xlab='',ylab='')
}
par(mfrow=c(1,1))
wss <- NULL
for (i in 1:10) {
  df.km <- kmeans(df,centers=i, nstart=20, iter.max = 20)
  wss[i] <- df.km$tot.withinss
}
plot(1:10,wss,type='b', xlab='Number of clusters',ylab='Total within SS')

####____HIERARCHICAL CLUSTERING____####
#When the number of clusters is not provided to the algorithm, we use hierarchical clustering
#There are two type of hierarchical clustering:
# * Bottom-up and top-down. Let us focus on top bottom-up
# * At first each point is its own cluster
# * Next find the closest two clusters and join them in a single cluster
# * Then repeat the process, noting that in the next steps we find distance from clusters
#   that is distances between the centers
# * The process continues until there is one single cluster
#dist(x) then hclust(x)
hclust(d = x)
####____DIMENSIONALITY REDUCTION_____####
df.hclust <- hclust(dist(df))
summary(df.hclust)
####____DENDOGRAMA____####
#This plot has every single entry at the bottom and uses vertical lines
#To represent distance between two clusters and horizontal lines to join them
plot(df.hclust)
abline(h=6, col='red')
abline(h=3, col='blue')
cutree(df.hclust, h=6)#this cuts the dendogram creating groups by height
df.hclust.cut <- cutree(df.hclust, k = 3)#by number of clusters
#clustering linkage
#The distance is key for this method, by default is euclidian distance
#Methods to determine which clusters to link:
# * Complete. Pairwise similarity between all clusters and use largest similarity.
# * Single. Pairwise similarity between all clusters and use smallest similarity.
# * Average. Same but use average of similarities.
# * Centroid. Finds centroid of clusters 1 and 2 an uses similarity between centroids.
#hclust(method = c('complete', 'average', 'single'))
#Finally, data measured on different scales can cause undesirable behaviour
#So normalizing the data is recommended
#scale(x) then colMean(x) then apply(x, 1, sd)

####____INTRODUCTION TO PCA____####
#Dimensionality reduction: One of the methods is Principal Component Analysis
# * Find a linear combination of features, these will be principal components
# * Maintain most of the variance from the original data
# * The new features will be uncorrelated or orthogonal to each other
df.pca <- prcomp(df, scale = FALSE, center=TRUE)
#interpreting pca results and visualizing the results
####___VISUALIZATIONOF PCA____####
#BIPLOT
#Shows the first two principal components
#it also maps the direction of each original column
biplot(df.pca,main='Biplot')
#SCREE PLOT
#They show the amount of variance explained
#Or the cumulative percentage of variance explained as the number of components increases
df.var <- df.pca$sdev^2
pve <- df.var/sum(df.var)
plot(pve, xlab='Numebr of Component',ylab='Proportion of Variance Explained',
     ylim = c(0,1), type='b')
plot(cumsum(pve), xlab='Numebr of Component',ylab='Proportion of Variance Explained',
     ylim = c(0,1), type='b')
screeplot(df.pca)
#Practical issues
# * Scaling the data
# * Missing values: Drop, impute
# * Categorical data: Do no include them/ encode categorical
head(mtcars)
colMeans(mtcars)
apply(mtcars,2,sd)
mtcars.scale <- scale(mtcars)

####____CASE STUDY____####
#Explory data
#Dealing with missing values, categorcal and target data
#Scaling, prepare the data for the analysis
#Putting it all together
#Hierarchical cluster model with pca
#Use components to explain at least 95% of variability
summary(df.pca)
df.hclust.pca <- hclust(dist(df.pca$x[,1:2]), method='complete')
df.hclustpca.cut <- cutree(df.hclust.pca,k=3)

table(df.km$cluster, iris$Species)
table(df.hclust.cut, iris$Species)
table(df.hclustpca.cut, iris$Species)#Applying pca first improved the model's performance


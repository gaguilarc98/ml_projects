####____CLUSTER ANALYSIS____####
#Data exploration technique. Goal intuition behind the underlying methods.
#Clustering means grouping observations so that all members of each group are similar 
#to one another but at the same time are different from the rest of the groups.
#Cluster analysis is a form of exploratory data analysis (EDA) where observations are divided
#into meaningful groups. Requirements:
# * Completa data (no missing values)
# * Values measured in similar scales
# * Choose appropiate metric to calculate similarity
# * Cluster according to any method. And then analyize the results.

####____DISTANCE VS SIMILARITY____####
#Instead of using measures of distance we could define similarity (or disimilarity)
#in some other sense. 
#Usually they refer to the same thing as disimilarity is measured with a distance metric.
#The closer the distance the more similar two observations are.
#One such a measure is euclidean distance dist(df) where df is data.frame or matrix.
#dist(df, method="euclidean")
set.seed(1234)
df <- mtcars[sample(1:length(mtcars),size = 6),]
autos_dist <- dist(df, method = "euclidean")
print(autos_dist)
####____THE IMPORTANCE OF SCALE____####
#Because of the way distance is calculated, when features are not measured on the same scale,
#the distances could be different from what expected. Features measured on larger scales will dominate
#over features measured in smaller scales even if the relative changes are way larger: 0.1 to 0.2 vs 20.1 and 20.2

#A mehtod for scaling is standarization. On each feature substract the mean and divide by sd.
#scale(df) will scale with means 0 and variances of 1
df_scaled <- scale(df)
dist_scaled <- dist(df_scaled)
print(dist_scaled)
####____DISTANCE FOR CATEGORICAL DATA____####
#Starting with BINARY variables. We could use the Jaccard index. 
#This captures the ratio of the number of times the features of both observations are true
#vs the number of times they are ever TRUE.
#Example: People's liking alcohol drinks
# Obs | wine | beer | whiskey | vodka
# 1 | T     |   T   |   F   |   F
# 2 |   F   |   T   |   T   |   T
#So the number of times both are T is 1 and the number of times when they are ever T is 4.
#Jaccard(1,2) = 1/4 = 0.25 then Distance = 1-similarity = 1-0.25 = 0.75.
#dist(df, method = "binary")
bin <- data.frame(a=c(T,F,T,T), b=c(T,F,F,T), c = c(T, T, F,F))
bin_dist <- dist(bin, method="binary")
print(bin_dist)
#MUTLPLE CATEGROY VARIABLES
#For this we need to do a dummification of the categories. Each category of each feature
#Is created its own variable of absence/presence.
install.packages("dummies")
library(dummies)
df <- data.frame(Sex = c("M","M","F","F"), EyeColor = c("Green","Brown","Brown","Blue"),
                 HairColor = c("Blond","Black","Brown","Red"))

df_dummy <- dummy.data.frame(df)
dist(df_dummy, method="binary")
####____COMPARING TWO OBSERVATIONS____####
#When a cluster is formed based on joining two closest obseravtions then we need 
#a method to measure distance between an observation to a cluster.
# * COMPLETE LINKAGE CRITERIA.
#We could define distance from an observation to a cluster as the largest distance
#between the observations and any member of that cluster.
#The decision of thow to select closest observation to a group is the LINKAGE CRITERIA.

# * SINGLE LINKAGE. Takes the minimum distance from an observation to each point in a group
#as the criteria for distance between a point and a cluster.
# * AVERAGE LINKAGE. Takes the average distance between a point and all observations in
#the group.

####____CAPTURING K CLUSTERS____####
set.seed(12345)
df <- mtcars[sample(1:length(mtcars),size = 8),]
df_scaled <- scale(df)
df_scaled
dist_scaled <- dist(df_scaled)
df_hclust <- hclust(dist_scaled, method = "complete")
df_cluster <- cutree(df_hclust, k=2) #The output is a vector
print(df_cluster)
df_scaled <- as.data.frame(df_scaled)
df_scaled$cluster <- factor(df_cluster)
df_scaled

library(ggplot2)
ggplot(df_scaled, aes(x=mpg, y= cyl, color=cluster))+
  geom_point()

####____DENDROGRAM___####
#Also known as a tree diagram
#We start with the closest observations on the bottom and the distance is mapped into
#The height of lines that match the observations.
plot(df_hclust,hang = -1) #hang forces lines to start at the bottom
df_hclustsingle <- hclust(dist_scaled, method="single")
df_hclustave <- hclust(dist_scaled, method="average")
plot(df_hclustsingle, hang=-1)
plot(df_hclustave, hang=-1)
####____CUTTING THE TREE####
install.packages("dendextend")
library(dendextend)#To plot colored branches
dend_autos <- as.dendrogram(df_hclust)
dend_colored <- color_branches(dend_autos, h=5)
plot(dend_colored)
dend_colored <- color_branches(dend_autos, k=3)
plot(dend_colored)

####____EXPLORING MORE THAN TWO DIMENSIONS____####
#When we have more than 2 features it becomes difficult to visualize the observations
#To overcomes this we could create multiple 2D plots of each pair of features coloring the clusters.
#Or use a Dimensionality Reduction Method such as PCA.
####____INTRODUCTION TO K MEANS____####
#This method is used when we know before-hand how many categories are there.
#That will be the k parameter. It also can be estimated from the data empirically.
#Then:
# * Randomly select starting k centroids (these do not need to coincide with observed points)
# 1 For each observation calculate the distance to a centroid.
# 2 The observations are initially assigned to the cluster of closest centroid.
# 3 Then recalculate the centroids from the resulting assigments.
# 4 Then Repeat steps 1:3.
# 5 Stop when no longer we have changes in assignment of clusters.
#kmeans(df, centers=2)
library(dplyr)
df <- mtcars[sample(1:30,8),]
df_scaled <- scale(df)
modelkm <- kmeans(df_scaled, centers = 3)
df_scaled_km <- mutate(as.data.frame(df_scaled), cluster=modelkm$cluster)
ggplot(df_scaled_km, aes(x=mpg, y=cyl, color=factor(cluster)))+
  geom_point()
####____EVALUATING K VALUES____####
#Estimating k with elbow method
# * Calculate the total within sum of squares for each value of k
# * Plot the SS as a function of k. 
df <- mtcars[sample(1:30,8),]
df_scaled <- scale(df)
modelkm$tot.withinss
library(purrr)
tot_withins <- map_dbl(1:7, function(k){
  model <- kmeans(df_scaled, centers = k)
  model$tot.withinss
})
elbowdf <- data.frame(k =1:7, tot_withinss = tot_withins)
print(elbowdf)
plot(elbowdf, type="b")
####____Silhouette analysis____####
#For this we need to construct the silhouette width for every observation
#It consists of two parts: within cluster distance C and the closest neighbor distance N
#The within cluster distance is the average euclidean distance from that observation to each
#observation within the same cluster.  
#The closest neighbor distance is the average distance from one observation to the closest neighbor cluster.
#The silhouette width is defined as:
#S(i) =  1-C(i)/N(i), si C(i)<N(i)
#S(i) = 0, si C(i)=N(i)
#S(i) =  N(i)/C(i)-1, si C(i)>N(i)
#We can calculate this with the pam(df, k=2)
library(cluster)
pam_k2 <- pam(scale(df), k=2)
str(pam_k2)
pam_k2$silinfo$widths
silplot <- silhouette(pam_k2)
plot(silplot)
pam_k2$silinfo$avg.width#Average width
#silhouette  width ranges from. -1 to 1. When the values is near one it means that the observation
#Is well fitted in the current cluster. 0 is frontier.
df_scaled <- scale(df)
vark_clust <- map_dbl(2:6, function(k){
  model <- pam(df_scaled, k=k)
  model$silinfo$avg.width
})
sil_df <- data.frame(k=2:6, silh = vark_clust)
ggplot(sil_df, aes(x=k, y=silh))+
  geom_point()
#Now we can plot the average silhouette width the greater the better.
####____MAKING SENSE OF THE KMEANS CLUSTER____####
#kmeans is widely used over hierarchical clustering because it is computationally
#more efficient on large sets of data. Bt hierarchical clustering is usually a better
#theoretical option.


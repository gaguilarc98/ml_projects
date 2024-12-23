####____SPARK WITH sparklyr in R____####
####____USE SPARK WITH DPLYR SYNTAX____####
#Spark is useful for parallel computing for large datasets. It can use several machines.
#It works with dplyr but it was developed recently. Is an open-source cluster computing platform.
#WORKFLOW PATTERN
#Connect, work, disconnect
spark_connect()
spark_disconnect()
#dplyr verbs work with local df's and spark df's

# Load sparklyr
library(sparklyr)
spark_conn <- spark_connect(master="local")# Connect to your Spark cluster
spark_version(sc=spark_conn)# Print the version of Spark
spark_disconnect(sc=spark_conn)# Disconnect from Spark

#When working in spark it is possible to copy the data stored in your pc to a remote location
#but it is a slow process. Use copy_to(dest=spark_dest, df=df) from dplyr
#To see all tables in spark use src_tbls(x=spark_conn)
library(dplyr)
str(iris)# Explore track_metadata structure
spark_conn <- spark_connect("local")# Connect to your Spark cluster
track_metadata_tbl <- copy_to(spark_conn, iris)# Copy track_metadata to Spark
src_tbls(spark_conn)# List the data frames available in Spark
spark_disconnect(spark_conn)# Disconnect from Spark

#dplyr tibbles are similar to R's data.frame objects, but it allows them to store remotely
#And the object only stores the connection. On the side of spark, the data is stored in a 
#variable called DataFrame that is most similar to R's data.frame with some variations.
track_metadata_tbl <- tbl(spark_conn, "track_metadata")# Link to the track_metadata table in Spark
dim(track_metadata_tbl)# See how big the dataset is
object_size(track_metadata_tbl)# See how small the tibble is

#When looking at the str() of a tibble it returns details of the connection and not the data
#when using glimpse it retreives only some of the first rows of the data as copying takes times.
print(track_metadata_tbl,n=5,width=Inf)# Print 5 rows, all columns
str(track_metadata_tbl)# Examine structure of tibble
glimpse(track_metadata_tbl)# Examine structure of data

#Once loaded the data you can use dplyr verbs to manipulate it. However, internally the 
#code is translated to sql so some functionalities are not supported such as filtering
#by regular expressions.
#You can use select(), filter() and arrange()
####____ADVANCED DPLYR USAGE____####
#starts_with(), ends_with(), contains(), matches()
#for categorical use levels() and for any comb of vars use distinct()

#Use count(x,y,z, sort=TRUE) instead of table() for categorical counting.
#top_n() is similar to head() but can work on Spark
#When working with remote data as in spark two useful funcions are:
compute() # to store results in Spark
collect() #to pull results back to R

#USE collect() to retrieve data into R and use plotting or modelling functions.
#Use compute() to store data temporarily in Spark
#This is recommended as opposed to piping hundreds of steps in a single object.
#And instead of copying intermediate steps into R as it is slow.
#compute(tibble, name) #These are its arguments.
####____NATIVE INTERFACE TO MANIPULATE SPARK DF____####
####____RUNNING ML MODELS ON SPARK____####
####____INTRODUCTION TO SPARKLYR____####
####____NATIVE INTERFACE OF SPARKLYR____####
#Spark has two other interfaces:
# MLlib that supports ML functions:
# * ft_function() #for feature transformation
# * ml_functiuon() #for machine learning
#Spark DataFrame API for sorting sampling and partitioning datasets
# * sdf_function()

#SPARK IS MUCH SCRICTER ABOUT VARIABLE TYPES THAN R, MOST WANT DoubleType and
#returns DoubleType outputs
#To convert a numeric var into binary or logical
ft_binarizer("var","dicotomic_name", threshold)
track_metadata_tbl# track_metadata_tbl has been pre-defined

hotttnesss <- track_metadata_tbl %>%
  select(artist_hotttnesss) %>%# Select artist_hotttnesss
  ft_binarizer("artist_hotttnesss","is_hottt_or_nottt",threshold=0.5) %>%# Binarize to is_hottt_or_nottt
  collect() %>%# Collect the result
  mutate(is_hottt_or_nottt = as.logical(is_hottt_or_nottt))# Convert is_hottt_or_nottt to logical

ggplot(hotttnesss, aes(x=is_hottt_or_nottt)) +
  geom_bar()# Draw a barplot of is_hottt_or_nottt

#To convert to buckets or categories ft_bucketizer("var","cat_name",splits=c())
#cut() by default is right.end included, but ft_bucketizer() has left.end included
track_metadata_tbl # track_metadata_tbl, decades, decade_labels have been pre-defined
decades #for split into decades
decade_labels #for recovering decade labels as factor

hotttnesss_over_time <- track_metadata_tbl %>%
  select(artist_hotttnesss, year) %>%# Select artist_hotttnesss and year
  mutate(year = as.numeric(year)) %>%# Convert year to numeric
  ft_bucketizer("year","decade",splits=decades) %>%# Bucketize year to decade using decades vector
  collect() %>%# Collect the result
  mutate(decade = factor(decade,labels=decade_labels))# Convert decade to factor using decade_labels
ggplot(hotttnesss_over_time, aes(decade, artist_hotttnesss)) +
  geom_boxplot()  # Draw a boxplot of artist_hotttnesss by decade

#To convert quantiles to categories ft_quantile_discretizer("var", "bin_name", n.buckets)
track_metadata_tbl # track_metadata_tbl, duration_labels have been pre-defined
duration_labels #For convertiong back into factor with labels

familiarity_by_duration <- track_metadata_tbl %>%
  select(duration, artist_familiarity) %>%# Select duration and artist_familiarity
  ft_quantile_discretizer("duration","duration_bin",n.buckets=5) %>%# Bucketize duration
  collect() %>%# Collect the result
  mutate(duration_bin = factor(duration_bin, labels=duration_labels))# Convert duration bin to factor

ggplot(familiarity_by_duration, aes(duration_bin, artist_familiarity)) +
  geom_boxplot()# Draw a boxplot of artist_familiarity by duration_bin

#To separate words and convert them to lowercase ft_tokenizer("var","name")
#The output is a list of strings for each observation.
#unnest() can be used to convert the list back to a single variable

track_metadata_tbl
title_text <- track_metadata_tbl %>%
  select(artist_name, title) %>%# Select artist_name, title
  ft_tokenizer("title","word") %>%# Tokenize title to words
  collect() %>%# Collect the result
  mutate(word = lapply(word, as.character)) %>% # Flatten the word column 
  unnest(word) # Unnest the list column

#tidytext package canbe used for Sentiment Analysis

#To split with regular expression use ft_regex_tokenizer("x","y",pattern=regex_pattern)
track_metadata_tbl# track_metadata_tbl has been pre-defined

track_metadata_tbl %>%
  select(artist_mbid) %>%# Select artist_mbid column
  ft_regex_tokenizer("artist_mbid","artist_mbid_chunks",pattern="-")# Split it by hyphens

#For sorting in Spark API style use sdf_sort(c("var1","var2","var3"))
track_metadata_tbl# track_metadata_tbl has been pre-defined

# Use microbecnhmark to Compare timings of arrange() and sdf_sort()

microbenchmark(
  arranged = track_metadata_tbl %>%
    arrange(year, artist_name, release, title) %>%# Arrange by year, then artist_name, then release, then title
    collect(), # Collect the result
  sorted = track_metadata_tbl %>%
    sdf_sort(c("year","artist_name","release","title")) %>% # Sort by year, then artist_name, then release, then title
    collect(), # Collect the result
  times = 5
)

#To explore columns of a tibble in R from sparklyr use sdf_schema()
#sdf_schema returns a list with name of variables and its type.
#logical -> BooleanType, numeric -> DoubleType, integer -> IntegerType(), character -> StringType(), list -> ArrayType()

#To get a subset of data to explore it use sdf_sample(fraction=0.1, replacement=FALSE, seed)
track_metadata_tbl # track_metadata_tbl has been pre-defined

track_metadata_tbl %>%
  sdf_sample(fraction=0.01, replacement=FALSE, seed=20000229) %>% # Sample the data without replacement
  compute("sample_track_metadata") # Compute the result

#To partition the data into training and testing use sdf_partition(training=0.7, testing=0.3)
#Actually you cna partition into any number of sets with their own names sdf_partition(a=0.1, b=0.8, c=0.1)
track_metadata_tbl # track_metadata_tbl has been pre-defined

partitioned <- track_metadata_tbl %>%
  sdf_partition(training=0.7, testing=0.3) # Partition into training and testing sets

dim(partitioned$training) # Get the dimensions of the training set
dim(partitioned$testing) # Get the dimensions of the testing set

####____ML MODELS IN SPARK____####
#MLlib to use Ml models, particularly random forests and gradient boosted trees
#to see all avilable models
ls("package:sparklyr", pattern = "^ml")
#All ml_functions() take a tibble, a response name variable and a character 
#vector of names of explanatory fields

#Working with parquet files. When storing data as parquet we are saving a whole directory
#of files where data is split across multiple .parquet files 
#To read them use spark_read_parquet(spark_conn, "spark_name", "path")
parquet_dir # parquet_dir has been pre-defined
filenames <- dir(parquet_dir, full.names=TRUE) # List the files in the parquet dir

# Show the filenames and their sizes
data_frame(
  filename = basename(filenames),
  size_bytes = file.size(filenames)
)
timbre_tbl <- spark_read_parquet(spark_conn, "timbre","parquet_dir")# Import the data into Spark

#PARTITIOING DATA TO PREDICT OUTPUT
track_data_tbl # track_data_tbl has been pre-defined

training_testing_artist_ids <- track_data_tbl %>%
  select(artist_id) %>% # Select the artist ID
  distinct(artist_id) %>% # Get distinct rows
  sdf_partition(training=0.7, testing=0.3) # Partition into training/testing sets

track_data_to_model_tbl <- track_data_tbl %>%
  inner_join(training_testing_artist_ids$training, by="artist_id") # Inner join to training partition

track_data_to_predict_tbl <- track_data_tbl %>%
  inner_join(training_testing_artist_ids$testing, by="artist_id") # Inner join to testing partition

#To train a gradient boosted model use ml_gradient_boosted_trees()

track_data_to_model_tbl # track_data_to_model_tbl has been pre-defined

#Subset columns whose name has #timbre on it
feature_colnames <- track_data_to_model_tbl %>%
  colnames() %>% # Get the column names
  str_subset(fixed("timbre")) # Limit to the timbre columns

gradient_boosted_trees_model <- track_data_to_model_tbl %>%
  ml_gradient_boosted_trees("year",feature_colnames) # Run the gradient boosted trees model

#To make predictions use R'2 native predict()
# training, testing sets & model are pre-defined
track_data_to_model_tbl
track_data_to_predict_tbl
gradient_boosted_trees_model

responses <- track_data_to_predict_tbl %>%
  select(year) %>% # Select the year column
  collect() %>% # Collect the results
  # Add in the predictions
  mutate(
    predicted_year = predict(
      gradient_boosted_trees_model,
      track_data_to_predict_tbl
    )
  )
#Spark does not support residuals() function in any of its models so they hace to be calculated manually
responses # responses has been pre-defined
ggplot(responses, aes(actual, predicted)) + # Draw a scatterplot of predicted vs. actual
  geom_point(alpha=0.1) + # Add the points
  geom_abline(intercept=0, slope=1) # Add a line at actual = predicted

residuals <- responses %>%
  transmute(residual = predicted-actual) # Transmute response data to residuals

ggplot(residuals, aes(residual)) + # Draw a density plot of residuals
  geom_density() + # Add a density curve
  geom_vline(xintercept=0) # Add a vertical line through zero

#To run a random forest model use ml_random_forest()
track_data_to_model_tbl # track_data_to_model_tbl has been pre-defined

# Get the timbre columns
feature_colnames <- track_data_to_model_tbl %>%
  colnames()%>%
  str_subset(fixed("timbre"))

random_forest_model <- track_data_to_model_tbl %>%
  ml_random_forest("year",feature_colnames) # Run the random forest model

# training, testing sets & model are pre-defined
track_data_to_model_tbl
track_data_to_predict_tbl
random_forest_model

# Create a response vs. actual dataset
responses <- track_data_to_predict_tbl %>%
  select(year)%>%
  collect() %>%
  mutate(predicted_year = predict(random_forest_model, track_data_to_predict_tbl))

#PLOT BOTH MODELS
both_responses # both_responses has been pre-defined as the union of both models outputs predicted and actual
ggplot(both_responses, aes(actual, predicted, color=model)) + # Draw a scatterplot of predicted vs. actual
  geom_smooth() + # Add a smoothed line
  geom_abline(intercept = 0, slope = 1) # Add a line at actual = predicted

residuals <- both_responses %>%
  mutate(residual = predicted-actual) # Create a tibble of residuals

ggplot(residuals, aes(residual, color=model)) + # Draw a density plot of residuals
  geom_density() + # Add a density curve
  geom_vline(xintercept = 0) # Add a vertical line through zero

#MEASURE TO COMPARE BETWEEN MODELS: rmse
both_responses # both_responses has been pre-defined

# Create a residual sum of squares dataset
both_responses %>%
  mutate(residual = predicted-actual)%>%
  group_by(model)%>%
  summarise(rmse = sqrt(mean(residual^2)))
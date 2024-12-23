--STATISTICS
--Practice and study of collecitng and analyzing data.
--It has to branches: Descriptive/Inferential.
/*
Statistics can answer practical questions. But they have to be measurable questions.
Types of data: 
-Numeric or qunatitative data which can be:
  * Continous data 
  * Interval/count data
 One way of visualizing is with scatterplots.
 -Categorical which can be:
   * Nominal
   * Ordinal
 To visualize numeric and categorical data we can group the values by categories.

Descriptive statistics. Describes or summarizes data.
Inferential statistics. Draws conclusions about a popultation based on a sample.
*/
/*
Measures of center. Answering questions of the type what is the 
most common, average, typical are expressions that requires measures of center.
HISTOGRAMS are a way to visualize numeric data.
Another way to summarize data is with measure of center:
  * Mean, sum of total value divided by the number of values.
  * Median, middle value for a given variable
  * Mode, most frequent value (useful for categorical data)
 When data is symmetrical both mean and median work well.
 When there are outliers on one side, the mean is biased, so median is better.
*/
/*
Measures of spread. Measures how far apart data points are. It tells how much variety occurs in data.
  * Range. Difference between maximum and minimum values.
  * Variance. Average distance squared from each data point to the mean.
  * Standard deviation. Square root of variance.
  * Quartiles. Splitting data into four equal parts.
  * Interquartile range (IQR). 3rd qurtile - 1st quartile.
*/
/*
Measuring chance. It can be measured with probabilities. 
Take the number of ways an event could happen and divide it by the total number of outcomes.

Sampling. If we have to select some individuals we can use a random choosing device to do it.
If we have to do this repeatedly two situations can arise. We replace individuals already chosen
on the next draw or we can leave them out.

This gives rise to a consept known as indepence of events. This is when the probability of an event
remains unchanged after the outcome of some other event.

Probability of categories. If we have a history of frequencies of categories
we can estimate the probability of each category dividing each frequnecy with the total number of records.
*/
/*
Conditional probability. When we have non independent events (usually when sampling without replacement).
Two events are dependent if the probability of the second event is affected by the outcome of the first event.
Venn diagrams are one way of visualizing dependence of events.
P(A|B) = P(AB)/P(B)
*/
/*
DISCRETE DISTRIBUTIONS.
This describes the probability of each possible outcome in a scenario.
We can get the mean of a distribution by multiplying each outcome with its probability and summing up.
LAW OF LARGE NUMBERS.
We can get an approximate to the distribution by taking a sample and making a histogram.
The larger the sample size the closer the sample distribution will be to the real distribution.

CONTINUOUS DISTRIBUTIONS.
We use continous lines to represent distribution. The probabilites
only make sense for range of intervals and is calculated as the area
under the curve for the selected range.
*/
/*
BINOMIAL DISTRIBUTION
Describes the number of successes in a sequence of independent events.
The expected value of the binomial distribution is n*p
NOTE. That this apply only to sequence of independent events where the probaiblity of success remains unchanged.

NORMAL DISTRIBUTION
Its shape is known as a bell curve and it has some noticeable properties:
  * Symmetrical
  * The probability never hits zero.
 It is described by its mean and standard deviation.  
 68% of the area is below 1 sd from the mean
 95% of the area falls between 2 sd fro the mean.
 99.7% of the area falls between 3 sd from the mean.
 Real world data often resembles a normal distribution.
 
Skewness. Positive skewed if the right tail is heavier.
Negative skewed if the left tail is heavier.
Kurtosis. Describes the occurrence of extreme values of the distribution.
Leptokurtic (+ kurtosis) has a narrow sd while platykurtic (- kurtosis) has a large sd.

CENTRAL LIMIT THEOREM
One of the reasons that the normal distribution is so important is based on:
The sampling distribution of the sample mean. That is plotting a histogram of
sample means for a large number of different samples.
The sampling distribution of a statistic becomes closer to the normal distribution as the size of
the sample increases.
NOTE: THIS ONLY APPLIES WHEN SAMPLES ARE TAKEN INDEPENDENTLY AND RANDOMLY.

POISSON DISTRIBUTION
A Poisson process is a one in which the average number of events in a period is known,
but the time or space between events is random.
It is described by the parameter lambda which is the number of events per interval of time.
The expected value is lambda, so this changes the shape of the distribution.
*/
/*
HYPOTHESIS TESTING
It is a group of methods and techniques to compare populations.
This is very common in applications and decission making.
The usual approach is this:
  * Start assuming no difference exists (null hypothesis).
  * Then state the alternative hypothesis.
First define target populations. 
Then develop null and alternative hypothesis.
Collect sample data.
Perform statistical tests on sample data.
Draw conclusions about the population.

INDEPENDENCE
When describing the results of an hypothesis test the terms dependent or independent variable arises.
If the population measures change across some other variable, then the measure is dependent on that variable.

EXPERIMENTS
They are a subset of hypothesis testing. They generally aim to answer the question:
What is the effect of the treatment on the response?
Where the treatment is taken as the independent variable and the reponse as the dependent.
CONTROLLED EXPERIMENTS
These are the ones in which participants are randomly assigned to either treatment or control group.
Groups should be comparable, theg goal is to eliminate all sorts of bias.
This can be done by randomization: randomized controlled trial.

Another technique is blinding, where participants do not know if they are in the treatment or the control group.
Double blinding is where the person administering the treatmnet also does not know
which participant has the treatment or a placebo.

The randomized controlled trial can have multiple treatment groups.
A/B testing only splits participants into two.

CORRELATION
Is a way to measure relationships. Pearson correlation coefficient 
quantifies the strength of a relationship and is a number between -1 and +1
The sign indicates the direction of the relationship and the magnitude the strength of it.
It only measures linear relationships (proportionate changes betwwen dependent and independent variables.
Confounding variables. Correlation does not equal causation. 
Sometimes a relationship can be explained by a confounding variable, that relates both of them.

INTERPRETING HYPOTHESIS TEST RESULTS
When collecting data for comparing two goups we may find differences
in the statistics for each group, hypothesis testing aims to answer if this differnce is not due to chance.
p-value
This is the probability of getting a larger difference than the observed assuming the null hypothesis.
Significance level.
This is a probability threshold for rejecting the null hypothesis.
This is known as alpha. A typical value is 0.05.

Type I Error. Rejecting the the null hypothesis when the hypothesis is true.
Type II Error. Accept the null hypothesis when it is false
*/
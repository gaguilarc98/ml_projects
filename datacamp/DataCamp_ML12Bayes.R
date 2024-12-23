####____BAYESIAN ANALYSIS____####
####____INTRODUCTION____####
#During Second World War, Alan Turing designed a machine to decrypt the Enigma machine
#from the nazis. One key of its success was the use of bayesian analysis.
#The enigma machine worked by flipping keys with three wheels.
#The allies could get encrypted messages and their purpose was: Given the message
#What was likely the confifuration of wheels that produced them.

#Bayes data analysis is the use of bayesian inference to learn from data.
#Bayesian inference was developed by Bayes and Laplace in the XVIII century. It makes
#full use of probability theory to draw conclusions and learn from data.

#Probability is a statement about the certainty or uncertainty of different outcomes.
#It does not only cover yes/no statements but it also refers to uncertainty in continuous
#or numeric outcomes such as the number of vehicles passing a street.

#PROPORTION OF SUCCESSES
#We could build a model that works with a prior probability distribution
#updating when data is available and get a posterior proability distribution.

#Summarizing the results with a density plot is useful, but another way to summarize
#it further is with a vector of samples where each portion is present proportionally
#to what the posterior probability dictates.

####____HOW BAYESIAN ANALYSIS WORKS____####
#Bayesian anlysis requires:
# 1. Data. Collection of characteristics of individuals with similar features
# 2. Generative model. Is any kind of machinery (computational, logical or mathematical)
#    that can be fed fixed  parameters to simulate data.
# 3. Priors

#Example. We want to simulate the proportion of cured patients. We model by saying that
#there is an underlying proportion of success. And then we say that the wether a patient
#is cured or not depends only on that proportion and nothing else.
prop_success <- 0.15
n_patients <- 15
data <- c()
for (i in 1:n_patients) {
  data[i] <- runif(1,0,1)<prop_success
}
data <- as.numeric(data)
data
data2 <- rbinom(15,1,prop_success)
data2
hist(data2)
#From this  point we know how to simulate or generate new data once we are sure what
#are the actual parameters. However in data science the situation is mostly the opposite.
#However we know by now how a generative model works and how data is generated from this.

#But we need a prior that ideally should represent our uncertainty.
#In this way the parameters also become random variables whose value we are uncertain.
#Suppose we learn that the parameter must lie between 0 and 0.2, but we are uncertain
#about how to allocate that probability, then we propose a uniform distribution.
n_samples <- 10000
n_patients <- 100
prop_success <- runif(n_samples, 0, 0.2)
n_cured <- rbinom(n_samples, size=n_patients, prob = prop_success)
hist(prop_success)
hist(n_cured)

#if we construct a data.frame such as the following:
prior <- data.frame(prop_success, n_cured)
#We get a joint prior probability distribution of prob of success and number of cured patients
#from a sample of 100.

#It is better to see this as a scatter plot
library(ggplot2)
ggplot(prior, aes(x=n_cured, y=prop_success)) +
  geom_point(color="blue",alpha=0.25)+
  theme_light()

#Based on that plot we could CONDITION in the x axis to get a credible interval
#For the actual prop_success parameter. OR we could CONDITION on the y axis to get
#A prediction for the number of cured patients (of 100) given a certain prop_success.

#When we condition on data, we get a new posterior probability of the prop_success
#that can be used as the PRIOR for upcoming experiments.
####____WHY USING BAYESIAN ANALYSIS____####
#The main reason is that is a very flexible method for a generate model and to learn
#from data.

#Apart from that you can include expert opinion, make comparisons between groups and use it
#to make decisions. Furthermore we can change the model with little effort.

#For example if we ask an expert on the actual cure rate of the treatment they could say
#sth. like it mostly cures 8% of patients but on some groups it could cure as high as 15% of
#the patients and in others it could cure as low as 2% of them.
#To tweak this info in the prior we could adjust a KNOWN DISTRIBUTION SUCH AS a BETA
#to match the expert description.
prop_success <- rbeta(n_samples, shape1 = 10, shape2=95)
hist(prop_success)
n_cured2 <- rbinom(n=n_samples, size = n_patients, prob = prop_success)
prior2 <- data.frame(n_cured2, prop_success)
ggplot(prior2, aes(x=n_cured2, y=prop_success)) +
  geom_point(color="blue",alpha=0.25)+
  theme_light()
#Another reason to use bayesian inference is that it is easy to make comparisons
#between groups. Especially if the samples from groups are in the form of long
#vectors of samples.

#We could model the difference in outcomes for both generative models (from both groups)

#Posterior mean or median are not the same as data mean or median. One comes from an
#updated probability distribution, the other comes from data only.

#3rd reason to use bayes
#Decision analysis simply post-processes the updated data to calculate a quantity
#that is much closer to what we use for makin decisions (the qunatities we care about)

#4th reason to use bayes is to change the model.
#For example if we change the success into succes per unit of time we switch the ditribution
#from a binomial to a poisson.

rpois(500, 3)
####____BAYESIAN INFERENCE____####

n_ads_shown <- 100
n_visitors <- seq(0,100, by =1)
proportion_clicks <- seq(0,1, by =0.01)
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
proportion_clicks 
n_visitors

####____BAYESIAN ANALYSIS WITH NORMAL DISTRIBUTION____####
install.packages("BEST")
library(BEST)
iq <- c(40,38,50,60,39,30,55,58,62,45,48)
fit <- BESTmcmc(iq)
# The IQ of zombies on a regular diet and a brain based diet.
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)

# Calculate the mean difference in IQ between the two groups
mean(iq_brains) - mean(iq_regular)

# Fit the BEST model to the data from both groups
library(BEST)
best_posterior <- BESTmcmc(iq_brains, iq_regular)

#Computational methods
#Rejection sampling
#Grid approximation
#MCMC with BEST



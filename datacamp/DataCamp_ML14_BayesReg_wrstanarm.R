####____BAYESIAN REGRESSION WITH rstanarm____####
install.packages("rstanarm")
library(rstanarm)
library(broom)
####_____INTRODUCTION TO BAYESIAN REGRESSION____####
kidiq
model <- lm(kid_score~mom_iq, data= kidiq)
summary(model)
tidy(model)

#Using bayesian methods
#One key difference is that p-values from frequentist models make inferences about the
#probability of data not parameter values.
#Posterior distribution: combination of likelihood and prior, then:
# * Sample the posterior distribution
# * Summarize the sample
# * Use summary statistics to make inferences about parameter values
#Stan is a language for bayesian probability

stan_model <- stan_glm(kid_score~mom_iq, data=kidiq)

stan_model 
#stan_model outputs come from a sample of the posterior distribution.
#The estimates include:
# * sigma: Standard deviation of errors
# * meanPPD: mean posterior predictive samples.
# * log-posterior: analogous to likelihood.
stan_model$stan_summary

#Diagnostics: Rhat has to be less than 1.1 in order to make sure that the model has converged
summary(stan_model)  

#KEY DIFFERENCE:
#Frequentist: Data is random, parameters are fixed.
#Bayesian: Data is fixed, parameters are random.
#That can be seen in the interpretation of the p-values:
# Probability of test statistic, given null hypithesis.
#In contrast, 
#Bayesians calculate probability of parameter values, given the observed data.

#We assess a model by:
#Confidence intervals (frequentist): Probability that a range of values contains the true value
# but it does not tell us anything about any specific value.
#Credible intervals (bayesian): Tells us how probable values are. Probability that the true value
#is within a range.

confint(model, parm = "mom_iq",level=0.95)
#This tells us the prob that the range of 0.49 and 0.72 contains the true value
#But we are interested in the prob of the value being between 2 point not the prob of the 
#points capturing the value.
posterior_interval(stan_model, pars="mom_iq", prob = 0.95)
#The prob that the parameters falls between 0.49 and 0.72 is 0.95. With this approach one can
#make questions susch us What is the prob that the par is between 0.6 and 0.65.

####____CUSTOMIZING REGRESSION MODELS____####
#Sampling of the posterior in groups called chains.
#Each chain is an iteration.
#Trace plot capture the values of the parameter samples at each chain
#as a function of the number of iteration.
#Each chain may start at different locations but they should CONVERGE on the same area.
#then only use the iterarions at which the chains converge, the rest is discarded for WARM-UP

#By default rstanarm uses 4 chains with 2000 iterations each, but these setting can be tuned.
stan_model2 <- stan_glm(kid_score~mom_iq, data=kidiq, chains=3, iter=1000, warmup=500)

summary(stan_model2)

#PRIOR DISTRIBUTIONS
#They reflect our prior beliefs about the values of parameters. This is combined with the
#likelihood to get a posterior.

#PRIORS ARE MORE INFORMATIVE WHEN THEIR DISTRIBUTIONS ARE NARROW or we have less data.
#They work kind of an extra data point, in presence of more its influence decreases.
#Because they can have effects, it is usually better to use non-informative priors.

prior_summary(stan_model)
#The default is a normal for intercept with mean 0 and sd of 10.
#Other coefficients get a mean of 0 and sd of 2.5.
#Error sd uses an exp dist with a reate of 1.

#However scales the prior according to the data in order not to be too informative
no_scale <- stan_glm(kid_score~mom_iq, data=kidiq,
                     prior_intercept = normal(autoscale = F),
                     prior = normal(autoscale = F),
                     prior_aux = normal(autoscale = F))
prior_summary(no_scale)

#Intercept: 10*sd(y)
#Coefficients: (2.5/sd(x))*sd(y)

#USED SPECIFIED PRIORS
#Appart from the scale we can define a new prior distributions with the same arguments.

stan_model3 <- stan_glm(kid_score~mom_iq, data=kidiq,
                       prior_intercept = normal(location=0, scale=10, autoscale = F),
                       prior = normal(location=0, scale=2.5, autoscale = F),
                       prior_aux = exponential(rate=1, autoscale = F))

prior_summary(stan_model3)
summary(stan_model3)
#normal(), cauchy(), student_t(), exponential()
?priors

#We can also set priors to NULL but this is not recommended as there is usually sth to use.

#ALTERING THE ESTIMATION PROCESS
#____
#Divergent transitions. When the step sizes are too big.
control_model <- stan_glm(kid_score~mom_iq, data=kidiq,
                          control = list(adapt_delta=0.95))
#By increasing the adapt_delta we can decrease the step size.
#____
#Maximum tree depth reached
#control = list(max_treedepth=10) by default.

####____EVALUATING REGRESSION MODELS____####
#Using r-squared for model evaluation.
#It is a measured of how well the ind. variables were able to predict the dependent var.
#It can be interpreted as the ratio of the variance explained by the deterministic part
#of the model and so it is also called the coefficient of determination.

#R^2 = 1-SRes/STot
#In rstansarm the r-squared is not saved in the summary object as in lm().
#But we can calcualted it by hand:
#SRes = var(residuals(model))
#Stot = var(fitted(model))+var(residual(model))

#Posterior model checks
#POSTERIOR PREDICTED MODEL CHECK
library(dplyr)
library(tidyverse)
spread_draws(stan_model, `(Intercept)`, mom_iq) %>% 
  select(-.draw)
pred <- posterior_linpred(stan_model)
pred[1:10, 1:5]
summary(pred[,1:5])
summary(pred[1,1:5])

#However, doing this one observation at a time is very inefficient
#A measure that summarizes this is bayes_RS(model)
r2_posterior <- bayes_R2(stan_model)
quantile(r2_posterior, probs=c(0.025,0.975))
hist(r2_posterior)

#Also
pp_check(stan_model, "dens_overlay")
pp_check(stan_model, "stat")
pp_check(stan_model, "stat_2d")

library(loo)
#Bayesian model comparisons
#Loo stands for leave-one-out. This is a type of cross-validation.
loo1 <- loo(stan_model)
#It provided the estimate elpd_loo, the effective number of parameters p_loo
#and the loo estimate converted to a deviance scale: looic = -2*elpd_loo

#These values only make sense comparing to different models estimates:
loo(stan_model2)

stan_model4 <- stan_glm(kid_score~mom_iq*mom_hs, data=kidiq)
loo4 <- loo(stan_model4)

compare(loo1, loo4)#The difference in loo estimates and the sd of the differences
#A positive score favors the second model argument.
#The sd helps us decide if the dif is meaningful. If abs(elpd_dif)<sd, there is essentially
#no difference and choose the simpler model.

####_____PRESENTING RESULTS____####
#VISUALIZING BAYESIAN MODEL
#Make predictions using new data
#tidy(stan_model) #NO LONGER WORKS

#MAKING PREDICTIONS
post <- posterior_predict(stan_model)
post[1:20,1:5]

#FOR NEW DATA
predict_data <- data.frame(mom_iq=110, mom_hs=c(0,1))
predicciones <- posterior_predict(stan_model4,predict_data)

predicciones[1:10,]
summary(predicciones[,1])
summary(predicciones[,2])

#VISUALIZATIONS FOR THE PREDICTIONS OF NEW DATA
library(tidyr)
library(ggplot2)
new_predictions <- as.data.frame(predicciones)

colnames(predicciones) <- c("HS", "NO-HS")

plot_posterior <- gather(new_predictions, key="HS", value="HighSchool")
glimpse(plot_posterior)
ggplot(plot_posterior, aes(x=HighSchool))+
  facet_wrap(~HS, ncol=1)+
  geom_density()

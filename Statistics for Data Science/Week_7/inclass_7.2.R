# Statistics for Data Science
# Lecture 7
# In-class/take home assignments 7.2
# Logistic regressions/GLM

library(ggplot2)

# Load data
load("website.RData")

# Estimate parameters of a logit model
#
logit <- glm( active ~ income + age + region, data = website, family = binomial )
# or if you want a different base level for the factor variable
#logit <- glm( active ~ income + age + relevel(region,3), data = website, family = binomial )

summary(logit)
# Note omitted region1 in output

# Plot predicted probabilities
plot(website$age, predict(logit, type="response"))

# Using effects package to get ceteris paribus plots
library(effects)
effects <- predictorEffects(logit)
plot(effects)

# If you need to get the corresponding numbers for (for example) income
effects$income

# Consider transformations of variables
largelogit <- update(logit, ~ . + I(age^2) + log(age) + I(income^2) + log(income))
summary(largelogit)
# Note not much sign. anymore -> large correlation between variables

# Reduce using AIC
logit <- step(largelogit)
summary(logit)

# Create plot of estimated probabilities
effects <- predictorEffects(logit)
plot(effects)
summary(effects)

# Check some numbers from effects function
# Check region probabilities
predict(logit, newdata = data.frame(age=mean(website$age), region=factor(c(1,2,3)), income=mean(website$income)), type="response")
# compare against
effects$region

# Check age probabilities (more difficult given that the "average factor" is used)
linpred <- predict(logit, newdata=data.frame(age=15, region=factor(c(1,2,3)), income=mean(website$income)))
linpred
meanlinpred <- mean(linpred[1]*mean(website$region==1) + linpred[2]*mean(website$region==2) + linpred[3]*mean(website$region==3) )
exp(meanlinpred) / (1+exp(meanlinpred))
# last line gives same prob as in the age curve at age=15
effects$age

#################
#### Part II ####
#################
# Try probit model using same variables
probit <- glm( formula(logit), data = website, family = binomial(link = probit))
summary(probit)

deviance(logit)
deviance(probit)
anova(logit, probit)
# logit is slightly better (cannot use a formal test: no p-value given)

# plot differences in probs
ggplot(data = data.frame(
  Diff = predict(probit, type = "response") 
          - predict(logit, type = "response"), 
  Age = website$age), aes(x=Age, y=Diff)) + geom_point()
# differences are very small

# Coefficients are quite different! (but ratio is almost constant)
cbind(logit = coef(logit), probit = coef(probit), ratio=coef(probit)/coef(logit))

library(caret)
#install.packages("caret")
#install.packages("e1071")

# Investigate predictive performance
confusionMatrix( 
  as.factor(predict(logit, type = "response") > .50), 
  as.factor(website$active==1), positive = "TRUE")
# Predictive quality is not very good (does this mean the model is bad?)
# -> No! This may just be difficult to predict (is not uncommon with human behavior)

##################
#### PART III ####
##################
#install.packages("margins")
library(margins)
library(tidyr)

# Average Marginal Effects 
margeffect <- margins(logit)
margeffect
summary(margeffect)

# or graphically for all obs
plot(margeffect$income, 1000*margeffect$dydx_income)

# Calculate marginal effect for specific observation
altdata <- data.frame(income=2500, age=40, min=1, active=1, region=factor(1, c(1,2,3)))
margins(logit, data = altdata)
          
##
# Create plot of marginal effects of log(income) for fixed age
incomerng <- seq(1000, 4000)
Region1Testdata <- data.frame(age = mean(website$age), region = factor(1,c(1,2,3)), income = incomerng)
Region2Testdata <- data.frame(age = mean(website$age), region = factor(2,c(1,2,3)), income = incomerng)
Region3Testdata <- data.frame(age = mean(website$age), region = factor(3,c(1,2,3)), income = incomerng)

# Calculate marginal effects
margeffects = data.frame(Income = incomerng,
                         Region1 = 1000*margins(logit, data = Region1Testdata)$dydx_income,
                         Region2 = 1000*margins(logit, data = Region2Testdata)$dydx_income,
                         Region3 = 1000*margins(logit, data = Region3Testdata)$dydx_income)

ggplot(data = gather(margeffects,"Region","prob", Region1:Region3), aes(x = Income, y = prob, color = Region)) + geom_point() + ylab("Change in probability x1000")



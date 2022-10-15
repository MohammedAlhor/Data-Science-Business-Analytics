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

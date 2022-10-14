# Statistics for Data Science
# Lecture 6
# In-class assignment 6.1
# Run backwards and forwards step regressions
# Compare with all subset regression
#------------------------------------------------------------

#install.packages("MASS")
#install.packages("leaps")
library(MASS)
library(car)

dataset <- as.data.frame(state.x77[, c("Murder", "Income", "Population", "Illiteracy", "Frost")])

# Step 1: run the full regression
fit <- lm(Murder ~ Income + Population + Illiteracy + Frost, data = dataset)
summary(fit)

# Step 2: do forward step regression
# First study the help
help(stepAIC)

fit1 <- lm(Murder ~ 1, data = dataset)
forresults <- stepAIC(fit1, scope = list(upper = ~ Income + Population + Illiteracy + Frost), 
                      direction = "forward")

# Record the best model selected by the forward method
# This line takes the model specification as 'code'
formodel <- forresults$call
formodel

# This line evaluates the 'code' of the model
formodel <- eval(formodel)
summary(formodel)

# Test model against full model
anova(formodel, fit)
# No significant difference (which is "good"), selected model is preferred

# Step 3: do the backwards step regression
backresults <- stepAIC(fit, direction = "backward")

# Record the best model selected by the backwards method
# This line takes the model specification as 'code'
backmodel <- backresults$call
backmodel
# This line evaluates the 'code' of the model
backmodel <- eval(backmodel)
summary(backmodel)
# We get the same model...

# Step 4: do the all subset regression
library(leaps)
allresults <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = dataset, nbest = 4)

# You may observe by eyes which model is the best
plot(allresults, scale = "adjr2")

# Get coefficients for best model (according to AdjR2 )
coef(allresults, which.max(summary(allresults)$adjr2))

# Alternatively, one can use BIC (something similar to AIC)
# Unfortunately, this function does not provide AIC
plot(allresults)

# Observe that the best is with "Population" and "Illiteracy"
# Conclusion: All three methods give you the overall best model

###############################################
# Extra: start with a larger set of variables #
###############################################
allresultsL <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost  + log(Income) + I(Income^2) + log(Population), data = dataset, nbest = 4)

plot(allresultsL, scale = "adjr2")
plot(allresultsL)
# On BIC the previous model still wins, on Adj-R2 we need to add income and income^2
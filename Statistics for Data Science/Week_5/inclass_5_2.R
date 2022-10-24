# Statistics for Data Science
# Lecture 5
# In-class assignment 5.2
# Regression exercise / Diagnostics

#------------------------------------------------------------
# requires packages car, lmtest and sandwich					      
# install.packages("car")			             
# install.packages("lmtest")
# install.packages("sandwich")

#------------------------------------------------------------

# Setup the data and run the basic regression

dataset <- as.data.frame(state.x77[, c("Murder","Population", "Illiteracy", "Income", "Frost")])

fit3 <- lm(Murder~Population+Illiteracy+Income+Frost, data=dataset)

# Test Normality
library(car)

qqPlot(fit3)

# More details about the outlier

dataset["Nevada", ]
fitted(fit3)["Nevada"]
fit1$residuals["Nevada"]

# Test independence
durbinWatsonTest(fit1)
# No autocorrelation detected, but
# does this test make sense for this type of data?
# -> Not really (data is sorted in alphabethical order)

# Test linearity...
fit2 <- lm(Murder~Population+Income, data=dataset)

# ...using component + residual plot
crPlots(fit2)
# Especially the impact of Income seems to be non-linear

# Check this with the RESET test
# H0: linear relation between x and y
# Ha: some nonlinearity
library(lmtest)
resettest(fit2, power=2)
?resettest
# p-value < 0.05 -> we reject linearity

# consider alternative model
fit3 <- lm(Murder~log(Population)+Income+I(Income^2), data=dataset)
crPlots(fit3)
resettest(fit3, power=2)
summary(fit3)
# Much better results!

#Part II

# Test homoscedasticity
fit2 <- lm(Murder~Population+Income, data=dataset)

ncvTest(fit2)
# We cannot reject H0 of homoskedasticity -> variances are constant

# run significance test with alternative standard errors
library(sandwich)
coeftest(fit2, vcov=vcovHC)
# vs original (wrong) results
coeftest(fit2)

# Test multicollinearity
fit1 <- lm(Murder~Population+Illiteracy+Income+Frost, data=dataset)

vif(fit1)
# Nothing to worry about here

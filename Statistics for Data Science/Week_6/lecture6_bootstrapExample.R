# Statistics for Data Science
# Lecture 6
# Bootstrap example

library(boot)

load('houseprice.RData')

# Run a regression
fit <- lm(price ~ lotsize, data=houseprice)
summary(fit)
coef(fit)
confint(fit)

# Goal: get bootstrap confidence interval for the price coef

# Step 1: program a function that gives this coefficient for a bootstrap sample

# Straightforward way using a simple function
regcoef <- function(data, indices){
  d=data[indices,]
  fit1 <- lm(price ~ lotsize, data=d)
  return(coef(fit1)[2])
}

# More fancy way: allow for a general model described by a formula eg "y ~ x1 + x2"
# The parameter coefindex allows to specify the parameter to select
regcoefFancy <- function(data, formula, coefindex, indices){
  d=data[indices,]
  fit1 <- lm(formula, data=d)
  return(coef(fit1)[coefindex])
}

bootresult <- boot(data = houseprice, statistic = regcoef, R = 1000)
# or
bootresultFancy <- boot(data = houseprice, statistic = regcoefFancy, formula=price~lotsize, coefindex = "lotsize", R = 1000)

# Show the results
print(bootresult)
print(bootresultFancy)  # results will differ due to simulation noise

plot(bootresult)

# Calculate the confidence intervals
bootci <- boot.ci(bootresult, conf = 0.95, type = c("perc", "bca"))
bootci


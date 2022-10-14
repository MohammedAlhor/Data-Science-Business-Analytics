# Statistics for Data Science
# Lecture 6
# In-class/take home assignment 6.4
# Bootstrapping ANCOVA

# Requires Package "boot"
# install.packages("boot")

# Part 1, bootstrap the coefficient in an ANCOVA analysis

load("houseprice.RData")

# Step 1: Create the factor for large medium and small house
houseprice$catbed[as.numeric(houseprice$bedrooms)<=2] <- "small"
houseprice$catbed[as.numeric(houseprice$bedrooms)==3] <- "medium"
houseprice$catbed[as.numeric(houseprice$bedrooms)>=4] <- "large"
houseprice$catbed <- as.factor(houseprice$catbed)

# Step 2: an experiment step to see how to obtain the coef
fit <- aov(price~catbed+lotsize, data=houseprice)
coef(fit)
coef(fit)["lotsize"]
# The last line shows the way to get the coefficient

# Step 3: program a function to extract this coefficient
# It has to be able to "talk" to the boot function
aovcoef <- function(data, indices){
  d <- data[indices,]
  fit <- aov(price~catbed+lotsize, data=d)
  return(coef(fit)["lotsize"])
}

# Step 4: Now, we can bootstrap!
library(boot)

bootresult <- boot(data = houseprice, statistic = aovcoef, R = 1000)
print(bootresult) # Show the results
plot(bootresult)  # Show the bootstrapped estimates

bootci <- boot.ci(bootresult, conf = 0.95, type = c("perc", "bca"))
bootci # Show the confidence band
confint(fit)

# Exercise 2, bootstrap the difference between large and medium house

# Back to Step 2, check how to obtain the difference
fit <- aov(price~catbed+lotsize, data=houseprice)
coef(fit)
-coef(fit)["catbedmedium"]

# Redo Step 3, program to get the difference
aovdiff <- function(data, indices){
  d <- data[indices,]
  fit <- aov(price~catbed+lotsize, data=d)
  return(-coef(fit)["catbedmedium"])
}

# Step 4: Now, we can bootstrap! As before, only except statistic

bootresult <- boot(data = houseprice, statistic = aovdiff, R = 1000)
print(bootresult) # Show the results
plot(bootresult)  # Show the bootstrapped estimates

bootci <- boot.ci(bootresult, conf = 0.95, type=c("perc", "bca"))
bootci # Show the confidence band

# Compare to standard confidence intervals
confint(fit)
# Bootstrap-based intervals are wider in this case
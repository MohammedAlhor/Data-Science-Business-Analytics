# Statistics for Data Science
# Lecture 2
# In-class assignment 2.3
# Confidence intervals

#Load data
load("houseprice.RData")
x <- houseprice$lotsize

# Calculate means
summary(x)
summary(x)["Mean"]
mean(x)

# Calculate standard error
std.err <- sqrt(var(x)/length(x))
alt.std.err <- sd(x)/sqrt(length(x))

std.err
alt.std.err

# Calculate confidence interval around mean 
alpha <- 0.05
interval <- mean(x) + qt(c(alpha/2, 1-alpha/2), length(x) - 1)*std.err
interval

# Confirm results
library(Rmisc)
res <- summarySE(data.frame(x), measurevar = "x")
res

# Check results using R
std.err-res["se"]
(interval[2]-mean(x)) - res["ci"]

qnorm(0.025)

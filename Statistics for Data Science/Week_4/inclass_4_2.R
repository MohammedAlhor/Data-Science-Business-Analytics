# Statistics for Data Science
# Lecture 4
# In-class assignment 4.2
# Regression exercise with non-linear models

#Load data
load("houseprice.RData")

# Perform OLS estimation
m <- lm(price ~ lotsize, data=houseprice)

summary(m)
coef(m)
fitted(m)
predict(m)
confint(m)
plot(houseprice$lotsize,houseprice$price)
abline(m,col="red")

# Obtain R2
summary(m)$r.squared
# R2 is not very high!

# Also useful to see what items are available
attributes(m)
attributes(summary(m))

# H0: lotsize no impact (see t-value)
summary(m)
cor.test(houseprice$lotsize, houseprice$price)
# Note that the t-test stat is exactly the same (this is not coincidental)

# Diagnostic plots
#plot(m)

# Consider alternative models
m2 <- lm(price ~ log(lotsize), data=houseprice)
summary(m2)$r.squared
# R2 is better!

m3 <- lm(log(price)~log(lotsize), data=houseprice)
summary(m3)$r.squared
# Do not compare this R2 to the earlier ones (this one relates to log(price), not price itself!)
summary(m3)
# If lotsize increases with 1%, the price increases by about 0.5%

# Create plot with all fitted lines
plot(houseprice$lotsize, houseprice$price)
legend <- c("linear","price~log(size)")
col <- c("blue","red")
points(houseprice$lotsize, fitted(m), col="blue")
points(houseprice$lotsize, fitted(m2), col="red")

predictm3naive <- exp(fitted(m3))
predictm3 <- exp(fitted(m3)+0.5*summary(m3)$sigma^2)  # Expected value of exp(normal) = exp(mu + 0.5 sigma^2)
points(houseprice$lotsize,predictm3naive,col="green")
points(houseprice$lotsize,predictm3,col="yellow")
legend = c(legend, "log~log naive", "log~log")
col = c(col, "green", "yellow")

# Show legend
legend("topright",legend=legend,col=col, lty=1)
  
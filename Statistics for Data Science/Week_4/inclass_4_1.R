# Statistics for Data Science
# Lecture 4
# In-class assignment 4.1
# Regression exercise

#Load data
load("houseprice.RData")

#Perform OLS estimation
m <- lm(price ~ lotsize, data=houseprice)

# Show some results
summary(m)
coef(m)
fitted(m)
predict(m)
confint(m)

# create scatter with fitted line
plot(houseprice$lotsize, houseprice$price)
abline(m, col = "red")
#or
library(ggplot2)
ggplot(houseprice, aes(y=price, x=lotsize)) + geom_point() + stat_smooth(method="lm", se=FALSE)

# Interpretation
## price increases with about 6.6 for every additional sq foot lotsize
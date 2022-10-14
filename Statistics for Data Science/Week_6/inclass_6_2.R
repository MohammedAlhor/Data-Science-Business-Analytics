# Statistics for Data Science
# Lecture 6
# In-class assignment 6.2
# ANOVA

library(gplots)

load('houseprice.RData')

# Transform variable
houseprice$catbed[as.numeric(houseprice$bedrooms)<=2] <- "small"
houseprice$catbed[as.numeric(houseprice$bedrooms)==3] <- "medium"
houseprice$catbed[as.numeric(houseprice$bedrooms)>=4] <- "large"
houseprice$catbed <- as.factor(houseprice$catbed)

# Run ANOVA
ass1 <- aov(price~catbed, data=houseprice)
summary(ass1)

plotmeans(houseprice$price ~ houseprice$catbed, xlab="Number of bedrooms", ylab="House price", main="Mean Plot with 95% CI")

coef(ass1)
# What do these coefficients mean?

# Confirm using regression
m <- lm(price~catbed, houseprice)
summary(m)

# Test direction of difference
TukeyHSD(ass1)
# or
pairwise.t.test(houseprice$price, houseprice$catbed, p.adj = "bonferroni")

plot(TukeyHSD(ass1))
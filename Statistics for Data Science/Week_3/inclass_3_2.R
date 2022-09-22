# Statistics for Data Science
# Lecture 3
# In-class assignment 3.2
# Comparing samples exercise
library(ggplot2)

#Load data
load("houseprice.RData")
withairco <- subset(houseprice, airco == 1)
withoutairco <- subset(houseprice, airco == 0)

# Plot both histograms
ggplot(houseprice, aes(x=price, fill=factor(airco))) + geom_histogram()

# Inspect summary statistics
summary(withoutairco$price)
summary(withairco$price)

var(withairco$price)/1000000
var(withoutairco$price)/1000000

# Perform tests
var.test(withoutairco$price, withairco$price)
#or
var.test(price ~ airco , houseprice)

# t-test for mean
t.test(withoutairco$price, withairco$price, var.equal = FALSE)
wilcox.test(withoutairco$price, withairco$price)
#or
t.test(price ~ airco, houseprice, var.equal = FALSE)
wilcox.test(price ~ airco, houseprice)

# Try with log transformation
ggplot(houseprice, aes(x=log(price), fill=factor(airco))) + geom_histogram() + labs(title = "Histogram of log prices")
# -> distributions become closer to the normal distribution

# Redo tests
var.test(log(price) ~ airco, houseprice)

t.test(log(price) ~ airco, houseprice, var.equal = TRUE)
t.test(log(price) ~ airco, houseprice, var.equal = FALSE)
wilcox.test(log(price) ~ airco, houseprice)

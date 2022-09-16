# Assigment 3.1
library(ggplot2)

truemean <- 0.05
var <- 1
n <- 100

data <- rnorm(n, mean =truemean, sd=sqrt(var))


t.test(data,mu=0)

power.t.test(n, delta=truemean -0, sd=sqrt(var), sig.level = 0.05, type='one.sample', strict=TRUE)
power.t.test(power=0.8,delta=truemean -0, sd=sqrt(var), sig.level = 0.05, type='one.sample', strict=TRUE)
?t.test()


?power.t.test


# Assignment 3.2
load("~/Documents/Data-Science-Business-Analytics/Data/houseprice.RData")
# first test for variance to determine which test to use to calculate the mean
houseprice$withairco <-  factor(houseprice$airco)
withairco <- houseprice[houseprice$airco == 1,]
withoutairco <- houseprice[houseprice$airco == 0,]

var.test(withoutairco$price, withairco$price)
var.test(price ~ airco, houseprice)
# p value quite small, F toets waarde is 0.56. null hypothese verworpen. Zijn dus niet hetzelfde. significant verschillend van elkaar
# t toets moet met varicance equal false 

t.test(withoutairco$price, withairco$price, var.equal=FALSE)
# we kunnen de null hypothese verwerpen, omdat p is kleiner dan 0.05
wilcox.test(withoutairco$price, withairco$price)
?var.test

#logtransformatie
# outliers minder impact op de variantieb bij het gebruik van logs

# dependant samples
# paired t test.
binom.test()





## TODO: doornemen verdelingen, beter begrijpen. One sample t test 



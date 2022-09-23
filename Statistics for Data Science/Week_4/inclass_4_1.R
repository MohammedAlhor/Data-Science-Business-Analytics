# import housing data
load("~/Documents/Data-Science-Business-Analytics/Data/houseprice.RData")
library(ggplot2)
# price and lotsize
plot(houseprice$price, houseprice$lotsize)

lm <- lm(houseprice$price ~ houseprice$lotsize)
lm <- lm(price ~ lotsize, data=houseprice)
#summary
summary(lm)
coef(lm)
fitted(lm)
predict(lm, data.frame(lotsize=(100)))
confint(lm)

#plotting
plot(houseprice$price, houseprice$lotsize)
abline(lm, col="red")

sct <- ggplot(houseprice, aes(x=lotsize, y=price)) + geom_point() + stat_smooth(method="lm", se=FALSE)
sct

+ geom_smooth(method='lm') 


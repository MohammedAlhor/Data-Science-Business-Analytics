load("~/Documents/Data-Science-Business-Analytics/Data/website.RData")
#
glm(active~income+age+region, family=binomial(link=probit), data = website) 
# because 
glm <- glm(active~log(income)+log(age)+region, family=binomial(link=probit), data = website) 
#income, age, region2, region3


library(effects)
plot(predictorEffects(glm))

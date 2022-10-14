load("~/Documents/Data-Science-Business-Analytics/Data/website.RData")
#

logit<-glm(active~income+age+region, family=binomial(link=probit), data = website) 
glm(active~income+age+relevel(region,3), family=binomial(link=probit), data = website) 
summary(logit)
# getallen zijn niet zo eenvoudig te interpreteren
# Tekens kun je wel interpreteren, min of plus.
# onderling kkun je ze wel vergelijken
# Regio niet significant maar het verschil tussen de regios is wel significant

# voorspelling
plot(website$age, predict(logit, type = "response"))


glm <- glm(active~log(income)+log(age)+region, family=binomial(link=probit), data = website) 
#income, age, region2, region3


library(effects)
plot(predictorEffects(glm))

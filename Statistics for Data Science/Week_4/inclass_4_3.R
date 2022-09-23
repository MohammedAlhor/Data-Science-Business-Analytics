# import housing data
dataset <- as.data.frame(state.x77)
library(ggplot2)
library(effects)

lm <- lm(Murder ~ Income + Population + Income:Population, data=dataset)

summary(dataset)
summary(lm)

plot(lm)


sct <- ggplot(dataset, aes(x=Murder, y=Population*Income)) + geom_point()
sct

m<- lm(Murder ~ Population*Income, data= dataset)
summary(m)
plot(effect("Population:Income", m), multiline = TRUE)

table(dataset$Income)

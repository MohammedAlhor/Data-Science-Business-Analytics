# Statistics for Data Science
# Lecture 4
# In-class assignment 4.3
# Interactions

#requires packages car and effects					     
#install.packages("car")			    
#install.packages("effects")			             
library(car)
library(effects)

dataset <- as.data.frame(state.x77)

NoInteraction <- lm(Murder ~ Income + Population , data=dataset)
summary(NoInteraction)
# Interpretation
# * An increase of 1000 dollars in per capita income decreases the murder rate by 1.9 murders/100000 (keeping population constant)
# * An increase of 1000 people in population increases the murder rate by 0.33 murders/100000 (keeping income constant)
# Question: does the impact of income depend on the population? (and vice versa)
# -> study interactions

ass3 <- lm(Murder ~ Income*Population , data=dataset)
summary(ass3)
# Note: insignificance of "Income" does NOT mean that income has no effect
plot(effect("Income:Population", ass3), multiline=TRUE)
# Income has an especially strong impact in states with high population

# Or if you want the plot "the other way around"
ass3 <- lm(Murder ~ Population*Income , data=dataset)
plot(effect("Population:Income", ass3), multiline=TRUE)
# For high income states the impact of population is negative (lower murder rate in high population states)
# For low income states the impact of population is positive (higher murder rate in high population states)
# But be careful: how many large + high income states are there?
dataset[(dataset$Population>10000) & (dataset$Income>5000),]

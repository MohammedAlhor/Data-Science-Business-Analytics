# Lesopdrachten
# 1 a

param2 <- 1

my_function <- function(param1)
{
  product <- param1*param2
  product
}

my_function(2)
# b
#Unused argument melding if param2 was in my global ArgVar

# c
#26
#13
#


#2
#a

my_function2 <- function(param1,param2)
{
    param1 <- 5
    
    max(param1,param2)
}

my_function2(2,3)

#b did it

#c Bestaande functie, geef em een andere naam.

#3
#a
mtcars %>% group_by(cyl) %>% summarise(group_count = n())
mtcars %>% group_by(carb) %>% summarise(group_count = n())
data("mtcars")
data(storms, package='dplyr')
data("starwars")
my_function_all <- function(df,group)
{
  {{df}} %>% group_by({{group}}) %>% summarise(group_count = n())
}
my_function_starwars(homeworld)
my_function_all(storms,year)


# 4
setwd("~/Documents/Data-Science-Business-Analytics/Data")
library(readr)
animal_species <- read_csv("animal_species.csv")

animal_species %>% 
  group_by(year) %>%
  summarise(average = mean(weight, na.rm = TRUE), 
            sd = sd(weight, na.rm = TRUE),
            q25 = quantile(weight, probs = 0.25, na.rm=TRUE),
            q75 = quantile(weight, probs = 0.75, na.rm=TRUE),
            missing = sum(!is.na(weight))
            )

get_group_stats <- function(df, group, ArgVar)
{
   {{df}} %>% 
    group_by({{group}}) %>%
    summarise(average = mean({{ArgVar}}, na.rm = TRUE), 
              sd = sd({{ArgVar}}, na.rm = TRUE),
              q25 = quantile({{ArgVar}}, probs = 0.25, na.rm=TRUE),
              q75 = quantile({{ArgVar}}, probs = 0.75, na.rm=TRUE),
              missing = sum(!is.na({{ArgVar}})))
}
get_group_stats(animal_species,1989, weight)
get_group_stats(animal_species,taxa, hindfoot_length)
ony<-get_group_stats(animal_species,genus, weight)

# lapply


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
#Unused argument melding if param2 was in my global variable

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
data("mtcars")
mtcars %>% group_by(cyl) %>% summarise(group_count = n())
mtcars %>% group_by(carb) %>% summarise(group_count = n())

my_function_mt <- function(group)
{
  mtcars %>% group_by({{group}}) %>% summarise(group_count = n())
}
my_function_mt(carb)

data("starwars")
my_function_starwars <- function(group)
{
  starwars %>% group_by({{group}}) %>% summarise(group_count = n())
}
my_function_starwars(homeworld)

data(storms, package='dplyr')

my_function_storms <- function(group)
{
  storms %>% group_by({{group}}) %>% summarise(group_count = n())
}

my_function_storms(year)


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

get_group_stats <- function(df, group, variable)
{
   {{df}} %>% 
    group_by({{group}}) %>%
    summarise(average = mean({{variable}}, na.rm = TRUE), 
              sd = sd({{variable}}, na.rm = TRUE),
              q25 = quantile({{variable}}, probs = 0.25, na.rm=TRUE),
              q75 = quantile({{variable}}, probs = 0.75, na.rm=TRUE),
              missing = sum(!is.na({{variable}})))
}

get_group_stats(animal_species,1989, weight)
get_group_stats(animal_species,taxa, hindfoot_length)
get_group_stats(animal_species,genus, weight)

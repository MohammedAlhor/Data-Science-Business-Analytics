# Inladen van de benodigde libraries
library(simmer)
library(simmer.plot)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readxl)
library(writexl)
library(fitdistrplus)
library(outliers)

# set seed
set.seed(42)

# 7. Now replace the statistical distributions in the simmer script with the empirical distributions. How do the different simulations compare?
env <- simmer("parcel depot")
env

parcel <- trajectory("parcel's path") %>%
  seize("express worker", 1) %>% # checking in of parcels by express worker
  timeout(function() rnorm(1, mean(df_final$check_time), sd(df_final$check_time))) %>% # empirical distributions
  release("express worker", 1) %>%
  
  seize("admin worker", 1) %>% # preparing the parcel for shipping by admin worker
  timeout(rnorm(1, mean(df_final$admin_time), sd(df_final$admin_time))) %>% # mean of 8 with sd of 2
  release("admin worker", 1)

env %>%
  add_resource("express worker", 1) %>%
  add_resource("admin worker", 2) %>%
  add_generator("parcel", parcel, function() rexp(1, 1/6))
  
# Dit is wat ik zelf heb geprobeerd, kunnen jullie me hier terugkoppeling over geven?  
# add_dataframe('arrivals', parcel, data =  df_final, col_time = 'Eind lossen', time = 'interarrival')
for (i in 1:100){
  env %>% run(until=110) # we run the simulation for 110 hours (10 working days)
}

env %>% get_mon_arrivals()
env %>% get_mon_resources()

summary(get_mon_resources(env))
plot(get_mon_resources(env))

env_per_user <- get_mon_arrivals(env, per_resource = TRUE)
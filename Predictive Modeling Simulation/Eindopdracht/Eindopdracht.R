# Inladen van de benodigde libraries
library(simmer)
library(simmer.plot)
library(dplyr)
library(tidyverse)
library(readxl)


set.seed(42)
# Set up the simulation environment
env <- simmer("parcel depot")
env

mu_1 = 5
sd_1 = 1
mu_2 = 8
sd_2 = 2
lambda=10
rate = 1/lambda # the rate is the 1 divided by the time between each parcel


parcel <- trajectory("parcel's path") %>%
  seize("express worker", 1) %>% # checking in of parcels by express worker
  timeout(function() rnorm(1, mu_1, sd_1)) %>% # mean of 5 with sd of 1
  release("express worker", 1) %>%
  
  seize("admin worker", 1) %>% # preparing the parcel for shipping by admin worker
  timeout(function() rnorm(1, mu_2, sd_2)) %>% # mean of 8 with sd of 2
  release("admin worker", 1)

env %>%
  add_resource("express worker", 1) %>%
  add_resource("admin worker", 2) %>%
  add_generator("parcel", parcel, function() rexp(1, rate))

env %>% run(until=6600) # we run the simulation for 6600 minutes (10 working days)
env %>% get_mon_arrivals()
env %>% get_mon_resources()

plot(get_mon_resources(env))

# 2 (a) The mean, and standard deviation of the checking time, admin time, wait time
#       before checking, wait time before admin, and total throughput time of a parcel.

env_per_user <- get_mon_arrivals(env, per_resource = TRUE)
# We keep dividing by 60 to get the output in hours
# Mean and standard deviation of checking time

# filter for only express workers
express_worker <- env_per_user %>% filter(resource == 'express worker')
#calculate the mean and sd:
mean(express_worker$activity_time)/60
sd(express_worker$activity_time)/60

# calculate the wait time for express worker and then the mean and sd
express_worker_wait <- express_worker$end_time - express_worker$start_time - express_worker$activity_time
mean(express_worker_wait)/60
sd(express_worker_wait)/60



# filter for only admin workers
admin_worker <- env_per_user %>% filter(resource == 'admin worker')

#calculate the mean and sd:
mean(admin_worker$activity_time)/60
sd(admin_worker$activity_time)/60

# calculate the wait time for admin workers and then the mean and sd
admin_worker_wait <- admin_worker$end_time - admin_worker$start_time - admin_worker$activity_time
mean(express_worker_wait)/60
sd(express_worker_wait)/60


# we calculate the total throughput of a parcel by first taking the end time of administration and subtracting the start time of checking.
total_throughput <- env_per_user %>% group_by(name) %>% 
  summarise(min_start_time = min(start_time), max_end_time = max(end_time)) %>%
  mutate(throughput = max_end_time - min_start_time)

mean(total_throughput$throughput)/60
sd(total_throughput$throughput)/60


# 2 (b) A density histogram of the total throughput, checking time, admin time, wait time before checking and wait time before admin in the simulation.
# Histogram of total throughput
hist(total_throughput$throughput/60)
# Histogram of checking time
hist(express_worker$activity_time/60)
# Histogram of admin time
hist(admin_worker$activity_time/60)
# Histogram of wait time before checking
hist(express_worker_wait/60)
# Histogram of wait time before admin
hist(admin_worker_wait/60)


# 2 (c) The proportion of parcels sent out in time. Please also state the simulated amount of parcels and the runtime.

shipping_time <- 2*11*60 # parcels should be shipped within 2 days, 11 working hours in a day, 60 minutes in an hour.
# We calculated the total throughput of parcels in question 2a, lets use this to determine the percentage that shipped in time.
shipped_in_time <- total_throughput %>% filter(throughput<= shipping_time)
shipped_in_time

# now let's see how many parcels were simulated
nrow(total_throughput)

# To calculate the amount of parcels shipped in time we do the following
nrow(shipped_in_time)/nrow(total_throughput)*100

# total runtime is as follows

sum(total_throughput$throughput)/60



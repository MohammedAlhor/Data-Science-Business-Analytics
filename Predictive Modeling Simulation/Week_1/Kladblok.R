hist(rnorm(100), breaks = 25)
library(simmer)

# simmer
# Objecten: ATM Voorbeeld zijn dit de klanten, worden ook wel entities genoemd.
# Resources: servers, computers
# Path: sequence of resources entities hav eto visit
# Monitor: keeps track of events
# Als we deze simuleren hebbne we het model beschreven


customer <- trajectory("customer")



library(simmer)
library(simmer.plot)
runtime <- 54
set.seed(123)

env <- simmer("atm case") #the name of the simmer simulation
env #the definition of this simmer simulation

customer <- trajectory("customer") %>%  #entity: customer
seize("ATM", 1) %>%  # the resource
timeout(function() rexp(1,rate = 0.2)) %>%  # how long is resource used
release("ATM", 1) 

env %>%
add_resource("ATM", 1) %>%  #one ATM is defined
add_generator("customer", customer, function() rexp(1, rate=0.110)) 
# this generates each time one customer with interval exp(15) distributed

env %>% run(until=runtime) # how long does the simulation run

env %>% get_mon_arrivals()  #gives information on the entities
env %>% get_mon_resources() #gives information on the resources

result <-
  env %>% get_mon_arrivals() %>% # notice: it is a dataframe!
  transform(waiting_time = end_time - start_time - activity_time) 
z <- mean(result$waiting_time)
print(c("gem wachttijd:", z))
resources <- get_mon_resources(env)
plot(resources,metric="usage") 


library(simmer)
library(simmer.plot)

env <- simmer("outpatient clinic")
env

patient <- trajectory("patients' path") %>%
  seize("nurse", 1) %>%
  timeout(function() rnorm(1,15)) %>%
  release("nurse", 1) %>%
  
  seize("doctor", 1) %>%
  timeout(function() rnorm(1,20)) %>%
  release("doctor", 1) %>%
  
  seize("administration", 1) %>%
  timeout(function() rnorm(1,5))%>%
  release("administration", 1)

env %>%
  
  add_resource("nurse", 2) %>%
  add_resource("doctor", 3) %>%
  add_resource("administration", 2) %>%
  add_generator("patient", patient, function() rnorm(1,5,0.5))

env %>% run(until=540)
env %>% get_mon_arrivals()
env %>% get_mon_resources()

plot(get_mon_resources(env))

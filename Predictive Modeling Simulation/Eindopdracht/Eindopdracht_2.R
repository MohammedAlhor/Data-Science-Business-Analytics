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

set.seed(42)
# 3. Now load the actual data into R and transform the data into an appropriate format for analysis using the scripts we will provide. Clean for outliers.Determine the average processing 
# time for each phase (checking and admin) and determine the proportion of parcels sent out in time. Is the KPI target of 90% fulfilled?

# inladen dataset, de eerste rij wordt overgeslagen en de missende waarden worden uit het dataframe gefilterd.
df <- read_excel(path = '~/Data-Science-Business-Analytics/Data/parcel processing data clean-crossed pakketjes.xlsx', 
                 sheet = 'Data', 
                 skip = 1) %>%
                 drop_na()


# Inladen functies 
source("./compute_working_hours.R")
# In dit onderdeel berekenen we de duratie van verschillende activiteiten mbv de working hours functie
data <- df %>% 
  mutate(check_time = working_hours(`Aangepast Begin checken`, `Eind checken`, saturday = FALSE),
         admin_time = working_hours(`Begin admin`, `Eind admin`, saturday = FALSE),
         total_throughput = working_hours(`Eind lossen`, `Eind admin`, saturday = FALSE))

summary(data$admin_time)
summary(data$check_time)

# Check for outliers
# Checking time
outlier_values_1 <- boxplot.stats(data$check_time)$out  # outlier values.
boxplot(data$check_time, main="Pressure Height", boxwex=0.2)
mtext(paste("Outliers: ", paste(outlier_values_1, collapse=", ")), cex=0.6)
outlier(data$check_time)
check_time_outliers <-scores(data$check_time, type="z", prob=0.95) # logicals with cut-off
# Admin time
outlier_values_2 <- boxplot.stats(data$admin_time)$out  # outlier values.
boxplot(data$admin_time, main="Pressure Height", boxwex=0.2)
mtext(paste("Outliers: ", paste(outlier_values_2, collapse=", ")), cex=0.6)
outlier(data$admin_time)
admin_time_outliers <-scores(data$admin_time, type="z", prob=0.95) # logicals with cut-off

# In deze stap wordt de mean berekend van beide activiteiten, deze wordt later gebruikt om te imputen in de data
mean_check_time <- mean(data$check_time[data$check_time > 0])
mean_admin_time <- mean(data$admin_time[data$admin_time > 0])


# In dit onderdeel doen we wat data manipulatie, de waarden die kleiner of gelijk aan nul zijn worden vervangen door gemiddelden. Hetzelfde geldt voor de outliers.
df_final <- data %>%
  mutate(check_time = ifelse(check_time_outliers,mean_check_time, check_time),
         admin_time = ifelse(admin_time_outliers,mean_admin_time, admin_time)) %>%
  mutate(check_time    = ifelse(check_time <= 0, mean_check_time, check_time),
         admin_time    = ifelse(admin_time <= 0, mean_admin_time, admin_time),
         total_throughput = check_time + admin_time)

# De uiteindelijke gemiddelden worden als volgt berekend.
mean(df_final$check_time)
mean(df_final$admin_time)

shipping_time <- 2*11*60 # parcels should be shipped within 2 days, 11 working hours in a day, 60 minutes in an hour.
shipped_in_time <- df_final %>% filter(total_throughput<= shipping_time)
nrow(df_final)
# To calculate the amount of parcels shipped in time we do the following
in_time <- nrow(shipped_in_time)/nrow(df_final)*100
in_time

# 4. (2 points) Determine the utilisation (= fraction of time a worker is busy) of the express workers (between 07h00 and 18h00). Do the same for the admin workers.

# Om de fractie te berekenen dat een medewerker bezig is met een pakketje berekenen we eerst de totale werktijd. Dit is 6 dagen per week, 11 uur per dag. 
# De periode over de hele dataset is van 3 oktober 2016 tot 30 november 2016. Dit zijn 59 werkdagen. 

total_working_hours <- 59*11

# De totale tijd dat iedere werknemer bezig is hebben we al berekend. Laten we eerst de berekening doen voor express workers.

utilization_check <- sum(df_final$check_time)/total_working_hours

# Er is maar 1 express worker, maar er zijn 2 admin workers. De beschikbare werktijd moeten we dus verdubbelen.

utilization_admin <- sum(df_final$admin_time)/(total_working_hours*2)


# 5. (3 points) Determine for each phase (checking and admin) the best fitting distribution (including the fitted parameters) and explain your choice.

# Laten we eerst kijken naar wat histogrammen

hist(df_final$check_time, breaks = 50)
hist(df_final$admin_time, breaks = 50)

plotdist(df_final$check_time, histo = TRUE, demp=TRUE)
plotdist(df_final$admin_time, histo = TRUE, demp=TRUE)

# Kijkende naar deze histogrammen, de empirical density en de CDF kunnen we wel stellen dat beide activiteiten niet normaal zijn verdeeld. 
# Laten we een aantal verdelingen proberen en op zoek gaan naar degene met de beste fit. We doen dit eerst voor checking time.

# CHECKING TIME
# Fit some other distributions
fit_n  <-  fitdist(df_final$check_time, "norm")
fit_w  <- fitdist(df_final$check_time,  "weibull")
fit_g  <- fitdist(df_final$check_time,  "gamma")
fit_ln <- fitdist(df_final$check_time,  "lnorm")

plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
par(mfrow=c(2,2))
denscomp(list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), legendtext = plot.legend)


descdist(df_final$check_time)
descdist(df_final$check_time, boot = 1000)

print(c("AIC normal =",fit_n$aic))
print(c("AIC weibull =",fit_w$aic))
print(c("AIC gamma =",fit_g$aic))
print(c("AIC lnorm =",fit_ln$aic))

# ADMIN TIME
# Fit some other distributions
fit_nn  <-  fitdist(df_final$admin_time, "norm")
fit_ww  <- fitdist(df_final$admin_time,  "weibull")
fit_gg  <- fitdist(df_final$admin_time,  "gamma")
fit_lnn <- fitdist(df_final$admin_time,  "lnorm")

plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
par(mfrow=c(2,2))
denscomp(list(fit_nn, fit_ww, fit_gg, fit_lnn), legendtext = plot.legend)
cdfcomp (list(fit_nn, fit_ww, fit_gg, fit_lnn), legendtext = plot.legend)
qqcomp  (list(fit_nn, fit_ww, fit_gg, fit_lnn), legendtext = plot.legend)
ppcomp  (list(fit_nn, fit_ww, fit_gg, fit_lnn), legendtext = plot.legend)


descdist(df_final$admin_time)
descdist(df_final$admin_time, boot = 1000)

print(c("AIC normal =",fit_nn$aic))
print(c("AIC weibull =",fit_ww$aic))
print(c("AIC gamma =",fit_gg$aic))
print(c("AIC lnorm =",fit_lnn$aic))


# 6. Replace the statistical distributions in the simmer script with the fitted distributions from the previous question. For the arrivals use the exact ‘Eind Lossen’ time stamps.
# Run the simulation for 10 working days, and repeat 100 times. Recompute the performance measures from question 1.


env <- simmer("parcel depot")
env
lambda = 6
rate = 1/lambda
parcel <- trajectory("parcel's path") %>%
  seize("express worker", 1) %>% # checking in of parcels by express worker
  timeout(function() rlnorm(1, fit_ln$estimate, fit_ln$sd)) %>% # estimate and sd from fit_ln we previously estimated
  release("express worker", 1) %>%
  
  seize("admin worker", 1) %>% # preparing the parcel for shipping by admin worker
  timeout(function() rlnorm(1, fit_lnn$estimate, fit_lnn$sd)) %>% # mean of 8 with sd of 2
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
# We keep dividing by 60 to get the output in hours
# Mean and standard deviation of checking time

# filter for only express workers
express_worker <- env_per_user %>% filter(resource == 'express worker')
#calculate the mean and sd:
print(c("Mean activity time =",mean(express_worker$activity_time)))
print(c("SD activity time =",sd(express_worker$activity_time)))


# calculate the wait time for express worker and then the mean and sd
express_worker_wait <- express_worker$end_time - express_worker$start_time - express_worker$activity_time
mean(express_worker_wait)

print(c("Mean wait time =",mean(express_worker_wait)))
print(c("SD wait time =",sd(express_worker_wait)))

# filter for only admin workers
admin_worker <- env_per_user %>% filter(resource == 'admin worker')

#calculate the mean and sd:
print(c("Mean activity time =",mean(admin_worker$activity_time)))
print(c("SD activity time =",sd(admin_worker$activity_time)))

# calculate the wait time for admin workers and then the mean and sd
admin_worker_wait <- admin_worker$end_time - admin_worker$start_time - admin_worker$activity_time
print(c("Mean wait time =",mean(express_worker_wait)))
print(c("SD wait time =",sd(express_worker_wait)))

# we calculate the total throughput of a parcel by first taking the end time of administration and subtracting the start time of checking.
total_throughput <- env_per_user %>% group_by(name) %>% 
  summarise(min_start_time = min(start_time), max_end_time = max(end_time)) %>%
  mutate(throughput = max_end_time - min_start_time)

print(c("Mean total throughput =",mean(total_throughput$throughput)))
print(c("SD total throughput  =",sd(total_throughput$throughput)))


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
# We keep dividing by 60 to get the output in hours
# Mean and standard deviation of checking time

# filter for only express workers
express_worker <- env_per_user %>% filter(resource == 'express worker')
#calculate the mean and sd:
print(c("Mean activity time =",mean(express_worker$activity_time)))
print(c("SD activity time =",sd(express_worker$activity_time)))


# calculate the wait time for express worker and then the mean and sd
express_worker_wait <- express_worker$end_time - express_worker$start_time - express_worker$activity_time

print(c("Mean wait time =",mean(express_worker_wait)))
print(c("SD wait time =",sd(express_worker_wait)))

# filter for only admin workers
admin_worker <- env_per_user %>% filter(resource == 'admin worker')

#calculate the mean and sd:
print(c("Mean activity time =",mean(admin_worker$activity_time)))
print(c("SD activity time =",sd(admin_worker$activity_time)))

# calculate the wait time for admin workers and then the mean and sd
admin_worker_wait <- admin_worker$end_time - admin_worker$start_time - admin_worker$activity_time
print(c("Mean wait time =",mean(express_worker_wait)))
print(c("SD wait time =",sd(express_worker_wait)))

# we calculate the total throughput of a parcel by first taking the end time of administration and subtracting the start time of checking.
total_throughput <- env_per_user %>% group_by(name) %>% 
  summarise(min_start_time = min(start_time), max_end_time = max(end_time)) %>%
  mutate(throughput = max_end_time - min_start_time)

print(c("Mean total throughput =",mean(total_throughput$throughput)))
print(c("SD total throughput  =",sd(total_throughput$throughput)))
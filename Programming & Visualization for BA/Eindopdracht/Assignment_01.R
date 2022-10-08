# Erasmus Q-Intelligence ----


## Midterm assignment  ----
# covering topics 01-05


### Instructions ----
# Please answer the questions below. Your answer file should contain the R code 
# to produce all results using consistent style, as well as brief discussions of 
# your results (using #). Hand in this R script.

# Your answers should be concise but complete. Unless stated otherwise, the (sub)questions
# build on each other so that you should use the result from the previous (sub)question 
# as starting point for the current one.

# Submit only your R script via Canvas. The deadline is October 9, 2021 at 23:59.


### Data description ----
# Consider the OECD data on regional inequality in the file 'oecd_data.csv'. This 
# data consists of economic indicators measured yearly for several regions across 
# many different countries. The dataset consists of the following variables:
#   reg_id          Abbreviated code of the region
#   region          Name of the region
#   year            Year of observation
#   country_code    Abbreviated code of the country
#   pc_real_ppp     Regional GDP in US Dollars, per head of population
#   per             Number of workers
#   real_ppp        Regional GDP in Million of US Dollars

# All prices are adjusted to 2010 levels and are therefore comparable. Additional 
# information is supplied in the file 'oecd_names.csv', which contains information 
# on different coding systems often used to abbreviate country names.


### Questions ----


#### Q1 - introduction and descriptives ----
# a)
# Load the tidyverse-package.
library(tidyverse)
library(dplyr)
library(ggplot2)
# b)
# Read the data from 'oecd_data.csv'.
# Set our working directory
setwd("~/Data-Science-Business-Analytics/Data")
# Load our csv file with headers
oecd_data <- read.csv("oecd_data.csv", header = TRUE)

# c)
# Get a first view on the data by getting the dimensions, show the first 5 rows of
# the data.frame and giving the summary. 

# To get the dimensions of a dataset we use the dim() function:
dim(oecd_data)
# Dimensions are 15168 rows, 7 columns

# To show the first 5 rows we use the head() function with n = 5
head(oecd_data,n=5)

# To get summary of the data we use the summary() function
summary(oecd_data)

# d)
# Use the standard plot-function from R (not ggplot()) that get a first visual view
# on the data.
# To get a plot of the whole dataframe we use the plot function and input the dataframe
plot(oecd_data)

# e)
# How many observations are there by country, by year? Show it in a table.

# To get the amount of observations by country and year we can use the table() function
table(oecd_data$country_code)
#   DE   ES   FR   IT   KR   SE   UK   US 
#   6432 944  1616 1760 272  336  2941 867 

table(oecd_data$year)
# 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
# 933  933  933  933  933  933  933  933  933  933  933  933  933  933  933  933  240 


#### Q2 - dplyr-preparations + first visualizations ----
# a)
# To get to our first visualizations, filter only the observations for UK.

# We can do this with indexing and filter in the 'row index':
oecd_uk <- oecd_data[oecd_data$country_code=='UK',]
# Or we can use the filter function from the dplyr package:
oecd_uk <- oecd_data %>%
  filter(oecd_data$country_code == 'UK')

# b)
# Group the observations in the dataset from Q2a) by year and get the minimum and
# maximum of pc_real_ppp in the UK.

# We use the group_by function to group the data by year and then we summarise using the summarise function (dplyr package)
uk_grouped <- oecd_uk %>% 
              group_by(year) %>%
              summarise(minimum = min(pc_real_ppp, na.rm=TRUE), maximum = max(pc_real_ppp, na.rm=TRUE))
uk_grouped
# c)
# Show in a time series plot the minimum and maximum of pc_real_ppp in the UK over 
# time.
library(ggplot2)
library(reshape2)
# Reshaping the dataframe
uk_grouped_m <- melt(uk_grouped,id.vars="year")

# Plot using ggplot
ggplot(uk_grouped_m, aes(x=year, y=value, color=variable)) + 
  geom_line() +
  labs(x = "Year",
       y = "GDP in US Dollars",
       title = "Maximum & Minimum GDP UK over time") 
                             

#### Q3 - data wrangling ----
# a)
# Back to our original dataset, loaded in Q1b). Get for each country in 2015 the 
# name of the region with the largest pc_real_ppp.

oecd_region <- oecd_data %>%  
               group_by(country_code, year) %>%
               filter(pc_real_ppp == max(pc_real_ppp), year==2015) %>%
               arrange(desc(pc_real_ppp)) %>%
               select(country_code, region, pc_real_ppp)
oecd_region
               
# b) 
# (Again use the dataset loaded in Q1b) We need to scale the data such that countries 
# are comparable. Mutate pc_real_ppp such that it is relative to the countries average 
# by year. You thus need to find the average over the observations of pc_real_ppp 
# grouped by country_code and by year.

oecd_scaled <- oecd_data %>% 
               group_by(country_code, year) %>%
               mutate(average_by_country_year = mean(pc_real_ppp, na.rm = TRUE), 
                     pc_real_ppp = pc_real_ppp/average_by_country_year) %>%
               select(country_code, year, region, pc_real_ppp)
oecd_scaled
  
# c)
# Repeat Q2b) over the dataset created in Q3b), but now having the minimum and maximum 
# for each year, for each country.

oecd_scaled_grouped <- oecd_scaled %>%
                       group_by(year, country_code) %>%
                       summarise(minimum = min(pc_real_ppp, na.rm=TRUE), maximum = max(pc_real_ppp, na.rm=TRUE)) %>%
                       arrange(country_code, year)

oecd_scaled_grouped


# c)
# Read the data from 'oecd_names.csv'.
# load the data using the read.csv function. Data has headers.
oecd_names <- read.csv("oecd_names.csv", header = TRUE)

# d)
# Join the oecd_names and the data.frame from Q3c) making sure all observations of 
# the latter data.frame are kept.

oecd_join <- oecd_scaled_grouped %>%
             left_join(oecd_names, by = c('country_code' = 'oecd.imp.code'))
oecd_join

# e) Repeat Q2c) and show the minimum and maximum by country (Use 'country' instead
# of 'country_code'). Give each country its own color (not the default colors). 
# Let minimum and maximum have different line types. Update the visualization such
# to make it look nicer using the tools at hand (given to you in the lectures)


oecd_3e <- oecd_data %>%
              left_join(oecd_names, by = c('country_code' = 'oecd.imp.code')) %>%
              group_by(country, year) %>%
              summarise(minimum = min(pc_real_ppp, na.rm=TRUE), maximum = max(pc_real_ppp, na.rm=TRUE))
?left_join
oecd_3e_m <- melt(oecd_3e, id.vars = c('country','year')) 


colors <- c('red', 'green', 'blue', 'yellow', 'purple', 'pink', 'orange', 'black')

ggplot(oecd_3e_m, aes(x=year, y=value, color = country, linetype = variable)) + geom_line() + 
  scale_color_manual(values = colors) +
  labs(x = "Year",
       y = "GDP in US Dollars",
       title = "Maximum & Minimum GDP over time")


?ggplot
#### Q4 - wrap-up ----
# Great Britain is highly dominating this visualization, since they have a region
# (City of London) where pc_real_ppp is relatively high. The reason is that this 
# region only has low capita but many businesses. It might therefore be better to 
# not use the real ppp per capita (pc_real_ppp), but the real_ppp per worker.

# Further, to be able to draw conclusions on the dispersion between regions, it 
# might be better not to only look at the minimum and maximum, which is highly 
# vulnerable to outliers. Better would be to look at, for example, the 5% and 95%
# quantiles.

# Reproduce the plot from Q3e) with
# 1. real_ppp per worker (which can be created using real_ppp and per from the data.frame
# 2. 5% en 95% quantiles instead of minimum and maximum
# Still remember to do the scaling as in Q3b)
# Can you do this without any intermediate results? (using Pipes %>%)
# Based on the first graph, people reacted: dispersion between regions grows!
# What can you conclude (differently)?


# The end ----
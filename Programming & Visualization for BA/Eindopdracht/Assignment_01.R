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


# b)
# Read the data from 'oecd_data.csv'.


# c)
# Get a first view on the data by getting the dimensions, show the first 5 rows of
# the data.frame and giving the summary. 


# d)
# Use the standard plot-function from R (not ggplot()) that get a first visual view
# on the data.


# e)
# How many observations are there by country, by year? Show it in a table.



#### Q2 - dplyr-preparations + first visualizations ----
# a)
# To get to our first visualizations, filter only the observations for UK.


# b)
# Group the observations in the dataset from Q2a) by year and get the minimum and
# maximum of pc_real_ppp in the UK.


# c)
# Show in a time series plot the minimum and maximum of pc_real_ppp in the UK over 
# time.



#### Q3 - data wrangling ----
# a)
# Back to our original dataset, loaded in Q1b). Get for each country in 2015 the 
# name of the region with the largest pc_real_ppp.


# b) 
# (Again use the dataset loaded in Q1b) We need to scale the data such that countries 
# are comparable. Mutate pc_real_ppp such that it is relative to the countries average 
# by year. You thus need to find the average over the observations of pc_real_ppp 
# grouped by country_code and by year.


# c)
# Repeat Q2b) over the dataset created in Q3b), but now having the minimum and maximum 
# for each year, for each country.


# c)
# Read the data from 'oecd_names.csv'.


# d)
# Join the oecd_names and the data.frame from Q3c) making sure all observations of 
# the latter data.frame are kept.


# e) Repeat Q2c) and show the minimum and maximum by country (Use 'country' instead
# of 'country_code'). Give each country its own color (not the default colors). 
# Let minimum and maximum have different line types. Update the visualization such
# to make it look nicer using the tools at hand (given to you in the lectures)



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
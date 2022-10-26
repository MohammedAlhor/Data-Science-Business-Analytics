# Erasmus Q-Intelligence ----


## Final assignment  ----
# covering topics 06-09


### Instructions ----
# Please answer the questions below. Your answer file should contain the R code
# to produce all results using consistent style, as well as brief discussions of
# your results (using #). Hand in this R script.

# Your answers should be concise but complete. Submit only your R script via Canvas.
# The deadline is October 30, 2021 at 23:59.

### Data description ----
library(tidyverse)


# Load your data using the folowing line of code (make sure the path is correct)
data <- readRDS("~/Documents/Data-Science-Business-Analytics/Data/data.Rds")

# Have a look at the data in RStudio
# The data contains a list of municipalities of The Netherlands
# Each municipality contains a list of postal codes in that municipality
# Each postal code contains
#  - "men" : a data.frame with age groups and the number of men in the age groups
#  - "women" : a data.frame with age groups and the number of women in the age groups
#  - "migration" : a named vector containing how many people have a migration background
#       (western, non-western)
#  - "households" : a named vector containing how many people are in three types
#       of households and the average size of these types

# HINT: use the debugger (browser()) in map()-functions to get to know what is happening in your functions.
# HINT: try your function first on a subset of the data, to see if it does what you want it to do
#   for example: data$Rotterdam$`3011`$men
#   or data$Rotterdam$`3011`


### Questions ----

data$Aalsmeer$`1431`[[1]]
#### Q1 ----
# Write a function 'get_postal_code_counts(data)' with output a data.frame with
#   columns 'municipality' and 'postal_code_count'.
# Each municipality has 1 row in this data.frame, containing
#   the name in column 'municipality' and
#   the number of postal codes in this municipality in the column 'postal_code_count'.
# HINT: use a map()-function to iterate through the listed data.
?map
data %>% map(mean)
get_postal_code_counts <- function(data) {
  
}
#### Q2 ----
# Write a function 'get_relative_migration_counts(data, non_western_only)' with
#   output a data.frame with columns 'municipality' and 'fraction_migration_background'.
# Each municipality has 1 row in this data.frame, containing
#   the name in column 'municipality' and
#   the fraction of inhabitants with a migration background.
# If non_western_only is TRUE (set this as the default-value of the function!),
#   the fraction is based on non_western_migration_background only.
#   If non_western_only is FALSE, the fraction is based on
#   western_migration_background + non_western_migration_background
# HINT: to get the number of inhabitants of a municipality, you will need to sum over
#   men and women, by postal code, and than sum over all postal codes
# HINT: use map(), functions and if-else statements. Try to make it readable with functions


#### Q3 ----
# Write a function 'get_household_info(data, youth_needed)' with output a character vector of postal codes
# The function returns the smallest set of postal codes where at least 'youth_needed' people of age under 20 live.
# 'youth_needed' as an argument to the function.
# EXAMPLE: if you need 7.500 youth, it would be enough to only target postal code 2134
# HINT: check the map_depth()-function to iterate over a different layer of the list, instead of over the first layer
# HINT: first create a data.frame with the number of youth in each postal code,
#   then arrange such that you always start with the largest postal code
#   then use a while loop to go on until 'youth_needed' is reached


#### Q4 ----
# Given circumstances (different teachers, late assignment availability),
# everyone gets 2 out of 2 points for Q4


# The end ----

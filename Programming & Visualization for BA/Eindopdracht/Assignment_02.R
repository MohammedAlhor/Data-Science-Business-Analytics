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
library(dplyr)


# Load your data using the folowing line of code (make sure the path is correct)
data <- readRDS("~/Data-Science-Business-Analytics/Data/data.Rds")

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


#### Q1 ----
# Write a function 'get_postal_code_counts(data)' with output a data.frame with
#   columns 'municipality' and 'postal_code_count'.
# Each municipality has 1 row in this data.frame, containing
#   the name in column 'municipality' and
#   the number of postal codes in this municipality in the column 'postal_code_count'.
# HINT: use a map()-function to iterate through the listed data.


get_postal_code_counts <- function(data) {
  df <- map_dfr(data, length) %>% # We use the map_dfr function to iterate through the list and return a dataframe
        t() %>% # We need to transpose the data for this question
        as.data.frame() %>% # list to dataframe
        rownames_to_column() %>% # municipalities are now the rownames, we want this to be a column
        rename(municipality=1, postal_code_count=2) # we use the rename function to rename the columns to the ones specified
  return(df)
}

get_postal_code_counts(data)


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


get_relative_migration_counts <- function(data, non_western_only = TRUE){
  
  
  num_of_inhab <- map_dfr(data, ~ map_dfr(.x, ~ map_dfr(.x[c("men", "women")], ~ sum(.x$count, na.rm = TRUE), 
                                           .id = 'sex'),# We use nested map functions to iterate through the list to get to the data we want (total number of inhabitants)
                                           .id = 'id'), 
                                           .id = 'municipality') %>% mutate(total_inhab = men+women) # We calculate the total number by adding men and women.
  
  migration <- map_dfr(data, ~ map_dfr(.x, ~map_dfr(.x[c("migration")], ~ .x[c('non_western_migration_background','western_migration_background')], # 
                                           .id='migration'), # Same concept as before, in this case we don't need to use sum, we just need the value.
                                           .id='id'),
                                           .id = 'municipality')

  if (non_western_only == TRUE){
    non_western <- num_of_inhab %>%
      left_join(migration, by = c('id' = 'id', 'municipality' = 'municipality')) %>% # We take the data frame for the number of inhabitants and join the dataframe with migration data
      mutate(frac_migr_bckgr_per_post = non_western_migration_background/total_inhab) %>% # First, we calculate the fraction of migration background per postal code
      group_by(municipality) %>% # We want to see the output at the municipality level, so we group the data
      summarise(fraction_migration_background = mean(frac_migr_bckgr_per_post, na.rm = TRUE)) %>% # We calculate the mean over this group
      select(municipality, fraction_migration_background) # We select the asked for variables
    return(non_western)
  }
  else{
    migration_all <- num_of_inhab %>%
      left_join(migration, by = c('id' = 'id', 'municipality' = 'municipality')) %>% # We take the data frame for the number of inhabitants and join the dataframe with migration data
      mutate(frac_migr_bckgr_per_post = (non_western_migration_background + western_migration_background)/
               total_inhab) %>% # First, we calculate the fraction of all migration background per postal code
      group_by(municipality) %>% # We want to see the output at the municipality level, so we group the data
      summarise(fraction_migration_background = mean(frac_migr_bckgr_per_post, na.rm = TRUE)) %>% # We calculate the mean over this group
      select(municipality, fraction_migration_background) # We select the asked for variables
    return(migration_all)
  }
}


get_relative_migration_counts(data, FALSE) # Default is set to return the fraction_migration_background for non_western migrants, if we set this to false we get this number for migrants of all backgrounds.


#### Q3 ----
# Write a function 'get_household_info(data, youth_needed)' with output a character vector of postal codes
# The function returns the smallest set of postal codes where at least 'youth_needed' people of age under 20 live.
# 'youth_needed' as an argument to the function.
# EXAMPLE: if you need 7.500 youth, it would be enough to only target postal code 2134
# HINT: check the map_depth()-function to iterate over a different layer of the list, instead of over the first layer
# HINT: first create a data.frame with the number of youth in each postal code,
#   then arrange such that you always start with the largest postal code
#   then use a while loop to go on until 'youth_needed' is reached

get_household_info <- function(data, youth_needed){
  
}

 num_of_inhab <- map_depth(data, 2, ~print(data))
?map_depth
  num_of_inhab <- map_dfr(data, ~ map_dfr(.x, ~ map_dfr(.x[c("men", "women")], ~ map_at("0 tot 5 jaar", ~ sum(.x$count, na.rm = TRUE),
                                                        .id='test'), 
                                           .id = 'sex'),# We use nested map functions to iterate through the list to get to the data we want (total number of inhabitants)
                                           .id = 'id'), 
                                           .id = 'municipality') %>% mutate(total_inhab = men+women)
  
  
  num_of_inhab <- map_dfr(data, ~ map_dfr(.x, ~ map_dfr(.x[c("men", "women")] ~ %>% sum(.x$count, na.rm=TRUE), 
                                           .id = 'sex'),# We use nested map functions to iterate through the list to get to the data we want (total number of inhabitants)
                                           .id = 'id'), 
                                           .id = 'municipality') %>% mutate(total_inhab = men+women)
  
  
num_of_inhab <- map_dfr(data, ~ map_dfr(.x, ~ map_dfr(.x, ~ map_dfr(.x[c("0 tot 5 jaar")], ~ sum(.x$count, na.rm= TRUE),
                                           .id='youth'),
                                           .id = 'sex'),# We use nested map functions to iterate through the list to get to the data we want (total number of inhabitants)
                                           .id = 'id'), 
                                           .id = 'municipality') %>% mutate(total_inhab = men+women) 


#### Q4 ----
# Given circumstances (different teachers, late assignment availability),
# everyone gets 2 out of 2 points for Q4


# The end ----

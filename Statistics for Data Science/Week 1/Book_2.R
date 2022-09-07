# install all needed packages for chapter 2 of the book
# install.packages(c('reshape2', 'DataCombine', 'Hmisc',
#                    'haven', 'foreign', 'gdata', 'XLConnect'
#                    , 'pwt', 'reshape', 'doBy'), dependencies = TRUE)

# Clean environment
rm(list= ls(all=TRUE))

getwd()
# Set working directory to data map
setwd("~/Data-Science-Business-Analytics/Data")
# Load CSV file
pwt7 <- read.csv("pwt70_w_country_names.csv", header = TRUE
                 , strip.white = TRUE, stringsAsFactors = FALSE,
                 na.strings = c("NA", ""))
View(pwt7)
head(pwt7, n=1)
tail(pwt7, n=1)
# examine the dimensions of the dataset, this consists of 11400 rows and 37 columns
dim(pwt7)
# gives us the names of the variables, the numbers next to the names uses square brackets to reference entries in vectors and dataframes
names(pwt7)
# gives us the types of data we're handling, the names and some values
str(pwt7)

# we are interested in the following three variables: country, year, rgdpl

# lets do some visualizing

par(mfrow=c(2,2))

hist(pwt7$rgdpl)
boxplot(pwt7$rgdpl)
qqnorm(pwt7$rgdpl)

## INDEXING ##
# Use square brackets shown first or a condition shown second, these reference an element in a vector
pwt7$POP[5]
pwt7$POP[pwt7$country == "Afghanistan" & pwt7$year == 1954]
# in the previous method we use the variable name to specify the column, in this method all information is put between the brackets
# observation first and variable second, this can be used in while, for loop algorithms. These reference an element in a dataframe
pwt7[5,4]
# we could also acomplish this by looking at the dataframe as a whole and specifying the conditions as such
pwt7[pwt7$country == "Afghanistan" & pwt7$year == 1954, "POP"]
# leaving either the row or column position blank with a comma tells R it needs to return all results op to that point
pwt7[5,]
pwt7[,4]
# Data management relies heavily on the principle of indexing.


# first things firts, lets order our dataset
pwt7 <- pwt7[order(pwt7$country, -pwt7$year), ]
View(pwt7)

# cherry picking records from our own data

pwt7[c(100,102),]
pwt7[c(100:102),]

#install.packages('lubridate', dependencies = T)
#library(lubridate)


# This is a function to calculate the amount of working time between each of two time points, and corrects
# for weekdays and non working hours. 
# Inputs:
# a vector with start times (beginTime),
# a vector with end times (endTime), 
# an integer specifying the number of working hours per day (workingHoursPerDay), default is 11 hours, and
# a boolean that indicates whether saturday is to be considered a non-working day (saturday), default TRUE (saturday is a non-working day by default))
# Outputs:
# a vector specifying the amount of working time between the start and end times (amount)

working_hours <- function(beginTime, endTime, workingHoursPerDay = 11L, saturday = TRUE){
  n <- length(beginTime)
  amount <- rep(0, n)
  for (i in 1:n){
    # Calculate the number of sundays between the two dates
    n_weekendDays <- 0
    n_weekendDays <- n_weekdays(as.Date(beginTime[i]), as.Date(endTime[i]), saturday = saturday)
    
    # Calculate the number of days between the two dates
    n_days <- 0
    n_days <- as.Date(endTime[i]) - as.Date(beginTime[i])
    n_days <- as.integer(floor(n_days))
    
    # Calculate the non working hours in seconds
    non_working_sec <- as.numeric(60*60*(24 - workingHoursPerDay)*n_days + 60*60*workingHoursPerDay*n_weekendDays)
    
    amount[i] <- (as.numeric(endTime[i]) - as.numeric(beginTime[i]) - non_working_sec)/3600
  }
  return(amount)
}

# This function counts the number of weekend days between date a and date b. 
# Inputs:
# a vector of dates (a) in Date format
# a vector of dates (b) in Date format
# a boolean that indicates whether saturday is to be considered a non-working day (saturday), default TRUE (saturday is a non-working day by default))
# Ouputs:
# the number of weekend days between the dates in (a) and (b) (n_weekends)

n_weekdays <- function(a, b, saturday = TRUE) {
  date_seq <- seq.Date(a, b, by = 'days')
  date_seq_wdays <- lubridate::wday(date_seq)
  if (saturday) {
    n_weekends <- sum(date_seq_wdays %in% c(1, 7))
  } else {
    n_weekends <- sum(date_seq_wdays == 1)
  }
  
  return(n_weekends)
}

# This function will check if the given dates are weekenddays. By default saturday is considered to be a weekend day (i.e., a non-working day).
# Inputs:
# a vector of dates (a) in Date format
# a boolean that indicates whether saturday is to be considered a non-working day (saturday), default TRUE (saturday is a non-working day by default))
# Outputs:
# a vector of booleans (is_weekend) indicating whether the dates in (a) are weekenddays

check_weekend <- function(a, saturday = TRUE) {
  n <- length(a)
  is_weekend <- rep(0,n)
  for(i in 1:n){
    dayA <- weekdays(a[i], abbreviate = FALSE)
    if (saturday == TRUE){
      if (dayA == "zaterdag" || dayA == "zondag"){
        is_weekend[i] = 1
      } 
    } 
    else {
      if (dayA == "zondag"){
        is_weekend[i] = 1
      }
    }
  }
  return(is_weekend)
}

# This function checks whether a given time is in a working day.
# Inputs:
# a vector of dates (a) in Date format
# an integer specifying the hour at which the working day begins (beginWorkday), default is 7:00
# an integer specifying the hour at which the working day is finished (beginWorkday), default is 18:00
# Outputs: 
# a vector of booleans (outside_workingday) indicating whether the times in (a) occur outside a working day


check_workingday <- function(a, beginWorkday = 7L, endWorkday = 18L){
  n <- length(a)
  outside_workingday <- rep(0,n)
  beginTime <- beginWorkday * 60 * 60
  endTime <- endWorkday * 60 * 60
  for (i in 1:n) {
    timeA <- as.numeric(as.ITime(a[i])) 
    if (((timeA - beginTime) >= 0) && ((timeA - endTime) <= 0)){
      outside_workingday[i] = 0
    } else outside_workingday[i] = 1
  }
  return(outside_workingday)
}
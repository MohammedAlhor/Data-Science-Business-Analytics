# R code for exercises of week 2
# load libraries
library(ggplot2)

# Exercise 1 ------------------------------------------------------------------------------------------------------


# Create time series
T <- 200
add <- 20 # shock
time <- 1:T

#generate a sequence of random disturbances
e <- rnorm(T,0,1)

#add a shock an t=100
e[100] <- e[100] + add

x1 <- x2 <- x3 <- rep(0,T)

#generate time series
for (i in 2:T){
  x1[i] <- 0.5*x1[i-1] + e[i]
  x2[i] <- 0.9*x2[i-1] + e[i]
  x3[i] <- -1*x3[i-1] + e[i]
}

# Put time series in a data frame
series <- data.frame(
  time = time,
  x1 = x1,
  x2 = x2,
  x3 = x3
)

# Create plot
ggplot(series, aes(x = time)) +
  geom_line(aes(y=x1, color = "x1")) +
  geom_line(aes(y=x2, color = "x2")) +
  geom_line(aes(y=x3, color = "x3")) +
  xlab("time") + ylab("x1,x2,x3") +
  #add a legend to the plot
  scale_colour_manual(
    "", values = c("x1"="red","x2"="blue","x3"="green"))


# Exercise 2 ------------------------------------------------------------------------------------------------------
# Create time series
T <- 200
time <- 1:T

e <- rnorm(T,0,1)

y1 <- y2 <- y3 <- rep(0,T)

for (i in 2:T){
  y1[i] <- 0.5*y1[i-1] + e[i]
  y2[i] <- 0.9*y2[i-1] + e[i]
  y3[i] <- y3[i-1] + e[i]
}

#The acf function automatically
#plots the ACF function
par(mfrow = c(2,2)) # Show multiple plots in one frame
acf.y1 <- acf(y1)
acf.y2 <- acf(y2)
acf.y3 <- acf(y3)
par(mfrow=c(1,1)) # Reset to show one plot in one frame for the next time we create a plot




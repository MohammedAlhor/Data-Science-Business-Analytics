#les opdrachten
1:4 +1
# vectorized, maar dit geldt niet voor alle functies
is.null(list(NULL, NA, 1,-5,3))
# Hier hebben we een for loop nodig

series <- list(NULL, NA, 1,-5,3)
# je zou map kunnen gebruiken
install.packages('purrr')
library(purrr)
map(list(NULL, NA, 1,-5,3), is.null)

is_null <- vector('logical', 5)
for (i in 1:5)
{
  is_null[i] <- is.null(series[[i]])
}

# Opdracht 1
#a

for (i in 10:20)
{
  i+1
  print(i)
}

#b
my_function <- function(start_value, increment)
{
  while(start_value>0)
  {
    print(start_value)
    print(increment)
    start_value <- start_value - increment
    increment <-increment+1
  }
}


my_function(20,1)

# Opdracht 2
fibonacci_func <- function(length)
{
  x1<-0
  x2<-1
  x3<-0
 for (i in 1:length)
 {
  print(i)
   x3<- x1+x2
   x2<- x1+x3
   x1<- 
 }
}


# This function prints the elemenst of a vector starting with a description
# name: the description in text
# sol: vector that needs to be printed
# start: first element that will be printed
# n: number of elements to be printed

printVector <- function(name,sol,start,n)
{
  cat(name)
  last=start+n-1
  for (i in start:last)
  {
    cat(sol[i])
    cat(' ')
  }
  cat('\n')
}
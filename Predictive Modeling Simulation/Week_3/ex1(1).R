
set.seed(1234)

n <- 100

z <- matrix(1, n)

for (i in 1:n)
{
  x <- sample(6,1)
  y <- x - 3.5
  z[i] <- sum(y[1:i])
}

plot(z)
hist(z)
max(z)


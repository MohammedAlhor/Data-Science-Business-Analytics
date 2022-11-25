set.seed(1)
n1 <-  50
x <- cbind(rnorm(n1), rnorm(n1))

plot(x)

n2 <- 50
y <- cbind(rnorm(n2, mean=4), rnorm(n2, mean=4))
plot(y)

z <- rbind(x,y)

plot(z)

d <- dist(z)

hclust.out <- hclust(d, method = 'single')

plot(hclust.out)

clus.ident<-cutree(hclust.out,k=2)
plot(z, col= clus.ident)

# IRIS dataset

irisdata <- iris
idata<-irisdata[,c(1:4)]

d <- idata$

d<-dist(idata)

hclust.out <- hclust(d)

plot(hclust.out)

clust.ident <- cutree(hclust.out, k=3)
plot(idata, col = clust.ident)

boxplot(idata[clust.ident==1,])
boxplot(idata[clust.ident==2,])
boxplot(idata[clust.ident==3,])

km<-kmeans(idata, 3)
plot(idata, col= km$cluster)


# random data kmeans


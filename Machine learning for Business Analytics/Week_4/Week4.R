# Nearest neighbour classifiers. 
library(caret)
set.seed(1)
smpl <- sample(nrow(df))
data.perm <- df[smpl,]
ntrain<- round(.7*nrow(df))
data.train <- data.perm[1:ntrain,]
data.test <- data.perm[(ntrain+1):nrow(df),]

xvars <- (4:32)
y <- 3
k=5

knn.out <- knn(train=data.train[,xvars], test= data.test[,xvars], cl=data.train[,y], k=k)


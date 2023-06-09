library(magrittr)
library(MASS)
library(reticulate)
library(tensorflow)
library(keras)
library(tidyverse)


Show_label <- function(row, data) {
  tmp <- data.frame(
    x = rep(1:28, times = 28),
    y = rep(28:1, each = 28),
    shade = as.numeric(data[row, -1])
  )
  ggplot(data = tmp) +
    geom_point(aes(x = x, y = y, color = shade), 
               size = 11, shape = 15) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.ticks = element_blank(), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), 
      legend.position = "none", panel.background = element_blank(),
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), plot.background = element_blank()
    ) +
    scale_color_gradient(low = "white", high = "black") +
    geom_text(aes(x = 28, y = 28), label = data[row, 1])
}


mnist <- dataset_mnist()
str(mnist)

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

n_train <- NROW(x_train)
n_test <- NROW(x_test)

trainn <-  matrix(nrow= n_train, ncol= 785)
for(i in 1:n_train){
  trainn[i,2:785] <- t(x_train[i,,])
}
trainn[,1] <- y_train

testt <- matrix(nrow= n_test, ncol= 785)
for(i in 1:n_test){
  testt[i,2:785] <- t(x_test[i,,])
}
testt[,1] <- y_test

ii <- 2500
Show_label(ii, trainn) 
Show_label(ii, data= testt) 


### C


# Now with keras
library(reticulate)
reticulate::py_discover_config()
# use_python(python= "")
library(tensorflow)
library(keras)
# tensorflow::tf_config()
# tf$constant("Hellow Tensorflow")

# convert, for training in keras
y_train <- to_categorical(trainn[,1]) %>% as.matrix
y_test <- to_categorical(testt[,1]) %>% as.matrix

first_model <- keras_model_sequential() %>% 
  layer_dense(units = 32, activation = "sigmoid", input_shape = c(28*28)) %>% 
  layer_dense(units = 10, activation = "softmax")

first_model %>% compile(
  optimizer = optimizer_sgd(), #optimizer_adam, 
  loss = "categorical_crossentropy",
  metrics = c("accuracy", "mse")
)

first_model %>% summary
epochss <- 2 # change to 5 or 10. 
first_model %>% fit(as.matrix(Trainx_scaled), 
                    y_train, epochs = epochss, 
                     verbose= 1)

evaluatee <- first_model %>% 
  evaluate(as.matrix(test_scaled[,-1]), y_test, verbose=1)

# convert, for training in keras

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

y_train <- to_categorical(y_train) %>% as.matrix
y_test <-  to_categorical(y_test) %>% as.matrix
dim(x_train)

x_train[ii,,,1] %>% View

n_train <- NROW(x_train)
n_test <- NROW(x_test)

nr <- nc <- 28
x_train <- keras::array_reshape(x_train, c(n_train, nr, nc, 1))
x_test <- keras::array_reshape(x_test, c(n_test, nr, nc, 1))


second_model <- keras_model_sequential()

second_model %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), 
                activation = 'relu', input_shape = c(28,28,1)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), 
                activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = 'softmax')


second_model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# More specs

epochss <- 3
mini_batch_size <- 2^9

second_model %>% fit(
  x_train, y_train,
  batch_size = mini_batch_size,
  epochs = epochss,
  verbose = 1
)

# Evaluate 
evaluatee <- second_model %>% evaluate(
  x_test, y_test, verbose = 1
)



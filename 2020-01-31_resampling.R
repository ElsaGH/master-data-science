rm(list = ls())
gc() #garbage collector - vacia la RAM del equipo

auto <- read.csv("auto.csv", sep = ";", header=T)

View(auto)
str(auto)

model1 <- lm(mpg ~ horsepower, data=auto)
model2 <- lm(mpg ~ horsepower + horsepower^2, data=auto)
model3 <- lm(mpg ~ horsepower + horsepower^2 + horsepower^3, data=auto)

#para acceder a los elementos de una lista hay que poner doble corchete

install.packages("caret")
library(caret)

prop_training <- 0.6
n_iter <- 10

set.seed(42)
train_index_list <- createDataPartition(1:nrow(auto), p = prop_training, times = n_iter)
str(train_index_list)

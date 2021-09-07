set.seed(100)
rm(list = ls())
library(nnet)
library(NeuralNetTools)
library(MASS)
attach(Boston)

samp <- sample(1:nrow(Boston),nrow(Boston)/4)
Boston.train <- Boston[-samp,]
Boston.test <- Boston[samp,]

# Fitting a neural net to predict medv based on all other variables in Boston dataset as predictors

nn <- nnet(medv~., data = Boston.train, size = 1, decay = 0, linout = TRUE, skip = TRUE)
print(nn)
plotnet(nn)
pred <- predict(nn,Boston.test)
rmse <- sqrt(mean((Boston.test$medv - pred)^2))

# A single layer neural net with 1 hidden neuron and decay = 0 gives a Test RMSE of 4.629

# Fit neural nets with different sizes and decay parameters, choose best neural net based on Test RMSE

decay.mat <- c(0,0.1,0.01,0.001,0.0001,0.00001)
size.mat <- c(1,2,5,10,20,30)
rmse.mat <- matrix(NA,6,6)

for (i in 1:length(size.mat)){
  for (j in 1:length(decay.mat)){
    nn.2 <- nnet(medv~., data = Boston.train, size = size.mat[i], decay = decay.mat[j], linout = TRUE, skip = TRUE)
    pred.2 <- predict(nn.2,Boston.test)
    rmse.mat[i,j] <- sqrt(mean((Boston.test$medv - pred.2)^2))
  }
}

decay.best <- decay.mat[floor(which.min(rmse.mat)/6)+1]
size.best <- size.mat[which.min(rmse.mat)%%6]
rmse.best <- rmse.mat[which.min(rmse.mat)]

# Best Test RMSE of 4.407 is obtained for a neural net with size = 5 and decay = 1
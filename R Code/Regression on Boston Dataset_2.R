##########
# Regression Models on Boston Dataset
##########

set.seed(1)
rm(list = ls())
library(MASS)
library(leaps)
library(glmnet)
library(pls)
attach(Boston)

### Subset Selection

predict.regsubsets = function (object , newdata ,id ,...){
  form=as.formula (object$call[[2]])
  mat=model.matrix(form,newdata )
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k <- 10
folds <- sample(1:k, nrow(Boston), replace=TRUE)
cv.err <- matrix(NA, k, ncol(Boston)-1, dimname=list(NULL, paste(1:(ncol(Boston) - 1))))

for (j in 1:k){
  Boston.fit.1 <- regsubsets(crim~., data=Boston[folds!=j,], nvmax=ncol(Boston)-1)
  for (i in 1:(ncol(Boston)-1)){
    Boston.pred.1 <- predict(Boston.fit.1, Boston[folds==j,], id=i)
    cv.err[j,i] <- mean((Boston$crim[folds==j] - Boston.pred.1)^2)
  }
}

mean.cv.err <- apply(cv.err,2,mean)
par(mfrow=c(1,1))
plot(mean.cv.err, type='b')
# Cross Validation has selected model with 12 variables as it has the lowest MSE

samp <- sample(1:nrow(Boston),nrow(Boston)/4)
Boston.train <- Boston[-samp,]
Boston.test <- Boston[samp,]
Boston.fit.1.best <- regsubsets(crim~., data=Boston.train, nvmax = which.min(mean.cv.err))
Boston.pred.1.best <- predict(Boston.fit.1.best, Boston.test, id=which.min(mean.cv.err))
Boston.rmse.1 <- sqrt(mean((Boston.test$crim - Boston.pred.1.best)^2))
# Test RMSE achieved is 4.01


### Lasso
Boston.train.mat <- model.matrix(crim~., data=Boston.train)
Boston.test.mat <- model.matrix(crim~., data=Boston.test)
Boston.mat <- model.matrix(crim~., data=Boston)
lambda <- 10 ^ seq(-2, 4, length=100)

Boston.fit.2 <- cv.glmnet(Boston.train.mat, Boston.train[, 'crim'], alpha=1, lambda=lambda)
lambda.best.2 <- Boston.fit.2$lambda.min
Boston.pred.2 <- predict(Boston.fit.2, newx=Boston.test.mat, s=lambda.best.2)
Boston.rmse.2 <- sqrt(mean((Boston.test[, 'crim'] - Boston.pred.2)^2))
# Test error obtained is 8.94


### Ridge Regression

Boston.fit.3 <- cv.glmnet(Boston.train.mat, Boston.train[, 'crim'], alpha=0, lambda=lambda, thresh=1e-12)
lambda.best.3 <- Boston.fit.3$lambda.min
Boston.pred.3 <- predict(Boston.fit.3, newx=Boston.test.mat, s=lambda.best.3)
Boston.rmse.3 <- sqrt(mean((Boston.test[, 'crim'] - Boston.pred.3)^2))
# Test error obtained is 3.88


### PCR

Boston.fit.4 <- pcr(crim~., data=Boston.train, scale=TRUE, validation='CV')
validationplot(Boston.fit.4, val.type='MSEP')
# RMSE decreases upto M=3 and is more or less constant thereafter. Hence, we select M=3.
Boston.pred.4 <- predict(Boston.fit.4, Boston.test, ncomp=3)
Boston.rmse.4 <- sqrt(mean((Boston.test[, 'crim'] - Boston.pred.4)^2))
# Test error obtained is 4.13

# The model that performs the best on the given data set is the one built using Ridge Regression. It has the lowest Test RMSE of all the models at 3.88.
# This model involves all the features (variables) in the data and it is by design. Ridge Regression shrinks the coefficients of variables to reduce variance but never makes them 0.
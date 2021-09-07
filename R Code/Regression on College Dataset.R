##########
# Regression Models on College dataset
##########

set.seed(1)
rm(list = ls())
library(ISLR)
library(glmnet)
library(pls)
attach(College)
samp <- sample(1:nrow(College),round(nrow(College)/4))
College.train <- College[-samp,]
College.test <- College[samp,]

### Fit linear model using least squares. Report test error.
College.fit.1 <- lm(Apps~., data=College.train)
College.pred.1 <- predict(College.fit.1, College.test)
err.1 <- sqrt(mean((College.test[, 'Apps'] - College.pred.1)^2))
# Test error obtained is 941.89

### Fit ridge regression model, choose lambda by cross validation. Report test error.
College.train.mat <- model.matrix(Apps~., data=College.train)
College.test.mat <- model.matrix(Apps~., data=College.test)
lambda <- 10 ^ seq(-2, 4, length=100)
College.fit.2 <- cv.glmnet(College.train.mat, College.train[, 'Apps'], alpha=0, lambda=lambda, thresh=1e-12)
lambda.best.2 <- College.fit.2$lambda.min
College.pred.2 <- predict(College.fit.2, newx=College.test.mat, s=lambda.best.2)
err.2 <- sqrt(mean((College.test[, 'Apps'] - College.pred.2)^2))
# Test error obtained is 941.88

### Fit lasso model, choose lambda by cross validation. Report test error, along with number of non zero coeff estimates.
College.mat <- model.matrix(Apps~., data=College)
College.fit.3 <- cv.glmnet(College.train.mat, College.train[, 'Apps'], alpha=1, lambda=lambda)
lambda.best.3 <- College.fit.3$lambda.min
College.pred.3 <- predict(College.fit.3, newx=College.test.mat, s=lambda.best.3)
sqrt(mean((College.test[, 'Apps'] - College.pred.3)^2))
# Test error obtained is 941.74
College.fit.3 <- glmnet(College.mat, College[, 'Apps'], alpha=1)
predict(College.fit.3, s=lambda.best.3, type="coefficients")
# No variables have 0 coeff estimates

### Fit PCR model, choose M by cross validation. Report test error, along with value of M selected by cross validation.
College.fit.4 <- pcr(Apps~., data=College.train, scale=TRUE, validation='CV')
validationplot(College.fit.4, val.type='MSEP')
# RMSE decreases upto M=5 and is more or less constant thereafter. Hence, we select M=5.
College.pred.4 <- predict(College.fit.4, College.test, ncomp=5)
sqrt(mean((College.test[, 'Apps'] - College.pred.4)^2))
# Test error obtained is 1274.35

### Fit PLS model, choose M by cross validation. Report test error, along with value of M selected by cross validation.
College.fit.5 <- plsr(Apps~., data=College.train, scale=TRUE, validation='CV')
validationplot(College.fit.5, val.type='MSEP')
# RMSE decreases upto M=5 and is constant thereafter. Hence, we select M=5.
College.pred.5 <- predict(College.fit.5, College.test, ncomp=5)
sqrt(mean((College.test[, 'Apps'] - College.pred.5)^2))
# Test error obtained is 1010.325

### Comment on results. How accurately can we predict number of college applications? Is there difference in error rates using 5 different approaches?
College.Apps.avg <- mean(College.test[, 'Apps'])
College.pred.1.test.r2 <- 1 - mean((College.test[, 'Apps'] - College.pred.1)^2) /mean((College.test[, 'Apps'] - College.Apps.avg)^2)
College.pred.2.test.r2 <- 1 - mean((College.test[, 'Apps'] - College.pred.2)^2) /mean((College.test[, 'Apps'] - College.Apps.avg)^2)
College.pred.3.test.r2 <- 1 - mean((College.test[, 'Apps'] - College.pred.3)^2) /mean((College.test[, 'Apps'] - College.Apps.avg)^2)
College.pred.4.test.r2 <- 1 - mean((College.test[, 'Apps'] - College.pred.4)^2) /mean((College.test[, 'Apps'] - College.Apps.avg)^2)
College.pred.5.test.r2 <- 1 - mean((College.test[, 'Apps'] - College.pred.5)^2) /mean((College.test[, 'Apps'] - College.Apps.avg)^2)
barplot(c(College.pred.1.test.r2,College.pred.2.test.r2,College.pred.3.test.r2,College.pred.4.test.r2,College.pred.5.test.r2), col="blue", names=c("Linear","Ridge", "Lasso", "PCR", "PLS"), main="Comparing the R-squared Values")
# The plot shows that test R2 for all models except PCR are around 0.9, while PCR has a smaller test R2 of about 0.8. All models except PCR predict college applications with high accuracy.
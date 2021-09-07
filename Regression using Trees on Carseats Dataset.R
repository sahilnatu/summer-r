##############
# Regression using Trees on Carseats dataset
##############

rm(list = ls())

library(tree)
library(ISLR)
library(randomForest)
attach(Carseats)

set.seed(100)
df <- Carseats

### Split into train/test
samp <- sample(1:nrow(df),nrow(df)/2)
df_train <- df[samp,]
df_test <- df[-samp,]

### Fit regression tree, plot tree, interpret results, Test MSE
tree.carseats <- tree(Sales~.,df_train)
print(length(unique(tree.carseats$where)))
summary(tree.carseats)
plot(tree.carseats,type="uniform")
text(tree.carseats,col="blue",label=c("yval"),cex=.6)
### 7 predictors used in tree construction - ShelveLoc, Price, CompPrice, Income, Population, Advertising, Age
### Number of splits = 16, Number of leaves = 17
### Residual Mean Deviance = 2.623
yhat <- predict(tree.carseats,df_test)
test_mse <- mean((yhat - df_test[,"Sales"])^2)

### Use CV to determine optimal level of tree complexity, does pruning improve Test MSE?
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type="b")
best <- which.min(cv.carseats$size)
### CV has selected the tree in this case with number of leaves = 15
### We prune
prune.carseats <- prune.tree(tree.carseats,best=best)
plot(prune.carseats)
text(prune.carseats,col="blue",label=c("yval"),cex=.6)
yhat_prune <- predict(prune.carseats,df_test)
test_mse_prune <- mean((yhat_prune - df_test[,"Sales"])^2)
### Pruning does not improve Test MSE

### Use bagging to analyze data, what is Test MSE? Use importance() to determine most important variables
bag.carseats <- randomForest(Sales~.,data=df_train,mtry=ncol(df_train)-1,importance=TRUE)
bag.carseats
### 10 variables considered at each split, no. of trees contructed = 500
### MSE = 2.53
yhat_bag <- predict(bag.carseats,df_test)
test_mse_bag <- mean((yhat_bag - df_test[,"Sales"])^2)
### Test MSE has improved to 3.27
importance(bag.carseats)
### List down variables by importance

### Use random forests to analyze data, what is Test MSE? Use importance() to determine most important variables, desc effect of 'm' variables considered at each split on error rate
m <- ceiling(sqrt(ncol(df_train)-1))
rf.carseats <- randomForest(Sales~.,data=df_train,mtry=m,importance=TRUE)
rf.carseats
### 4 variables considered at each split, no. of trees constructed = 500
### MSE = 2.64
yhat_rf <- predict(rf.carseats,df_test)
test_mse_rf <- mean((yhat_rf - df_test[,"Sales"])^2)
### Test MSE has further improved to 3.24
importance(rf.carseats)
### List down variables of importance
### Since we consider only a subset of available predictors for each split in the tree, the trees are more decorrelated than in bagging where all predictors are considered for every split. Combining these uncorrelated trees reduces variance, thereby improving the Test MSE.
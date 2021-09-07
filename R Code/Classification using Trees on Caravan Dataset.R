##############
# Classification using Trees on Caravan dataset
##############

rm(list = ls())

library(gbm)
library(ISLR)
library(kknn)
attach(Caravan)

set.seed(100)
df <- Caravan
n <- c(1:1000)

### Create train data set of first 1000 observations and test data set of the remaining observations
purchase_1 <- ifelse(Purchase=="Yes",1,0)
df <- data.frame(df,purchase_1)
df_train <- df[n,-c(86)]
df_test <- df[-n,-c(86)]

### Fit boosting model for Y=Purchase and all the other variables as predictors, number of trees = 1000, shrinkage = 0.01, which are most important predictors
boost.caravan <- gbm(purchase_1~., data = df_train, distribution = "bernoulli", n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
summary(boost.caravan)
### Predictor PPERSAUT is the most important one, followed by MOSTYPE,MGODGE,MOPLHOOG,PBRAND

### Predict response on test data, if prob of purchase > 20% then 1 else 0, create confusion matrix, what fraction of ppl predicted to make purchase actually purchase?
yhat_boost_raw <- predict(boost.caravan,df_test,n.trees = 1000,type="response")
yhat_boost <- ifelse(yhat_boost_raw > 0.2, 1, 0)
table(yhat_boost,df_test[,"purchase_1"])
error_rate <- 1-sum(yhat_boost==df_test[,"purchase_1"])/nrow(df_test)
purchase_rate <- sum(df_test[,"purchase_1"]==1 & yhat_boost==1)/sum(yhat_boost==1)

### KNN
purchase_rate_knn_matrix <- NULL
rm(near)
for (i in c(1,2,3,4,5,10,20,35,50,75,100,150,200,250,300,400,500)) {
  near <- kknn(purchase_1~.,df_train,df_test,k=i,kernel="rectangular")
  yhat_knn <- ifelse(near$fitted > 0.2, 1, 0)
  purchase_rate_knn_temp <- sum(df_test[,"purchase_1"]==1 & yhat_knn==1)/ifelse(sum(yhat_knn==1)==0,1,sum(yhat_knn==1))
  purchase_rate_knn_matrix <- c(purchase_rate_knn_matrix,purchase_rate_knn_temp)
}
purchase_rate_knn <- max(purchase_rate_knn_matrix)
### Desc output
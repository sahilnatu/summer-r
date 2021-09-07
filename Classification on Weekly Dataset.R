############
# Classification on Weekly dataset
############

rm(list=ls())
library(ISLR)
library(kknn)
attach(Weekly)

### Produce numerical and graphical summaries, comment on any pattern
names(Weekly)
dim(Weekly)
summary(Weekly)
cor(Weekly[,-9])
plot(Year,Volume)
# Weekly datasource has 9 columns and 1089 rows. Of the 9 columns, 1 is categorical.
# From the pairwise correlation matrix, there is no correlation between current week's return and that of any previous weeks
# There seems to be a strong correlation between Year and Volume
# Upon plotting Year vs Volume, it is evident that Volume of trade has been increasing every year

### Perform Logistic Regression as Direction ~ Lag1,2,3,4,5,Volume. Summarize the model for results. Statistically significant variables?
Weekly.glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial)
summary(Weekly.glm.fit)
# Lag2 has the lowest p-value, and has a positive coefficient. Thus direction of market in current week is the same as 2 weeks preceding that week.
# Other variables have a large p-value and thus are not statistically significant

### Confusion Matrix and overall fraction of correct predictions. Explain mistakes in model using confusion matrix.
Weekly.glm.prob <- predict(Weekly.glm.fit,type="response")
contrasts(Direction)
Weekly.glm.pred <- rep("Down",length(Weekly.glm.prob))
Weekly.glm.pred[Weekly.glm.prob>0.5] <- "Up"
table(Weekly.glm.pred,Direction)
correct.pred <- mean(Weekly.glm.pred==Direction)
wrong.pred <- 1-correct.pred
# Model correctly predicts UP movement in 557 weeks and DOWN movement in 54 weeks out of a total of 1089 weeks
# Fraction of correct predictions is 0.561
# Training error rate is 0.439, which is too high since training error rate is lower compared to test error rate
# The model gives more 'False UPs' than 'True DOWNs' thus being poor at predicting if the market would move down
# The model does a decent job of predicting if the market would move up, as it has more than 10x 'True UPs' than 'False DOWNs'

### Perform Logistic Regression as Direction ~ Lag2 for data between 1990 and 2008. Use rest of the data as test set. Show confusion matrix and fraction of correct predictions
Weekly.train <- Weekly[Year<=2008,]
Weekly.test <- Weekly[!Year<=2008,]
Weekly.glm.fit.2 <- glm(Direction~Lag2, family = binomial, data = Weekly.train)
Weekly.glm.prob.2 <- predict(Weekly.glm.fit.2, Weekly.test, type="response")
Weekly.glm.pred.2 <- rep("Down",length(Weekly.glm.prob.2))
Weekly.glm.pred.2[Weekly.glm.prob.2>0.5] <- "Up"
table(Weekly.glm.pred.2,Weekly.test[,"Direction"])
correct.pred.2 <- mean(Weekly.glm.pred.2==Weekly.test[,"Direction"])
wrong.pred.2 <- 1-correct.pred.2
# Model correctly predicts UP movement in 56 weeks and DOWN movement in 9 weeks out of a total of 104 weeks
# Fraction of correct predictions is 0.625
# Training error rate is 0.375, lower than the previous model
# The model gives more 'False UPs' than 'True DOWNs' thus being poor at predicting if the market would move down
# The model does a decent job of predicting if the market would move up, as it has more than 10x 'True UPs' than 'False DOWNs'

### Perform KNN with K=1 for same data as above
Weekly.knn.fit.3 <- kknn(Direction~Lag2,Weekly.train,Weekly.test,k=1)
Weekly.knn.pred.3 <- Weekly.knn.fit.3$fitted.values
table(Weekly.knn.pred.3,Weekly.test[,"Direction"])
correct.pred.3 <- mean(Weekly.knn.pred.3==Weekly.test[,"Direction"])
wrong.pred.3 <- 1 - correct.pred.3
# KNN Model correctly predicts UP movement in 30 weeks and DOWN movement in 22 weeks out of a total of 104 weeks
# Fraction of correct predictions is 0.5
# Training error rate is 0.5, higher than linear regression model
# The model gives equal 'False UPs' and 'True DOWNs' as well as equal 'True UPs' and 'False DOWNs', hence it is poor at predicting the response

### Which method provides the best result on this data?

# The Logistic Regression Model provides a better result on this data with an error rate of 37.5% as against KNN's error rate of 50%

### Try different predictors to check for better results, also try different values of K in KNN

# Attempt 1 - Adding Lag1 as predictor along with Lag2
Weekly.glm.fit.4 <- glm(Direction~Lag1+Lag2, family = binomial, data = Weekly.train)
Weekly.glm.prob.4 <- predict(Weekly.glm.fit.4, Weekly.test, type="response")
Weekly.glm.pred.4 <- rep("Down",length(Weekly.glm.prob.4))
Weekly.glm.pred.4[Weekly.glm.prob.4>0.5] <- "Up"
table(Weekly.glm.pred.4,Weekly.test[,"Direction"])
correct.pred.4 <- mean(Weekly.glm.pred.4==Weekly.test[,"Direction"])
wrong.pred.4 <- 1-correct.pred.4
# Model correctly predicts UP movement in 53 weeks and DOWN movement in 7 weeks out of a total of 104 weeks
# Fraction of correct predictions is 0.577
# Training error rate is 0.423, higher than the logistic regression model with only Lag2 as predictor
# The model gives more 'False UPs' than 'True DOWNs' thus being poor at predicting if the market would move down
# The model does a decent job of predicting if the market would move up, as it has more than 10x 'True UPs' than 'False DOWNs'

# Attempt 2 - Adding Volume as predictor along with Lag2
Weekly.glm.fit.5 <- glm(Direction~Volume+Lag2, family = binomial, data = Weekly.train)
Weekly.glm.prob.5 <- predict(Weekly.glm.fit.5, Weekly.test, type="response")
Weekly.glm.pred.5 <- rep("Down",length(Weekly.glm.prob.5))
Weekly.glm.pred.5[Weekly.glm.prob.5>0.5] <- "Up"
table(Weekly.glm.pred.5,Weekly.test[,"Direction"])
correct.pred.5 <- mean(Weekly.glm.pred.5==Weekly.test[,"Direction"])
wrong.pred.5 <- 1-correct.pred.5
# Model correctly predicts UP movement in 36 weeks and DOWN movement in 20 weeks out of a total of 104 weeks
# Fraction of correct predictions is 0.538
# Training error rate is 0.461, higher than the logistic regression model with only Lag2 as predictor
# This model brings down the proportion of 'false UPs' given by the model in comparison to 'true DOWNs' when compared with all previous logistic regression models

# Attempt 3 - Carrying out KNN with K ranging from 1 to 50
wrong.pred.6 <- rep(0,50)
for (i in c(1:50)) {
  Weekly.knn.fit.6 <- kknn(Direction~Lag2,Weekly.train,Weekly.test,k=i)
  Weekly.knn.pred.6 <- Weekly.knn.fit.6$fitted.values
  correct.pred.6 <- mean(Weekly.knn.pred.6==Weekly.test[,"Direction"])
  wrong.pred.6[i] <- 1 - correct.pred.6
}
plot(c(1:50),wrong.pred.6,type="l")
wrong.pred.6.lowestK <- which.min(wrong.pred.6)
wrong.pred.6.lowest <- wrong.pred.6[which.min(wrong.pred.6)]
# KNN with K=15 has lowest error rate among all attempted KNN models at 0.413
# This is an improvement over the error rate of 0.5 achieved with KNN where K=1

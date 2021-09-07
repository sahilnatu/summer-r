#############
# Linear Regression on Boston dataset
#############

rm(list=ls())
library(MASS)
attach(Boston)

n <- nrow(Boston)

reg_coeff <- data.frame(NAME=c(colnames(Boston[,2:ncol(Boston)])),SLR=c(matrix(0,ncol(Boston)-1)),MLR=c(matrix(0,ncol(Boston)-1)))

### Fit simple linear regression for each predictor, describe results, which models have statistically significant association between predictor and response, create plots to backup your assertion

boston.zn.fit <- lm(crim~zn)
summary(boston.zn.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between proportion of residential land zoned and per capita crim rate
### R^2 value is extremely small, i.e. 0.04, hence zn does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.zn.fit,data.frame(zn=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on zn are very large and thus zn isn't a good predictor for crim
plot(zn,crim)
abline(boston.zn.fit)
reg_coeff[1,'SLR'] <- boston.zn.fit$coefficients[2]

boston.indus.fit <- lm(crim~indus)
summary(boston.indus.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between proportion of non-retail business acres per town and per capita crime rate
### R^2 value is small, i.e. 0.16, hence indus does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.indus.fit,data.frame(indus=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on indus are very large and thus indus isn't a good predictor for crim
plot(indus,crim)
abline(boston.indus.fit)
reg_coeff[2,'SLR'] <- boston.indus.fit$coefficients[2]

boston.chas.fit <- lm(crim~chas)
summary(boston.chas.fit)
### Since p-value is not sufficiently small, we accept the Null Hypothesis, i.e. there is no relationship between proximity to Charles river and per capita crim rate
reg_coeff[3,'SLR'] <- boston.chas.fit$coefficients[2]

boston.nox.fit <- lm(crim~nox)
summary(boston.nox.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between nitrous oxide concentration and per capita crim rate
### R^2 value is small, i.e. 0.17, hence nox does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.nox.fit,data.frame(nox=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on nox are fairly large and thus nox isn't a good predictor for crim
plot(nox,crim)
abline(boston.nox.fit)
reg_coeff[4,'SLR'] <- boston.nox.fit$coefficients[2]

boston.rm.fit <- lm(crim~rm)
summary(boston.rm.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between avg number of rooms per house and per capita crim rate
### R^2 value is extremely small, i.e. 0.04, hence rm does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.rm.fit,data.frame(rm=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on rm are very large and thus rm isn't a good predictor for crim
plot(rm,crim)
abline(boston.rm.fit)
reg_coeff[5,'SLR'] <- boston.rm.fit$coefficients[2]

boston.age.fit <- lm(crim~age)
summary(boston.age.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between proportion of owner occupied units built before 1940 and per capita crim rate
### R^2 value is fairly small, i.e. 0.12, hence age does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.age.fit,data.frame(age=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on age are very large and thus age isn't a good predictor for crim
plot(age,crim)
abline(boston.age.fit)
reg_coeff[6,'SLR'] <- boston.age.fit$coefficients[2]

boston.dis.fit <- lm(crim~dis)
summary(boston.dis.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between weighted mean of distances to 5 Boston employment centres and per capita crim rate
### R^2 value is fairly small, i.e. 0.14, hence dis does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.dis.fit,data.frame(dis=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on dis are very large and thus dis isn't a good predictor for crim
plot(dis,crim)
abline(boston.dis.fit)
reg_coeff[7,'SLR'] <- boston.dis.fit$coefficients[2]

boston.rad.fit <- lm(crim~rad)
summary(boston.rad.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between accessibility to radial hwy and per capita crim rate
### R^2 value is about 0.39, hence rad does explain to a certain extent the variability in crim
predict(boston.rad.fit,data.frame(rad=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on rad are however very large and thus rad isn't a good predictor for crim
plot(rad,crim)
abline(boston.rad.fit)
reg_coeff[8,'SLR'] <- boston.rad.fit$coefficients[2]

boston.tax.fit <- lm(crim~tax)
summary(boston.tax.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between full value property tax rates and per capita crim rate
### R^2 value is about 0.34, hence tax does explain to a certain extent the variability in crim
predict(boston.tax.fit,data.frame(tax=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on tax are however very large and thus tax isn't a good predictor for crim
plot(tax,crim)
abline(boston.tax.fit)
reg_coeff[9,'SLR'] <- boston.tax.fit$coefficients[2]

boston.ptratio.fit <- lm(crim~ptratio)
summary(boston.ptratio.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between pupil to teacher ratio and per capita crim rate
### R^2 value is fairly small, i.e. 0.08, hence ptratio does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.ptratio.fit,data.frame(ptratio=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on ptratio are very large and thus tax isn't a good predictor for crim
plot(ptratio,crim)
abline(boston.ptratio.fit)
reg_coeff[10,'SLR'] <- boston.ptratio.fit$coefficients[2]

boston.black.fit <- lm(crim~black)
summary(boston.black.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between proportion of blacks in town and per capita crim rate
### R^2 value is fairly small, i.e. 0.14, hence black does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.black.fit,data.frame(black=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on black are very large and thus black isn't a good predictor for crim
plot(black,crim)
abline(boston.black.fit)
reg_coeff[11,'SLR'] <- boston.black.fit$coefficients[2]

boston.lstat.fit <- lm(crim~lstat)
summary(boston.lstat.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between lower status of population and per capita crim rate
### R^2 value is fairly small, i.e. 0.2, hence lstat does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.lstat.fit,data.frame(lstat=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on lstat are very large and thus lstat isn't a good predictor for crim
plot(lstat,crim)
abline(boston.lstat.fit)
reg_coeff[12,'SLR'] <- boston.lstat.fit$coefficients[2]

boston.medv.fit <- lm(crim~medv)
summary(boston.medv.fit)
### Since p-value is sufficiently small, we reject the Null Hypothesis, i.e. there is relationship between median value of owner occupied homes and per capita crim rate
### R^2 value is fairly small, i.e. 0.15, hence medv does not explain sufficiently the variability in crim, and hence is not a good predictor
predict(boston.medv.fit,data.frame(medv=c(5,10,15)),interval = 'prediction')
### The prediction intervals with the fit on medv are very large and thus medv isn't a good predictor for crim
plot(medv,crim)
abline(boston.medv.fit)
reg_coeff[13,'SLR'] <- boston.medv.fit$coefficients[2]

### The predictors 'accessibility to radial hwy' and 'full value property tax rates' have a relatively better statistical association with crime rate per capita
### However, these predictors still fail to sufficiently explain crim as evidenced by their prediction intervals and visually by the plots.
par(mfrow=c(1,2))
plot(rad,crim)
abline(boston.rad.fit)
plot(tax,crim)
abline(boston.tax.fit)

############################################################################################################

### Fit multiple regression model using all predictors, describe results, for which predictors can we reject the null hypothesis?

boston.all.fit <- lm(crim~.,data=Boston)
summary(boston.all.fit)
### The model is not particularly good at predicting the variability of crim since the R^2 value is only about 0.45
### The F-statistic is sufficiently greater than 1, thus we reject the Null Hypothesis that none of the predictors have a relationship with crim
### However, when we look at p-value of individual predictors in this model, it is evident that we can reject the Null Hypothesis for only 2 predictors
## 1. Distance to 5 Boston Employment Centers and 2. Accessibility to Radial Hwy


for (i in 1:(ncol(Boston)-1)){
  reg_coeff[i,'MLR'] <- as.numeric(boston.all.fit$coefficients[i+1])
}

### How do results from (a) compare with (b), plot regression coeff for all predictors with SLR on Y and MLR on X axes

### Multiple Linear Regression model provides a much better fit than any of the individual Simple Linear Regression models as evidenced by the respective R^2 values
### While 'full value property tax rate' was a statistically important predictor for crim in the Simple Linear Regression model, this was not the case in the Multiple Linear Regression Model
### Also, 'distance to 5 Boston employment centers' has come up as a staistically important predictor in the Multiple Linear Regression model, which was not the case in the Simple Linear Regression model
par(mfrow=c(1,1))
plot(reg_coeff$MLR,reg_coeff$SLR)
text(x=reg_coeff$MLR,y=reg_coeff$SLR+1,labels=reg_coeff$NAME,cex=0.5)

### Is their evidence of non linear association between any predictor and crim?

boston.zn.fit.nonlin <- lm(crim~poly(zn,3))
summary(boston.zn.fit.nonlin)
### Since p-value for degrees 2 and 3 are not sufficiently small, there is no non-linear relationship between proportion of residential land zoned and per capita crime rate

boston.indus.fit.nonlin <- lm(crim~poly(indus,3))
summary(boston.indus.fit.nonlin)
### Since p-value for degree 3 is sufficiently small, there is a non-linear relationship between proportion of non-retail business acres and per capita crime rate

boston.chas.fit.nonlin <- lm(crim~chas+I(chas^2)+I(chas^3))
summary(boston.chas.fit.nonlin)
### Since predictor chas has only 2 unique values, we cannot fit a model with a 2nd and 3rd degree polynomial of chas

boston.nox.fit.nonlin <- lm(crim~poly(nox,3))
summary(boston.nox.fit.nonlin)
### Since p-value for degrees 2 and 3 are sufficiently small, there is a non-linear relationship between nitrogen oxide concentration and per capita crime rate

boston.rm.fit.nonlin <- lm(crim~poly(rm,3))
summary(boston.rm.fit.nonlin)
### Since p-value for degrees 2 and 3 are not sufficiently small, there is no non-linear relationship between average number of rooms per house and per capita crime rate

boston.age.fit.nonlin <- lm(crim~poly(age,3))
summary(boston.age.fit.nonlin)
### Since p-value for degree 2 is sufficiently small, there is a non-linear relationship between proportion of owner occupied units built before 1940 and per capita crime rate

boston.dis.fit.nonlin <- lm(crim~poly(dis,3))
summary(boston.dis.fit.nonlin)
### Since p-value for degrees 2 and 3 are sufficiently small, there is a non-linear relationship between distance to 5 Boston employment centers and per capita crime rate

boston.rad.fit.nonlin <- lm(crim~poly(rad,3))
summary(boston.rad.fit.nonlin)
### Since p-value for degrees 2 and 3 are not sufficiently small, there is no non-linear relationship between accessibility to radial hwy and per capita crime rate

boston.tax.fit.nonlin <- lm(crim~poly(tax,3))
summary(boston.tax.fit.nonlin)
### Since p-value for degree 2 is sufficiently small, there is a non-linear relationship between full value property tax rate and per capita crime rate

boston.ptratio.fit.nonlin <- lm(crim~poly(ptratio,3))
summary(boston.ptratio.fit.nonlin)
### Since p-value for degrees 2 and 3 are not sufficiently small, there is no non-linear relationship between pupil to teacher ratio and per capita crime rate

boston.black.fit.nonlin <- lm(crim~poly(black,3))
summary(boston.black.fit.nonlin)
### Since p-value for degrees 2 and 3 are not sufficiently small, there is no non-linear relationship between proportion of blacks in town and per capita crime rate

boston.lstat.fit.nonlin <- lm(crim~poly(lstat,3))
summary(boston.lstat.fit.nonlin)
### Since p-value for degrees 2 and 3 are not sufficiently small, there is no non-linear relationship between lower status of population and per capita crime rate

boston.medv.fit.nonlin <- lm(crim~poly(medv,3))
summary(boston.medv.fit.nonlin)
### Since p-value for degrees 2 and 3 are sufficiently small, there is a non-linear relationship between median value of owner occupied homes and per capita crime rate

### Thus, the predictors indus, nox, age, tax, medv have a non-linear relationship with crim


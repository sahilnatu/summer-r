############
# Analysis on Boston dataset
############

library(MASS)
boston_df <- Boston

n_rows <- nrow(boston_df)
n_col <- ncol(boston_df)
?Boston

###  Make some pairwise scatter-plots of the predictors (columns) in this data set. Describe your findings.
pairs(boston_df[,c('lstat','crim','rm','medv')])
### lstat,medv | medv,rm | crim, medv | crim, lstat


### Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
### yes -> crim
plot(boston_df$medv~boston_df$crim,xlab='per capita crime rate',ylab='median value of home in \\$1000',main='Median Value of Homes vs Per Capita Crime Rate')


###  Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
par(mfrow = c(3,1))
boxplot(boston_df$crim,horizontal = TRUE,xlab='per capita crim rate')
text(x=median(boston_df$crim),y=1.4,labels=round(median(boston_df$crim),2))
boxplot(boston_df$tax,horizontal = TRUE,xlab='property tax rate per \\$10,000')
text(x=median(boston_df$tax),y=1.4,labels=round(median(boston_df$tax),2))
boxplot(boston_df$ptratio,horizontal = TRUE,xlab='pupil to teacher ratio')
text(x=median(boston_df$ptratio),y=1.4,labels=round(median(boston_df$ptratio),2))
### some suburbs have particularly high per capita crime rate as evidenced by the boxplot. while the median is 0.26 crimes per capita, some suburbs have crim rate as high as 88.97

###  How many of the suburbs in this data set bound the Charles river?
sum(boston_df$chas == 1)
### 35 suburbs

### What is the median pupil-teacher ratio among the towns in this data set?
median(boston_df$ptratio)
### 19.05

### Which suburb of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.
index_for_min_medv <- which.min(boston_df$medv)
par(mfrow = c(4,4))
boxplot(boston_df$crim,horizontal = TRUE,xlab='per capita crim rate')
text(x=boston_df$crim[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$crim[which.min(boston_df$medv)],2))
boxplot(boston_df$zn,horizontal = TRUE,xlab='residential zoning proportion')
text(x=boston_df$zn[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$zn[which.min(boston_df$medv)],2))
boxplot(boston_df$indus,horizontal = TRUE,xlab='non retail business acres')
text(x=boston_df$indus[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$indus[which.min(boston_df$medv)],2))
boxplot(boston_df$chas,horizontal = TRUE,xlab='bounded by Charles river')
text(x=boston_df$chas[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$chas[which.min(boston_df$medv)],2))
boxplot(boston_df$nox,horizontal = TRUE,xlab='NOx concentration')
text(x=boston_df$nox[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$nox[which.min(boston_df$medv)],2))
boxplot(boston_df$rm,horizontal = TRUE,xlab='rooms per home')
text(x=boston_df$rm[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$rm[which.min(boston_df$medv)],2))
boxplot(boston_df$age,horizontal = TRUE,xlab='proportion of homes built <1940')
text(x=boston_df$age[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$age[which.min(boston_df$medv)],2))
boxplot(boston_df$dis,horizontal = TRUE,xlab='distance to employment center')
text(x=boston_df$dis[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$dis[which.min(boston_df$medv)],2))
boxplot(boston_df$rad,horizontal = TRUE,xlab='accessibility to radial hwy')
text(x=boston_df$rad[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$rad[which.min(boston_df$medv)],2))
boxplot(boston_df$tax,horizontal = TRUE,xlab='prop tax rate \\$10,000')
text(x=boston_df$tax[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$tax[which.min(boston_df$medv)],2))
boxplot(boston_df$ptratio,horizontal = TRUE,xlab='pupil-teacher ratio')
text(x=boston_df$ptratio[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$ptratio[which.min(boston_df$medv)],2))
boxplot(boston_df$black,horizontal = TRUE,xlab='proportion of blacks')
text(x=boston_df$black[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$black[which.min(boston_df$medv)],2))
boxplot(boston_df$lstat,horizontal = TRUE,xlab='proportion of lower status population')
text(x=boston_df$lstat[which.min(boston_df$medv)],y=1.4,labels=round(boston_df$lstat[which.min(boston_df$medv)],2))
### The suburb at index location 399 in the data set has the lowest median house value. Looking at the boxplots, it is apparent that this suburb has higher than average
### - Per Capita Crime Rate
### - NOx Concentration
### - Proportion of Houses built before 1940
### And, lower than average
### - Rooms per House
### Intuitively, the observations suggest that the houses in such a suburb are less desirable and thus are valued lower.

### In this data set, how many of the suburbs average more than seven rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.
sum(boston_df$rm>7)
sum(boston_df$rm>8)
boston_df_subset <- boston_df[boston_df$rm>8,]
result <- data.frame(all_suburbs = apply(boston_df,2,median), subset_suburbs = apply(boston_df_subset,2,median))
### In this data set, 64 suburbs average more than 7 rooms per dwelling and 13 suburbs average more than 8 rooms per dwelling.
### Looking at the table comparing medians for each of the attributes among the full data and data for suburbs averaging over 8 rooms per dwelling, we can conclude the following -
### - Suburbs averaging over 8 rooms per dwelling have higher than average per capita crim rate and median house value
### - Suburbs averaging over 8 rooms per dwelling have lower than average proportion of non-retail business acres, property tax rate, lower status of population

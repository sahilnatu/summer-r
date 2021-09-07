rm(list = ls())

df <- read.csv(file = 'MidCity.csv')
dim(df)
names(df)
summary(df)


MidCity.fit.1 <- lm(Price~., data = df)
summary(MidCity.fit.1)

# Upon performing multiple linear regression on the data with Price to be predicted as a linear function of all other variables, the p-value for Brick is very small, thereby we reject the Null Hypothesis
# Also, the coefficient of Brick (Yes = 1 and No = 0) is postive. In fact, assuming all other predictors are unchanged, brick houses command a premium of USD 15601.82 over non-brick houses

df$Nbhd.2 = ifelse(df$Nbhd==2,1,0)
df$Nbhd.3 = ifelse(df$Nbhd==3,1,0)

Midcity.fit.2 <- lm(Price~Nbhd.2+Nbhd.3, data = df)
summary(Midcity.fit.2)

# Here, we perform a linear regression on the data with Price to be predicted as a linear function of Nbhd.2 and Nbhd.3 variables
# Nbhd has 3 levels and hence we make 2 dummy variables to run linear regression
# Nbhd.3 == 1 then Nbhd is 3
# Nbhd.2 == 1 then Nbhd is 2
# Nbhd.2 == Nbhd.3 == 0 then Nbhd is 1
# The p-value for Nbhd.3 is very small, thus we reject the Null Hypothesis
# Also, the coefficient of Nbhd.3 is positive and greater than that of Nbhd.2, thus houses in neighborhood 3 command premium over the other houses

df.nbhd3 <- df[df$Nbhd=='3',]
Midcity.fit.3 <- lm(Price~Brick, data = df.nbhd3)
summary(Midcity.fit.3)

# We first take subest of the data for neighborhood 3 and then perform linear regression with Brick as the predictor
# The p-value is very small, thus we reject the Null Hypothesis
# Also, the coefficient of Brick (Yes = 1 and No = 0) is positive, and a brick house commands a premium of USD 26970 over a non brick house in neighborhood 3

df$Nbhd.Modified <- ifelse(df$Nbhd=='3','3','older')
MidCity.fit.4 <- lm(Price~Nbhd.Modified, data = df)
summary(MidCity.fit.4)

# Yes, the neighborhoods 1 and 2 can be combined together as 'older' and we still arrive at the same conclusion that the houses in neighborhood 3 command a premium over the houses in older neighborhoods 1 and 2.
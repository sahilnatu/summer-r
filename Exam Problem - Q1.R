rm(list = ls())

df <- read.csv(file = "BeautyData.csv")
dim(df)
names(df)
summary(df)

Beauty.fit <- lm(CourseEvals~BeautyScore, data = df)
summary(Beauty.fit)
# Beauty has a very low p-value, thereby Null Hypothesis is rejected
# Beauty has a positive coefficient meaning that a unit increase in the beauty score leads to a 0.27 increase in course evaluation
# Beauty explains 16.57% of the variance in the Course Evaluations

Beauty.fit.multiple <- lm(CourseEvals~., data = df)
summary(Beauty.fit.multiple)
# Beauty has a very low p-value again, thereby Null Hypothesis is rejected
# Beauty has a positive coefficient meaning that Course Evaluations are indeed positively correlated with Beauty
# Keeping everything else constant, a unit increase in beauty score leads to 0.3 increase in course evaluation
# Using all models, 34% of the variance in Course Evaluations can be explained, which is more than just 16.57% variance explained by beauty alone

# Thus, Dr. Hamermesh is right in pointing out that beauty plays a role in course evaluations, and by extension in labor income
# However, beauty alone is not the determinant of course evaluations, there are multiple other factors that affect course evaluations as well
# What Dr. Hamermesh means by the sentence "Disentangling whether this outcome represents productivity or discrimination is, as with the issue generally, probably impossible" is that he is puzzled whether beauty leads to more productivity in professors and thus higher evaluation (and income), or whether the professors are being discriminated on the basis of beauty and thus the poor evaluation (and income).
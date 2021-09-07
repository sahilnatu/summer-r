rm(list = ls())

### Q1

# In most of the cases, cities with more crime have more police personnnel on the streets. As such, a simple regression to predict crime based on police would yeild a positive correlation between the two meaning that more police equals more crime.
# While there is correlation, there is no causation to prove that more police leads to more crime, in fact it sounds counter intuitive as we have more police because of more crime
# Thus, since we cannot determine any causal relationship between police and crime, we cannot run a simple regression as it would throw up bizzare results such as the lesser police personnel you deploy on the streets, the lower would be the crime rate.

### Q2

# The researchers studied an example from Washington DC where additional police personnel are deployed on the streets irrespective of the crime due to other factors such as the terror alert level.
# When the terror alert level rises to orange, additional police personnel are deployed on the streets, and in turn crime drops as well
# Thus they were able to build a case for a causal relationship between police and crime where rise in police reduces crime
# The Table 2 shows negative coefficient for high alert for both regressions, thereby corroborating the finding that crime reduces on high alert days.
# In the 2nd column of this table, we see that the coefficient for metro ridership is positive, meaning that increased metro ridership (hypothesized as more tourists in the city) leads to more crime.


### Q3

# The researchers studied the metro ridership to test the hypothesis that less tourists went out in the city on high alert days and that led to drop in crime rates
# As it turns out, the metro ridership numbers remained unaffected on high alert days, thus nullifying this hypothesis.
# Therefore, the researchers were able to make a strong case for a causal relationship between more police on the streets and less crime

### Q4

# The first column in Table 4 provides the Robust Regression Coefficients for multiple variables. Thus, the model used here is Robust Regression.
# Robust Regression weighs each observation differently so as to reduce the impact of outliers on the final model.
# It also helps identify influential observations - such as the observation that high alert in District 1 has more substantial effect on reduction of crime than high alert in the other districts.
# This is inferred based on the magnitude of the coefficients of the first two variables in the table.
# To conclude, high alert leads to a reduction in crime rate with greater reduction in crime seen when this alert is issued in District 1.
# This conclusion seems intuitive as all the high profile targets in Washington DC are within the first police district, and a higher threat level in this district would lead to more deployment of police personnel on the streets, thereby reducing the crime the largest.

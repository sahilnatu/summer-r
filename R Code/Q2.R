rm(list = ls())
library(dplyr)
library(stringr)
library(reshape2)

df <- read.csv("ABIA.txt", header = TRUE, sep = ",")
dim(df)
colnames(df)
sapply(df,class)
head(df)

for (i in c(5,6,7,8,10)){
  df[,i] <- as.character(df[,i])
  print(class(df[,i]))
}

df$type <- ifelse(df$Origin=="AUS","dep","arr")

df$DepHour <- substr(str_pad(df$DepTime,width=4,side="left",pad="0"),1,2)
df$ArrHour <- substr(str_pad(df$ArrTime,width=4,side="left",pad="0"),1,2)

### Busiest time of the year
flights_by_month <- df %>% group_by(Month) %>% tally()
plot(flights_by_month$n, type="o",
        main = "Busiest Month", xlab = "Months", ylab = "Number of Flights")
# Spring and Summer are the busiest times of the year at the Austin airport with a maximum of 9k flights handled in Jun.

### Busiest day of the week
flights_by_weekday <- df %>% group_by(DayOfWeek) %>% tally()
plot(flights_by_weekday$n, type="o",
     main = "Busiest Weekday", xlab = "Weekdays", ylab = "Number of Flights")
# All 5 working days, Mon to Fri are equally busy at the airport with a fall in flights on Sat and a slight bounce back on Sun.

### Most popular airlines
flight_by_carriers <- df %>% group_by(UniqueCarrier) %>% tally()
barplot(flight_by_carriers$n, names.arg = flight_by_carriers$UniqueCarrier,
        main = "Most Popular Flights", xlab = "Airlines", ylab = "Number of Flights")
# WN is the most popular airlines with over 30k flights in 2008. This is followed by AA and CO.

### Departure Delay Arrival delay capping at 0
# Since a negative delay isn't a bad thing, we will cap the negative delay at 0 for better evaluation
df$DepDelayCap <- ifelse(df$DepDelay<0,0,df$DepDelay)
df$ArrDelayCap <- ifelse(df$ArrDelay<0,0,df$ArrDelay)

### Case 1 - Let us look at delays in departure due to bad weather.
# We check for delay in departure due to bad weather across multiple months
df_scen1 <- df[df$type=="dep" & df$Cancelled=="0",]
df_scen1$WeatherDelay[is.na(df_scen1$WeatherDelay)] <-  0
weatherdelay_by_month <- df_scen1 %>% group_by(Month) %>% summarize(WeatherDelay = mean(WeatherDelay))
plot(weatherdelay_by_month$WeatherDelay, type="o",
     main = "Avg Weather related Delays (min)", xlab = "Months", ylab = "Avg Delay in Mins")
# While this provides us with average delay in minutes due to bad weather across all months, we want to further see the frequency of weather related delay occurrences in each month.
weatherdelay_by_month$delayed <- df_scen1[df_scen1$WeatherDelay>0,] %>% group_by(Month) %>% tally()
weatherdelay_by_month$all <- df_scen1 %>% group_by(Month) %>% tally()
weatherdelay_by_month$delay_freq <- weatherdelay_by_month$delayed$n / weatherdelay_by_month$all$n
plot(weatherdelay_by_month$delay_freq*100, type="o",
     main = "Flights delayed by Bad Weather", xlab = "Months", ylab = "% of Flights Delayed")
# We find similar results to the previous graph with a maximum delays occurring in Mar. There is 1 weather related delay among every 65th flight in March.
# With March being the month with most weather related delays, we check for frequency of delays in March.
df_scen1$WeatherDelayInd <- ifelse(df_scen1$WeatherDelay>0,1,0)
weatherdelay_in_march <- df_scen1[df_scen1$Month==3,] %>% group_by(DayofMonth) %>% summarize(count = sum(WeatherDelayInd))
weatherdelay_in_march$all <- df_scen1[df_scen1$Month==3,] %>% group_by(DayofMonth) %>% tally()
weatherdelay_in_march$delay_freq <- weatherdelay_in_march$count / weatherdelay_in_march$all$n
plot(weatherdelay_in_march$delay_freq*100, type="o",
     main = "Flights delayed by Bad Weather in March", xlab = "Days of Month", ylab = "% of Flights Delayed")
# Since flights were delayed for almost all days across the month (barring a week at the end), the delays cannot be attributed to a standalone weather event. Thus we can conclude that March has the highest number of weather related delays at the Austin airport.
# We can further break down weather related delays in March by time of the day to figure out flights from which hours are most likely to be affected by bad weather.
weatherdelay_in_march_2 <- df_scen1[df_scen1$Month==3,] %>% group_by(DayofMonth,DepHour) %>% summarize(count = sum(WeatherDelayInd))
weatherdelay_in_march_2$all <- df_scen1[df_scen1$Month==3,] %>% group_by(DayofMonth,DepHour) %>%  tally()
weatherdelay_in_march_2$delay_freq <- weatherdelay_in_march_2$count / weatherdelay_in_march_2$all$n
weatherdelay_in_march_2$DepHour <- as.numeric(weatherdelay_in_march_2$DepHour)
weatherdelay_in_march_2_mat <- as.matrix(recast(weatherdelay_in_march_2[,c(1,2,5)], DayofMonth ~ DepHour, id.var = c("DayofMonth", "DepHour")))
weatherdelay_in_march_2_mat[is.na(weatherdelay_in_march_2_mat)] <- 0
heatmap(weatherdelay_in_march_2_mat[,-1], Rowv=NA, Colv=NA,main="Delays by hour and day in March",xlab="Hour of the Day",ylab="Day of the Month")
# We can infer from the heatmap that majority of the weather related delays in March have happened before noon and none have happened during the night time.
# We can plot a similar heatmap for all months
weatherdelay_by_month_2 <- df_scen1 %>% group_by(Month,DepHour) %>% summarize(count = sum(WeatherDelayInd))
weatherdelay_by_month_2$all <- df_scen1 %>% group_by(Month,DepHour) %>%  tally()
weatherdelay_by_month_2$delay_freq <- weatherdelay_by_month_2$count / weatherdelay_by_month_2$all$n
weatherdelay_by_month_2$DepHour <- as.numeric(weatherdelay_by_month_2$DepHour)
weatherdelay_by_month_2_mat <- as.matrix(recast(weatherdelay_by_month_2[,c(1,2,5)], Month ~ DepHour, id.var = c("Month", "DepHour")))
weatherdelay_by_month_2_mat[is.na(weatherdelay_by_month_2_mat)] <- 0
heatmap(weatherdelay_by_month_2_mat[,-1], Rowv=NA, Colv=NA,main="Delays by hour and month",xlab="Hour of the Day",ylab="Month")
# When looking at data for all the months, we still see a trend of no weather related delays during the night hours with delays concentrated in the morning hours right before noon and in the late evening hours.

### Case 2 - Let us look at flight cancellations and how those cancellations are spread out in our data.
# We will now look at flight cancellations by various carriers
can_by_carriers <- df %>% group_by(UniqueCarrier) %>% summarize(total_cancellations = sum(as.numeric(Cancelled)), avg_cancellations = mean(as.numeric(Cancelled)))
barplot(can_by_carriers$total_cancellations, names.arg = can_by_carriers$UniqueCarrier,
        main = "Carriers with Most Cancellations", xlab = "Airlines", ylab = "Number of Cancelled Flights")
# AA has the dubious distinction of being the carrier with most cancelled flights. However, this is an incomplete picture since all carriers may not be flying as many flights in and out of Austin as AA. Hence, we look at cancellation frequency among the carriers.
barplot(can_by_carriers$avg_cancellations*100, names.arg = can_by_carriers$UniqueCarrier,
        main = "Carriers with Most Frequent Cancellations", xlab = "Airlines", ylab = "% of Flights Cancelled")
# It is now apparent that MQ has the highest frequency of cancellations with 1 in every 16 of its flights getting cancelled.
# Another important metric to look at when studying flight cancellations is frequency of cancellation on routes that do not have more than 1 daily connection. A cancelled flight on such a route means waiting at least a day before the next flight. We will call such routes as infrequent routes.
df$Route <- str_c(df$Origin,'-',df$Dest)
routes_agg <- df %>% group_by(Month,DayofMonth,Route,Origin,Dest) %>% tally()
routes_agg <- routes_agg[routes_agg$n <= 1,]
infrequent_routes <- routes_agg$Route
df_infrequent_routes <- df[df$Route %in% infrequent_routes,]
can_by_carriers_2 <- df_infrequent_routes %>% group_by(UniqueCarrier) %>% summarize(total_cancellations = sum(as.numeric(Cancelled)), avg_cancellations = mean(as.numeric(Cancelled)))
barplot(can_by_carriers_2$total_cancellations, names.arg = can_by_carriers_2$UniqueCarrier,
        main = "Carriers with Most Cancellations", xlab = "Airlines", ylab = "Number of Cancelled Flights")
# Again, AA pops up as the carrier with most cancelled flights. We further look at carrier wise flight cancellation frequency for infrequent routes.
barplot(can_by_carriers_2$avg_cancellations*100, names.arg = can_by_carriers_2$UniqueCarrier,
        main = "Carriers with Most Frequent Cancellations", xlab = "Airlines", ylab = "% of Flights Cancelled")
# AA and 9E are carriers with highest cancellation frequency for infrequent routes. Surprisingly, MQ, which had the worst cancellation rate overall, does not feature here as it does not serve any infrequent route.
# EV and NW have had no cancellations serving infrequent routes, thus these carriers can be relied upon when traveling to or from uncommon destinations.


### Case 3 - Let us look at Normalized Delay (with respect to total flight time) across multiple variables
# Not all delays are equal. A 30min delay on a Transatlantic flight is not the same as a 30min delay on a short route like Austin - Dallas or Austin - Houston. We will normalize the delay by dividing it by total flight duration. This gives us delay per unit route duration.
# Note: We calculate total normalized delay as sum of normalized departure and arrival delays. While these 2 delays cannot be added directly as a delay in departure will inadvertently lead to delayed arrival, we still proceed with this formula to penalize flights that were not able to compensate their delayed departure in flight as compared to those flights which had a delayed departure but were able to compensate for the delay by arriving before the estimated time of arrival based on the route flight time.
df$NormalizedArrDelay = ifelse(is.na(df$ArrDelayCap/df$ActualElapsedTime),0,df$ArrDelayCap/df$ActualElapsedTime)
df$NormalizedDepDelay = ifelse(is.na(df$DepDelayCap/df$ActualElapsedTime),0,df$DepDelayCap/df$ActualElapsedTime)
df$NormalizedTotalDelay = df$NormalizedArrDelay + df$NormalizedDepDelay
delay_by_carriers = df %>% group_by(UniqueCarrier) %>% summarize(avg_total_delay = mean(NormalizedTotalDelay), avg_dep_delay = mean(NormalizedDepDelay), avg_arr_delay = mean(NormalizedArrDelay))
barplot(delay_by_carriers$avg_total_delay*60, names.arg = delay_by_carriers$UniqueCarrier,
        main = "Carriers with Highest Normalized Delay", xlab = "Airlines", ylab = "Avg Normalized Delay in seconds")
# As evident from the bar plot, MQ has the highest normalized delays to the tune of 20sec for every 1min of flight time. US has the least delays.
# We can further look at routes with the most amount of normalized delays, to and from Austin.
delay_by_routes = df %>% group_by(Route,Origin,Dest,type) %>% summarize(avg_total_delay = mean(NormalizedTotalDelay), avg_dep_delay = mean(NormalizedDepDelay), avg_arr_delay = mean(NormalizedArrDelay))
# 1) Routes with highest delays flying out of Austin
delay_by_routes_dep <- delay_by_routes[delay_by_routes$type == "dep",][order(delay_by_routes[delay_by_routes$type == "dep",]$avg_total_delay, decreasing=TRUE),]
barplot(delay_by_routes_dep$avg_total_delay[1:5]*60, names.arg = delay_by_routes_dep$Route[1:5],
        main = "Top 5 Austin Departures with Highest Normalized Delay", xlab = "Routes", ylab = "Avg Normalized Delay in seconds")
# The plot throws up AUS-DSM as the most delayed route with a delay of 2 mins for every 1 min of flight time. This looks fishy, and hence upon further inspection, it turns out that the data has just 1 flight that has flown this route, and the extraordinary delay belongs to this flight. Since we cannot generalize with such a small data sample, AUS-HOU is the next most delayed route with high number of routine flights and an average delay of 20sec for every 1 minute of flight time.
# 2) Routes with highest delays flying into Austin
delay_by_routes_arr <- delay_by_routes[delay_by_routes$type == "arr",][order(delay_by_routes[delay_by_routes$type == "arr",]$avg_total_delay, decreasing=TRUE),]
barplot(delay_by_routes_arr$avg_total_delay[1:5]*60, names.arg = delay_by_routes_arr$Route[1:5],
        main = "Top 5 Austin Arrivals with Highest Normalized Delay", xlab = "Routes", ylab = "Avg Normalized Delay in seconds")
# As we can see, flights from TYS have the highest delays averaging over 1 min of delay for every 1 min of flight time. However, there were only 3 flights from TYS to AUS in the year, and hence the sample is too small to generalize that route as the most delayed at arrival in Austin. Hence, we look at the next route from OKC to AUS. Flights from OKC to AUS are delayed by an average of 40sec for every 1 min of flight time. Thus, this is the most delayed route coming in to Austin.
# OKC and HOU have featured in both the plots and thus we can say with confidence that flights to and from OKC and HOU are the most delayed relative to their flight times.
# We can further look at normalized delays across various months.
delay_by_months = df %>% group_by(Month) %>% summarize(avg_total_delay = mean(NormalizedTotalDelay), avg_dep_delay = mean(NormalizedDepDelay), avg_arr_delay = mean(NormalizedArrDelay))
barplot(delay_by_months$avg_total_delay*60, names.arg = delay_by_months$Month,
        main = "Months with Highest Normalized Delay", xlab = "Months", ylab = "Avg Normalized Delay in seconds")
# The average normalized delays are the highest in December and March, i.e. during the holiday season and in Spring.

### Case 4 - Let us try to find answers to particular questions a traveler may have using the data we have.
# a) Which carriers could be avoided during the Holiday Season?
# Any flight that gets cancelled or delayed during the Holiday Season puts a damper on one's spirits. Hence one might want to avoid such flights. We specifically look for cancellations and/or delays between Dec 20 and Jan 10 to answer this question.
df_holiday <- df[(df$Month==12 & df$DayofMonth>=20) | (df$Month==1 & df$DayofMonth<=10),]
holiday_avoid <- df_holiday %>% group_by(UniqueCarrier) %>% summarize(avg_normalized_delay = mean(NormalizedTotalDelay), total_cancellations = sum(as.numeric(Cancelled)))
holiday_avoid$all <- df_holiday %>% group_by(UniqueCarrier) %>% tally()
holiday_avoid$can_freq <- holiday_avoid$total_cancellations / holiday_avoid$all$n
barplot(holiday_avoid$avg_normalized_delay*60, names.arg = holiday_avoid$UniqueCarrier,
        main = "Normalized Delays during Holiday Season", xlab = "Airlines", ylab = "Avg Normalized Delay in seconds")
# CO has the highest delays, an average of over 25sec per 1 minute of flight time. NW has the least average delay. The observations are different from those we obtained earlier when we had considered the data for all months. MQ was the worst performing carrier overall, but that has been replaced by CO during the holiday season, followed by WN.
barplot(holiday_avoid$can_freq*100, names.arg = holiday_avoid$UniqueCarrier,
        main = "Frequency of Flight Cancellations during Holiday Season", xlab = "Airlines", ylab = "% of Flights Cancellled")
# Upon looking at flight cancellations, it is evident that MQ has the highest cancellations at 5% of their total scheduled flights, followed by UA.
# Thus some of the carriers to avoid during the holiday season are CO, WN, MQ and UA. One can expect and smoother start or end to their holiday travels by hopping onto a flight operated by NW, US or EV.

# b) Which overnight flights could be avoided?
# Overnight flights or red-eye flights are the ones that have a substantial part of their journey during the night hours. Travelers generally prefer these flights to avoid spending precious daylight hours in traveling. Any delay in these flights not only throws off the plans, but also causes sleep deprivation as a traveler might be expecting to sleep during these hours in the flight. We look at departure delays in flights that depart between 11:00 PM and 3:59 AM to answer this question.
df_overnight <- df[(as.numeric(df$DepHour)>=23 | as.numeric(df$DepHour)<=3),]
overnight_avoid <- df_overnight %>% group_by(UniqueCarrier) %>% summarize(avg_dep_delay = mean(NormalizedDepDelay))
barplot(overnight_avoid$avg_dep_delay*60, names.arg = overnight_avoid$UniqueCarrier,
        main = "Normalized Departure Delays during Overnight Flights", xlab = "Airlines", ylab = "Avg Normalized Delay in seconds")
# MQ and WN have the highest normalized delays at departure for overnight flights.
# Departure delays in overnight flights are a cause of concern irrespective of the duration of the flight that is to be boarded. Thus, we also need to look at absolute departure delays in overnight flights and not just the normalized delays.
df$DepDelayCap <- ifelse(is.na(df$DepDelayCap),0,df$DepDelayCap)
overnight_avoid$abs <- df_overnight %>% group_by(UniqueCarrier) %>% summarize(avg_dep_delay = mean(DepDelayCap))
barplot(overnight_avoid$abs$avg_dep_delay, names.arg = overnight_avoid$UniqueCarrier,
        main = "Absolute Departure Delays during Overnight Flights", xlab = "Airlines", ylab = "Avg Delay in minutes")
# With absolute delays considered, NW is the worst performing airline with highest average delay per flight at 5 hours, followed by B6 and F9. The best performing airlines for this metric are YV, AA, and CO. MQ and WN still show up average delays on the higher side (~4 hours).
# Thus, the carriers to avoid on overnight flights are NW, B6, F9, WN and MQ.

# c) Which flights could be avoided if you are flying out of Austin at the end of the day?
# Many travelers take a flight out of a city at the end of the day to save on accommodation costs for the night. A person can spend the entire day in the city and fly out in late evening hours. However, if such a flight is cancelled, the traveler seldom has options apart from staying the night in the city, which entails additional costs for accommodation over and above their planned trip budget. To answer this question, we will be looking at flight cancellations data for departures from Austin between 7:00 PM and 10:59 PM.
df_evening <- df[((as.numeric(df$DepHour)>=19 & as.numeric(df$DepHour)<=22) & df$Origin=="AUS"),]
evening_avoid <- df_evening %>% group_by(UniqueCarrier) %>% summarize(total_cancellations = sum(as.numeric(Cancelled)))
evening_avoid$all <- df_evening %>% group_by(UniqueCarrier) %>% tally()
evening_avoid$can_freq <- evening_avoid$total_cancellations / evening_avoid$all$n
barplot(evening_avoid$total_cancellations*100, names.arg = evening_avoid$UniqueCarrier,
        main = "Cancellation Frequency on Flights flying out of Austin in the EVening", xlab = "Airlines", ylab = "% of Flights Cancelled")
# It turns out that no flights flying out of Austin during these hours were cancelled in 2008. To further broaden our problem, we will look at delays on flight flying out of Austin during these hours. Again, we will look at absolute departure delay and not the normalized departure delay as any delay, irrespective of the flight time upon boarding, is a cause of concern when flying out of Austin at the end of the day.
evening_avoid$delay <- df_evening %>% group_by(UniqueCarrier) %>% summarize(avg_dep_delay = mean(DepDelay))
barplot(evening_avoid$delay$avg_dep_delay, names.arg = evening_avoid$UniqueCarrier,
        main = "Absolute Departure Delays for Flights flying out of Austin in the Evening", xlab = "Airlines", ylab = "Delay in minutes")
# 9E has the highest average delay of about 4 and a half hours per flight when flying out of Austin in late evening. This is followed by DL. These carriers could be avoided when flying out of Austin at the end of the day.

# d) Which flights to avoid if flying into Austin for work in the morning?
# Many professionals that work in Austin fly into the city during the morning hours. Any cancellations or delays on these flights would be detrimental to the objectives of these flyers. Thus, we look at the data for cancellations and delays on flights arriving into Austin between 8:00 AM and 10:59 AM.
df$ArrDelayCap <- ifelse(is.na(df$ArrDelayCap),0,df$ArrDelayCap)
df_morning <- df[((as.numeric(df$ArrHour)>=7 & as.numeric(df$ArrHour)<=10) & df$Dest=="AUS"),]
morning_avoid <- df_morning %>% group_by(UniqueCarrier) %>% summarize(avg_arr_delay = mean(ArrDelayCap), total_cancellations = sum(as.numeric(Cancelled)))
barplot(morning_avoid$avg_arr_delay, names.arg = morning_avoid$UniqueCarrier,
        main = "Absolute Arrival Delays for Flight flying into Austin in the Morning", xlab = "Airlines", ylab = "Delay in minutes")
# OH has the highest average delay of 8 minuted on flights arriving into Austin in the Morning. This is followed by MQ.
morning_avoid$all <- df_morning %>% group_by(UniqueCarrier) %>% tally()
morning_avoid$can_freq <- morning_avoid$total_cancellations / morning_avoid$all$n
barplot(morning_avoid$total_cancellations*100, names.arg = morning_avoid$UniqueCarrier,
        main = "Cancellation Frequency on Flights flying into Austin in the Morning", xlab = "Airlines", ylab = "% of Flights Cancelled")
# No flights flying into Austin in the Morning hours were cancelled in 2008. Thus, based on delays alone, professionals can avoid flying OH and MQ when flying into Austin in the Morning hours. However, the delays here are not as significant as those seen during other hours of the day.


# Energy Consumption analysis #
# Floriana Trama #
# Data analysis department #
# G1: Understand benefits of sub-metering and convince the home developer to become our client explaining advantages that he could get throught data analytics #
# March 2019 #
# Version 4 #



# Retrieve previous dataframe ---------------------------------------------

source(file = "Energy_V2.R")


# Calculating the average per YEAR ----------------------------------------

AvgYearAll <- EnergyConsumption1 %>%
  group_by(Year) %>% 
  filter(Year != 2010) %>% 
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power) * (1000/60))


# Calculating the average per MONTH ---------------------------------------

AvgMonthAll <- EnergyConsumption1 %>%
  filter(Year != 2010) %>% 
  group_by(Month) %>%
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power)* (1000/60))


# Calculating the average per WEEK ----------------------------------------

AvgWeekAll <- EnergyConsumption1 %>%
  filter(Year != 2010) %>%
  group_by(Week) %>%
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power)* (1000/60))


# Calculating the average per DAY -----------------------------------------

AvgDayAll <- EnergyConsumption1 %>%
  filter(Year != 2010) %>%
  mutate(Weekdays = wday(DateTime, label = T, abbr = F, week_start = 1, 
                         locale = Sys.getlocale("LC_TIME"))) %>% 
  group_by(Weekdays) %>%
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power)* (1000/60))


# Calculating the average per HOUR ----------------------------------------

AvgHourAll <- EnergyConsumption1 %>%
  filter(Year != 2010) %>%
  group_by(Hour) %>%
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power)* (1000/60))


# Subsets monthly averages per YEAR ---------------------------------------
# Calculating the average per MONTH ---------------------------------------

AvgYearMonthAll <- EnergyConsumption1 %>%
  filter(Year != 2010) %>% 
  group_by(Year, Month) %>%
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power)* (1000/60))

# 2007
AvgMonth2007 <- AvgYearMonthAll %>%
  filter(Year == 2007)

# 2008
AvgMonth2008 <- AvgYearMonthAll %>%
  filter(Year == 2008) 

# 2009
AvgMonth2009 <- AvgYearMonthAll %>%
  filter(Year == 2009) 


# Visualizations ----------------------------------------------------------
# Yearly patterns

plot_ly(AvgYearAll, x = ~AvgYearAll$Year, y = ~AvgYearAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~AvgYearAll$meanSub_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~AvgYearAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~AvgYearAll$meanSub_4, name = 'Other Appliances', mode = 'lines') %>%
  add_trace(y = ~AvgYearAll$meanGAP, name = 'GAP', mode = 'lines') %>%
  layout(title = "Power Consumption by Year",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (watt-hours)"))

ggplot(AvgYearAll, aes(x = AvgYearAll$Year, y = AvgYearAll$meanSub_1)) +
  theme_bw() + 
  geom_bar(stat="identity", color = "black", fill = "orange") + 
  labs(y = "Power (Watt-hours)",
       x = "Year",
       title = "Power Consumption by Year")


# Monthly patterns

plot_ly(AvgMonthAll, x = ~AvgMonthAll$Month, y = ~AvgMonthAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~AvgMonthAll$meanSub_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~AvgMonthAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~AvgMonthAll$meanSub_4, name = 'Other Appliances', mode = 'lines') %>%
  add_trace(y = ~AvgMonthAll$meanGAP, name = 'GAP', mode = 'lines') %>%
  layout(title = "Power Consumption by Month",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))

ggplot(AvgMonthAll, aes(x = AvgMonthAll$Month, y = AvgMonthAll$meanSub_1)) +
  theme_bw() + 
  geom_bar(position = "stack", stat = "identity", color = "black", fill = "orange") + 
  labs(y = "Power (Watt-hours)",
       x = "Month",
       title = "Power Consumption by Month")


# Weekly patterns

plot_ly(AvgWeekAll, x = ~AvgWeekAll$Week, y = ~AvgWeekAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~AvgWeekAll$meanSub_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~AvgWeekAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~AvgWeekAll$meanSub_4, name = 'Other Appliances', mode = 'lines') %>%
  add_trace(y = ~AvgWeekAll$meanGAP, name = 'GAP', mode = 'lines') %>%
  layout(title = "Power Consumption by Week",
         xaxis = list(title = "Week"),
         yaxis = list (title = "Power (watt-hours)"))


# Daily patterns

plot_ly(AvgDayAll, x = ~AvgDayAll$Weekdays, y = ~AvgDayAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~AvgDayAll$meanSub_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~AvgDayAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~AvgDayAll$meanSub_4, name = 'Other Appliances', mode = 'lines') %>%
  add_trace(y = ~AvgDayAll$meanGAP, name = 'GAP', mode = 'lines') %>%
  layout(title = "Power Consumption by Day",
         xaxis = list(title = "Day"),
         yaxis = list (title = "Power (watt-hours)"))


# Hourly patterns

plot_ly(AvgHourAll, x = ~AvgHourAll$Hour, y = ~AvgHourAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~AvgHourAll$meanSub_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~AvgHourAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~AvgHourAll$meanSub_4, name = 'Other Appliances', mode = 'lines') %>%
  add_trace(y = ~AvgHourAll$meanGAP, name = 'GAP', mode = 'lines') %>%
  layout(title = "Power Consumption by Hour",
         xaxis = list(title = "Hour"),
         yaxis = list (title = "Power (watt-hours)"))


# Differences in energy consumptions by YEAR
# GAP

plot_ly(AvgMonthAll, x = ~AvgMonthAll$Month, y = ~AvgMonth2007$meanGAP,
        name = 'GAP2007', 
        type = "scatter", 
        mode = "lines") %>%
  add_trace(y = ~AvgMonth2008$meanGAP, name = 'GAP2008', mode = 'lines') %>%
  add_trace(y = ~AvgMonth2009$meanGAP, name = 'GAP2009', mode = 'lines') %>%
  layout(title = "Differences in energy consumptions - GAP by Year",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))


# Meter 1

plot_ly(AvgMonthAll, x = ~AvgMonthAll$Month, y = ~AvgMonth2007$meanSub_1,
        name = 'Meter1-2007', 
        type = "scatter", 
        mode = "lines") %>%
  add_trace(y = ~AvgMonth2008$meanSub_1, name = 'Meter1-2008', mode = 'lines') %>%
  add_trace(y = ~AvgMonth2009$meanSub_1, name = 'Meter1-2009', mode = 'lines') %>%
  layout(title = "Differences in energy consumptions - Meter 1 by Year",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))


# Meter 2

plot_ly(AvgMonthAll, x = ~AvgMonthAll$Month, y = ~AvgMonth2007$meanSub_2,
        name = 'Meter22007', 
        type = "scatter", 
        mode = "lines") %>%
  add_trace(y = ~AvgMonth2008$meanSub_2, name = 'Meter22008', mode = 'lines') %>%
  add_trace(y = ~AvgMonth2009$meanSub_2, name = 'Meter22009', mode = 'lines') %>%
  layout(title = "Differences in energy consumptions - Meter 2 by Year",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))


# Meter 3

plot_ly(AvgMonthAll, x = ~AvgMonthAll$Month, y = ~AvgMonth2007$meanSub_3,
        name = 'Meter32007', 
        type = "scatter", 
        mode = "lines") %>%
  add_trace(y = ~AvgMonth2008$meanSub_3, name = 'Meter32008', mode = 'lines') %>%
  add_trace(y = ~AvgMonth2009$meanSub_3, name = 'Meter32009', mode = 'lines') %>%
  layout(title = "Differences in energy consumptions - Meter 3 by Year",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))


# Meter 4

plot_ly(AvgMonthAll, x = ~AvgMonthAll$Month, y = ~AvgMonth2007$meanSub_4,
        name = 'Meter42007', 
        type = "scatter", 
        mode = "lines") %>%
  add_trace(y = ~AvgMonth2008$meanSub_4, name = 'Meter42008', mode = 'lines') %>%
  add_trace(y = ~AvgMonth2009$meanSub_4, name = 'Meter42009', mode = 'lines') %>%
  layout(title = "Differences in energy consumptions - Meter 4 by Year",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))



# Forecast ----------------------------------------------------------------
# Forecast time series by month -------------------------------------------

# Sub-meter 1 -------------------------------------------------------------
# Make subset grouping by Year and Month 

AvgMonthAllTS <- EnergyConsumption1 %>%
  filter(Year != 2010) %>%
  group_by(Year, Month) %>%
  summarise(meanSub_1 = mean(Sub_metering_1)*(24*(365/12)),
            meanSub_2 = mean(Sub_metering_2)*(24*(365/12)),
            meanSub_3 = mean(Sub_metering_3)*(24*(365/12)),
            meanSub_4 = mean(Sub_metering_4)*(24*(365/12)),
            meanGAP = mean(Global_active_power)* (1000/60)*(24*(365/12)))


# Create TS object with sub-meter 1

TSAvgMonthSM1 <- ts(AvgMonthAllTS$meanSub_1, frequency = 12, start = c(2007,1))


# Plot sub-meter 1 with autoplot - add labels, color

autoplot(TSAvgMonthSM1, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 1")


# Plot sub-meter 1 with plot.tsn(same as before, just different layout)

plot.ts(TSAvgMonthSM1)


# Apply time series linear regression to the sub-meter 1

FitSM1 <- tslm(TSAvgMonthSM1 ~ trend + season)

summary(FitSM1)

sqrt(mean(FitSM1$residuals^2))


# Create sub-meter 1 forecast ahead 36 time periods with confidence levels 80 and 90 

forecastFitSM1 <- forecast(FitSM1, h=36, level=c(80,90))


# Plot sub-meter 1 forecast, limit y and add labels

plot(forecastFitSM1, ylab= "Watt-Hours", xlab="Year")


# Decompose sub-meter 1 into trend, seasonal and remainder

componentsTSAvgMonthSM1 <- decompose(TSAvgMonthSM1)


# Plot decomposed sub-meter 1 

plot(componentsTSAvgMonthSM1)


# Check summary statistics for decomposed sub-meter 1 

summary(componentsTSAvgMonthSM1)


# Seasonal adjusting sub-meter 1 by REMOVING the seasonal component & plot

TSAvgMonthSM1Adj <- TSAvgMonthSM1 - componentsTSAvgMonthSM1$seasonal

autoplot(TSAvgMonthSM1Adj)


# Test Seasonal Adjustment by running Decompose AGAIN
# (It is difficult to see if seasonality has been removed by looking at the plot 
# Let's try decompose again and see if the  seasonal component was removed
# Note the very, very small scale for Seasonal)

plot(decompose(TSAvgMonthSM1Adj))


# Holt Winters Exponential Smoothing & Plot
# Create ts object that contains exponentially smoothed data with no seasonality

TSAvgMonthSM1HW <- HoltWinters(TSAvgMonthSM1Adj, alpha=0.9, beta=0.2, gamma=FALSE)

plot(TSAvgMonthSM1HW)


# HoltWinters forecast & plot

TSAvgMonthSM1HWForecast <- forecast(TSAvgMonthSM1HW, h=25)

plot(TSAvgMonthSM1HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 1")


# Forecast HoltWinters with diminished confidence levels

TSAvgMonthSM1HWForecastC <- forecast(TSAvgMonthSM1HW, h=25, level=c(10,25))


# Plot only the forecasted area

plot(TSAvgMonthSM1HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))


# Sub-meter 2 -------------------------------------------------------------
# Create TS object with sub-meter 2

TSAvgMonthSM2 <- ts(AvgMonthAllTS$meanSub_2, frequency = 12, start = c(2007,1))


# Plot sub-meter 2 with autoplot - add labels, color

autoplot(TSAvgMonthSM2, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 2")


# Plot sub-meter 2 with plot.tsn(same as before, just different layout)

plot.ts(TSAvgMonthSM2)


# Apply time series linear regression to the sub-meter 2

FitSM2 <- tslm(TSAvgMonthSM2 ~ trend + season)

summary(FitSM2)

sqrt(mean(FitSM2$residuals^2))


# Create sub-meter 2 forecast ahead 36 time periods with confidence levels 80 and 90 

forecastFitSM2 <- forecast(FitSM2, h=36, level=c(80,90))


# Plot sub-meter 2 forecast, limit y and add labels

plot(forecastFitSM2, ylab= "Watt-Hours", xlab="Year")


# Decompose sub-meter 2 into trend, seasonal and remainder

componentsTSAvgMonthSM2 <- decompose(TSAvgMonthSM2)


# Plot decomposed sub-meter 2

plot(componentsTSAvgMonthSM2)


# Check summary statistics for decomposed sub-meter 2 

summary(componentsTSAvgMonthSM2)


# Seasonal adjusting sub-meter 2 by REMOVING the seasonal component & plot

TSAvgMonthSM2Adj <- TSAvgMonthSM2 - componentsTSAvgMonthSM2$seasonal

autoplot(TSAvgMonthSM2Adj)


# Test Seasonal Adjustment by running Decompose AGAIN
# (It is difficult to see if seasonality has been removed by looking at the plot 
# Let's try decompose again and see if the  seasonal component was removed
# Note the very, very small scale for Seasonal)

plot(decompose(TSAvgMonthSM2Adj))


# Holt Winters Exponential Smoothing & Plot
# Create ts object that contains exponentially smoothed data with no seasonality

TSAvgMonthSM2HW <- HoltWinters(TSAvgMonthSM2Adj, alpha=0.9, beta=0.2, gamma=FALSE)

plot(TSAvgMonthSM2HW)


# HoltWinters forecast & plot

TSAvgMonthSM2HWForecast <- forecast(TSAvgMonthSM2HW, h=25)

plot(TSAvgMonthSM2HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 2")


# Forecast HoltWinters with diminished confidence levels

TSAvgMonthSM2HWForecastC <- forecast(TSAvgMonthSM2HW, h=25, level=c(10,25))


# Plot only the forecasted area

plot(TSAvgMonthSM2HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))


# Sub-meter 3 -------------------------------------------------------------
# Create TS object with sub-meter 3

TSAvgMonthSM3 <- ts(AvgMonthAllTS$meanSub_3, frequency = 12, start = c(2007,1))


# Plot sub-meter 3 with autoplot - add labels, color

autoplot(TSAvgMonthSM3, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 3")


# Plot sub-meter 3 with plot.tsn(same as before, just different layout)

plot.ts(TSAvgMonthSM3)


# Apply time series linear regression to the sub-meter 3

FitSM3 <- tslm(TSAvgMonthSM3 ~ trend + season)

summary(FitSM3)

sqrt(mean(FitSM3$residuals^2))


# Create sub-meter 3 forecast ahead 36 time periods with confidence levels 80 and 90 

forecastFitSM3 <- forecast(FitSM3, h=36, level=c(80,90))


# Plot sub-meter 3 forecast, limit y and add labels

plot(forecastFitSM3, ylab= "Watt-Hours", xlab="Year")


# Decompose sub-meter 3 into trend, seasonal and remainder

componentsTSAvgMonthSM3 <- decompose(TSAvgMonthSM3)


# Plot decomposed sub-meter 3

plot(componentsTSAvgMonthSM3)


# Check summary statistics for decomposed sub-meter 3

summary(componentsTSAvgMonthSM3)


# Seasonal adjusting sub-meter 3 by REMOVING the seasonal component & plot

TSAvgMonthSM3Adj <- TSAvgMonthSM3 - componentsTSAvgMonthSM3$seasonal

autoplot(TSAvgMonthSM3Adj)


# Test Seasonal Adjustment by running Decompose AGAIN
# (It is difficult to see if seasonality has been removed by looking at the plot 
# Let's try decompose again and see if the  seasonal component was removed
# Note the very, very small scale for Seasonal)

plot(decompose(TSAvgMonthSM3Adj))


# Holt Winters Exponential Smoothing & Plot
# Create ts object that contains exponentially smoothed data with no seasonality

TSAvgMonthSM3HW <- HoltWinters(TSAvgMonthSM3Adj, alpha=0.9, beta=0.2, gamma=FALSE)

plot(TSAvgMonthSM3HW)


# HoltWinters forecast & plot

TSAvgMonthSM3HWForecast <- forecast(TSAvgMonthSM3HW, h=25)

plot(TSAvgMonthSM3HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 3")


# Forecast HoltWinters with diminished confidence levels

TSAvgMonthSM3HWForecastC <- forecast(TSAvgMonthSM3HW, h=25, level=c(10,25))


# Plot only the forecasted area

plot(TSAvgMonthSM3HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


# Sub-meter 4 -------------------------------------------------------------
# Create TS object with sub-meter 4

TSAvgMonthSM4 <- ts(AvgMonthAllTS$meanSub_4, frequency = 12, start = c(2007,1))


# Plot sub-meter 4 with autoplot - add labels, color

autoplot(TSAvgMonthSM4, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 4")


# Plot sub-meter 4 with plot.tsn(same as before, just different layout)

plot.ts(TSAvgMonthSM4)


# Apply time series linear regression to the sub-meter 4

FitSM4 <- tslm(TSAvgMonthSM4 ~ trend + season)

summary(FitSM4)

sqrt(mean(FitSM4$residuals^2))


# Create sub-meter 4 forecast ahead 36 time periods with confidence levels 80 and 90 

forecastFitSM4 <- forecast(FitSM4, h=36, level=c(80,90))


# Plot sub-meter 4 forecast, limit y and add labels

plot(forecastFitSM4, ylab= "Watt-Hours", xlab="Year")


# Decompose sub-meter 4 into trend, seasonal and remainder

componentsTSAvgMonthSM4 <- decompose(TSAvgMonthSM4)


# Plot decomposed sub-meter 4

plot(componentsTSAvgMonthSM4)


# Check summary statistics for decomposed sub-meter 4

summary(componentsTSAvgMonthSM4)


# Seasonal adjusting sub-meter 4 by REMOVING the seasonal component & plot

TSAvgMonthSM4Adj <- TSAvgMonthSM4 - componentsTSAvgMonthSM4$seasonal

autoplot(TSAvgMonthSM4Adj)


# Test Seasonal Adjustment by running Decompose AGAIN
# (It is difficult to see if seasonality has been removed by looking at the plot 
# Let's try decompose again and see if the  seasonal component was removed
# Note the very, very small scale for Seasonal)

plot(decompose(TSAvgMonthSM4Adj))


# Holt Winters Exponential Smoothing & Plot
# Create ts object that contains exponentially smoothed data with no seasonality

TSAvgMonthSM4HW <- HoltWinters(TSAvgMonthSM4Adj, alpha=0.9, beta=0.2, gamma=FALSE)

plot(TSAvgMonthSM4HW)


# HoltWinters forecast & plot

TSAvgMonthSM4HWForecast <- forecast(TSAvgMonthSM4HW, h=25)

plot(TSAvgMonthSM4HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 4")


# Forecast HoltWinters with diminished confidence levels

TSAvgMonthSM4HWForecastC <- forecast(TSAvgMonthSM4HW, h=25, level=c(10,25))


# Plot only the forecasted area

plot(TSAvgMonthSM4HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 4", start(2010))

 
# GAP ---------------------------------------------------------------------
# Create TS object with GAP

TSAvgMonthGAP <- ts(AvgMonthAllTS$meanGAP, frequency = 12, start = c(2007,1))


# Plot GAP with autoplot - add labels, color

autoplot(TSAvgMonthGAP, xlab = "Time", ylab = "Watt Hours", 
         main = "GAP")


# Plot GAP with plot.tsn(same as before, just different layout)

plot.ts(TSAvgMonthGAP)


# Apply time series linear regression to the GAP

FitGAP <- tslm(TSAvgMonthGAP ~ trend + season)

summary(FitGAP)

sqrt(mean(FitGAP$residuals^2))


# Create GAP forecast ahead 36 time periods with confidence levels 80 and 90 

forecastFitGAP <- forecast(FitGAP, h=36, level=c(80,90))


# Plot GAP forecast, limit y and add labels

plot(forecastFitGAP, ylab= "Watt-Hours", xlab="Year")


# Decompose GAP into trend, seasonal and remainder

componentsTSAvgMonthGAP <- decompose(TSAvgMonthGAP)


# Plot decomposed GAP

plot(componentsTSAvgMonthGAP)


# Check summary statistics for decomposed GAP

summary(componentsTSAvgMonthGAP)


# Seasonal adjusting GAP by REMOVING the seasonal component & plot

TSAvgMonthGAPAdj <- TSAvgMonthGAP - componentsTSAvgMonthGAP$seasonal

autoplot(TSAvgMonthGAPAdj)


# Test Seasonal Adjustment by running Decompose AGAIN
# (It is difficult to see if seasonality has been removed by looking at the plot 
# Let's try decompose again and see if the  seasonal component was removed
# Note the very, very small scale for Seasonal)

plot(decompose(TSAvgMonthGAPAdj))


# Holt Winters Exponential Smoothing & Plot
# Create ts object that contains exponentially smoothed data with no seasonality

TSAvgMonthGAPHW <- HoltWinters(TSAvgMonthGAPAdj, alpha=0.9, beta=0.2, gamma=FALSE)

plot(TSAvgMonthGAPHW)


# HoltWinters forecast & plot

TSAvgMonthGAPHWForecast <- forecast(TSAvgMonthGAPHW, h=25)

plot(TSAvgMonthGAPHWForecast, ylab= "Watt-Hours", xlab="Time - GAP")


# Forecast HoltWinters with diminished confidence levels

TSAvgMonthGAPHWForecastC <- forecast(TSAvgMonthGAPHW, h=25, level=c(10,25))


# Plot only the forecasted area

plot(TSAvgMonthGAPHWForecastC, ylab= "Watt-Hours", xlab="Time - GAP", start(2010))


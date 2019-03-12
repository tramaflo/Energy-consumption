# Energy Consumption analysis #
# Floriana Trama #
# Data analysis department #
# G1: Understand benefits of sub-metering and convince the home developer to become our client explaining advantages that he could get throught data analytics #
# March 2019 #
# Version 3 #


source(file = "Energy_V2.R")

library(ggfortify)
library(forecast)


# Plot all of sub-meter 1 -------------------------------------------------

plot(EnergyConsumption$Sub_metering_1)


# Subset the second week of 2008 - All Observations -----------------------

houseWeek <- filter(EnergyConsumption, Year == 2008 & Week == 2)


# Plot subset houseWeek ---------------------------------------------------

plot(houseWeek$Sub_metering_1)


# Subset the 9th day of January 2008 - All observations -------------------

houseDay <- filter(EnergyConsumption, Year == 2008 & Month == 1 & Day == 9)


# Plot sub-meter 1 --------------------------------------------------------

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        type = 'scatter', 
        mode = 'lines')


# Plot the 3 sub-meters by day - All obs ----------------------------------

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Subset the 9th day of January 2008 - 10 Minute frequency ----------------

houseDay10 <- filter(EnergyConsumption, Year == 2008 & Month == 1 & Day == 9 & 
                       (Minute == 0 | Minute == 10 | Minute == 20 | 
                        Minute == 30 | Minute == 40 | Minute == 50))


# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minu freq --
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Two additional visualizations -------------------------------------------


# Optional pie-chart ------------------------------------------------------


# Subset to one obs. per week on Mondays at 8:00pm for 2007, 2008, 2009 ---

house070809weekly <- filter(EnergyConsumption, Weekdays == 'lunedì' & Hour == 20 & Minute == 1)


# Create TS object with SubMeter3 -----------------------------------------

tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))


# Plot sub-meter 3 with autoplot ------------------------------------------

autoplot(tsSM3_070809weekly)


# Plot sub-meter 3 with autoplot - add labels, color ----------------------
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", 
         main = "Sub-meter 3")


# Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)


# Two more visualizations -------------------------------------------------


# Time series linear regression to Sub-met_3 and summary to get R2 --------

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)

summary(fitSM3)


# Create the forecast for sub-meter 3 and forecast ahead 20 time periods --

forecastfitSM3 <- forecast(fitSM3, h=20)


# Plot the forecast for sub-meter 3 ---------------------------------------

plot(forecastfitSM3)

# Create sub-meter 3 forecast with confidence levels 80 and 90 ------------

forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))


# Plot sub-meter 3 forecast, limit y and add labels -----------------------

plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


# Two more forecast -------------------------------------------------------

# Decompose Sub-meter 3 into trend, seasonal and remainder ----------------

components070809SM3weekly <- decompose(tsSM3_070809weekly)


# Plot decomposed sub-meter 3 ---------------------------------------------

plot(components070809SM3weekly)


# Check summary statistics for decomposed sub-meter 3 ---------------------

summary(components070809SM3weekly)


# Two more decomposed visualizations --------------------------------------


# Seasonal adjust sub-met_3 by subtracting the seasonal component&plot ----

tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal

autoplot(tsSM3_070809Adjusted)


# Test Seasonal Adjustment by running Decompose again ---------------------

plot(decompose(tsSM3_070809Adjusted))


# Holt Winters Exponential Smoothing & Plot -------------------------------

tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)

plot(tsSM3_HW070809, ylim = c(0, 25))


# HoltWinters forecast & plot ---------------------------------------------

tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)

plot(tsSM3_HW070809for, ylim = c(0, 20), 
     ylab= "Watt-Hours", 
     xlab="Time - Sub-meter 3")


# Forecast HoltWinters with diminished confidence levels ------------------

tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))


# Plot only the forecasted area -------------------------------------------

plot(tsSM3_HW070809forC, 
     ylim = c(0, 20), ylab= "Watt-Hours", 
     xlab="Time - Sub-meter 3", 
     start(2010))

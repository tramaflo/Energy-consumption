# Libraries ---------------------------------------------------------------

library(RMySQL)

library(dplyr)

library(lubridate)

library(tidyverse)

library(caret)

library(plotly)

library(ggfortify)

library(forecast)


# Importing data - create a dataset for Prophet ---------------------------

EnergyConsumption <- read_delim("C:/Users/T450S/Desktop/household_power_consumption.txt", 
                                ";", escape_double = FALSE, trim_ws = TRUE)

str(EnergyConsumption)

EnergyConsumption$Date<- strptime(as.character(EnergyConsumption$Date), "%d/%m/%Y")

format(EnergyConsumption$Date, "%Y-%m-%d")


# Convert Date from POSIXlt to POSIXct ------------------------------------

EnergyConsumption$Date <- as.POSIXct(EnergyConsumption$Date, "%Y-%m-%d")

attr(EnergyConsumption$Date, "tzone") <- "Europe/Paris"


# Inspect the data types --------------------------------------------------

summary(EnergyConsumption)

str(EnergyConsumption)

sum(is.na(EnergyConsumption))


# Delete "Date" and "Time" ------------------------------------------------

EnergyConsumption$Time <- NULL

summary(EnergyConsumption)


# Energy not currently measured by the submeters (in watt hour)

EnergyConsumption <- EnergyConsumption %>%
  mutate(Sub_metering_4 = Global_active_power*(1000/60) -
           Sub_metering_1 -
           Sub_metering_2 -
           Sub_metering_3) 

summary(EnergyConsumption)


# Delete NAs --------------------------------------------------------------

EnergyConsumption1 <- 
  EnergyConsumption %>%
  na.omit(EnergyConsumption)

summary(EnergyConsumption1)


# Creating subsets --------------------------------------------------------
# Subset for Sub-meter 1

DailySM1 <- EnergyConsumption1 %>%
  group_by(Date) %>% 
  summarise(y = mean(Sub_metering_1))


# Fine tuning data frame 

DailySM1 <- DailySM1 %>%
  select(ds = Date,
         y = y)

DailySM1$ds <- date(DailySM1$ds)


# Subset for Sub-meter 2

DailySM2 <- EnergyConsumption1 %>%
  group_by(Date) %>% 
  summarise(y = mean(Sub_metering_2))


# Fine tuning data frame 

DailySM2 <- DailySM2 %>%
  select(ds = Date,
         y = y)

DailySM2$ds <- date(DailySM2$ds)


# Subset for Sub-meter 3

DailySM3 <- EnergyConsumption1 %>%
  group_by(Date) %>% 
  summarise(y = mean(Sub_metering_3))

# Fine tuning data frame 

DailySM3 <- DailySM3 %>%
  select(ds = Date,
         y = y)

DailySM3$ds <- date(DailySM3$ds)


# Subset for Sub-meter 4

DailySM4 <- EnergyConsumption1 %>%
  group_by(Date) %>% 
  summarise(y = mean(Sub_metering_4))


# Fine tuning data frame 

DailySM4 <- DailySM4 %>%
  select(ds = Date,
         y = y)

DailySM4$ds <- date(DailySM4$ds)


# Subset for Global active energy

DailyGAP <- EnergyConsumption1 %>%
  group_by(Date) %>% 
  summarise(y = mean(Global_active_power))


# Fine tuning data frame 

DailyGAP <- DailyGAP %>%
  select(ds = Date,
         y = y)

DailyGAP$ds <- date(DailyGAP$ds)


# Forecasting -------------------------------------------------------------
# Sub-meter 1

m <- prophet(DailySM1, daily.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 730)

forecastSM1 <- predict(m, future)

plot(m, forecastSM1)

dyplot.prophet(m, forecastSM1)


# For individual components

prophet_plot_components(m, forecastSM1)


# Sub-meter 2

m <- prophet(DailySM2, daily.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 730)

forecastSM2 <- predict(m, future)

plot(m, forecastSM2)

dyplot.prophet(m, forecastSM2)


# For individual components

prophet_plot_components(m, forecastSM2)


# Sub-meter 3

m <- prophet(DailySM3, daily.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 730)

forecastSM3 <- predict(m, future)

plot(m, forecastSM3)

dyplot.prophet(m, forecastSM3)


# For individual components

prophet_plot_components(m, forecastSM3)


# Sub-meter 4

m <- prophet(DailySM4, daily.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 730)

forecastSM4 <- predict(m, future)

plot(m, forecastSM4)

dyplot.prophet(m, forecastSM4)


# For individual components

prophet_plot_components(m, forecastSM4)


# Global active energy

m <- prophet(DailyGAP, daily.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 730)

forecastGAP <- predict(m, future)

plot(m, forecastGAP)

dyplot.prophet(m, forecastGAP)


# For individual components

prophet_plot_components(m, forecastGAP)

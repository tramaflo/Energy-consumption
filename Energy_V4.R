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
        name = 'Meter12007', 
        type = "scatter", 
        mode = "lines") %>%
  add_trace(y = ~AvgMonth2008$meanSub_1, name = 'Meter12008', mode = 'lines') %>%
  add_trace(y = ~AvgMonth2009$meanSub_1, name = 'Meter12009', mode = 'lines') %>%
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


# Create TS object with AvgYearAll ----------------------------------------

tsAvgYearAll <- ts(AvgYearAll$meanSub_1, frequency=52, start=c(2007,1))
fittsAvgYearAll <- tslm(tsAvgYearAll ~ trend + season) 
summary(fittsAvgYearAll)

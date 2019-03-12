# Energy Consumption analysis #
# Floriana Trama #
# Data analysis department #
# G1: Understand benefits of sub-metering and convince the home developer to become our client explaining advantages that he could get throught data analytics #
# March 2019 #
#Version 1 #



# Libraries ---------------------------------------------------------------

library(RMySQL)

library(dplyr)

library(lubridate)

library(tidyverse)

library(caret)


# Importing data - create a database connection with the server ----------- 

con = dbConnect(MySQL(), user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')


# List the tables contained in the database -------------------------------

dbListTables(con)


# List the attributes contained in a table --------------------------------

dbListFields(con,"yr_2006")


# Use attribute names to specify attributes for download (or use a --------

yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

summary(yr_2006)

str(yr_2006)


# Understand if datasets cover an entire year -----------------------------

head(yr_2006)

tail(yr_2006)

head(yr_2007)

tail(yr_2007)

head(yr_2008)

tail(yr_2008)

head(yr_2009)

tail(yr_2009)

head(yr_2010)

tail(yr_2010)


# Combine tables into one single dataframe using dplyr --------------------

EnergyConsumption <- bind_rows(yr_2007, yr_2008, yr_2009)

summary(EnergyConsumption)

head(EnergyConsumption)

tail(EnergyConsumption)


# Combine Date and Time attributes in a new attribute column --------------

EnergyConsumption <- cbind(EnergyConsumption, 
                           paste(EnergyConsumption$Date,EnergyConsumption$Time), 
                           stringsAsFactors = FALSE)


# Give the new attribute in the 6th column a header name ------------------

colnames(EnergyConsumption)[7] <-"DateTime"


# Move the new DateTime attribute within the dataset ----------------------

EnergyConsumption <- EnergyConsumption[,c(ncol(EnergyConsumption), 1:(ncol(EnergyConsumption)-1))]

head(EnergyConsumption)


# Convert DateTime from POSIXlt to POSIXct --------------------------------

EnergyConsumption$DateTime <- as.POSIXct(EnergyConsumption$DateTime, "%Y/%m/%d %H:%M:%S")


# Add France time zone ----------------------------------------------------

attr(EnergyConsumption$DateTime, "tzone") <- "Europe/Paris"


# Inspect the data types --------------------------------------------------

summary(EnergyConsumption)

str(EnergyConsumption)


# Create new different "time-attributes" with lubridate -------------------

EnergyConsumption$year <- year(EnergyConsumption$DateTime)

EnergyConsumption$Quarter <- quarter(EnergyConsumption$DateTime)

EnergyConsumption$Month <- month(EnergyConsumption$DateTime)

EnergyConsumption$Week <- week(EnergyConsumption$DateTime)

EnergyConsumption$Weekday <- weekdays(EnergyConsumption$DateTime)

EnergyConsumption$Day <- day(EnergyConsumption$DateTime)

EnergyConsumption$Hour <- hour(EnergyConsumption$DateTime)

EnergyConsumption$Minute <- minute(EnergyConsumption$DateTime)

summary(EnergyConsumption)


# Visualization -----------------------------------------------------------

#Energy consumption in a day by meters
One_day <- EnergyConsumption %>% 
  filter(DateTime > "2008-03-07" & DateTime < "2008-03-08")

  ggplot(One_day) +
  geom_line(aes(x = DateTime, y = Sub_metering_1, color = "blue")) +
  geom_line(aes(x = DateTime, y = Sub_metering_2, color = "red")) +
  geom_line(aes(x = DateTime, y = Sub_metering_3, color = "green")) +
  labs(x = "Day", y = "kWh",
  title = "Energy consumption in a day by meters")

#Energy consumption in a winter week by meters  
Winter_week <- EnergyConsumption %>%
  filter(DateTime > "2009-01-19" & DateTime < "2009-01-25")

ggplot(Winter_week) +
geom_line(aes(x = DateTime, y = Sub_metering_1, color = "blue")) +
labs(x = "Day", y = "kWh",
     title = "Energy consumption in a winter week by meters")

ggplot(Winter_week) +  
geom_line(aes(x = DateTime, y = Sub_metering_2, color = "red")) +
labs(x = "Day", y = "kWh",
     title = "Energy consumption in a winter week by meters")

ggplot(Winter_week) +
geom_line(aes(x = DateTime, y = Sub_metering_3, color = "green")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in a winter week by meters")

#Energy consumption in a autumn week by meters
Autumn_week <- EnergyConsumption %>%
  filter(DateTime > "2009-11-09" & DateTime < "2009-11-15")

ggplot(Autumn_week) +
  geom_line(aes(x = DateTime, y = Sub_metering_1, color = "blue")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in an autumn week by meters")

ggplot(Autumn_week) +  
  geom_line(aes(x = DateTime, y = Sub_metering_2, color = "red")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in an autumn week by meters")

ggplot(Autumn_week) +
  geom_line(aes(x = DateTime, y = Sub_metering_3, color = "green")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in an autumn week by meters")
  
#Energy consumption in a spring week by meters
Spring_week <- EnergyConsumption %>%
  filter(DateTime > "2009-05-11" & DateTime < "2009-05-17")

ggplot(Spring_week) +
  geom_line(aes(x = DateTime, y = Sub_metering_1, color = "blue")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in a spring week by meters")

ggplot(Spring_week) +  
  geom_line(aes(x = DateTime, y = Sub_metering_2, color = "red")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in a spring week by meters")

ggplot(Spring_week) +
  geom_line(aes(x = DateTime, y = Sub_metering_3, color = "green")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in a spring week by meters")  

#Energy consumption in a summer week by meters
Summer_week <- EnergyConsumption %>%
  filter(DateTime > "2009-07-27" & DateTime < "2009-08-02")

ggplot(Summer_week) +
  geom_line(aes(x = DateTime, y = Sub_metering_1, color = "blue")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in a summer week by meters")

ggplot(Summer_week) +  
  geom_line(aes(x = DateTime, y = Sub_metering_2, color = "red")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in a summer week by meters")

ggplot(Summer_week) +
  geom_line(aes(x = DateTime, y = Sub_metering_3, color = "green")) +
  labs(x = "Day", y = "kWh",
       title = "Energy consumption in a summer week by meters")


#Global active power plotted over the day
ggplot(One_day, aes(x = DateTime, y = (Global_active_power*60))) + 
  geom_line(size = .75, color = "black") +  
  theme_classic() +
  labs(x = "Day", y = "kWh",
       title = "Global Active Power Usage On a Day")

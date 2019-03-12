# Energy Consumption analysis #
# Floriana Trama #
# Data analysis department #
# G1: Understand benefits of sub-metering and convince the home developer to become our client explaining advantages that he could get throught data analytics #
# March 2019 #
# Version 2 #



# Libraries ---------------------------------------------------------------

library(RMySQL)

library(dplyr)

library(lubridate)

library(tidyverse)

library(caret)

library(plotly)


# Importing data - create a database connection with the server ----------- 

con = dbConnect(MySQL(), user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='35.239.91.216')


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

sum(is.na(EnergyConsumption))


# Create new different "time-attributes" with lubridate -------------------

EnergyConsumption$Year <- year(EnergyConsumption$DateTime)

EnergyConsumption$Quarter <- quarter(EnergyConsumption$DateTime)

EnergyConsumption$Month <- month(EnergyConsumption$DateTime)

EnergyConsumption$Week <- week(EnergyConsumption$DateTime)

EnergyConsumption$Weekdays <- weekdays(EnergyConsumption$DateTime)

EnergyConsumption$Day <- day(EnergyConsumption$DateTime)

EnergyConsumption$Hour <- hour(EnergyConsumption$DateTime)

EnergyConsumption$Minute <- minute(EnergyConsumption$DateTime)


# Change "Active power", "SubMetering 1/2" and " Weekdays" in NUMERIC var -

EnergyConsumption$Global_active_power <- as.numeric(EnergyConsumption$Global_active_power)

EnergyConsumption$Sub_metering_1 <- as.numeric(EnergyConsumption$Sub_metering_1)

EnergyConsumption$Sub_metering_2 <- as.numeric(EnergyConsumption$Sub_metering_2)

summary(EnergyConsumption)


# Change year quarter, month, week, weekdays and day into FACTOR variable -

EnergyConsumption$Year <- as.factor(EnergyConsumption$Year)

EnergyConsumption$Quarter <- as.factor(EnergyConsumption$Quarter)

EnergyConsumption$Month <- as.factor(EnergyConsumption$Month)

EnergyConsumption$Week <- as.factor(EnergyConsumption$Week)

EnergyConsumption$Weekdays <- as.factor(EnergyConsumption$Weekdays)

EnergyConsumption$Day <- as.factor(EnergyConsumption$Day)


# Delete "Date" and "Time" ------------------------------------------------

EnergyConsumption$Date <- NULL

EnergyConsumption$Time <- NULL

summary(EnergyConsumption)
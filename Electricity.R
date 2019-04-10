#Electricity

#Library ---
library(DBI)
library(RMySQL)
library (dplyr)
library(lubridate)
library(chron)
library(tidyverse)
library(downloader)
library(plotly)

###Data___________________________________________________________________________###
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

##working with iris database -------------------------------------------------------------------------------##
dbListTables(con)

dbListFields(con,"yr_2006")
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2006_Selected <- dbGetQuery(con, "SELECT SELECT Date, Time, Global_active_power,
                             Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")


dbListFields(con,"yr_2007")
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2007_SELECT <- dbGetQuery(con, "SELECT SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")

dbListFields(con,"yr_2008")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2008_SELECT <- dbGetQuery(con, "SELECT SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")


dbListFields(con,"yr_2009")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2009_SELECT <- dbGetQuery(con, "SELECT SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")

dbListFields(con,"yr_2010")
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")
yr_2010_SELECT <- dbGetQuery(con, "SELECT SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")




#--------------------------------------------------------------------------------------------------------------#
#### exploring data ####

str (yr_2006)
str (yr_2007)
str (yr_2008)
str (yr_2009)
str (yr_2010)

summary(yr_2006)
summary(yr_2007)
summary(yr_2008)
summary(yr_2009)
summary(yr_2010)

head(yr_2006)
head(yr_2007)
head(yr_2008)
head(yr_2009)
head(yr_2010)

tail(yr_2006)
tail(yr_2007)
tail(yr_2008)
tail(yr_2009)
tail(yr_2010)

#---------------------------------------------------------------------------------------------------------------#
#### Marging different years in just one data table####

Energy_cons <- bind_rows(yr_2007, yr_2008, yr_2009)

write.csv(Energy_consDisk, file = "Datos/Energy_consDisk.csv")
Energy_consDisk <- read.csv (file = "Datos/Energy_Cons.csv")
Energy_consDisk$X<- NULL



str(Energy_consDisk)
summary(Energy_consDisk)
head(Energy_consDisk)
tail(Energy_consDisk)


#---------------------------------------------------------------------------------------------------------------#
#### Lubridate ####

Energy_consDisk_time <-cbind(Energy_consDisk,
                         DateTime = paste(Energy_consDisk$Date,Energy_consDisk$Time), 
                         stringsAsFactors=FALSE)



## Move the DateTime attribute within the dataset
Energy_consDisk_time1 <- Energy_consDisk_time[,c(ncol(Energy_consDisk_time), 1:(ncol(Energy_consDisk_time)-1))]

head(Energy_consDisk_time1)


## Convert DateTime from POSIXlt to POSIXct 
Energy_consDisk_time1$DateTime <- as.POSIXct(Energy_consDisk_time1$DateTime, "%Y/%m/%d %H:%M:%S")


## Add the time zone
attr(Energy_consDisk_time1$DateTime, "tzone") <- "Europe/Paris"


str(Energy_consDisk_time1)


#-----------------------------------------------------------------------------------------------------------#
#### Lubridate####

Energy_consDisk_time1$year <- year(Energy_consDisk_time1$DateTime)
Energy_consDisk_time1$Quarter <- quarter(Energy_consDisk_time1$DateTime)
Energy_consDisk_time1$Month <- month(Energy_consDisk_time1$DateTime)
Energy_consDisk_time1$Week <- week(Energy_consDisk_time1$DateTime)
Energy_consDisk_time1$Weekday <- weekdays(Energy_consDisk_time1$DateTime)
Energy_consDisk_time1$Day <- day(Energy_consDisk_time1$DateTime)
Energy_consDisk_time1$Hour <- hour(Energy_consDisk_time1$DateTime)
Energy_consDisk_time1$Minute <- minute(Energy_consDisk_time1$DateTime)


summary(Energy_consDisk_time1)


#-----------------------------------------------------------------------------------------------#
####Ploting, it is not working--####


houseWeek <- filter(Energy_consDisk_time1, year == 2008 & Week == 2)



plot(houseWeek$Sub_metering_1)
plot(houseWeek$Sub_metering_2)
plot(houseWeek$Sub_metering_3)

### ploting one week of different seasons____________________####

houseWeekWinter <- filter(Energy_consDisk_time1, year == 2008, 2009, 2010 & Week == 2)



###Using plot_ly___________________________###

houseDay <- filter(Energy_consDisk_time1, year == 2008 & Month == 1 & Day == 9)

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

#Ploting the thre sub-meters_________________#

plot_ly(houseDay, x = ~houseDay$DateTime, y 
        = ~houseDay$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%

  add_trace(y = ~houseDay$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  
  add_trace(y = ~houseDay$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


###ploting per month------####

houseWeekSeasonal <- filter(Energy_consDisk_time1, year == c(2007,2008,2009) & Month == 6)

plot_ly(houseWeekSeasonal, x = ~houseWeekSeasonal$DateTime, y 
        = ~houseWeekSeasonal$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  
  add_trace(y = ~houseWeekSeasonal$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  
  add_trace(y = ~houseWeekSeasonal$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  
  layout(title = "Power Consumption June 2007, 2008, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


###Grouping by each ten minutes__________________________####
houseDay10 <- filter(houseDay, year == 2008 & Month == 1 
                     & Day == 9 
                     & (Minute == 0
                        | Minute == 10
                        | Minute == 20 
                        | Minute == 30 
                        | Minute == 40 
                        | Minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  
  add_trace(y = ~houseDay10$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  
  add_trace(y = ~houseDay10$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

###perweek _____ no funciona####
Energy_consDisk_time1$Week<- as.factor(Energy_consDisk_time1$Week)


Week1 <- filter(Energy_consDisk_time1, year == 2008 
                     & Week == 1
                     & (Minute == 0
                        | Minute == 15
                        | Minute == 30
                        | Minute == 45))

plot_ly(Week1, x = ~Week1$DateTime, y = ~Week1$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  
  add_trace(y = ~Week1$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  
  add_trace(y = ~Week1$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  
  layout(title = "One week of every season, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#Promedio por dia_____________________

MeanPerDay <- Energy_consDisk_time1 %>% 
  group_by(DateTime = date(DateTime), Day = day(DateTime)) %>%
  summarise_if(is.numeric, mean)



plot_ly(MeanPerDay, x = ~MeanPerDay$DateTime, y = ~MeanPerDay$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  
  add_trace(y = ~MeanPerDay$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  
  add_trace(y = ~MeanPerDay$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  
  layout(title = "Mean per day, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(MeanPerDay, x = ~MeanPerDay$DateTime, y = ~MeanPerDay$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  
  add_trace(y = ~MeanPerDay$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  
  add_trace(y = ~MeanPerDay$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  
  layout(title = "Years (in mean per day)",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




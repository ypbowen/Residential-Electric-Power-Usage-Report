
install.packages("RMariaDB")

install.packages("lubridate")
install.packages("plotly")
install.packages("forecast")
library(RMariaDB)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggfortify)
library(forecast)


## Create a database connection 
con = dbConnect(MariaDB(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
head(irisALL)
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
head(irisSELECT)


## Lists attributes contained in a table
dbListFields(con,'yr_2006')


yr_2006<-dbGetQuery(con,"SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006 ")
head(yr_2006)
str(yr_2006)
summary(yr_2006)
tail(yr_2006)

yr_2007<-dbGetQuery(con,"SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007 ")
head(yr_2007)
str(yr_2007)
summary(yr_2007)
tail(yr_2007)

yr_2008<-dbGetQuery(con,"SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008 ")
head(yr_2008)
str(yr_2008)
summary(yr_2008)
tail(yr_2008)

yr_2009<-dbGetQuery(con,"SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009 ")
head(yr_2009)
str(yr_2009)
summary(yr_2009)
tail(yr_2009)

yr_2010<-dbGetQuery(con,"SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010 ")
head(yr_2010)
str(yr_2010)
summary(yr_2010)
tail(yr_2010)

data<-bind_rows(yr_2006,yr_2007,yr_2008,yr_2009,yr_2010)
str(data)
summary(data)
head(data)
tail(data)



## Combine Date and Time attribute values in a new attribute column
data <-cbind(data,paste(data$Date,data$Time), stringsAsFactors=FALSE)
head(data)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(data)[6] <-"DateTime"
head(data)


## Move the DateTime attribute within the dataset
data <- data[,c(ncol(data), 1:(ncol(data)-1))]
head(data)



# Convert DateTime from character to POSIXct 
data$DateTime <- as.POSIXct(data$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(data$DateTime, "tzone") <- "Europe/Paris"
str(data)




## Create "year" attribute with lubridate

data$year <- year(data$DateTime)
head(data)
tail(data)


data$quarter <- quarter(data$DateTime)
head(data)


data$month <- month(data$DateTime)
head(data)


data$week <- week(data$DateTime)
head(data)


data$weekday <- weekdays(data$DateTime)
head(data)


data$day <- day(data$DateTime)
head(data)

data$hour <- hour(data$DateTime)
head(data)


data$minute <- minute(data$DateTime)
head(data)
tail(data)



##############################################################STEP 1


## Plot all of sub-meter 1
#plot(data$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(data, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)


## Subset the 9th day of January 2008 - All observations
houseDay <- filter(data, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

# Plot sub-meter 1, 2 and 3 with title, legend and labels January 9th 2008 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




##Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(data, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

#Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#######Need weekly and seasonally





##Subset of January 2008 - week 1  frequency
houseDay <- filter(data, year == 2008 & month == 1 & week == 1 & (day == 1 | day == 2 | day == 3 | day == 4 | day ==5 | day ==6 | day == 7))

# Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency 

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, Week 1, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Subset of January 2009 - week 1
houseWeek09 <- filter(data, year == 2009 & month == 1 & week == 1 & (day == 1 | day == 2 | day == 3 | day == 4 | day ==5 | day ==6 | day == 7))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - week 1 
plot_ly(houseWeek09, x = ~houseWeek09$DateTime, y = ~houseWeek09$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek09$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek09$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, Week 1, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))








#Subset of January 2007 - week 1
houseWeek07 <- filter(data, year == 2007 & month == 1 & week == 1 & (day == 1 | day == 2 | day == 3 | day == 4 | day ==5 | day ==6 | day == 7))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - week 1 
plot_ly(houseWeek07, x = ~houseWeek07$DateTime, y = ~houseWeek07$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek07$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek07$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, Week 1, 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))






#Subset of January 2008 - week 1
houseWeek08 <- filter(data, year == 2008 & month == 1 & week == 1 & (day == 1 | day == 2 | day == 3 | day == 4 | day ==5 | day ==6 | day == 7))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - week 1 
plot_ly(houseWeek08, x = ~houseWeek08$DateTime, y = ~houseWeek08$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek08$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek08$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, Week 1, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))











#Subset of February 2009 - week 5
houseWeek5 <- filter(data, year == 2009 & month == 2 & week == 5 & (day == 1| day == 2| day == 3| day == 4| day ==5| day ==6| day == 7))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - week 5

plot_ly(houseWeek5, x = ~houseWeek5$DateTime, y = ~houseWeek5$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek5$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek5$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption February, Week 5, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Subset  2007 - quarterly  
houseQuarter07 <- filter(data, year == 2007 & (quarter == 1 | quarter == 2 | quarter == 3 | quarter == 4 ))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2007

plot_ly(houseQuarter07, x = ~houseQuarter07$DateTime, y = ~houseQuarter07$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter07$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter07$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quarterly, 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#Subset  2007 - quarter 3  
houseQuarter3 <- filter(data, year == 2007 & (quarter == 3))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2007

plot_ly(houseQuarter3, x = ~houseQuarter3$DateTime, y = ~houseQuarter3$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter3$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter3$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quarter 3, 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Subset  2008 - quarterly  
houseQuarter08 <- filter(data, year == 2008 & (quarter == 1 | quarter == 2 | quarter == 3 | quarter == 4 ))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2008

plot_ly(houseQuarter08, x = ~houseQuarter08$DateTime, y = ~houseQuarter08$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter08$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter08$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quarterly, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Subset  2008 - quarter 3  
houseQuarter308 <- filter(data, year == 2008 & (quarter == 3))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2008

plot_ly(houseQuarter308, x = ~houseQuarter308$DateTime, y = ~houseQuarter308$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter308$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter308$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quarter 3, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))









#Subset  2009 - quarterly  
houseQuarter <- filter(data, year == 2009 & (quarter == 1 | quarter == 2 | quarter == 3 | quarter == 4 ))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2009

plot_ly(houseQuarter, x = ~houseQuarter$DateTime, y = ~houseQuarter$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quarterly, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))







#Subset  2009 - quarter 3  
houseQuarter309 <- filter(data, year == 2009 & (quarter == 3))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2007

plot_ly(houseQuarter309, x = ~houseQuarter309$DateTime, y = ~houseQuarter309$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter309$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter309$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quarter 3, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#Subset Quarterly 2010  frequency
houseQuarter10 <- filter(data, year == 2010 & (quarter == 1 | quarter == 2 | quarter == 3 | quarter == 4 ))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2010

plot_ly(houseQuarter10, x = ~houseQuarter10$DateTime, y = ~houseQuarter10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quaterly, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))







#Subset  2010 - quarter 3  
houseQuarter310 <- filter(data, year == 2010 & (quarter == 3))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - Quarterly 2007

plot_ly(houseQuarter310, x = ~houseQuarter310$DateTime, y = ~houseQuarter310$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseQuarter310$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseQuarter310$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Quarter 3, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))














#####################################################STEP 2 #####################################################################

#data$Date<-NULL
#data$Time<-NULL

data<-bind_rows(yr_2007,yr_2008,yr_2009)
str(data)
summary(data)
head(data)
tail(data)







## Combine Date and Time attribute values in a new attribute column
data <-cbind(data,paste(data$Date,data$Time), stringsAsFactors=FALSE)
head(data)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(data)[6] <-"DateTime"
head(data)


## Move the DateTime attribute within the dataset
data <- data[,c(ncol(data), 1:(ncol(data)-1))]
head(data)



# Convert DateTime from character to POSIXct 
data$DateTime <- as.POSIXct(data$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(data$DateTime, "tzone") <- "Europe/Paris"
str(data)


data$Date<-NULL
data$Time<-NULL



## Create "year" attribute with lubridate

data$year <- year(data$DateTime)
head(data)
tail(data)


data$quarter <- quarter(data$DateTime)
head(data)


data$month <- month(data$DateTime)
head(data)


data$week <- week(data$DateTime)
head(data)


data$weekday <- day(data$DateTime)
head(data)


data$day <- day(data$DateTime)
head(data)

data$hour <- hour(data$DateTime)
head(data)


data$minute <- minute(data$DateTime)
head(data)
tail(data)

data$wkdaynum<-format(data$DateTime,'%u')
data$wkdaynum<-as.numeric(data$wkdaynum)

##########      SUB_ METER 3




## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(data, wkdaynum == 2 & hour == 20 & minute == 1)
#52*3

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))
#52:2007 52:2008 52:2009


#autoplot(tsSM3_070809weekly) 

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time(weekly)", ylab = "Watt Hours", main = "Sub-Meter 3 A/C and Water Heater")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly, xlab = "Time ( weekly)", ylab = "Watt Hours", main = "Sub-Meter 3 A/C and Water Heater")







#Subset monthly  on Mondays at 8:00pm for 2007, 2008 and 2009 
house070809monthly3<- filter(data, month == 1 & hour == 20 & minute == 1)
#31*3
head(house070809monthly3)

## Create TS object with SubMeter3
tsSM3_070809monthly3 <- ts(house070809monthly3$Sub_metering_3, frequency=31, start=c(2007,1))



#autoplot(tsSM3_070809monthly3)

## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM3_070809monthly3, ts.colour = 'red', xlab = "Time (monthly data)", ylab = "Watt Hours", main = "Sub-Meter 3 A/C and Water Heater")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM3_070809monthly3, xlab = "Time (monthly data)", ylab = "Watt Hours", main = "Sub-Meter 3 A/C and Water Heater")







########## Sub_meter 3  Subset quarterly  on Mondays at 8:00pm for 2007, 2008 and 2009 
house070809quarterly3 <- filter(data, quarter==1  & wkdaynum ==1 & hour == 20 & minute==1)
head(house070809quarterly3)

#We are taking saturday 8:00 for all the weeks in first quarter. Each quarter has 13 values/weeks.

## Create TS object with SubMeter1
tsSM3_070809quarterly3<- ts(house070809quarterly3$Sub_metering_3, frequency=13, start=c(2007,1))



#autoplot(tsSM3_070809quarterly3)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809quarterly3, ts.colour = 'red', xlab = "Time (quarterly data)", ylab = "Watt Hours", main = "Sub-Meter  3 A/C & Water Heater")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809quarterly3, xlab = "Time (quarterly data)", ylab = "Watt Hours", main = "Sub-Meter 3 A/C & Water Heater")








##########                        SUB_METER 1 




## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly1 <- filter(data, wkdaynum == 2 & hour == 20 & minute == 1)


## Create TS object with SubMeter1
tsSM1_070809weekly1 <- ts(house070809weekly1$Sub_metering_1, frequency=52, start=c(2007,1))


#autoplot(tsSM1_070809weekly1) 

## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM1_070809weekly1, ts.colour = 'red', xlab = "Time(weekly)", ylab = "Watt Hours", main = "Sub-Meter 1   Kitchen")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly1, xlab = "Time ( weekly)", ylab = "Watt Hours", main = "Sub-Meter 1  Kitchen")












#Subset monthly  on Mondays at 8:00pm for 2007, 2008 and 2009 
house070809monthly1 <- filter(data, month == 1 & hour == 20 & minute == 1)
head(house070809monthly1)

## Create TS object with SubMeter1
tsSM1_070809monthly1 <- ts(house070809monthly1$Sub_metering_1, frequency=31, start=c(2007,1))



#autoplot(tsSM1_070809monthly1)

## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM1_070809monthly1, ts.colour = 'red', xlab = "Time (monthly data)", ylab = "Watt Hours", main = "Sub-Meter 1 Kitchen")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809monthly1, xlab = "Time (monthly data)", ylab = "Watt Hours", main = "Sub-Meter 1 Kitchen")





########## Sub_meter 1  Subset quarterly  on Mondays at 8:00pm for 2007, 2008 and 2009 
house070809quarterly1 <- filter(data, quarter==1  & wkdaynum ==1 & hour == 20 & minute==1)
head(house070809quarterly1)

## Create TS object with SubMeter1
tsSM1_070809quarterly1<- ts(house070809quarterly1$Sub_metering_1, frequency=13, start=c(2007,1))



#autoplot(tsSM1_070809quarterly1)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM1_070809quarterly1, ts.colour = 'red', xlab = "Time (quarterly data)", ylab = "Watt Hours", main = "Sub-Meter 1 Kitchen")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809quarterly1, xlab = "Time (quarterly data)", ylab = "Watt Hours", main = "Sub-Meter 1 Kitchen")








########## SUB_METR 2 

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly2 <- filter(data, wkdaynum == 2 & hour == 20 & minute == 1)


## Create TS object with SubMeter2
tsSM2_070809weekly2 <- ts(house070809weekly2$Sub_metering_2, frequency=52, start=c(2007,1))


#autoplot(tsSM2_070809weekly2) 

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM2_070809weekly2, ts.colour = 'red', xlab = "Time(weekly)", ylab = "Watt Hours", main = "Sub-Meter 2 Laundry")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM2_070809weekly2, xlab = "Time ( weekly)", ylab = "Watt Hours", main = "Sub-Meter 2 Laundry")









#Subset monthly  on Mondays at 8:00pm for 2007, 2008 and 2009 
house070809monthly2 <- filter(data, month == 1 & hour == 20 & minute == 1)
head(house070809monthly2)

## Create TS object with SubMeter1
tsSM2_070809monthly2 <- ts(house070809monthly2$Sub_metering_2, frequency=31, start=c(2007,1))



#autoplot(tsSM2_070809monthly2)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809monthly2, ts.colour = 'red', xlab = "Time (monthly data) ", ylab = "Watt Hours", main = "Sub-Meter 2 Laundry")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM2_070809monthly2, xlab = "Time (monthly data)", ylab = "Watt Hours", main = "Sub-Meter 2 Laundry")



########## Sub_meter 2  Subset quarterly  on Mondays at 8:00pm for 2007, 2008 and 2009 
house070809quarterly2 <- filter(data, quarter==1 &wkdaynum==1 & hour == 20 & minute==1)
head(house070809quarterly2)

## Create TS object with SubMeter1
tsSM2_070809quarterly2<- ts(house070809quarterly2$Sub_metering_2, frequency=13, start=c(2007,1))



#autoplot(tsSM2_070809quarterly2)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809quarterly2, ts.colour = 'red', xlab = "Time (quarterly data)", ylab = "Watt Hours", main = "Sub-Meter 2 Laundry")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM2_070809quarterly2, xlab = "Time (quarterly data)", ylab = "Watt Hours", main = "Sub-Meter 2 Laundry")






#########################################################STEP 3 #####################################################

#####                     Sub_Meter 3 Forecast


## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

# Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
#plot(forecastfitSM3, xlab = "Time (weekly)", ylab = "Watt Hours", main = "Sub-Meter 3 LR Model")



# Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time (weekly)", main = "Sub-Meter 3 LR Model")





#tsSM3_070809monthly3


## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3monthly3 <- tslm(tsSM3_070809monthly3 ~ trend + season) 
summary(fitSM3monthly3)

# Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3monthly3 <- forecast(fitSM3monthly3, h=30)
## Plot the forecast for sub-meter 3. 
#plot(forecastfitSM3monthly3, xlab = "Time (monthly)", ylab = "Watt Hours", main = "Sub-Meter 3 LR Model")



# Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3monthly3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3monthly3c, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time (monthly)", main = "Sub-Meter 3 LR Model")






#tsSM3_070809quarterly3





## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3quarterly3 <- tslm(tsSM3_070809quarterly3~ trend + season) 
summary(fitSM3quarterly3)

# Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3quarterly3 <- forecast(fitSM3quarterly3, h=30)
## Plot the forecast for sub-meter 3. 
#plot(forecastfitSM3quarterly3, xlab = "Time (quarterly)", ylab = "Watt Hours", main = "Sub-Meter 3 LR Model")



# Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3quarterly3c <- forecast(fitSM3quarterly3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3quarterly3c, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time (quarterly)", main = "Sub-Meter 3 LR Model")















#####                     Sub_Meter 1 Forecast




##tsSM1_070809weekly1

fitSM1w <- tslm(tsSM1_070809weekly1 ~ trend + season) 
summary(fitSM1w)


# Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM1w <- forecast(fitSM1w, h=20)
## Plot the forecast for sub-meter 3. 
#plot(forecastfitSM1w, xlab = "Time (weekly)", ylab = "Watt Hours", main = "Sub-Meter 1 LR Model")



# Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM1wc <- forecast(fitSM1w, level=c(80,90), h=40)

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM1wc, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time (weekly)", main = "Sub-Meter 1 LR Model")





#Monthly



fitSM1m <- tslm(tsSM1_070809monthly1 ~ trend + season) 
summary(fitSM1m)
forecastfitSM1m <- forecast(fitSM1m, h=20)
#plot(forecastfitSM1m, xlab = "Time", ylab = "Watt Hours",main = "Sub-Meter 1 Kitchen  LR Model ")
forecastfitSM1c <- forecast(fitSM1m, h=20, level=c(80,90))
plot(forecastfitSM1c, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time (monthly)", main = "Sub-Meter 1 Kitchen  LR Model")



#Quarterly
#tsSM1_070809quarterly1

fitSM1Q <- tslm(tsSM1_070809quarterly1 ~ trend + season) 
summary(fitSM1Q)
forecastfitSM1Q <- forecast(fitSM1Q, h=20)
#plot(forecastfitSM1Q, xlab = "Time", ylab = "Watt Hours")
forecastfitSM1Qc <- forecast(fitSM1Q, h=20, level=c(80,90))
plot(forecastfitSM1Qc, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time (quarterly)", main = "Sub-Meter 1 Kitchen  LR")





#####                     Sub_Meter 2 Forecast

#tsSM2_070809weekly2


fitSM2w <- tslm(tsSM2_070809weekly2 ~ trend + season) 
summary(fitSM2w)


# Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM2w <- forecast(fitSM2w, h=20)
## Plot the forecast for sub-meter 3. 
#plot(forecastfitSM1w, xlab = "Time (weekly)", ylab = "Watt Hours", main = "Sub-Meter 2 LR Model")



# Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM2wc <- forecast(fitSM2w, level=c(80,90), h=40)

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM2wc, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time (weekly)", main = "Sub-Meter 2 LR Model")






#tsSM2_070809monthly2


fitSM2 <- tslm(tsSM2_070809monthly2 ~ trend + season) 
summary(fitSM2)
forecastfitSM2 <- forecast(fitSM2, h=20)
#plot(forecastfitSM2, xlab = "Time", ylab = "Watt Hours", main = "Sub-Meter 2 Laundry LR")
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))
plot(forecastfitSM2c, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time (monthly)", main = "Sub-Meter 2 Laundry LR")


  
#tsSM2_070809quarterly2

fitSM2Q <- tslm(tsSM2_070809quarterly2 ~ trend + season) 
summary(fitSM2Q)
forecastfitSM2Q <- forecast(fitSM2Q, h=20)
plot(forecastfitSM2Q, xlab = "Time", ylab = "Watt Hours")
forecastfitSM2Qc <- forecast(fitSM1Q, h=20, level=c(80,90))
plot(forecastfitSM2Qc, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time (quarterly)", main = "Sub-Meter 2 Laundr LR")






#############################################Step 4#############################################################


  #########Sub_meter 3

#tsSM3_070809weekly

# Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly, xlab = "Time(weekly) SM3")
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)



#tsSM3_070809monthly3


components070809SM3monthly <- decompose(tsSM3_070809monthly3)
plot(components070809SM3monthly, xlab = "Time(monthly) SM3")
summary(components070809SM3monthly)

#tsSM3_070809quarterly3

components070809SM3quarterly <- decompose(tsSM3_070809quarterly3)
plot(components070809SM3quarterly, xlab = "Time(quarterly) SM3")
summary(components070809SM3quarterly)




#########Sub_meter 1



#tsSM1_070809weekly1



# Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly1)
## Plot decomposed sub-meter 1 
plot(components070809SM1weekly, xlab = "Time(weekly) SM1")
## Check summary statistics for decomposed sub-meter 1
summary(components070809SM1weekly)



#tsSM1_070809monthly1


components070809SM1monthly <- decompose(tsSM1_070809monthly1)
plot(components070809SM1monthly, xlab = "Time(monthly) SM1")
summary(components070809SM1monthly)


#tsSM1_070809quarterly1

components070809SM1quarterly <- decompose(tsSM1_070809quarterly1)
plot(components070809SM1quarterly, xlab = "Time(quarterly) SM1")
summary(components070809SM1quarterly)







#########Sub_meter 2

#tsSM2_070809weekly2





# Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly2)
## Plot decomposed sub-meter 1 
plot(components070809SM2weekly, xlab = "Time(weekly) SM2")
## Check summary statistics for decomposed sub-meter 1
summary(components070809SM2weekly)



#tsSM2_070809monthly2


components070809SM2monthly <- decompose(tsSM2_070809monthly2)
plot(components070809SM2monthly, xlab = "Time(monthly) SM2")
summary(components070809SM2monthly)


#tsSM2_070809quarterly2

components070809SM2quarterly <- decompose(tsSM2_070809quarterly2)
plot(components070809SM2quarterly, xlab = "Time(quarterly) SM2")
summary(components070809SM2quarterly)




############################################STEP 5################################################



weekly

#Sub Metter 3

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted,  xlab = "Time ( weekly) SM3")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted), xlab = "Time(weekly) SM3")

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25), xlab = "Time(weekly) SM3")


## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(weekly) - Sub-Meter 3")



## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(weekly) - Sub-Meter 3", start(2010))




#tsSM3_070809monthly3

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjustedm <- tsSM3_070809monthly3 - components070809SM3monthly$seasonal
autoplot(tsSM3_070809Adjustedm,  xlab = "Time(monthly) SM3")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjustedm), xlab = "Time(monthly) SM3")

## Holt Winters Exponential Smoothing & Plot
tsSM3_HM070809m <- HoltWinters(tsSM3_070809Adjustedm, beta=FALSE, gamma=FALSE)
plot(tsSM3_HM070809m, ylim = c(0, 25), xlab = "Timemonthly SM3")


## HoltWinters forecast & plot
tsSM3_HM070809mfor <- forecast(tsSM3_HM070809m, h=25)
plot(tsSM3_HM070809mfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(monthly) - Sub-Meter 3")



## Forecast HoltWinters with diminished confidence levels
tsSM3_HM070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HM070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(monthly) - Sub-Meter 3", start(2010))


##tsSM3_070809quarterly3




## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjustedq <- tsSM3_070809quarterly3 - components070809SM3quarterly$seasonal
autoplot(tsSM3_070809Adjustedm,  xlab = "Time(quarterly) SM3")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjustedq), xlab = "Time(quarterely) SM3")

## Holt Winters Exponential Smoothing & Plot
tsSM3_HQ070809q <- HoltWinters(tsSM3_070809Adjustedq, beta=FALSE, gamma=FALSE)
plot(tsSM3_HQ070809q, ylim = c(0, 25), xlab = "Time(quarterly) SM3")


## HoltWinters forecast & plot
tsSM3_HQ070809qfor <- forecast(tsSM3_HQ070809q, h=25)
plot(tsSM3_HQ070809qfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(quarterly) - Sub-Meter 3")



## Forecast HoltWinters with diminished confidence levels
tsSM3_HQ070809forC <- forecast(tsSM3_HQ070809q, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HQ070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(qarterly) - Sub-Meter 3", start(2010))















#Sub_meter 1

weekly


#tsSM1_070809weekly1


## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly1 - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted,  xlab = "Time ( weekly) SM1")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted), xlab = "Time(weekly) SM1")

## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25), xlab = "Time(weekly) SM1")


## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(weekly) - Sub-Meter 1")



## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(weekly) - Sub-Meter 1", start(2010))




#tsSM1_070809monthly1


## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM1_070809Adjustedm <- tsSM1_070809monthly1 - components070809SM1monthly$seasonal
autoplot(tsSM1_070809Adjustedm,  xlab = "Time(monthly) SM1")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjustedm), xlab = "Time(monthly) SM1")

## Holt Winters Exponential Smoothing & Plot
tsSM1_HM070809m <- HoltWinters(tsSM1_070809Adjustedm, beta=FALSE, gamma=FALSE)
plot(tsSM1_HM070809m, ylim = c(0, 25), xlab = "Time(monthly) SM1")


## HoltWinters forecast & plot
tsSM1_HM070809mfor <- forecast(tsSM3_HW070809m, h=25)
plot(tsSM1_HM070809mfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(monthly) - Sub-Meter 1")



## Forecast HoltWinters with diminished confidence levels
tsSM1_HM070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HM070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(monthly) - Sub-Meter 1", start(2010))






#tsSM1_070809quarterly1



## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM1_070809Adjustedq <- tsSM1_070809quarterly1 - components070809SM1quarterly$seasonal
autoplot(tsSM1_070809Adjustedm,  xlab = "Time(quarterly) SM1")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjustedq), xlab = "Time(quarterely) SM1")

## Holt Winters Exponential Smoothing & Plot
tsSM1_HQ070809q <- HoltWinters(tsSM1_070809Adjustedq, beta=FALSE, gamma=FALSE)
plot(tsSM1_HQ070809q, ylim = c(0, 25), xlab = "Time(quarterly) SM1")


## HoltWinters forecast & plot
tsSM1_HQ070809qfor <- forecast(tsSM1_HQ070809q, h=25)
plot(tsSM1_HQ070809qfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(quarterly) - Sub-Meter 1")



## Forecast HoltWinters with diminished confidence levels
tsSM1_HQ070809forC <- forecast(tsSM1_HQ070809q, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HQ070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(qarterly) - Sub-Meter 1", start(2010))





#Sub Meter 2


#tsSM2_070809weekly2


weekly

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809weekly2 - components070809SM2weekly$seasonal
autoplot(tsSM1_070809Adjusted,  xlab = "Time ( weekly) SM2")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted), xlab = "Time(weekly) SM2")

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25), xlab = "Time(weekly) SM2")


## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(weekly) - Sub-Meter 2")



## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(weekly) - Sub-Meter 2", start(2010))



#tsSM2_070809monthly2


## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM2_070809Adjustedm <- tsSM2_070809monthly2 - components070809SM2monthly$seasonal
autoplot(tsSM3_070809Adjustedm,  xlab = "Time(monthly) SM2")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjustedm), xlab = "Time(monthly) SM2")

## Holt Winters Exponential Smoothing & Plot
tsSM2_HM070809m <- HoltWinters(tsSM2_070809Adjustedm, beta=FALSE, gamma=FALSE)
plot(tsSM2_HM070809m, ylim = c(0, 25), xlab = "Timemonthly SM2")


## HoltWinters forecast & plot
tsSM2_HM070809mfor <- forecast(tsSM2_HM070809m, h=25)
plot(tsSM2_HM070809mfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(monthly) - Sub-Meter 2")



## Forecast HoltWinters with diminished confidence levels
tsSM2_HM070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HM070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(monthly) - Sub-Meter 2", start(2010))


#tsSM2_070809quarterly2




## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM2_070809Adjustedq <- tsSM2_070809quarterly2 - components070809SM2quarterly$seasonal
autoplot(tsSM3_070809Adjustedm,  xlab = "Time(quarterly) SM2")


## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjustedq), xlab = "Time(quarterely) SM2")

## Holt Winters Exponential Smoothing & Plot
tsSM2_HQ070809q <- HoltWinters(tsSM2_070809Adjustedq, beta=FALSE, gamma=FALSE)
plot(tsSM2_HQ070809q, ylim = c(0, 25), xlab = "Time(quarterly) SM2")


## HoltWinters forecast & plot
tsSM2_HQ070809qfor <- forecast(tsSM2_HQ070809q, h=25)
plot(tsSM2_HQ070809qfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(quarterly) - Sub-Meter 2")



## Forecast HoltWinters with diminished confidence levels
tsSM2_HQ070809forC <- forecast(tsSM2_HQ070809q, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HQ070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time(qarterly) - Sub-Meter 2", start(2010))














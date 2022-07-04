library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(plyr)
library(lubridate)

# data from - https://w2.weather.gov/climate/local_data.php?wfo=pqr

# Need to change YR to Year, MO to Month, add "Type" to variable header
# delete average column
#read data
#setwd('Downloads')
climdata <- data.frame(read.csv(file = '/Users/gt7/Desktop/Other_Projects/Rdata/Portland_dailyclimate.csv', header = T))
head(climdata)
# delete non-high temperature rows
tempdata <- climdata[climdata$Type == 'TX',]
# limit to semi recent data
newtempdata <- tempdata[tempdata$Year > '1945',]
#drop "TX" name column
newtempdata[,c("Type")] <- list(NULL)
#Change column names to numbers
colnames(newtempdata[,-1:-2]) <- c(paste(seq(1:31)))
head(newtempdata)
# unpivot data - row for every day
dailytempdata <- gather(newtempdata, Day, Max, X1:X31)
#remove X from day name
dailytempdata$Day <- substring(dailytempdata$Day,2)
#change day name and Max to numbers
dailytempdata$Day <- as.numeric(dailytempdata$Day)
dailytempdata$Max <- as.numeric(dailytempdata$Max)
#order correctly
dailytempdata <- dailytempdata[order(dailytempdata$Year, dailytempdata$Month, dailytempdata$Day), ]
dailytempdata <- dailytempdata %>%
  mutate(date = as.Date(paste(Year,"-",Month,"-",Day,sep="")))
#### grid plot month week

dtd <- dailytempdata
#cut out 2022 (or most recent year that doesn't have full data)
dtd <- dtd[dtd$Year != '2022', ]
#add a bunch of new date columns
dtd$week <- strftime(dtd$date, format = '%W')
dtd$weekday <- weekdays(dtd$date)
dtd$yearmonth <- as.yearmon(dtd$date)
dtd$yearmonthf <- factor(dtd$yearmonth)
dtd$week <- as.numeric(dtd$week)
dtd <- ddply(dtd,.(yearmonthf), transform, monthweek = 1+week-min(week))
head(dtd)


dtd$wday <- wday(dtd$date)

ggplot(dtd) +
  geom_histogram(aes(x = dtd$Max))
head(dtd)


dtd$weekyear <- strftime(dtd$date, format = "%V")
dtd$newDay <- strftime(dtd$date, format = '%j')
head(dtd)

dtd$newDay <- as.numeric(dtd$newDay)
dtd$Year <- as.numeric(dtd$Year)

#discrete temp scale
dtd$MaxDiscrete[dtd$Max < 0] <- -10
dtd$MaxDiscrete[dtd$Max > 0 & dtd$Max < 10] <- 0
dtd$MaxDiscrete[dtd$Max >= 10 & dtd$Max < 20] <- 10
dtd$MaxDiscrete[dtd$Max >= 20 & dtd$Max < 30] <- 20
dtd$MaxDiscrete[dtd$Max >= 30 & dtd$Max < 40] <- 30
dtd$MaxDiscrete[dtd$Max >= 40 & dtd$Max < 50] <- 40
dtd$MaxDiscrete[dtd$Max >= 50 & dtd$Max < 60] <- 50
dtd$MaxDiscrete[dtd$Max >= 60 & dtd$Max < 70] <- 60
dtd$MaxDiscrete[dtd$Max >= 70 & dtd$Max < 80] <- 70
dtd$MaxDiscrete[dtd$Max >= 80 & dtd$Max < 90] <- 80
dtd$MaxDiscrete[dtd$Max >= 90 & dtd$Max < 100] <- 90
dtd$MaxDiscrete[dtd$Max >= 100] <- 100
dtd$MaxDiscrete[dtd$Max >= 110] <- 110


#month days of the year

mds <- c(31, #jan
         31+28, #feb
         31+28+31, #mar
         31+28+31+30, #apr
         31+28+31+30+31, #may
         31+28+31+30+31+30, #june
         31+28+31+30+31+30+31, #jul
         31+28+31+30+31+30+31+31, #aug
         31+28+31+30+31+30+31+31+30, #sep
         31+28+31+30+31+30+31+31+30+31, #oct
         31+28+31+30+31+30+31+31+30+31+30, #nov
         31+28+31+30+31+30+31+31+30+31+30+31) #dec

t <- ggplot(dtd, aes(newDay, Year, fill =  as.factor(MaxDiscrete))) + 
  geom_tile() + 
  #facet_grid(Year~Month) + 
  #scale_fill_discrete() +
  scale_fill_manual(values = c("blue4","blue1","cornflowerblue","darkseagreen","moccasin","lightsalmon","darkgoldenrod1","darkorange","red2","black","purple","white")) +
  #scale_fill_continuous(low = "linen", high = "orangered4")+
  #scale_colour_brewer(palette = "Set2") +
  labs(title = "PDX Daily Maximums 1946 - 2021",
       #subtitle = "Data up to April 2022",
       x = "Day of Year",
       y = "Year",
       fill = "Max Temp in the x's (F)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.text=element_text(size=10))+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_x_continuous(limits = c(0,366), breaks = seq(0,365,30)) +
  scale_y_continuous(limits = c(1945,2022), breaks = seq(1946,2022,1)) +
  
  # lines indicating month breaks - whole graph
  
  #geom_segment(aes(x = mds[1], y = 1981.5, xend = mds[1], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[2], y = 1981.5, xend = mds[2], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[3], y = 1981.5, xend = mds[3], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[4], y = 1981.5, xend = mds[4], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[5], y = 1981.5, xend = mds[5], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[6], y = 1981.5, xend = mds[6], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[7], y = 1981.5, xend = mds[7], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[8], y = 1981.5, xend = mds[8], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
#geom_segment(aes(x = mds[9], y = 1981.5, xend = mds[9], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
#geom_segment(aes(x = mds[10], y = 1981.5, xend = mds[10], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
#geom_segment(aes(x = mds[11], y = 1981.5, xend = mds[11], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+

# lines indicating month breaks - just top

geom_segment(aes(x = mds[1], y = 2020.5, xend = mds[1], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[2], y = 2020.5, xend = mds[2], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[3], y = 2020.5, xend = mds[3], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[4], y = 2020.5, xend = mds[4], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[5], y = 2020.5, xend = mds[5], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[6], y = 2020.5, xend = mds[6], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[7], y = 2020.5, xend = mds[7], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[8], y = 2020.5, xend = mds[8], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[9], y = 2020.5, xend = mds[9], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[10], y = 2020.5, xend = mds[10], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[11], y = 2020.5, xend = mds[11], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  
  
  # lines indicating seasonal breaks
  # spring
  #  geom_segment(aes(x = mds[2] + 20, y = 1981.5, xend = mds[2] + 20, yend = 2017.5),
  #               size = 0.8, color = "dark green", alpha = 0.7) +
  #summer
  #  geom_segment(aes(x = mds[5] + 20, y = 1981.5, xend = mds[5] + 20, yend = 2017.5),
  #               size = 0.8, color = "dark green", alpha = 0.7) +
  #fall
  #  geom_segment(aes(x = mds[8] + 20, y = 1981.5, xend = mds[8] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +
#winter
#  geom_segment(aes(x = mds[11] + 20, y = 1981.5, xend = mds[11] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +

# month labels

annotate("text", x = mds[1]/2, y = 2022, label = "Jan", size = 4)+
  annotate("text", x = mds[2]-(mds[2]-mds[1])/2, y = 2022, label = "Feb", size = 4)+
  annotate("text", x = mds[3]-(mds[3]-mds[2])/2, y = 2022, label = "Mar", size = 4)+
  annotate("text", x = mds[4]-(mds[4]-mds[3])/2, y = 2022, label = "Apr", size = 4)+
  annotate("text", x = mds[5]-(mds[5]-mds[4])/2, y = 2022, label = "May", size = 4)+
  annotate("text", x = mds[6]-(mds[6]-mds[5])/2, y = 2022, label = "Jun", size = 4)+
  annotate("text", x = mds[7]-(mds[7]-mds[6])/2, y = 2022, label = "Jul", size = 4)+
  annotate("text", x = mds[8]-(mds[8]-mds[7])/2, y = 2022, label = "Aug", size = 4)+
  annotate("text", x = mds[9]-(mds[9]-mds[8])/2, y = 2022, label = "Sep", size = 4)+
  annotate("text", x = mds[10]-(mds[10]-mds[9])/2, y = 2022, label = "Oct", size = 4)+
  annotate("text", x = mds[11]-(mds[11]-mds[10])/2, y = 2022, label = "Nov", size = 4)+
  annotate("text", x = mds[12]-(mds[12]-mds[11])/2, y = 2022, label = "Dec", size = 4) 

t




## monthly average max

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(plyr)
library(lubridate)

# data from - https://w2.weather.gov/climate/local_data.php?wfo=pqr

# Need to change YR to Year, MO to Month, add "Type" to variable header
# delete average column
#read data
#setwd('Downloads')
climdata <- data.frame(read.csv(file = '/Users/gt7/Desktop/Other_Projects/Rdata/Portland_dailyclimate.csv', header = T))
head(climdata)
# delete non-high temperature rows
tempdata <- climdata[climdata$Type == 'TX',]
# limit to semi recent data
newtempdata <- tempdata[tempdata$Year > '1945',]
#drop "TX" name column
newtempdata[,c("Type")] <- list(NULL)
#Change column names to numbers
colnames(newtempdata[,-1:-2]) <- c(paste(seq(1:31)))
head(newtempdata)
# unpivot data - row for every day
dailytempdata <- gather(newtempdata, Day, Max, X1:X31)
#remove X from day name
dailytempdata$Day <- substring(dailytempdata$Day,2)
#change day name and Max to numbers
dailytempdata$Day <- as.numeric(dailytempdata$Day)
dailytempdata$Max <- as.numeric(dailytempdata$Max)
#order correctly
dailytempdata <- dailytempdata[order(dailytempdata$Year, dailytempdata$Month, dailytempdata$Day), ]
dailytempdata <- dailytempdata %>%
  mutate(date = as.Date(paste(Year,"-",Month,"-",Day,sep="")))
#### grid plot month week

dtd <- dailytempdata
#cut out 2022 (or most recent year that doesn't have full data)
#dtd <- dtd[dtd$Year != '2022', ]
#add a bunch of new date columns
dtd$week <- strftime(dtd$date, format = '%W')
dtd$weekday <- weekdays(dtd$date)
dtd$yearmonth <- as.yearmon(dtd$date)
dtd$yearmonthf <- factor(dtd$yearmonth)
dtd$week <- as.numeric(dtd$week)
dtd <- ddply(dtd,.(yearmonthf), transform, monthweek = 1+week-min(week))
head(dtd)

month_temp <- aggregate(Max ~ Month + Year,       # Aggregate data
                        dtd,
                        FUN = mean)


dtd$wday <- wday(dtd$date)

ggplot(dtd) +
  geom_histogram(aes(x = dtd$Max))
head(dtd)


dtd$weekyear <- strftime(dtd$date, format = "%V")
dtd$newDay <- strftime(dtd$date, format = '%j')
head(dtd)

dtd$newDay <- as.numeric(dtd$newDay)
dtd$Year <- as.numeric(dtd$Year)

#discrete temp scale
month_temp$MaxDiscrete[month_temp$Max < 0] <- -10
month_temp$MaxDiscrete[month_temp$Max > 0 & month_temp$Max < 10] <- 0
month_temp$MaxDiscrete[month_temp$Max >= 10 & month_temp$Max < 20] <- 10
month_temp$MaxDiscrete[month_temp$Max >= 20 & month_temp$Max < 30] <- 20
month_temp$MaxDiscrete[month_temp$Max >= 30 & month_temp$Max < 40] <- 30
month_temp$MaxDiscrete[month_temp$Max >= 40 & month_temp$Max < 50] <- 40
month_temp$MaxDiscrete[month_temp$Max >= 50 & month_temp$Max < 60] <- 50
month_temp$MaxDiscrete[month_temp$Max >= 60 & month_temp$Max < 70] <- 60
month_temp$MaxDiscrete[month_temp$Max >= 70 & month_temp$Max < 80] <- 70
month_temp$MaxDiscrete[month_temp$Max >= 80 & month_temp$Max < 90] <- 80
#dtd$MaxDiscrete[dtd$Max >= 90 & dtd$Max < 100] <- 90
#dtd$MaxDiscrete[dtd$Max >= 100] <- 100
#dtd$MaxDiscrete[dtd$Max >= 110] <- 110


#month days of the year

mds <- c(31, #jan
         31+28, #feb
         31+28+31, #mar
         31+28+31+30, #apr
         31+28+31+30+31, #may
         31+28+31+30+31+30, #june
         31+28+31+30+31+30+31, #jul
         31+28+31+30+31+30+31+31, #aug
         31+28+31+30+31+30+31+31+30, #sep
         31+28+31+30+31+30+31+31+30+31, #oct
         31+28+31+30+31+30+31+31+30+31+30, #nov
         31+28+31+30+31+30+31+31+30+31+30+31) #dec

t <- ggplot(month_temp, aes(Month, Year, fill =  as.factor(MaxDiscrete))) + 
  geom_tile() + 
  #facet_grid(Year~Month) + 
  #scale_fill_discrete() +
  scale_fill_manual(values = c("blue4","blue1","cornflowerblue","darkseagreen","moccasin","lightsalmon","darkgoldenrod1","darkorange","red2","black","purple","white")) +
  #scale_fill_continuous(low = "linen", high = "orangered4")+
  #scale_colour_brewer(palette = "Set2") +
  labs(title = "PDX Average Monthly High Temperatures 1946 - 2022",
       #subtitle = "Data up to April 2022",
       x = "Day of Year",
       y = "Year",
       fill = "Average High Temp in the x's (F)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.text=element_text(size=10))+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_x_continuous(limits = c(0,13), breaks = seq(0,12,1)) +
  scale_y_continuous(limits = c(1945,2023), breaks = seq(1946,2022,1)) +
  
  # lines indicating month breaks - whole graph
  
  #geom_segment(aes(x = mds[1], y = 1981.5, xend = mds[1], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[2], y = 1981.5, xend = mds[2], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[3], y = 1981.5, xend = mds[3], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[4], y = 1981.5, xend = mds[4], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[5], y = 1981.5, xend = mds[5], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[6], y = 1981.5, xend = mds[6], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[7], y = 1981.5, xend = mds[7], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
  #geom_segment(aes(x = mds[8], y = 1981.5, xend = mds[8], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
#geom_segment(aes(x = mds[9], y = 1981.5, xend = mds[9], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
#geom_segment(aes(x = mds[10], y = 1981.5, xend = mds[10], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+
#geom_segment(aes(x = mds[11], y = 1981.5, xend = mds[11], yend = 2017.5), color = "grey", size = 0.4, alpha = 0.5)+

# lines indicating month breaks - just top

geom_segment(aes(x = mds[1], y = 2020.5, xend = mds[1], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[2], y = 2020.5, xend = mds[2], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[3], y = 2020.5, xend = mds[3], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[4], y = 2020.5, xend = mds[4], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[5], y = 2020.5, xend = mds[5], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[6], y = 2020.5, xend = mds[6], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[7], y = 2020.5, xend = mds[7], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[8], y = 2020.5, xend = mds[8], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[9], y = 2020.5, xend = mds[9], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[10], y = 2020.5, xend = mds[10], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  geom_segment(aes(x = mds[11], y = 2020.5, xend = mds[11], yend = 2022), color = "grey", size = 0.4, alpha = 0.5)+
  
  
  # lines indicating seasonal breaks
  # spring
  #  geom_segment(aes(x = mds[2] + 20, y = 1981.5, xend = mds[2] + 20, yend = 2017.5),
  #               size = 0.8, color = "dark green", alpha = 0.7) +
  #summer
  #  geom_segment(aes(x = mds[5] + 20, y = 1981.5, xend = mds[5] + 20, yend = 2017.5),
  #               size = 0.8, color = "dark green", alpha = 0.7) +
  #fall
  #  geom_segment(aes(x = mds[8] + 20, y = 1981.5, xend = mds[8] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +
#winter
#  geom_segment(aes(x = mds[11] + 20, y = 1981.5, xend = mds[11] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +

# month labels

annotate("text", x = 1, y = 2023, label = "Jan", size = 4)+
  annotate("text", x = 2, y = 2023, label = "Feb", size = 4)+
  annotate("text", x = 3, y = 2023, label = "Mar", size = 4)+
  annotate("text", x = 4, y = 2023, label = "Apr", size = 4)+
  annotate("text", x = 5, y = 2023, label = "May", size = 4)+
  annotate("text", x = 6, y = 2023, label = "Jun", size = 4)+
  annotate("text", x = 7, y = 2023, label = "Jul", size = 4)+
  annotate("text", x = 8, y = 2023, label = "Aug", size = 4)+
  annotate("text", x = 9, y = 2023, label = "Sep", size = 4)+
  annotate("text", x = 10, y = 2023, label = "Oct", size = 4)+
  annotate("text", x = 11, y = 2023, label = "Nov", size = 4)+
  annotate("text", x = 12, y = 2023, label = "Dec", size = 4) 

t



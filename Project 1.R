setwd("/Users/blake/Documents/UVA/Fall 2018/DS 4001/Chicago-Crime")
setwd("~/Desktop/DS/Project1")
#Read in the data
to2004 <- read.csv("Chicago_Crimes_2001_to_2004.csv")
to2007 <- read.csv("Chicago_Crimes_2005_to_2007.csv")
to2011 <- read.csv("Chicago_Crimes_2008_to_2011.csv")
to2017 <- read.csv("Chicago_Crimes_2012_to_2017.csv")
#load the appriopriate libraries

library(varhandle)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(gridExtra)

## Code to extract only crimes in year 2001
some_date <- lubridate::mdy_hms(to2004$Date)
only_2001 <- to2004[year(some_date) == 2001,]
## Code to extract only crimes in year 2008
some_date3 <- lubridate::mdy_hms(to2011$Date)
only_2008 <- to2011[year(some_date3) == 2008,]
## Code to extract only crimes in year 2012
some_date4 <- lubridate::mdy_hms(to2017$Date)
only_2012 <- to2017[year(some_date4) == 2012,]
## Code to extract only crimes in year 2017
only_2017 <- to2017[year(some_date4) == 2017,]

##Only going to use data after 2008
Chicago.Aft.2008 <- rbind(to2011, to2017)
## Select only the necessary columns to help run time
Chicago.Aft.2008.Small <- Chicago.Aft.2008 %>% select(Date, Primary.Type, Location.Description, Arrest, Year, Latitude, Longitude)
Chicago.Aft.2008.Small <- na.omit(Chicago.Aft.2008.Small) ## removes any NA's

Chicago.Aft.2008.Small$Longitude <- unfactor(Chicago.Aft.2008.Small$Longitude) ## Unfactors longitude
Chicago.Aft.2008.Small$Latitude <- unfactor(Chicago.Aft.2008.Small$Latitude) ## Unfactors latitude
Chicago.Aft.2008.Small$Latitude <- as.numeric(Chicago.Aft.2008.Small$Latitude) ## Changes latitude to a numeric 
Chicago.Aft.2008.Small$Longitude <- as.numeric(Chicago.Aft.2008.Small$Longitude) ## Changes latitude to a numeric 
Chicago.Aft.2008.Small$Day <- factor(day(as.POSIXlt(Chicago.Aft.2008.Small$Date, format="%m/%d/%Y %I:%M:%S %p")))
Chicago.Aft.2008.Small$Month <- factor(month(as.POSIXlt(Chicago.Aft.2008.Small$Date, format="%m/%d/%Y %I:%M:%S %p")))
Chicago.Aft.2008.Small$Year <- factor(year(as.POSIXlt(Chicago.Aft.2008.Small$Date, format="%m/%d/%Y %I:%M:%S %p")))

Chicago.Aft.2008.Small$Date <- as.Date(Chicago.Aft.2008$Date, "%m/%d/%Y %I:%M:%S %p")
Chicago.Aft.2008.Small <- Chicago.Aft.2008.Small[Chicago.Aft.2008.Small$Year != "2017", ] ##exclude the year 2017
Chicago.Aft.2008.Small <- na.omit(Chicago.Aft.2008.Small) ## removes any NA's introduced by coercion
str(Chicago.Aft.2008.Small) ##make sure our variables are of the proper class

## Google Maps Code
get_googlemap(urlonly = TRUE)  
Chicago_Map <- ggmap(get_googlemap(center = c(lon = -87.6298, lat = 41.8781),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

## Group by the num of crimes
by_Crime <- Chicago.Aft.2008.Small %>% group_by(Primary.Type) 
numCrimes <- summarize(by_Crime, num = n())
View(numCrimes)
numCrimes <- numCrimes[numCrimes$num>5000,]

## Histogram of the Crimes Committed
ggplot(numCrimes, aes(x=Primary.Type, y = num)) + geom_bar(stat = "identity") + coord_flip() +
  xlab("Crime") + ylab("Number of Crimes Committed") + ggtitle("Distribution of Crimes Committed") +
  theme(
    axis.text.y = element_text(vjust = .5, hjust = 1, size = 5)
  )

by_Location <- Chicago.Aft.2008.Small %>% group_by(Location.Description) 
numLocations <- summarize(by_Location, num = n())
numLocations <- numLocations[numLocations$num>5000,]

ggplot(numLocations, aes(x=Location.Description, y = num)) + geom_bar(stat = "identity") + coord_flip() +
  xlab("Location") + ylab("Number of Crimes Committed") + ggtitle("Distribution of Locations") +
  theme(
    axis.text.y = element_text(vjust = .5, hjust = 1, size = 5)
  )


## All of the crimes that have a count greater than 5000
Primary_Crimes <- c("NARCOTICS", "ASSAULT", "GAMBLING", "KIDNAPPING", "ROBBERY","HOMICIDE")
## Extract only the crimes that had more than 5000
Chicago.Aft.2008.Small <- Chicago.Aft.2008.Small[Chicago.Aft.2008.Small$Primary.Type %in% Primary_Crimes,]
Chicago.Aft.2008.Small$Month <- month(Chicago.Aft.2008.Small$Date) 
Chicago.Aft.2008.Small$Month <- as.factor(Chicago.Aft.2008.Small$Month)
Chicago.Aft.2008.Small$Hour <- hour(Chicago.Aft.2008.Small$Date) 
Chicago.Aft.2008.Small$Hour <- as.factor(Chicago.Aft.2008.Small$Hour)


## Lineplot to look at the number of crimes per month
## First we need to groupby the date
by_Date <- Chicago.Aft.2008.Small%>% group_by(Date) %>% summarise(Total = n())
by_Date2 <- Chicago.Aft.2008.Small%>% group_by(Date, Primary.Type) %>% summarise(Total = n())
by_Date_Month <- Chicago.Aft.2008.Small%>% group_by(months=month(Date), Primary.Type) %>% summarise(Total = n())
by_Crime <- Chicago.Aft.2008.Small%>% group_by(Primary.Type, Location.Description) %>% summarize(Total = n())
by_Crime <- by_Crime[by_Crime$Total>10,]

## Now plotting the graphic
ggplot(by_Date, aes(x=by_Date$Date, y=by_Date$Total)) + geom_smooth(se=FALSE) + 
  xlab("Date") + ylab("Total Amount of Crimes") + ggtitle("Number of Crimes per Day") 

ggplot(by_Date2, aes(x=by_Date2$Date, y=by_Date2$Total, color = by_Date2$Primary.Type)) + geom_smooth(se = FALSE) + 
  xlab("Date") + ylab("Total Amount of Crimes") + ggtitle("Number of Crimes per Day") +
  labs(color = "Crime")

## From the graphic we see that there was a steep decline in crime from 2010 to 2012

## plot the distribution of the amount of crimes per month
ggplot(Chicago.Aft.2008.Small, aes(x=Month)) +
  geom_bar(colour="black", fill="white") +
  ylab('Count') + ggtitle("Distribution of Crimes per Month") +
  xlab("Month")

## From the graphic it appears as though the most crimes happen around July and August. Falling off dramatically in 
## November and December

Chicago.Aft.2008.Small <- Chicago.Aft.2008.Small[Chicago.Aft.2008.Small$Primary.Type%in%Primary_Crimes,]
## PLot the distribution for each crime type by month

ggplot(Chicago.Aft.2008.Small, aes(x=Month)) +
  geom_bar(colour="black", fill="white") +
  ylab('Count') + ggtitle("Distribution of Crimes per Month") +
  xlab("Month") + facet_wrap(~Chicago.Aft.2008.Small$Primary.Type, scales='free')

## From the graphic it appears as though the most crimes happen around July and August. Falling off dramatically in 
## November and December.

## This is to help adjust the x axis values so the more popular crimes show only the more popular locations and
## the less popular crimes can show all the locations
by_Crime <- by_Crime[((by_Crime$Total > 2000 & by_Crime$Primary.Type %in% c("ASSAULT", "NARCOTICS", "ROBBERY")) | 
            (by_Crime$Primary.Type %in% c("GAMBLING","HOMICIDE","KIDNAPPING") )),]

ggplot(by_Crime, aes(x=Location.Description, y=Total)) +
  geom_bar(stat = "identity",colour="black", fill="white") +
  ylab('Count') + ggtitle("Distribution of Crimes per Month") +
  xlab("Month") + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 6)) +
  facet_wrap(.~Primary.Type, scales = "free")

## Attempt to try and make a heat map'

## Colors for the gradient scale
colors <- c('navyblue', 'darkmagenta', 'darkorange1')

## Gets only the rows that resulted in arrests
arrests_data <- Chicago.Aft.2008.Small[Chicago.Aft.2008.Small$Arrest == 'True',]
## Groups the arrests count by year and month with a total which is the count
arrests_count <- arrests_data %>% group_by(Year, Month) %>% summarise(Total = n())
## Creates a tile map of the number of arrests by month and year
arrests <- ggplot(arrests_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") + scale_fill_gradientn(colors = colors) + 
  geom_text(aes(label=Total), color='white', size = 3) +
  ggtitle("Arrests by Year and Month")

## Groups by the Yean and month with a total which is count
crime_count <- Chicago.Aft.2008.Small %>% group_by(Year, Month) %>% summarise(Total = n())

crimes <- ggplot(crime_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") + scale_fill_gradientn(colors = colors) + 
  geom_text(aes(label=Total), color='white', size = 3) +
  ggtitle("Crimes by Year and Month")
crimes 

grid.arrange(crimes, arrests, ncol = 2)

total <- cbind(crime_total=crime_count$Total, arrests_count)
total$prop <- total$Total/total$crime_total

prop <- ggplot(total, aes(Year, Month, fill = total$prop)) +
  geom_tile(size = 1, color = "white") + scale_fill_gradientn(colors = colors) + 
  geom_text(aes(label=round(total$prop, digits = 3)), color='white', size = 3) +
  labs(fill = "Proportion") +
  ggtitle("Proportion of Crimes Resulting in \nArrests by Year and Month") + 
  theme(legend.title = element_text(hjust = .5),  ## Centered the legend title 
        plot.title = element_text(hjust = .5) ## Centered the plot title
  )
prop

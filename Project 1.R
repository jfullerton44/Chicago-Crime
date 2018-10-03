
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)



#setwd("/Users/blake/Documents/UVA/Fall 2018/DS 4001/Project 1")

setwd("~/Desktop/DS/Project1")
to2004 <- read.csv("Chicago_Crimes_2001_to_2004.csv",stringsAsFactors=FALSE)
to2007 <- read.csv("Chicago_Crimes_2005_to_2007.csv",stringsAsFactors=FALSE)
to2011 <- read.csv("Chicago_Crimes_2008_to_2011.csv",stringsAsFactors=FALSE)
to2017 <- read.csv("Chicago_Crimes_2012_to_2017.csv",stringsAsFactors=FALSE)
all <- rbind(to2004,to2007,to2011,to2017)

backup<- all
all<-backup
crimes <- filter(all,Year>2007)
crimes <-filter(crimes,Year<2018)
crimes$Longitude <- as.numeric(crimes$Longitude)
crimes$Latitude <- as.numeric(crimes$Latitude)
hasLocation <- filter(crimes, !is.na(Longitude),!is.na(Latitude))
crimesPerYear<-count(crimes,Year)
typesOfCrimes <- count(hasLocation, Primary.Type)
year2017 <- filter(to2017,Year==2017)

rand1000 <-sample_n(hasLocation,1000)


gambling <- filter(crimes,Primary.Type=='GAMBLING')
gamblingNew <- filter(crimes,Primary.Type=='GAMBLING',as.numeric(Year)>2007)

gambling <- filter(gambling,!is.na(Year))
# Create some color variables for graphing later
col1 = "#011f4b" ## sapphire
col2 = "#6497b1" ## moderate cyan/moderate cornflower blue
col3 = "#b3cde0" ## very light blue
col4 = "#CC0000" ## red

get_googlemap(urlonly = TRUE)
ggmap(get_googlemap(key='AIzaSyBzD0JsQd1dyQzz8iUcB4sqyzhRSD17mGQ'))

##1) Create a map with all of the crime locations plotted.
p <- ggmap(get_googlemap(center = c(lon = -87.645167, lat = 41.808013),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color',
                         key = 'AIzaSyBzD0JsQd1dyQzz8iUcB4sqyzhRSD17mGQ'
                         ))


Kidnappingp + geom_point(aes(x = Longitude, y = Latitude,  colour = Primary.Type), data = rand1000, size = 0.5) + 
  theme(legend.position="bottom")



p + geom_point(aes(x = Longitude, y = Latitude,  colour = Year), data = singleRand, size = 0.5) + 
  theme(legend.position="bottom")






map <- get_map(location=c(lon=-87.645167,lat=41.808013), zoom=11, maptype='roadmap', color='bw')#Get the map from Google Maps


singleCrime <- filter(hasLocation, Primary.Type =="BURGLARY")
ggmap(map, extent = "device") + geom_density2d(data = singleCrime, aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = singleCrime, 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 50, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)





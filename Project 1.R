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
to2004 <- read.csv("Chicago_Crimes_2001_to_2004.csv")
to2007 <- read.csv("Chicago_Crimes_2005_to_2007.csv")
to2011 <- read.csv("Chicago_Crimes_2008_to_2011.csv")
to2017 <- read.csv("Chicago_Crimes_2012_to_2017.csv")
all <- rbind(to2004,to2007,to2011,to2017)

incidents <- to2004
incidents$Latitude <- as.numeric(as.character(incidents$Latitude))
incidents$Longitude <- as.numeric(as.character(incidents$Longitude))
# Create some color variables for graphing later
col1 = "#011f4b" ## sapphire
col2 = "#6497b1" ## moderate cyan/moderate cornflower blue
col3 = "#b3cde0" ## very light blue
col4 = "#CC0000" ## red

get_googlemap(urlonly = TRUE)
ggmap(get_googlemap())

##1) Create a map with all of the crime locations plotted.
p <- ggmap(get_googlemap(center = c(lon = -87.835167, lat = 41.808013),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
p + geom_point(aes(x = Longitude, y = Latitude,  colour = Primary.Type), data = incidents, size = 0.5) + 
  theme(legend.position="bottom")

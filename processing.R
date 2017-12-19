##-----------------

rm(list=ls())

##load packages----
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(ggrepel)
library(plyr)
library(sqldf)
library(knitr)
library(ggmap)
library(rowr)
library(viridis)
library(ggmap)
library(googleway)
library(stplanr)
library(colorspace)
library(scales)
library(tidyverse)


setwd("/Users/Daniel/Documents/Bikeshare Data/")

source("/Users/Daniel/Documents/Bikeshare Data/R script/ingesting functions.R")
source("/Users/Daniel/Documents/Bikeshare Data/R script/tallying functions.R")

##----

bs <- read.csv("/Users/Daniel/Documents/Bikeshare Data/datasources/Q316.csv")

q117_cleaned <- cleaning("/Users/Daniel/Documents/Bikeshare Data/datasources/Q12017.csv")
q416_cleaned <- cleaning("/Users/Daniel/Documents/Bikeshare Data/datasources/Q416.csv")
q316_cleaned <- cleaning("/Users/Daniel/Documents/Bikeshare Data/datasources/Q316.csv")
q216_cleaned <- cleaning("/Users/Daniel/Documents/Bikeshare Data/datasources/Q216.csv")


#stations <- stations.function("/Users/Daniel/Documents/Bikeshare Data/bs.locs.csv")

##aggregate inflow and outflow by station number and per hour


Q117_count <- count_allridesperhour_function(q117_cleaned)
Q216_count <- count_allridesperhour_function(q216_cleaned)
Q416_count <- count_allridesperhour_function(q416_cleaned)
Q316_count <- count_allridesperhour_function(q316_cleaned)

##full year

full <- rbind(q117_cleaned, q416_cleaned, q316_cleaned, q216_cleaned)
write.csv(full, "/Users/Daniel/Documents/Bikeshare Data/datasources/fullyear.csv")
full_count <- count_allridesperhour_function(full)

ok <- read.csv("/Users/Daniel/Documents/Bikeshare Data/datasources/full_count.csv") %>% 
  select(-X) %>% 
  melt(id.vars = c("Start.station.number", "starthour"))


plot_singlestation_yearlyrides("31005")

plotsinglestation <- full_count %>% 
  filter(Start.station.number == "31000") 


##plotting the sum rides per hour over the Quarter for a single station
##negative values are rides in, positive is rides out

ggplot(data = plotsinglestation, mapping = aes(x = starthour, y = value, fill = variable))+
  geom_bar(stat = "identity", width = 1, alpha = 0.5)+
  geom_vline(xintercept=hour(Sys.time()), col = "brown")+
  labs(x = "Hour", y = "Number of Rides", 
       title = "Rides Inflow and Outflow by Station")+
  xlim(0,24)






##aggregating by station, date, and hour
count_ridesperdayperhour_hourfunction <- function(data) {
trips_startstation_perhour = plyr::count(data,
                          .(Start.station.number, startdate, starthour))
trips_endstation_perhour = plyr::count(data,
                                         .(End.station.number, enddate, endhour))

full_join(trips_startstation_perhour, trips_endstation_perhour,
          by = c("Start.station.number" = "End.station.number",
                 "startdate" = "enddate",
                 "starthour" = "endhour"))
}



trips_station_perhour <- count_ridesperdayperhour_hourfunction(bs)


day <- trips_station_perhour %>% 
  filter(Start.station.number == "31000",
         startdate == "2017-01-04") %>% 
  melt(id.vars = c("Start.station.number", "startdate", "starthour"))

ggplot(data = day, mapping = aes(x = starthour, y = value, fill = variable))+
  geom_bar(stat = "identity", width = 1, alpha = 0.5)+
  xlim(0,24)

##Data ready for sampling
trips_daily <- read.csv("/Users/Daniel/Documents/Bikeshare Data/aggregateddailytrips.csv")



#sample <- getSample.basic(trips_daily, n)
#plotSample(sample, 14)
#geocode <- getRevGeocode(sample)
#write.csv(geocode, "sample50_revgeocode.csv")
#routes <- getCyclingRoutes(geocode)
#plotRoutes(sample, 12, routes)


unique <- plyr::count(trips_daily,
                     .(Start.station.number, End.station.number, start.lat, start.lon, end.lat, end.lon))

unique <- unique[order(-unique$freq),]
unique$id <- rownames(unique)


##run this
routes_polylines <- getRoutes(unique) 
output <- prepDF(routes_polylines)
merged <- left_join(ok, unique1, by = "id")
merged1 <- merged %>% 
  select(long, lat, Start.station.number, End.station.number, freq.y) 
coordinates(merged1) <- ~long+lat
proj4string(merged1) = CRS("+init=epsg:4326")
merged2 <- merged1[1:1000,]

##


wards <- readOGR(dsn = path.expand("./datasources/Wards2012/wards2012"), 
                                   layer = "Ward_from_2012")
wards <- spTransform(wards, "+init=epsg:4326")

wards.fort <- fortify(wards)

##figure out color rescaling, because right now the divergence is too large

dc <- shapefile("./datasources/DC_boundary/DC_boundary.shp")
dc <- spTransform(dc, "+init=epsg:4326")
dc.fort <- fortify(dc)

streets <- shapefile("./datasources/dc_street_centerlines/Street_Centerlines.shp")
streets <- spTransform(dc, "+init=epsg:4326")
streets.fort <- fortify(streets)


ggmap(get_map(location = c(lon = mean(sample$start.lon), lat = mean(sample$start.lat)),
        zoom = 14
       ), darken = 0.95)+  #completely random location 
  geom_polygon(data = dc.fort, aes(x=long, y = lat, group = group), 
  col = "white", size = 0.4, alpha = 0)+
  geom_line(data = streets.fort, aes(x=long, y = lat, group=group), col = "white")+
  geom_path(data = ok, aes(x=long, y = lat, group = group,
                                  col = freq), size = 0.4)+
  #scale_alpha(limits= c(0, max(ok$freq)/3), guide = FALSE)+
  scale_colour_gradientn(colours = c("blue", "yellow", "red"),
                         values=c(0, 0.1, 0.4, 1),
                         guide = "colourbar")+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x =  element_blank())+
  theme(axis.text.y =  element_blank())+
  coord_cartesian()


##add in Q2 and Q3, then make station-level day-long timeseries to include 
##in a shiny file

rideData <- read.csv("/Users/Daniel/Documents/Bikeshare Data/datasources/full_count.csv") %>% 
  select(-X) %>% 
  mutate(inflow = freq.y,
         outflow = freq.x) %>% 
  select(-freq.y) %>% 
  select(-freq.x) %>% 
  melt(id.vars = c("Start.station.number", "starthour")) 

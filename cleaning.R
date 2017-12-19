##get latlon to work as variables, then do basic direction mapping
##run three other quarters through functions 


rm(list=ls())

setwd("/Users/Daniel/Documents/Bikeshare Data/")

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

cleaning <- function(data){
  bs = read.csv(data, stringsAsFactors = FALSE)
  
  bs$startdatetime <- mdy_hm(as.character(bs$Start.date))
  bs$enddatetime <- mdy_hm(as.character(bs$End.date))
  bs$starthour <- hour(bs$startdatetime)
  bs$startminute <- minute(bs$startdatetime)
  bs$endhour <- hour(bs$enddatetime)
  bs$endminute <- minute(bs$enddatetime)
  bs$daytime <- "Morning"
  bs[bs$starthour > 11,]$daytime <- "Afternoon"
  bs[bs$starthour > 17,]$daytime <- "Evening"
  bs$daytime <- as.factor(bs$daytime)
  bs$morningcommute <- "N"
  bs$morningcommute[bs$starthour > 6 & bs$starthour < 9] <- "Y"
  bs$eveningcommute <- "N"
  bs$eveningcommute[bs$starthour > 16 & bs$starthour < 19] <- "Y"
  bs$duration.seconds <- as.double(as.duration(bs$Duration / 1000))
  bs$month.num <- month(bs$startdate)
  bs$month.name <- months(bs$startdate)
  bs$dow.num <- lubridate::wday(bs$startdate)
  bs$dow.name <- lubridate::wday(bs$startdate, label = T)
  bs$startdate <- as.Date(bs$startdatetime)
  bs$endddate <- as.Date(bs$enddatetime)
  bs$startstation <- as.factor(bs$Start.station)
  bs$endstation <- as.factor(bs$End.station)
  bs$Bike.number[which(bs$Bike.number %in% "?(0x0000000074BEBCE4)")] <- "W99999"
  bs$bikenumber <- as.factor(bs$Bike.number)
  bs$weekday <- "Weekday"
  bs$weekday[bs$dow.name %in% c("Sat", "Sun")] <- "Weekend"
  bs$weekday <- as.factor(bs$weekday)
  bs$bikenumber <- as.factor(bs$bikenumber)
  
  bs
}

bs <- cleaning("/Users/Daniel/Documents/Bikeshare Data/Q12017.csv")

write.csv(bs, "clean_bs_Q12017.csv")

##Ride Count by start hour and conditioned on Weekday vs. Weekend, 
##filled by member vs. casual
ggplot(bs, aes(x = starthour, fill = Member.Type), stat ="count") +
  geom_histogram(binwidth = 1) +
  facet_grid(. ~weekday)
ggplot(bs, aes(x = starthour, fill = Member.Type), stat ="count") +
  geom_histogram(binwidth = 1) +
  facet_grid(. ~month.name)
ggplot(bs, aes(x = starthour, fill = Member.Type), stat ="count") +
  geom_histogram(binwidth = 1) +
  facet_grid(. ~dow.name)
ggplot(bs, aes(x = weekday, fill = Member.Type), stat ="count") +
  geom_bar()
ggplot(bs, aes(x = dow.name, fill = Member.Type), stat ="count") +
  geom_bar()

##Station Date Processing----
stations.function <- function(data){
  stations <- read.csv(data)
  
  stations$ID <- as.factor(stations$ID)
  stations$OBJECTID <- as.factor(stations$OBJECTID)
  stations$station.num <- stations$TERMINAL_NUMBER
  stations$rides.out <- apply(stations, 1, function(station) nrow(bs[which(bs$Start.station.number == station['TERMINAL_NUMBER']),]))
  stations$rides.in <- apply(stations, 1, function(station) nrow(bs[which(bs$End.station.number == station['TERMINAL_NUMBER']),]))
  stations$traffic <- stations$rides.in + stations$rides.out
  stations$amount.net <- stations$rides.in - stations$rides.out
  stations$amount.net.percent <- 100*stations$amount.net/stations$traffic
  stations <- stations[order(-stations$traffic),]
  #stations$amount.net.percent.bin <- "large net outflow"
  #stations$amount.net.percent.bin[stations$amount.net.percent >= -1,] <- "small net outflow"
  #stations$amount.net.percent.bin[stations$amount.net.percent >= 0,] <- "small net inflow"
  #stations$amount.net.percent.bin[stations$amount.net.percent >= 1,] <- "large net inflow"
  
  
  stations
}


##Processing data and visualizing bike station locations

stations.data <- stations.function("/Users/Daniel/Documents/Bikeshare Data/bs.locs.csv")

write.csv(stations.data, "stations_locations.csv")

stations.data <- read.csv("stations_locations.csv")

long <- mean(stations.data$LONGITUDE)
lat <- mean(stations.data$LATITUDE)

dcmap <- get_map(location = c(long, lat),
                 zoom = 10, color = "bw")

ggmap(dcmap, ylab = "Latitude", xlab = "Longitude", darken = 0.5)+
  geom_point(data = stations.data, aes(x = LONGITUDE, y = LATITUDE, 
                                       col = stations.data$amount.net.percent.bin), 
             size = 0.1)+
  scale_color_viridis(discrete = TRUE)

ggmap(dcmap, ylab = "Latitude", xlab = "Longitude", darken = 0.5)+
  geom_point(data = stations.data, aes(x = LONGITUDE, y = LATITUDE, col = "red"), 
             size = 0.1)+
  theme(legend.position = "none")


##FINDING MODES OF RIDE FREQUENCY PER BIKE------------

bikes <- count(bs, 'bikenumber')

find_modes <- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] > x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}

plotBikeUsageHistogram <- function(bikes){
  md <- find_modes(density(bikes$freq)$y)
  
  ggp <- ggplot(bikes, aes(freq)) + geom_histogram(binwidth = 10) 
  ggp <- ggp + geom_vline(xintercept = density(bikes$freq)$x[md][1], color = "red", linetype="dashed")
  ggp <- ggp + geom_vline(xintercept = density(bikes$freq)$x[md][2], color = "red", linetype="dashed")
  
  ggp
}

##Plotting Bike Usage to find modes visually

plotBikeUsageHistogram(bikes)
usageModes <- find_modes(density(bikes$freq)$y)

bs <- read.csv("clean_bs_Q12017.csv")
##Aggregate trips by paired starting and ending stations, 
##also with member.type, daytime, weekday----------------

aggregateTrips <- function(data){
  trips_daily = plyr::count(data[data$Start.station.number != data$End.station.number,],
                       .(Start.station.number, End.station.number, Member.Type, daytime, weekday))
  
  trips_daily <- cbind.fill(trips_daily, ldply(apply(trips_daily, 1, 
                                                     function(trip) stations.data[stations.data$TERMINAL_NUMBER == trip['Start.station.number'],
                                                    'LATITUDE']), data.frame), fill = NULL)
  colnames(trips_daily)[colnames(trips_daily) == 'X..i..'] <- 'start.lat'
  
  trips_daily <- cbind.fill(trips_daily, ldply(apply(trips_daily, 1, 
                                                     function(trip) stations.data[stations.data$TERMINAL_NUMBER == trip['Start.station.number'],
                                                    'LONGITUDE']), data.frame), fill = NULL)
  colnames(trips_daily)[colnames(trips_daily) == 'X..i..'] <- 'start.lon'
  
  trips_daily <- cbind.fill(trips_daily, ldply(apply(trips_daily, 1, 
                                                     function(trip) stations.data[stations.data$TERMINAL_NUMBER == trip['End.station.number'],
                                                    'LATITUDE']), data.frame), fill = NULL)
  colnames(trips_daily)[colnames(trips_daily) == 'X..i..'] <- 'end.lat'
  
  trips_daily <- cbind.fill(trips_daily, ldply(apply(trips_daily, 1, 
                                                     function(trip) stations.data[stations.data$TERMINAL_NUMBER == trip['End.station.number'],
                                                     'LONGITUDE']), data.frame), fill = NULL)
  colnames(trips_daily)[colnames(trips_daily) == 'X..i..'] <- 'end.lon'
  
  trips_daily
}

trips_daily <- aggregateTrips(bs)

write.csv(trips_daily, file = "/Users/Daniel/Documents/Bikeshare Data/aggregateddailytrips.csv")
##------

##Subset data to visualize some of it

trips_daily <- read.csv("/Users/Daniel/Documents/Bikeshare Data/aggregateddailytrips.csv")

getSample.full <- function(data, n){
  data.ordered <- data[order(-data$freq),]
  sample <- data.ordered[1:n,] ##change this to change the sample size
  
  sample
}

getSample.basic <- function(data, n){
  unique = plyr::count(data,
                              .(Start.station.number, End.station.number, start.lat, start.lon, end.lat, end.lon))
  unique.ordered <- unique[order(-unique$freq),]
  sample <- unique.ordered[1:n,] ##change this to change the sample size
  sample
  
}

sample1 <- getSample.basic(trips_daily, 50)
sample2 <- getSample.full(trips_daily, 20)

#Reordering daytime variable into a new factor for correct faceting in mapping
sample$daytime.f <- factor(sample$daytime, levels = c("Morning", "Afternoon", "Evening"))

##14 is the limit on zoom
##Mapping top 50 most frequent routes in the Q1 period

##mapping top 200 based on data source and zoom aspect
plotSample <- function(sample, zoom){
  dcmap <- get_map(location = c(mean(sample$start.lon), mean(sample$start.lat)),
                   zoom = zoom, color = "bw")
  
  ggmap(dcmap, ylab = "Latitude", xlab = "Longitude", darken = 0.5)+
    geom_curve(data = sample, aes(x = start.lon, xend = end.lon,
                                  y = start.lat, yend = end.lat, alpha = freq), 
               curvature = 0.05, inherit.aes = TRUE, arrow = arrow(length = unit(0.2, "cm"))) +
    scale_alpha(limits=c(0, max(sample$freq)), guide=FALSE) +
    coord_cartesian()
}

plotSample(sample1, 14)


plotSample(sample2, 14)+
  facet_grid(.~weekday)

##refine visualization of top 200
##identify stations by inflow and outflow at extreme levels
##describe the quality of the directions of the top 200 routes based on daytime and weekday
##look into using shiny to show timeseries for individual stations on a given day

##EXPERIMENTAL API KEY USED
getRevGeocode <- function(sample){
  start.revgeocode <- do.call(rbind,
                              lapply(1:nrow(sample),
                                     function(i)revgeocode(as.numeric(sample[i,4:3])),
                                     api = "google", key = "AIzaSyDacwBjgWD9UkwW55eK8GGgGZeedIxtuPM"))
  end.revgeocode <- do.call(rbind,
                              lapply(1:nrow(sample),
                                     function(i)revgeocode(as.numeric(sample[i,6:5])), 
                    api = "google", key = "AIzaSyDacwBjgWD9UkwW55eK8GGgGZeedIxtuPM"))
  sample <- cbind(sample,start.revgeocode, end.revgeocode)
  
  sample
}

sample3 <- getRevGeocode(sample1)

##get the routes from google maps
getCyclingRoutes <- function(data){
  a <- function(startingpoint, endpoint){
    Sys.sleep(0.2)
    route(from = startingpoint, 
        to = endpoint,
        mode = "bicycling",
        structure = "route")
}
  calculatedroutes <- mapply(a,
                             startingpoint = as.character(data$start.revgeocode),
                             endpoint = as.character(data$end.revgeocode),
                             SIMPLIFY = FALSE)
  
  long_routes <- do.call(rbind.data.frame, lapply(names(calculatedroutes), function(x) {
    cbind.data.frame(route=x, calculatedroutes[[x]], stringsAsFactors=FALSE)
  })) 
}

CyclingRoutes <- getCyclingRoutes(sample3)

##plotting
plotRoutes <- function(sample1, zoom, routes){
  basicmap <- get_map(location = c(lon = mean(sample1$start.lon), lat = mean(sample1$start.lat)),
                    zoom = zoom,
                    maptype = "roadmap",
                    source = "google",
                    color = "bw")  #completely random location
basicmap <- ggmap(basicmap, darken = 0.75)

basicmap + geom_path(data=routes, 
                     aes(x=lon, y=lat, group=route, color=route),
                     size=1, linetype = "solid",
                     arrow = arrow(length = unit(0.3, "cm")))+
  geom_curve(data = sample1, aes(x = start.lon, xend = end.lon,
                               y = start.lat, yend = end.lat, alpha = freq, size = freq, col = "white"), 
            curvature = 0.05, inherit.aes = TRUE, arrow = arrow(length = unit(0.1, "cm")))+
  scale_size(limits=c(0, max(sample1$freq)*2), guide=FALSE)+
  scale_alpha(limits=c(0, max(sample1$freq)*2), guide=FALSE)+ ##*2 factor introduced to keep all alpha
 # geom_point(data = sample1,
  #           aes(x=end.lon, y=end.lat, col = "red", size = freq))+
 # scale_size(limits=c(0, max(sample1$freq)), guide=FALSE)+
  theme(legend.position = "none")+
  coord_cartesian()

}

sample1 <- getSample.basic(trips_daily, 50)
sample3 <- getRevGeocode(sample1)
write.csv(sample3, "data_wihRevgeocode.csv")
CyclingRoutes <- getCyclingRoutes(sample3)
write.csv(CyclingRoutes, "CyclingRoutes.csv")
plotRoutes(sample1, 14, CyclingRoutes)

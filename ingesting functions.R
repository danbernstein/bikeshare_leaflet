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


setwd("/Users/Daniel/Documents/Bikeshare Data/")

##import and clean data----

cleaning <- function(data){
  bs = read.csv(data, stringsAsFactors = FALSE)
  
  colnames(bs) <- c("duration.ms", "Start.date", "End.date", "Start.station.number",
                   "Start.station", "End.station.number", "End.station", "Bike.number",
                   "Member.Type")
  bs$startdatetime <- mdy_hm(as.character(bs$Start.date))
  bs$startdate <- as.Date(bs$startdatetime)
  bs$starthour <- hour(bs$startdatetime)
  bs$startminute <- minute(bs$startdatetime)
  
  bs$enddatetime <- mdy_hm(as.character(bs$End.date))
  bs$enddate <- as.Date(bs$enddatetime)
  bs$endhour <- hour(bs$enddatetime)
  bs$endminute <- minute(bs$enddatetime)
  
  bs$daytime <- as.factor(case_when(bs$starthour > 17 ~ "Evening",
                                    bs$starthour > 12 ~ "Afternoon",
                                    TRUE ~ "Morning"))
  
  bs$morningcommute <- FALSE
  bs$morningcommute[bs$starthour > 6 & bs$starthour < 9] <- TRUE
  bs$eveningcommute <- FALSE
  bs$eveningcommute[bs$starthour > 16 & bs$starthour < 19] <- TRUE
  
  bs$duration.sec <- as.double(as.duration(bs$duration.ms / 1000))
  bs$month.num <- month(bs$startdate)
  bs$month.name <- months(bs$startdate)
  bs$dow.num <- lubridate::wday(bs$startdate)
  bs$dow.name <- lubridate::wday(bs$startdate, label = T)
  bs$Start.station <- as.factor(bs$Start.station)
  bs$End.station <- as.factor(bs$End.station)
  bs$Bike.number[which(bs$Bike.number %in% "?(0x0000000074BEBCE4)")] <- "W99999"
  bs$bikenumber <- as.factor(bs$Bike.number)
  bs$weekend <- FALSE
  bs$weekend[bs$dow.name %in% c("Sat", "Sun")] <- TRUE
  bs$bikenumber <- as.factor(bs$bikenumber)

  bs
}


##import and clean station data----
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


##find modes of bike use and plot----
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

##aggregate trips data at the start and end station key pair-----
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


##Sampling, full is all features, basic is only a few----
getSample.full <- function(data, n){
  data.ordered <- data[order(-data$freq),]
  sample <- data.ordered[1:n,] ##change this to change the sample size
  
  sample
}

getSample.basic <- function(data, n){
  unique = plyr::count(data,
                       .(Start.station.number, End.station.number, start.lat, start.lon, end.lat, end.lon)) %>% 
  unique.ordered <- unique[order(-unique$freq),]
  sample <- unique.ordered[1:n,] ##change this to change the sample size
  sample
  
}

##plot sample data ----
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

##Reverse geocode the locations to then get routes----
getRevGeocode <- function(sample){
  start.revgeocode <- do.call(rbind,
                              lapply(1:nrow(sample),
                                     function(i)revgeocode(as.numeric(sample[i,4:3]))#,
                                    # api = "google", key = "AIzaSyDacwBjgWD9UkwW55eK8GGgGZeedIxtuPM"
                                     ))
  end.revgeocode <- do.call(rbind,
                            lapply(1:nrow(sample),
                                   function(i)revgeocode(as.numeric(sample[i,6:5]))#, 
                                  # api = "google", key = "AIzaSyDacwBjgWD9UkwW55eK8GGgGZeedIxtuPM"
                                   ))
  sample <- cbind(sample,start.revgeocode, end.revgeocode)
  
  sample
}


##get cycling routes from google maps----
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

##plot all routes----

##Zoom limit is 20+
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



getRoutes <- function(odf){
  odf$ID <- seq.int(nrow(odf))
  
  l <- vector("list", nrow(odf))
  for(i in 1:nrow(odf)){
    o = c(odf$start.lon[i], odf$start.lat[i])
    d = c(odf$end.lon[i], odf$end.lat[i])
    l[[i]] <- sp::Lines(list(sp::Line(rbind(o, d))), as.character(i))
  }
  
  l <- sp::SpatialLines(l)
  proj4string(l) <- CRS("+init=epsg:4326")
  l <- SpatialLinesDataFrame(l, odf, match.ID = "ID")
  
  routes_fast <- line2route(l = l, route_fun = route_osrm)
  routes_fast$freq <- l$freq
  rnet <- overline(routes_fast, "freq", fun = sum) 
  rnet$flow <- rnet$freq / mean(rnet$freq) * 3
  rnet$freq <- rescale(rnet$freq, c(0,1))
  
  rnet
}


prepDF <- function(network){
  id=rownames(network@data)
  rnet.df    <- data.frame(id=rownames(network@data),
                           network@data, stringsAsFactors=F)
  data_fort   <- fortify(network)
  data_merged <- join(data_fort, rnet.df, by="id")
}


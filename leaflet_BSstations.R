library(sp)
library(leaflet)

source("/Users/Daniel/Documents/Bikeshare Data/R script/ingesting functions.R")
source("/Users/Daniel/Documents/Bikeshare Data/R script/tallying functions.R")


rides <- read.csv("/Users/Daniel/Documents/Bikeshare Data/datasources/full_count.csv")

bikestations_data_raw <- read.csv("/Users/Daniel/Documents/Bikeshare Data/stations_locations.csv")

bikestations_data <- bikestations_data_raw %>% 
  select(c("ADDRESS", "TERMINAL_NUMBER", "LONGITUDE", "LATITUDE")) %>% 
  mutate(long = LONGITUDE,
         lat = LATITUDE) %>% 
  select(-LONGITUDE, -LATITUDE)

bikes <- list(bikestations_data,
              rides)


coordinates(bikestations_data) <- ~LONGITUDE+LATITUDE
proj4string(bikestations_data) = CRS("+init=epsg:4326")


leaflet(data = bikes[[1]]) %>% addTiles() %>% 
  addCircles(~long, ~lat, 
             popup = ~as.character(ADDRESS),
             label = ~as.character(ADDRESS),
             color = "red") %>% 
  addProviderTiles("CartoDB")


combo <- left_join(rides, bikestations_data, by = c("Start.station.number" = "TERMINAL_NUMBER"))

rideData <- bikes[[2]] %>% 
  select(-X) %>% 
  mutate(inflow = freq.y,
         outflow = freq.x) %>% 
  mutate(net = outflow - inflow,
         sum = outflow + inflow) %>% 
  select(-freq.y) %>% 
  select(-freq.x) %>% 
  melt(id.vars = c("Start.station.number", "starthour")) 


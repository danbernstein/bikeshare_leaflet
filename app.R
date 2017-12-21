library(shiny)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(readr)
library(leaflet)
library(sp)
library(stplanr)
library(plyr)


##data 

##for individual station inflow and outflow for ggplot
inflow_outflow_bystation <-read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data1/fullyear_keypair_aggregated_byhour.csv")

flows.data <- inflow_outflow_bystation %>% 
  mutate(inflow = freq.y,
         outflow = freq.x) %>% 
  select(-freq.y) %>% 
  select(-freq.x) %>% 
  melt(id.vars = c("Start.station.number", "starthour")) 

##for selecting individual stations on the map
bikestations_data_raw <-read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data1/stations_locations.csv")

bikestations_data <- bikestations_data_raw %>% 
  select(c("ADDRESS", "TERMINAL_NUMBER", "LONGITUDE", "LATITUDE")) %>% 
  mutate(long = LONGITUDE,
         lat = LATITUDE) %>% 
  select(-LONGITUDE, -LATITUDE)

##for mapping the routesb between staations
keypairs_latlon <- read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data1/fullyear_keypair_lonlat.csv") 
keypairs_latlon <- keypairs_latlon[order(-keypairs_latlon$freq),]

routing_function <- function(odf){
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
}


ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("leaflet", width = "100%", height = "100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 55, left = "auto", right = 10, bottom = "auto",
                  width = 400, height = "auto", 
                selectInput("Start.station.number", 
                    label = "Station:",
                    choices = c(unique(inflow_outflow_bystation$Start.station.number)),
                    selected = "31000"),
                sliderInput("routes", 
                             label = "Number of Mapped Routes:",
                             min = 1,
                             max = 5,
                            value = 3,
                            step = 1,
                            animate = T),
                    plotOutput("stationPlot", height = 200),
                    style = "opacity: 0.95"),
    absolutePanel(top =5, right = 10,
                    width = 400, height = "auto", 
                      verbatimTextOutput("Click_text"))
)


flows.data <- inflow_outflow_bystation %>% 
  mutate(inflow = freq.y,
         outflow = freq.x) %>% 
  select(-freq.y) %>% 
  select(-freq.x) %>% 
  melt(id.vars = c("Start.station.number", "starthour")) 

keypairs_latlon <- read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data1/fullyear_keypair_lonlat.csv") 
keypairs_latlon <- keypairs_latlon[order(-keypairs_latlon$freq),]

bikestations_data <- bikestations_data_raw %>% 
  select(c("ADDRESS", "TERMINAL_NUMBER", "LONGITUDE", "LATITUDE")) %>% 
  mutate(long = LONGITUDE,
         lat = LATITUDE) %>% 
  select(-LONGITUDE, -LATITUDE)




server <- function(input, output, session) {
  
  
  reactiveDF <- reactive({return((flows.data) %>% 
                                 filter(flows.data$Start.station.number == input$Start.station.number))})

  reactiveDF2 <- reactive({return((bikestations_data) %>% 
                                  filter(TERMINAL_NUMBER == input$Start.station.number) )}) 
  reactiveDF3 <- reactive({return((bikestations_data) %>% 
                                  filter(TERMINAL_NUMBER != input$Start.station.number) )}) 

  reactiveDF5 <- reactive({return((keypairs_latlon) %>% 
                                    filter(Start.station.number == input$Start.station.number,
                                           Start.station.number != End.station.number) %>% 
                                    arrange(desc(as.numeric(freq))) %>% 
                                    slice(1:input$routes) %>% 
                                    as.data.frame() %>% 
                                    routing_function() )})
   
output$stationPlot <- renderPlot({
    
    print(ggplot(data = reactiveDF(), aes_string(x = "starthour", y = "value", 
                                                 fill = "variable"))+
            geom_bar(stat = "identity", width = 1, alpha = 0.5)+
            geom_vline(xintercept=hour(as.POSIXct(Sys.time(), tz = "EST")), col = "brown")+
            labs(x = "Hour", y = "Number of Rides", 
                 title = "Station Inflow and Outflow Throughout The Day")+
            guides(fill=guide_legend(title=element_blank()))+
            scale_x_continuous(limits=c(0, 24), expand = c(0, 0)) )
  output$dayplot <- renderPlot({
    print(ggplot(data = me, mapping = aes(x = startdate, y = net))+
        geom_line(aes(group = 1))+
        scale_x_date(date_breaks = "1 month",date_labels = "%b") + xlab("month")  
    )
  })
  
            
  })
  
  
 output$leaflet <- renderLeaflet({
   leaflet() %>% 
     setView(-77.06279, 38.91519, 11) %>% ##mean of stations lat and lon
      addProviderTiles("CartoDB.DarkMatter")
 })
 
  observe({
    leafletProxy("leaflet") %>% 
      clearControls() %>% 
      clearShapes() %>% 
      addCircleMarkers(data = reactiveDF2(), 
                       radius = 4,
                       color = "red",
                       fillOpacity = 1,
                     #  popup = ~as.character(TERMINAL_NUMBER),
                       label = ~as.character(ADDRESS),
                       layerId = ~as.character(TERMINAL_NUMBER)) %>% 
      addCircleMarkers(data = reactiveDF3(),
                       radius = 2,
                       color = "blue",
                       fillOpacity = 0,
                   #   popup = ~as.character(TERMINAL_NUMBER),
                       label = ~as.character(ADDRESS),
                       layerId = ~as.character(TERMINAL_NUMBER)) %>% 
      addPolylines(data = reactiveDF5(), color = "red")
   #   addCircles(data = merged1,
   #                radius = 0.1,
   #                color = "green",
   #                fillOpacity = 1,
   #                layerId = ~Start.station.number)
    })
    
   observe({
     click <- input$leaflet_marker_click
     if(is.null(click))
       return()
     text <- paste("Lat", click$lat, "Long", click$lng)
     text2 <- paste("Selected Location:", bikestations_data[which(bikestations_data$TERMINAL_NUMBER == click$id),]$ADDRESS)
     output$Click_text <- renderText({
       text2})
       
  
     
  observeEvent(input$Start.station.number,{
       updateSelectInput(session, "Start.station.number", label = "Station:",
                         choices = c(unique(inflow_outflow_bystation$Start.station.number)),
                         selected = c(input$Start.station.number))
     })
     
  observeEvent(input$leaflet_marker_click,{
     click <- input$leaflet_marker_click
     station <- bikestations_data[which(bikestations_data$TERMINAL_NUMBER == input$leaflet_marker_click$id),]$TERMINAL_NUMBER
     updateSelectInput(session, "Start.station.number", label = "Station:",
                       choices = c(unique(inflow_outflow_bystation$Start.station.number)),
                       selected = c(input$Start.station.number, station))
                 
    })

  })
 
  }
  


shinyApp(ui, server)

##mlinke station clicks on map to changing the numericinput 
##add polylines from a clicked station to the top five destinations
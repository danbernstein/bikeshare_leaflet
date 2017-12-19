library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
library(leaflet)
library(sp)
library(stplanr)

rides <-read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data1/full_count.csv")

bikestations_data_raw <-read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data1/stations_locations.csv")

unique <- read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data1/aggregateddailytrips.csv")

doit <- function(odf){
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


bikestations_data <- bikestations_data_raw %>% 
  select(c("ADDRESS", "TERMINAL_NUMBER", "LONGITUDE", "LATITUDE")) %>% 
  mutate(long = LONGITUDE,
         lat = LATITUDE) %>% 
  select(-LONGITUDE, -LATITUDE)

bikes <- list(bikestations_data,
              rides, 
              unique)

rideData <- bikes[[2]] %>% 
  select(-X1) %>% 
  mutate(inflow = freq.y,
         outflow = freq.x) %>% 
  mutate(net = outflow - inflow,
         sum = outflow + inflow) %>% 
  select(-freq.y) %>% 
  select(-freq.x) %>% 
  melt(id.vars = c("Start.station.number", "starthour")) 



ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("leaflet", width = "100%", height = "100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                  width = 400, height = "auto", 
                selectInput("Start.station.number", 
                    label = "Station:",
                    choices = c(unique(rides$Start.station.number)),
                    selected = "31000"),
                    plotOutput("stationPlot", height = 200),
                    style = "opacity: 0.95"),
    absolutePanel(top =5, right = 10,
                    width = 400, height = "auto", 
                      verbatimTextOutput("Click_text"))
)


rideData <- bikes[[2]] %>% 
  select(-X1) %>% 
  mutate(inflow = freq.y,
         outflow = freq.x) %>% 
  mutate(net = outflow - inflow,
         sum = outflow + inflow) %>% 
  select(-freq.y) %>% 
  select(-freq.x) %>% 
  melt(id.vars = c("Start.station.number", "starthour")) 

stationname <- bikes[[1]]

unique <- bikes[[3]]





server <- function(input, output, session) {
  
  
  reactiveDF <- reactive({return((rideData) %>% 
                                 filter(rideData$Start.station.number == input$Start.station.number))})

  reactiveDF2 <- reactive({return((bikes[[1]]) %>% 
                                  filter(TERMINAL_NUMBER == input$Start.station.number) )}) 
  reactiveDF3 <- reactive({return((bikes[[1]]) %>% 
                                  filter(TERMINAL_NUMBER != input$Start.station.number) )}) 

  reactiveDF4 <- reactive({return((merged2) %>% 
                                    filter(Start.station.number == input$Start.station.number) )}) 
 
  reactiveDF5 <- reactive({return((unique) %>% 
                                    filter(Start.station.number == input$Start.station.number) %>% 
                                    top_n(., n = 3) %>% 
                                    doit() )})
   
output$stationPlot <- renderPlot({
    
    print(ggplot(data = reactiveDF(), aes_string(x = "starthour", y = "value", 
                                                 fill = "variable"))+
            geom_bar(stat = "identity", width = 1, alpha = 0.5)+
            geom_vline(xintercept=hour(Sys.time()), col = "brown")+
            labs(x = "Hour", y = "Number of Rides", 
                 title = "Rides Inflow and Outflow by Station")+
            guides(fill=guide_legend(title="Flows"))+
            xlim(0,24))
   
  })
  
  
 output$leaflet <- renderLeaflet({
   leaflet() %>% 
     setView(-77.06279, 38.91519, 12) %>% ##mean of stations lat and lon
  #   addTiles() %>% 
  #   addCircleMarkers(data = bikes[[1]],
  #                    radius = 2,
  #                    color = "blue",
  #                    fillOpacity = 0,
  #                    popup = ~as.character(TERMINAL_NUMBER),
  #                    label = ~as.character(ADDRESS),
  #                    layerId = ~TERMINAL_NUMBER) %>% 
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
     text2 <- paste("Selected Location:", click$id)
     output$Click_text <- renderText({
       text2})
       
  
     
  observeEvent(input$Start.station.number,{
       updateSelectInput(session, "Start.station.number", label = "Station:",
                         choices = c(unique(rides$Start.station.number)),
                         selected = c(input$Start.station.number))
     })
     
  observeEvent(input$leaflet_marker_click,{
     click <- input$leaflet_marker_click
     station <- bikestations_data[which(bikestations_data$TERMINAL_NUMBER == input$leaflet_marker_click$id),]$TERMINAL_NUMBER
     updateSelectInput(session, "Start.station.number", label = "Station:",
                       choices = c(unique(rides$Start.station.number)),
                       selected = c(input$Start.station.number, station))
                 
    })

  })
 


  
  
  }
  



shinyApp(ui, server)

##mlinke station clicks on map to changing the numericinput 
##add polylines from a clicked station to the top five destinations
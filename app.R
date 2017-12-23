##load packages----
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
#library(dygraphs)


##data ----

##the shiny.io app.R file is old, with the GitHub directories to the data files incorrect since renaming
##think about having two tabs, one is just the full map with controls, the other a split screen with access to dygraph data
##or maybe on the same page but you scroll down for split screen


##for individual station inflow and outflow for ggplot
flows.data <-read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data/flows.data.csv")

##inflow outflow for dygraph
#dygraph_inputs <- read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data/dygraph_inputs.csv")
#dygraph_inputs = ok
###for selecting individual stations on the map
bikestations_data <-read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data/bikestations_data.csv")

###for mapping the routesb between staations
keypairs_latlon <- read_csv("https://raw.githubusercontent.com/danbernstein/bikeshare_leaflet/master/data/keypairs_latlon.csv") 

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

##ui----
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("leaflet", width = "100%", height = "100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 65, left = "auto", right = 0, bottom = "auto",
                  width = "35%", height = "auto", 
               selectInput("Start.station.number", 
                   label = "Station:",
                   choices = c(unique(flows.data$Start.station.number)),
                   selected = "31263"),
                radioButtons("Origin",
                             "Is the station the origin or the destination?",
                             c("Origin" = "Origin",
                               "Destination" = "Destination"
                               )),
                sliderInput("routes", 
                             label = "Number of Most-Travelled Routes:",
                             min = 1,
                             max = 10,
                            value = 3,
                            step = 1,
                            animate = T),
                  plotOutput("stationPlot", height = 200),
                  style = "opacity: 0.95"),
    absolutePanel(top =5, right = 0,
                    width = "50%", height = "auto", 
                      verbatimTextOutput("Click_text")),
    absolutePanel(top = "1.5%", left = "5%", width = 175, draggable = FALSE,
                  selectInput("bmap", label = NULL,
                              choices = c("CartoDB.Positron",
                                          "CartoDB.DarkMatter",
                                          "Stamen"),
                              selected = "CartoDB.Positron"))
  
      )
    

##server----
server <- function(input, output, session) {
  
##reactive elements----
  reactiveDF <- reactive({return((flows.data) %>% 
                                 filter(Start.station.number == input$Start.station.number))})

  reactiveDF2 <- reactive({return((bikestations_data) %>% 
                                  filter(TERMINAL_NUMBER == input$Start.station.number) )}) 
  reactiveDF3 <- reactive({return((bikestations_data) %>% 
                                  filter(TERMINAL_NUMBER != input$Start.station.number) )}) 

  #reactive.dygraph <- reactive({return((dygraph_inputs) %>% 
  #                                       filter(Start.station.number == input$leaflet_marker_click$id |
  #                                                End.station.number == input$leaflet_marker_click$id) %>%
  #                                       mutate(Month = as.POSIXct(Month)) %>%                                          
  #                                       group_by(Month) %>%  ##here is the grouping variable to use
  #                                       dplyr::summarize(freq.start = sum(Start.station.number == input$leaflet_marker_click$id),
  #                                                        freq.end = sum(End.station.number == input$leaflet_marker_click$id)) %>% 
  #                                       timetk::tk_xts(start = 2016, freq = 12))
#
  #})
  
  reactive_bikeroutes <- reactive({
    if(input$Origin == "Origin"){
    return((keypairs_latlon) %>% 
        filter(Start.station.number == input$Start.station.number,
        Start.station.number != End.station.number) %>% 
        arrange(desc(as.numeric(freq))) %>% 
        slice(1:input$routes) %>% 
        as.data.frame() %>% 
        routing_function())}
    else{
      return((keypairs_latlon) %>% 
              filter(End.station.number == input$Start.station.number,
              Start.station.number != End.station.number) %>% 
              arrange(desc(as.numeric(freq))) %>% 
              slice(1:input$routes) %>% 
              as.data.frame() %>% 
              routing_function()) }
                                        })
                                        
  
#reactive_bikeroutes <- 
#   reactive({
#  return((keypairs_latlon) %>% 
#                                          filter(End.station.number == input$Start.station.number,
#                                                 Start.station.number != End.station.number) %>% 
#                                          arrange(desc(as.numeric(freq))) %>% 
#                                          slice(1:input$routes) %>% 
#                                          as.data.frame() %>% 
#                                          routing_function() )})
#   
output$stationPlot <- renderPlot({
###ggplot ----    
   print(ggplot(data = reactiveDF(), aes_string(x = "starthour", y = "value", 
                                                fill = "variable"))+
           geom_bar(stat = "identity", width = 1, alpha = 0.5)+
           geom_vline(xintercept=hour(as.POSIXct(Sys.time(), tz = "EST")), col = "brown")+
           labs(x = "Hour", y = "Number of Rides", 
                title = "Station Inflow and Outflow Throughout The Day")+
           guides(fill=guide_legend(title=element_blank()))+
           scale_x_continuous(limits=c(0, 24), expand = c(0, 0))+
           theme(text = element_text(family = "Century Gothic"),
                 axis.text = element_text(size = 8),
                 axis.title = element_text(size = 12, face = "bold"),
                 legend.position = c(0.95,0.75)                      
                 ))
#  output$dayplot <- renderPlot({
#    print(ggplot(data = me, mapping = aes(x = startdate, y = net))+
#        geom_line(aes(group = 1))+
#        scale_x_date(date_breaks = "1 month",date_labels = "%b") + xlab("month")  
#    )
#  })
  
            
  })

  
output$leaflet <- renderLeaflet({
   leaflet() %>% 
     setView(-76.9566191, 38.899584, 11)# %>% ##mean of stations lat and lon
 })
 
  observe({
    leafletProxy("leaflet") %>% 
      addProviderTiles(input$bmap) %>% 
      clearControls() %>% 
      clearShapes() %>% 
      addCircleMarkers(data = reactiveDF2(), 
                       radius = 5,
                       color = "red",
                       fillOpacity = 1,
                     #  popup = ~as.character(TERMINAL_NUMBER),
                       label = ~as.character(ADDRESS),
                       layerId = ~as.character(TERMINAL_NUMBER)) %>% 
      addCircleMarkers(data = reactiveDF3(),
                       radius = 3,
                       color = "blue",
                       fillOpacity = 0,
                   #   popup = ~as.character(TERMINAL_NUMBER),
                       label = ~as.character(ADDRESS),
                       layerId = ~as.character(TERMINAL_NUMBER)) %>% 
      addPolylines(data = reactive_bikeroutes(), color = "red", 
                   dashArray = "7") #%>% 
   #   addPolylines(data = reactive_bikeroutes_destination(), color = "green")
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
     text2 <- paste("Selected Location:", bikestations_data[which(bikestations_data$TERMINAL_NUMBER == input$leaflet_marker_click$id),]$ADDRESS)
     output$Click_text <- renderText({
       text2})
       
  
     
  observeEvent(input$Start.station.number,{
       updateSelectInput(session, "Start.station.number", label = "Station:",
                         choices = c(unique(flows.data$Start.station.number)),
                         selected = c(input$Start.station.number))
     })
     
  observeEvent(input$leaflet_marker_click,{
    click <- input$leaflet_marker_click
    station <- bikestations_data[which(bikestations_data$TERMINAL_NUMBER == input$leaflet_marker_click$id),]$TERMINAL_NUMBER
    updateSelectInput(session, "Start.station.number", label = "Station:",
                      choices = c(unique(flows.data$Start.station.number)),
                      selected = c(input$Start.station.number, station))
    
       
    })

  })
 
  }

shinyApp(ui, server)

##mlinke station clicks on map to changing the numericinput 
##add polylines from a clicked station to the top five destinations
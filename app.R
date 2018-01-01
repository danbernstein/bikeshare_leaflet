##load packages----
library(shiny)
library(leaflet)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
library(sp)
library(stplanr)
library(plyr)
library(ggmap)
library(rgdal)


##data ----
##for individual station inflow and outflow for ggplot
flows.data <-read_csv("https://raw.githubusercontent.com/danbernstein/capitalbikeshareviz/master/data/flows.data.csv")

###for selecting individual stations on the map
bikestations_data <-read_csv("https://raw.githubusercontent.com/danbernstein/capitalbikeshareviz/master/data/bikestations_data.csv")

###for mapping the routesb between staations
keypairs_latlon <- read_csv("https://raw.githubusercontent.com/danbernstein/capitalbikeshareviz/master/data/keypairs_latlon.csv") 

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
ui <- navbarPage("bikeshareviz", id = "nav", theme = "https://raw.githubusercontent.com/danbernstein/capitalbikeshareviz/master/styles.css",
    tabPanel("Interactive map", 
           div(class = "outer",
              
              tags$head(
                includeCSS("https://raw.githubusercontent.com/danbernstein/capitalbikeshareviz/master/styles.css")
                        ),
                
    leafletOutput("leaflet", width = "100%", height = "100%"),
    
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = "20%", left = "auto", right = "3%", bottom = "auto",
                  width = "37%", height = "auto", opacity = 0.4,
                  
                  h2("Historical Data"),
               selectInput("Start.station.number", 
                   label = "Station:",
                   choices = c(unique(flows.data$Start.station.number)),
                   selected = "31263"),
                radioButtons("Origin",
                             "Is the station the origin or the destination?",
                             c("Origin" = "Origin",
                               "Destination" = "Destination"
                               )
                             ),
                sliderInput("routes", 
                             label = "Number of Most-Travelled Routes:",
                             min = 1,
                             max = 10,
                            value = 3,
                            step = 1,
                            animate = T
                            ),
                  plotOutput("stationPlot", height = 200)
               ),
    absolutePanel(top = "5%", right = "3%",
                    width = "50%", height = "auto", 
                      verbatimTextOutput("Click_text")
                  ),
    absolutePanel(top = "2%", left = "5%", width = 175, draggable = FALSE,
                  selectInput("bmap", label = NULL,
                              choices = c("CartoDB.Positron",
                                          "CartoDB.DarkMatter",
                                          "Stamen"
                                          ),
                              selected = "CartoDB.Positron"
                              )
                  )
      )
    )#,
  
   # tabPanel("Codevelopment", 
   #          div(class = "outer",
   #              
   #              tags$head(
   #                includeCSS("/Users/Daniel/Documents/Bikeshare Data/Shiny/www/styles.css")
   #                       ),
   #     mainPanel(
   #       leafletOutput("leaflet2", width = "1000", height = "500"),
   #       absolutePanel(width = "400", height = "600", top = "10%", right = "10%", 
   #                        draggable = T,
   #                        sliderInput("YEAR", 
   #                                    "Year:",
   #                                    c(unique(stations_firstuse$YEAR)),
   #                                    min = 2009,
   #                                    max = 2016,
   #                                    value = 2009, 
   #                                    step = 1,
   #                                    sep = "",
   #                                    animate =  animationOptions(interval = 1200)
   #                                    )
   #                        )
   #          ) 
    )#))

##server----
server <- function(input, output, session) {
  
##reactive elements----
  reactiveDF <- reactive({return((flows.data) %>% 
                                 filter(Start.station.number == input$Start.station.number))})

  reactiveDF2 <- reactive({return((bikestations_data) %>% 
                                  filter(TERMINAL_NUMBER == input$Start.station.number) )}) 
  reactiveDF3 <- reactive({return((bikestations_data) %>% 
                                  filter(TERMINAL_NUMBER != input$Start.station.number) )}) 
  
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
           theme(axis.text = element_text(size = 8),
                 axis.title = element_text(size = 12, face = "bold"),
                 legend.position = c(0.12,0.82)                      
                 ))
  })

output$leaflet <- renderLeaflet({
   leaflet() %>% 
     setView(-76.9566191, 38.899584, 11) ##mean of stations lat and lon
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
                       label = ~as.character(ADDRESS),
                       layerId = ~as.character(TERMINAL_NUMBER)) %>% 
      addCircleMarkers(data = reactiveDF3(),
                       radius = 3,
                       color = "blue",
                       fillOpacity = 0,
                       label = ~as.character(ADDRESS),
                       layerId = ~as.character(TERMINAL_NUMBER)) %>% 
      addPolylines(data = reactive_bikeroutes(), color = "red", 
                   dashArray = "7") 
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
  })}

shinyApp(ui, server)
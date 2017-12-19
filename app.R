library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)


rideData <- bikes[[2]] %>% 
  select(-X) %>% 
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
                  draggable = TRUE, top = 40, left = "auto", right = 20, bottom = "auto",
                  width = 400, height = "auto", 
                selectInput("Start.station.number", 
                    label = "Station:",
                    choices = c(unique(rides$Start.station.number)),
                    selected = "31000"),
                    plotOutput("stationPlot", height = 200),
                    style = "opacity: 0.95"),
    absolutePanel(top =10, right = 10,
                    width = 400, height = "auto", 
                      verbatimTextOutput("Click_text"))
)


rideData <- bikes[[2]] %>% 
  select(-X) %>% 
  mutate(inflow = freq.y,
         outflow = freq.x) %>% 
  mutate(net = outflow - inflow,
         sum = outflow + inflow) %>% 
  select(-freq.y) %>% 
  select(-freq.x) %>% 
  melt(id.vars = c("Start.station.number", "starthour")) 

stationname <- bikes[[1]]





server <- function(input, output, session) {
  
  
  reactiveDF <- reactive({return((rideData) %>% 
                                 filter(rideData$Start.station.number == input$Start.station.number))})

  reactiveDF2 <- reactive({return((bikes[[1]]) %>% 
                                  filter(TERMINAL_NUMBER == input$Start.station.number) )}) 
  reactiveDF3 <- reactive({return((bikes[[1]]) %>% 
                                  filter(TERMINAL_NUMBER != input$Start.station.number) )}) 

  reactiveDF4 <- reactive({return((merged2) %>% 
                                    filter(Start.station.number == input$Start.station.number) )}) 
 
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
                       layerId = ~as.character(TERMINAL_NUMBER))# %>% 
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
count_allridesperhour_function <- function(data) {
  trips_startstation_perhour = plyr::count(data,
                                           .(Start.station.number, starthour))
  trips_endstation_perhour = plyr::count(data,
                                         .(End.station.number, endhour))
  trips_endstation_perhour$freq = -abs(trips_endstation_perhour$freq)
  
  trips = full_join(trips_startstation_perhour, trips_endstation_perhour,
                    by = c("Start.station.number" = "End.station.number",
                           "starthour" = "endhour"))
  
  trips
}


##input a given station, will filter Q216-Q117 and plots on 24 hour 
plot_singlestation_yearlyrides <- function(n){
  full_count %>% 
    filter(Start.station.number == n) %>% 
    melt(id.vars = c("Start.station.number", "starthour")) %>% 
    ggplot(data = ., mapping = aes(x = starthour, y = value, fill = variable))+
    geom_bar(stat = "identity", width = 1, alpha = 0.5)+
    geom_vline(xintercept=hour(Sys.time()), col = "brown")+
    labs(x = "Hour", y = "Number of Rides", 
         title = "Rides Inflow and Outflow by Station")+
    xlim(0,24)
}

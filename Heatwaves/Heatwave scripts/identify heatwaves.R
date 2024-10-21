# identify heatwaves from the previously identified sites

library(heatwaveR)

# create a new column which has site_station
all.temp = all.temp %>% mutate(site_station = paste(site, station, sep = "_"))

site_stations = unique(all.temp$site_station)

i = 1
for(i in 1:length(site_stations)){
  
  cur.site.station = site_stations[i]
  cur.all.temp = all.temp %>% filter(site_station == cur.site.station)
  
  # format for heatwaveR
  cur.all.temp = cur.all.temp %>% rename(t = date, temp = temp_mean) %>% 
    select(t, temp)
  
  climOutput = ts2clm(cur.all.temp, climatologyPeriod = c(min(cur.all.temp$t), max(cur.all.temp$t)))
  hwOutput = detect_event(climOutput)$event
  
  store = detect_event(climOutput)
  
  hwOutput$site_station = cur.site.station
  
  if(i == 1){
    hw.all = hwOutput
  }
  if(i > 1){
    hw.all = rbind(hw.all, hwOutput)
  }
  
}


## there are 3000 heatwaves
# average length is 8.5 days

hw.all = hw.all %>% select(site_station, everything())

hw.stats = hw.all %>% 
  group_by(site_station) %>% 
  summarize(n.hw = n(), hw.duration = mean(duration, na.rm= TRUE)) 

hw.stats = hw.stats %>% mutate(site = substr(site_station, 1, 3))


ggplot(hw.stats, aes(x = site, y = n.hw, fill = site))+
  geom_boxplot()+
  theme_classic()


ggplot(hw.stats %>% filter(site != "kac"), aes(x = site, y = hw.duration, fill = site))+
  geom_boxplot()+
  theme_classic()


ggplot(hw.all, aes(x = duration)) +
  geom_histogram( fill = "darkred", color = "black") +
  labs(title = "", x = "Duration of heatwaves", y = "Frequency") +
  theme_minimal()+
  scale_x_log10(labels = scales::comma) 
  

min.date = min(store$climatology$t)
max.date = max(store$climatology$t)
event_line(store, spread = 300, category = TRUE)



## save the heatwaves dataframe ##
write.csv(hw.all, "./Heatwaves/Heatwave results/NERR heatwave events 2024-10-21.csv", row.names = FALSE)

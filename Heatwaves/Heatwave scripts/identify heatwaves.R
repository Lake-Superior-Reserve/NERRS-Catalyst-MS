# identify heatwaves from the previously identified sites

library(heatwaveR)
library(tidyverse)
library(dplyr)
library(purrr)

# create a new column which has site_station
all.temp = all.temp %>% mutate(site_station = paste(site, station, sep = "_"))

site_stations = unique(all.temp$site_station)

#write.csv(all.temp, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/all data daily.csv", row.names = FALSE)

all.temp = read.csv("./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/all data daily.csv") %>% 
  mutate(date = as.Date(date))

i = 1
for(i in 1:length(site_stations)){
  
  cur.site.station = site_stations[i]
  cur.all.temp = all.temp %>% filter(site_station == cur.site.station)
  
  # format for heatwaveR
  cur.all.temp = cur.all.temp %>% rename(t = date, temp = temp_mean) %>% 
    select(t, temp)
  
  climOutput = ts2clm(cur.all.temp, climatologyPeriod = c(min(cur.all.temp$t), max(cur.all.temp$t)))
  hwOutput = detect_event(climOutput)$event
  temp.data = climOutput
  
  store = detect_event(climOutput)
  
  hwOutput$site_station = cur.site.station
  temp.data$site_station = cur.site.station
  
  if(i == 1){
    hw.all = hwOutput
    temp.with.bounds = temp.data
  }
  if(i > 1){
    hw.all = rbind(hw.all, hwOutput)
    temp.with.bounds = rbind(temp.data, temp.with.bounds)
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



# ## save the heatwaves dataframe ##
# write.csv(hw.all, "./Heatwaves/Heatwave results/NERR heatwave events 2024-10-21.csv", row.names = FALSE)
# write.csv(temp.with.bounds, "./Heatwaves/Heatwave results/NERR heatwave temp thresholds 2024-11-08.csv", row.names = FALSE)




#### identify extremes in ph #####

# create a new column which has site_station
all.temp = all.temp %>% mutate(site_station = paste(site, station, sep = "_"))

site_stations = unique(all.temp$site_station)

i = 1
for(i in 1:length(site_stations)){
  
  cur.site.station = site_stations[i]
  cur.all.temp = all.temp %>% filter(site_station == cur.site.station)
  
  # format for heatwaveR
  cur.all.temp = cur.all.temp %>% rename(t = date, temp = ph_mean) %>% 
    select(t, temp)
  
  
  for(j in 0:100){
    print(i)
    climOutput = ts2clm(cur.all.temp, climatologyPeriod = c(min(cur.all.temp$t), max(cur.all.temp$t)), pctile = j)
    climOutput = climOutput %>% rename_with(~ paste0("thresh.", j), .cols = thresh)
    print(names(climOutput))
    print(j)
    
    if(j == 1){
      climOutput.all = climOutput
    }
    
    if(j > 1){
      climOutput.all = climOutput.all %>% 
        full_join(climOutput, by = c("doy", "t", "temp", "seas"))
    }
    
  }
  
  climOutput.all$site_station = cur.site.station
  
  if(i == 1){
    final.climOutput.ph = climOutput.all
  }
  
  if(i > 1){
    final.climOutput.ph = rbind(final.climOutput.ph, climOutput.all)
  }
  
  # hwOutput = detect_event(climOutput)$event
  # temp.data = climOutput
  # 
  # store = detect_event(climOutput)
  # 
  # hwOutput$site_station = cur.site.station
  # temp.data$site_station = cur.site.station
  # 
  # if(i == 1){
  #   hw.all = hwOutput
  #   temp.with.bounds = temp.data
  # }
  # if(i > 1){
  #   hw.all = rbind(hw.all, hwOutput)
  #   temp.with.bounds = rbind(temp.data, temp.with.bounds)
  # }
  
}



# Select only the threshold columns
thresh_cols <- grep("^thresh\\.", names(final.climOutput.ph), value = TRUE)

# Find the percentile range for each row
final.climOutput.ph = final.climOutput.ph %>%
  rowwise() %>%
  mutate(
    percentile = {
      # Find the first threshold that temp is less than
      higher_thresh <- which(temp < c_across(all_of(thresh_cols)))[1]
      
      # If temp is below all thresholds, assign 0
      # If temp is above all thresholds, assign 100
      if (is.na(higher_thresh)) {
        100
      } else if (higher_thresh == 1) {
        0
      } else {
        higher_thresh - 1  # Adjust index to match percentile
      }
    }
  ) %>%
  ungroup()

#final.climOutput.ph = test

final.climOutput.ph = final.climOutput.ph %>% 
  select(doy, t, temp, seas, percentile, everything()) %>% 
  mutate(percentile = replace(percentile, is.na(temp), NA)) %>% 
  rename(ph = temp) %>% 
  select(site_station, doy, t, ph, seas, percentile) %>% 
  rename(ph.percentile = percentile, ph.seas.avg = seas)

# save the ph dataframe of percentiles
write.csv(final.climOutput.ph, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/ph percentiles.csv", row.names = FALSE)


#### identify extremes in turbidity #####

# create a new column which has site_station
all.temp = all.temp %>% mutate(site_station = paste(site, station, sep = "_"))

site_stations = unique(all.temp$site_station)

i = 1
for(i in 1:length(site_stations)){
  
  cur.site.station = site_stations[i]
  cur.all.temp = all.temp %>% filter(site_station == cur.site.station)
  
  # format for heatwaveR
  cur.all.temp = cur.all.temp %>% rename(t = date, temp = turb_mean) %>% 
    select(t, temp)
  
  
  for(j in 0:100){
    print(i)
    climOutput = ts2clm(cur.all.temp, climatologyPeriod = c(min(cur.all.temp$t), max(cur.all.temp$t)), pctile = j)
    climOutput = climOutput %>% rename_with(~ paste0("thresh.", j), .cols = thresh)
    print(names(climOutput))
    print(j)
    
    if(j == 1){
      climOutput.all = climOutput
    }
    
    if(j > 1){
      climOutput.all = climOutput.all %>% 
        full_join(climOutput, by = c("doy", "t", "temp", "seas"))
    }
    
  }
  
  climOutput.all$site_station = cur.site.station
  
  if(i == 1){
    final.climOutput.turb = climOutput.all
  }
  
  if(i > 1){
    final.climOutput.turb = rbind(final.climOutput.turb, climOutput.all)
  }
  
  # hwOutput = detect_event(climOutput)$event
  # temp.data = climOutput
  # 
  # store = detect_event(climOutput)
  # 
  # hwOutput$site_station = cur.site.station
  # temp.data$site_station = cur.site.station
  # 
  # if(i == 1){
  #   hw.all = hwOutput
  #   temp.with.bounds = temp.data
  # }
  # if(i > 1){
  #   hw.all = rbind(hw.all, hwOutput)
  #   temp.with.bounds = rbind(temp.data, temp.with.bounds)
  # }
  
}



# Select only the threshold columns
thresh_cols <- grep("^thresh\\.", names(final.climOutput.turb), value = TRUE)

# Find the percentile range for each row
final.climOutput.turb = final.climOutput.turb %>%
  rowwise() %>%
  mutate(
    percentile = {
      # Find the first threshold that temp is less than
      higher_thresh <- which(temp < c_across(all_of(thresh_cols)))[1]
      
      # If temp is below all thresholds, assign 0
      # If temp is above all thresholds, assign 100
      if (is.na(higher_thresh)) {
        100
      } else if (higher_thresh == 1) {
        0
      } else {
        higher_thresh - 1  # Adjust index to match percentile
      }
    }
  ) %>%
  ungroup()

# format to simplify
final.climOutput.turb = final.climOutput.turb %>% 
  select(doy, t, temp, seas, percentile, everything()) %>% 
  mutate(percentile = replace(percentile, is.na(temp), NA)) %>% 
  rename(turb = temp) %>% 
  select(site_station, doy, t, turb, seas, percentile) %>% 
  rename(turb.percentile = percentile, turb.seas.avg = seas)

write.csv(final.climOutput.turb, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/turbidity percentiles.csv", row.names = FALSE)  


final.climOutput.turb = read.csv("./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/turbidity percentiles.csv")



#### identify extremes in DO #####
# site 60 does not work

sites = c(34:59, 61:67)

# create a new column which has site_station
all.temp = all.temp %>% mutate(site_station = paste(site, station, sep = "_"))

site_stations = unique(all.temp$site_station) 

## run this code in two chunks, 1:33 and 34:67
for(i in 60:61){
  
  cur.site.station = site_stations[i]
  cur.all.temp = all.temp %>% filter(site_station == cur.site.station)
  
  # format for heatwaveR
  cur.all.temp = cur.all.temp %>% rename(t = date, temp = do_pct_mean) %>% 
    select(t, temp)
  
  
  for(j in 0:100){
    print(i)
    climOutput = ts2clm(cur.all.temp, climatologyPeriod = c(min(cur.all.temp$t), max(cur.all.temp$t)), pctile = j)
    climOutput = climOutput %>% rename_with(~ paste0("thresh.", j), .cols = thresh)
    print(names(climOutput))
    print(j)
    
    if(j == 1){
      climOutput.all = climOutput
    }
    
    if(j > 1){
      climOutput.all = climOutput.all %>% 
        full_join(climOutput, by = c("doy", "t", "temp", "seas"))
    }
    
  }
  
  climOutput.all$site_station = cur.site.station
  
  if(i == 60){
    final.climOutput.do = climOutput.all
  }
  
  if(i > 60){
    final.climOutput.do = rbind(final.climOutput.do, climOutput.all)
  }
  
  # hwOutput = detect_event(climOutput)$event
  # temp.data = climOutput
  # 
  # store = detect_event(climOutput)
  # 
  # hwOutput$site_station = cur.site.station
  # temp.data$site_station = cur.site.station
  # 
  # if(i == 1){
  #   hw.all = hwOutput
  #   temp.with.bounds = temp.data
  # }
  # if(i > 1){
  #   hw.all = rbind(hw.all, hwOutput)
  #   temp.with.bounds = rbind(temp.data, temp.with.bounds)
  # }
  
}



# Select only the threshold columns
thresh_cols <- grep("^thresh\\.", names(final.climOutput.do), value = TRUE)

# Find the percentile range for each row
final.climOutput.do = final.climOutput.do %>%
  rowwise() %>%
  mutate(
    percentile = {
      # Find the first threshold that temp is less than
      higher_thresh <- which(temp < c_across(all_of(thresh_cols)))[1]
      
      # If temp is below all thresholds, assign 0
      # If temp is above all thresholds, assign 100
      if (is.na(higher_thresh)) {
        100
      } else if (higher_thresh == 1) {
        0
      } else {
        higher_thresh - 1  # Adjust index to match percentile
      }
    }
  ) %>%
  ungroup()

# format to simplify
final.climOutput.do = final.climOutput.do %>% 
  select(doy, t, temp, seas, percentile, everything()) %>% 
  mutate(percentile = replace(percentile, is.na(temp), NA)) %>% 
  rename(do = temp) %>% 
  select(site_station, doy, t, do, seas, percentile) %>% 
  rename(do.percentile = percentile, do.seas.avg = seas)

#write.csv(final.climOutput.do, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/dissolved oxygen percentiles 1st 33 sites.csv", row.names = FALSE)  
#write.csv(final.climOutput.do, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/dissolved oxygen percentiles 2st 34-59 sites.csv", row.names = FALSE)  
#write.csv(final.climOutput.do, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/dissolved oxygen percentiles 2st 63-67 sites.csv", row.names = FALSE)  
#write.csv(final.climOutput.do, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/dissolved oxygen percentiles 2st 60-61 sites.csv", row.names = FALSE)  

# combine all final.climOutput.do dataframes into one



#### identify extremes in temp #####

# create a new column which has site_station
all.temp = all.temp %>% mutate(site_station = paste(site, station, sep = "_"))

site_stations = unique(all.temp$site_station)

for(i in 28:60){
  
  cur.site.station = site_stations[i]
  cur.all.temp = all.temp %>% filter(site_station == cur.site.station)
  
  # format for heatwaveR
  cur.all.temp = cur.all.temp %>% rename(t = date, temp = temp_mean) %>% 
    select(t, temp)
  
  
  for(j in 0:100){
    print(i)
    climOutput = ts2clm(cur.all.temp, climatologyPeriod = c(min(cur.all.temp$t), max(cur.all.temp$t)), pctile = j)
    climOutput = climOutput %>% rename_with(~ paste0("thresh.", j), .cols = thresh)
    print(names(climOutput))
    print(j)
    
    if(j == 1){
      climOutput.all = climOutput
    }
    
    if(j > 1){
      climOutput.all = climOutput.all %>% 
        full_join(climOutput, by = c("doy", "t", "temp", "seas"))
    }
    
  }
  
  climOutput.all$site_station = cur.site.station
  
  if(i == 28){
    final.climOutput.temp = climOutput.all
  }
  
  if(i > 28){
    final.climOutput.temp = rbind(final.climOutput.temp, climOutput.all)
  }
  
  # hwOutput = detect_event(climOutput)$event
  # temp.data = climOutput
  # 
  # store = detect_event(climOutput)
  # 
  # hwOutput$site_station = cur.site.station
  # temp.data$site_station = cur.site.station
  # 
  # if(i == 1){
  #   hw.all = hwOutput
  #   temp.with.bounds = temp.data
  # }
  # if(i > 1){
  #   hw.all = rbind(hw.all, hwOutput)
  #   temp.with.bounds = rbind(temp.data, temp.with.bounds)
  # }
  
}



# Select only the threshold columns
thresh_cols <- grep("^thresh\\.", names(final.climOutput.temp), value = TRUE)

# Find the percentile range for each row
final.climOutput.temp = final.climOutput.temp %>%
  rowwise() %>%
  mutate(
    percentile = {
      # Find the first threshold that temp is less than
      higher_thresh <- which(temp < c_across(all_of(thresh_cols)))[1]
      
      # If temp is below all thresholds, assign 0
      # If temp is above all thresholds, assign 100
      if (is.na(higher_thresh)) {
        100
      } else if (higher_thresh == 1) {
        0
      } else {
        higher_thresh - 1  # Adjust index to match percentile
      }
    }
  ) %>%
  ungroup()

#final.climOutput.temp = test

final.climOutput.temp = final.climOutput.temp %>% 
  select(doy, t, temp, seas, percentile, everything()) %>% 
  mutate(percentile = replace(percentile, is.na(temp), NA)) %>% 
  rename(temp = temp) %>% 
  select(site_station, doy, t, temp, seas, percentile) %>% 
  rename(temp.percentile = percentile, temp.seas.avg = seas)

# save the temp dataframe of percentiles
#write.csv(final.climOutput.temp, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/temp percentiles 1 to 27.csv", row.names = FALSE)
write.csv(final.climOutput.temp, "./Catalyst Project Data/Formatted data DKS/seasonal percentiles of values/temp percentiles 28 to 60.csv", row.names = FALSE)





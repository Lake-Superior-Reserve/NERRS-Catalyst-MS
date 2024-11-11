library(heatwaveR)
library(tidyverse)

# plot heatwaves for sites of interest

# we just want to plot heatwaves for PDB, RKB, and Great Bay (GRB)

# read in the heatwaves and temp data
hw.all = read.csv("./Heatwaves/Heatwave results/NERR heatwave events 2024-10-21.csv")
temp.with.bounds = read.csv("./Heatwaves/Heatwave results/NERR heatwave temp thresholds 2024-11-08.csv")

# create columns for site and station
hw.all = hw.all %>% 
  mutate(site = substr(site_station, 1, 3), station = substr(site_station, 5, 7))

temp.with.bounds = temp.with.bounds %>% 
  mutate(site = substr(site_station, 1, 3), station = substr(site_station, 5, 7))


hw.pdb = hw.all %>% filter(site == "pdb")

hw.rkb = hw.all %>% filter(site == "rkb")

#===============================================================================#
### Great Bay heatwaves ###
hw.grb = hw.all %>% filter(site == "grb")
temp.grb = temp.with.bounds %>% filter(site == "grb")
temp.grb = temp.grb %>% mutate(year = year(t))
years = unique(temp.grb$year)


for(i in 1:length(years)){

temp = temp.grb %>% filter(year == years[i])  

print(ggplot(temp, aes(x = as.Date(t), y = temp, color = station))+
  geom_line(linewidth = 1)+
  geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = seas))+
    geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = thresh))+
    
  facet_wrap(~station))+
  theme_classic()+
  labs(title = years[i])
}





for(i in 1:length(years)) {
  
  # Filter for the specific year
  temp = temp.grb %>% filter(year == years[i])
  
  # Identify runs of days where temp > thresh
  temp <- temp %>%
    arrange(t) %>%
    mutate(
      exceed_thresh = ifelse(is.na(temp) | is.na(thresh), FALSE, temp > thresh),
      run_id = with(rle(exceed_thresh), rep(seq_along(lengths), lengths)),
      exceed_count = ave(exceed_thresh, run_id, FUN = function(x) if(all(x, na.rm = TRUE)) length(x) else 0),
      flag = ifelse(exceed_thresh & exceed_count >= 5, TRUE, FALSE)
    )
  
  # Plot with the area filled for the flagged sections
  print(
    ggplot(temp, aes(x = as.Date(t), y = temp, color = station)) +
      geom_line(linewidth = 1) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = seas)) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = thresh)) +
      geom_ribbon(data = temp %>% filter(flag),
                  aes(ymin = -Inf, ymax = temp, fill = station, group = run_id),
                  alpha = 0.3) +
      facet_wrap(~station) +
      theme_classic() +
      labs(title = paste("Year:", years[i]))
  )
}

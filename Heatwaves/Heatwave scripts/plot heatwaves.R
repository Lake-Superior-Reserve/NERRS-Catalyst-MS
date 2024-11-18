library(heatwaveR)
library(tidyverse)

# plot heatwaves for sites of interest

# we just want to plot heatwaves for PDB, RKB, and Great Bay (GRB)
# instead of PDB and RKB, we'll try ELK and GTM

# read in the heatwaves and temp data
hw.all = read.csv("./Heatwaves/Heatwave results/NERR heatwave events 2024-10-21.csv")
temp.with.bounds = read.csv("./Heatwaves/Heatwave results/NERR heatwave temp thresholds 2024-11-08.csv")

# create columns for site and station
hw.all = hw.all %>% 
  mutate(site = substr(site_station, 1, 3), station = substr(site_station, 5, 7))

temp.with.bounds = temp.with.bounds %>% 
  mutate(site = substr(site_station, 1, 3), station = substr(site_station, 5, 7))


hw.elk = hw.all %>% filter(site == "elk")

hw.gtm = hw.all %>% filter(site == "gtm")

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




##### Guana Tolomato Matanzas heatwaves #######
hw.gtm = hw.all %>% filter(site == "gtm")
temp.gtm = temp.with.bounds %>% filter(site == "gtm")
temp.gtm = temp.gtm %>% mutate(year = year(t))
years = unique(temp.gtm$year)
temp.gtm$hw = FALSE
temp.gtm$run_id = 0

for(i in 1:nrow(hw.gtm)){
  
  cur.station = hw.gtm$station[i]
  
  dates = seq(as.Date(hw.gtm$date_start[i]), as.Date(hw.gtm$date_end[i]), 1)
  temp.gtm = temp.gtm %>% mutate(hw = replace(hw, as.Date(t) %in% dates & station == cur.station, TRUE),
                                 run_id = replace(run_id, as.Date(t) %in% dates & station == cur.station, i))
  print(dates)
  
}

pdf("./figures/heatwaves/GTM heatwaves.pdf", height = 6.5, width = 11, onefile = TRUE)
for(i in 1:length(years)) {
  
  # Filter for the specific year
  temp = temp.gtm %>% filter(year == years[i])
  
  # Plot with the area filled for the flagged sections
  print(
    ggplot(temp, aes(x = as.Date(t), y = temp, color = station)) +
      geom_line(linewidth = 1) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = seas)) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = thresh)) +
      geom_ribbon(data = temp %>% filter(hw),
                  aes(ymin = -Inf, ymax = temp, fill = station, group = run_id),
                  alpha = 0.3) +
      facet_wrap(~station) +
      theme_classic() +
      scale_fill_manual(values = c("fm" = "darkred", "pc" = "darkblue", "pi" = "steelblue2", "ss" = "salmon"))+
      scale_color_manual(values = c("fm" = "darkred", "pc" = "darkblue", "pi" = "steelblue2", "ss" = "salmon"))+
      labs(title = paste("GTM", years[i]), x = "date", y = "temp (°C)")+
      theme(legend.position = "none")
  )
}

dev.off()







##### Elkhorn Slough heatwaves #######
hw.elk = hw.all %>% filter(site == "elk")
temp.elk = temp.with.bounds %>% filter(site == "elk")
temp.elk = temp.elk %>% mutate(year = year(t))
years = unique(temp.elk$year)
temp.elk$hw = FALSE
temp.elk$run_id = 0

for(i in 1:nrow(hw.elk)){
  
  cur.station = hw.elk$station[i]
  
  dates = seq(as.Date(hw.elk$date_start[i]), as.Date(hw.elk$date_end[i]), 1)
  temp.elk = temp.elk %>% mutate(hw = replace(hw, as.Date(t) %in% dates & station == cur.station, TRUE),
                                 run_id = replace(run_id, as.Date(t) %in% dates & station == cur.station, i))
  print(dates)
  
}

pdf("./figures/heatwaves/ELK heatwaves.pdf", height = 6.5, width = 11, onefile = TRUE)
for(i in 1:length(years)) {
  
  # Filter for the specific year
  temp = temp.elk %>% filter(year == years[i])
  
  # Plot with the area filled for the flagged sections
  print(
    ggplot(temp, aes(x = as.Date(t), y = temp, color = station)) +
      geom_line(linewidth = 1) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = seas)) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = thresh)) +
      geom_ribbon(data = temp %>% filter(hw),
                  aes(ymin = -Inf, ymax = temp, fill = station, group = run_id),
                  alpha = 0.3) +
      facet_wrap(~station) +
      theme_classic() +
      scale_fill_manual(values = c("ap" = "darkgreen", "nm" = "darkblue", "sm" = "steelblue2", "vm" = "lightgreen"))+
      scale_color_manual(values = c("ap" = "darkgreen", "nm" = "darkblue", "sm" = "steelblue2", "vm" = "lightgreen"))+
      labs(title = paste("ELK", years[i]), x = "date", y = "temp (°C)")+
      theme(legend.position = "none")
  )
}

dev.off()










##### Great Bay heatwaves #######
hw.grb = hw.all %>% filter(site == "grb")
temp.grb = temp.with.bounds %>% filter(site == "grb")
temp.grb = temp.grb %>% mutate(year = year(t))
years = unique(temp.grb$year)
temp.grb$hw = FALSE
temp.grb$run_id = 0

for(i in 1:nrow(hw.grb)){
  
  cur.station = hw.grb$station[i]
  
  dates = seq(as.Date(hw.grb$date_start[i]), as.Date(hw.grb$date_end[i]), 1)
  temp.grb = temp.grb %>% mutate(hw = replace(hw, as.Date(t) %in% dates & station == cur.station, TRUE),
                                 run_id = replace(run_id, as.Date(t) %in% dates & station == cur.station, i))
  print(dates)
  
}

pdf("./figures/heatwaves/GRB heatwaves.pdf", height = 6.5, width = 11, onefile = TRUE)
for(i in 1:length(years)) {
  
  # Filter for the specific year
  temp = temp.grb %>% filter(year == years[i])
  
  # Plot with the area filled for the flagged sections
  print(
    ggplot(temp, aes(x = as.Date(t), y = temp, color = station)) +
      geom_line(linewidth = 1) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = seas)) +
      geom_line(inherit.aes = FALSE, data = temp, aes(x = as.Date(t), y = thresh)) +
      geom_ribbon(data = temp %>% filter(hw),
                  aes(ymin = -Inf, ymax = temp, fill = station, group = run_id),
                  alpha = 0.3) +
      facet_wrap(~station) +
      theme_classic() +
      scale_fill_manual(values = c("gb" = "darkorange", "lr" = "darkblue", "or" = "steelblue2", "sq" = "brown"))+
      scale_color_manual(values = c("gb" = "darkorange", "lr" = "darkblue", "or" = "steelblue2", "sq" = "brown"))+
      labs(title = paste("GRB", years[i]), x = "date", y = "temp (°C)")+
      theme(legend.position = "none")
  )
}

dev.off()


# identify which years have enough years (>=10) to be used in 
# heatwave analysis

# DK Szydlowski 2024-10-20
library(tidyverse)

# list all of the daily data files

daily.data.files = list.files("./Catalyst Project Data/QAQCd_daily/", full.names = TRUE)

# filter to just water quality data, which has water temperature
daily.data.files = daily.data.files[grepl("wq", daily.data.files)]

# loop through and identify which files have at least 10 years of data
for(i in 1:length(daily.data.files)){
  
  # read in each csv
  temp = read.csv(daily.data.files[i])
  
  # format date and create a year column
  temp = temp %>% 
    mutate(date = ymd(date)) %>% 
    mutate(year = year(date)) 

  print(max(temp$year, na.rm = TRUE) - min(temp$year, na.rm = TRUE))

  
  # if there are enough years, keep the data
  if(max(temp$year, na.rm = TRUE) - min(temp$year, na.rm = TRUE) >= 10){
    
    temp = temp %>% select(station, date, year, temp_nValid, temp_min, temp_median, temp_max,
                           temp_mean, temp_sd, temp_iqr, do_pct_mean, ph_mean, turb_mean)
    
    if( i == 1){
      all.temp = temp
    }
    if(i > 1){
      all.temp = rbind(all.temp, temp)
    }
    
  }
  
}
  


# create a site and station column
all.temp = all.temp %>% mutate(site = substr(station, 1, 3)) %>% 
  mutate(station = substr(station, 4, 5))

# there are 17 NERR's with sufficient data (gaps notwithstanding) to identify heatwaves





##### PRECIPITATION DATA #######


daily.data.files = list.files("./Catalyst Project Data/QAQCd_daily/", full.names = TRUE)

# filter to just water quality data, which has water temperature
daily.data.files = daily.data.files[grepl("met", daily.data.files)]

# loop through and identify which files have at least 10 years of data
for(i in 1:length(daily.data.files)){
  
  # read in each csv
  temp = read.csv(daily.data.files[i])
  
  # format date and create a year column
  temp = temp %>% 
    mutate(date = ymd(date)) %>% 
    mutate(year = year(date)) 
  
  print(max(temp$year, na.rm = TRUE) - min(temp$year, na.rm = TRUE))
  
  
  # if there are enough years, keep the data
  if(max(temp$year, na.rm = TRUE) - min(temp$year, na.rm = TRUE) >= 10){
    

    
    if( i == 1){
      all.temp = temp
    }
    if(i > 1){
      all.temp = rbind(all.temp, temp)
    }
    
  }
  
}



# create a site and station column
all.temp = all.temp %>% mutate(site = substr(station, 1, 3)) %>% 
  mutate(station = substr(station, 4, 5))

# there are 17 NERR's with sufficient data (gaps notwithstanding) to identify heatwaves
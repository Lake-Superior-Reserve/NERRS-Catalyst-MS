setwd("~/Documents/School/NERRSData")
library(tidyverse)
library(BreakPoints)

reserveFiles <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)
data_list <- list()

for (reserve in reserveFiles) {
  
  # Get all the CSV files in the current subfolder
  csv_files <- list.files(path = reserve, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop through each CSV file in the subfolder
  for (file in csv_files) {
    
    # Read the CSV file
    data <- read.csv(file)
    
    # Store the data in the list using a unique name
    data_name <- paste0(basename(reserve), "_", basename(file))
    data_list[[data_name]] <- data
  }
}

#create year.frac column

addYearFrac <- function(data) {
  data$yearfrac <- data$year + data$month/12
  return(data)
}

data_list <- lapply(data_list, addYearFrac)

#subset the lists so that they only contain one type of file for each site

metfiles <- data_list[grep("met", names(data_list))]
wqfiles <- data_list[grep("wq", names(data_list))]
nutfiles <- data_list[grep("nut", names(data_list))]


#create lists of all the varialbes in each file type

subsetmet <- function(data) {
  select(data, contains("station"), contains("year"), contains("month"), contains("mean"))
}
metfiles <- lapply(metfiles, subsetmet)

subsetwq <- function(data) {
  select(data, contains("station"), contains("year"), contains("month"), contains("mean"))
}
wqfiles <- lapply(wqfiles, subsetwq)

subsetnut <- function(data) {
  select(data,c("station", "year", "month","po4f", "nh4f", "no2f","no3f", "no23f","chla_n", "yearfrac"))
}
nutfiles <- lapply(nutfiles, subsetnut)

#Run breakpresults_df <- data.frame(site = character(),
wq_breakpoint_df <- data.frame(site = character(),
                         station = character(),
                         variable = character(),
                         test_type = character(),
                         breaks = numeric(),
                         pvalue = numeric())

  
# Loop through each data frame in wqfiles
for (name in names(wqfiles)) {
  
  # Extract the site name (first 3 letters of the file name)
  site <- substr(name, 1, 3)
  print(site)
  # Extract the current data frame
  df <- wqfiles[[name]]
  
  # Loop through each unique station in the current data frame
  unique_stations <- unique(df$station)
  
  for (stations in unique_stations) {
    print(stations)
    # Subset the data frame by the current station
    station_data <- subset(df, station == stations)
    
    # List of variables to run the tests on 
    variables <- c("temp_mean", "spcond_mean","sal_mean","do_pct_mean","do_mgl_mean","ph_mean","turb_mean")
    
    for (variable in variables) {
      if (variable %in% colnames(station_data) & nrow(station_data) > 10) {
        # Buishand's Test
        buishand_result <- Buishand_R(station_data[[variable]], n_period = 5, dstr = 'self', simulations = 1000)
        # Extract the relevant outputs, handling potential NULLs
        buishand_breaks <- ifelse(length(buishand_result$breaks) > 0, buishand_result$breaks, NA)
        buishand_pvalue <- ifelse(!is.null(buishand_result$p.value), buishand_result$p.value, NA)
        
        # Store Buishand result in the results data frame
        wq_breakpoint_df <- rbind(wq_breakpoint_df, data.frame(site = site,
                                                               station = stations,
                                                               variable = variable,
                                                               test_type = "Buishand",
                                                               breaks = buishand_breaks,
                                                               pvalue = buishand_pvalue))
        
        # Pettit's Test
        pettit_result <- pettit(station_data[[variable]], n_period = 5)
        # Extract the relevant outputs, handling potential NULLs
        pettit_breaks <- ifelse(length(pettit_result$breaks) > 0, pettit_result$breaks, NA)
        pettit_pvalue <- ifelse(!is.null(pettit_result$p.value), pettit_result$p.value, NA)
        
        # Store Pettit result in the results data frame
        wq_breakpoint_df <- rbind(wq_breakpoint_df, data.frame(site = site,
                                                               station = stations,
                                                               variable = variable,
                                                               test_type = "Pettit",
                                                               breaks = pettit_breaks,
                                                               pvalue = pettit_pvalue,
                                                               stringsAsFactors = FALSE))
        
      }
    }
  }
}


# View the results
print(wq_breakpoint_df)


wq_breakpoint_df <- wq_breakpoint_df %>%
  group_by(site) %>%
  mutate(station_number = as.factor(as.numeric(factor(station)))) %>%
  ungroup()


library(RColorBrewer)




###############NUT BREAKPOITNS#################
#Run breakpresults_df <- data.frame(site = character(),
nut_breakpoint_df <- data.frame(site = character(),
                               station = character(),
                               variable = character(),
                               test_type = character(),
                               breaks = numeric(),
                               pvalue = numeric())


# Loop through each data frame in wqfiles
for (name in names(nutfiles)) {
  
  # Extract the site name (first 3 letters of the file name)
  site <- substr(name, 1, 3)
  print(site)
  # Extract the current data frame
  df <- nutfiles[[name]]
  
  # Loop through each unique station in the current data frame
  unique_stations <- unique(df$station)
  
  for (stations in unique_stations) {
    print(stations)
    # Subset the data frame by the current station
    station_data <- subset(df, station == stations)
    
    # List of variables to run the tests on 
    variables <- c("po4f", "nh4f","no23f","chla_n")
    
    for (variable in variables) {
      if (variable %in% colnames(station_data) & nrow(station_data) > 10) {
        # Buishand's Test
        buishand_result <- Buishand_R(station_data[[variable]], n_period = 5, dstr = 'self', simulations = 1000)
        # Extract the relevant outputs, handling potential NULLs
        buishand_breaks <- ifelse(length(buishand_result$breaks) > 0, buishand_result$breaks, NA)
        buishand_pvalue <- ifelse(!is.null(buishand_result$p.value), buishand_result$p.value, NA)
        
        # Store Buishand result in the results data frame
        nut_breakpoint_df <- rbind(nut_breakpoint_df, data.frame(site = site,
                                                               station = stations,
                                                               variable = variable,
                                                               test_type = "Buishand",
                                                               breaks = buishand_breaks,
                                                               pvalue = buishand_pvalue))
        
        # Pettit's Test
        pettit_result <- pettit(station_data[[variable]], n_period = 5)
        # Extract the relevant outputs, handling potential NULLs
        pettit_breaks <- ifelse(length(pettit_result$breaks) > 0, pettit_result$breaks, NA)
        pettit_pvalue <- ifelse(!is.null(pettit_result$p.value), pettit_result$p.value, NA)
        
        # Store Pettit result in the results data frame
        nut_breakpoint_df <- rbind(nut_breakpoint_df, data.frame(site = site,
                                                               station = stations,
                                                               variable = variable,
                                                               test_type = "Pettit",
                                                               breaks = pettit_breaks,
                                                               pvalue = pettit_pvalue,
                                                               stringsAsFactors = FALSE))
        
      }
    }
  }
}


# View the results
print(nut_breakpoint_df)


nut_breakpoint_df <- nut_breakpoint_df %>%
  group_by(site) %>%
  mutate(station_number = as.factor(as.numeric(factor(station)))) %>%
  ungroup()


library(RColorBrewer)






###############MET BREAKPOITNS#################
#Run breakpresults_df <- data.frame(site = character(),
met_breakpoint_df <- data.frame(site = character(),
                                station = character(),
                                variable = character(),
                                test_type = character(),
                                breaks = numeric(),
                                pvalue = numeric())


# Loop through each data frame in wqfiles
for (name in names(metfiles)) {
  
  # Extract the site name (first 3 letters of the file name)
  site <- substr(name, 1, 3)
  print(site)
  # Extract the current data frame
  df <- metfiles[[name]]
  
  # Loop through each unique station in the current data frame
  unique_stations <- unique(df$station)
  
  for (stations in unique_stations) {
    print(stations)
    # Subset the data frame by the current station
    station_data <- subset(df, station == stations)
    
    # List of variables to run the tests on 
    variables <- c("atemp_mean", "rh_mean","bp_mean","wspd_mean", "wdir_mean", "dailyPAR_mean")
    
    for (variable in variables) {
      if (variable %in% colnames(station_data) & nrow(station_data) > 10) {
        # Buishand's Test
        buishand_result <- Buishand_R(station_data[[variable]], n_period = 5, dstr = 'self', simulations = 1000)
        # Extract the relevant outputs, handling potential NULLs
        buishand_breaks <- ifelse(length(buishand_result$breaks) > 0, buishand_result$breaks, NA)
        buishand_pvalue <- ifelse(!is.null(buishand_result$p.value), buishand_result$p.value, NA)
        
        # Store Buishand result in the results data frame
        met_breakpoint_df <- rbind(met_breakpoint_df, data.frame(site = site,
                                                                 station = stations,
                                                                 variable = variable,
                                                                 test_type = "Buishand",
                                                                 breaks = buishand_breaks,
                                                                 pvalue = buishand_pvalue))
        
        # Pettit's Test
        pettit_result <- pettit(station_data[[variable]], n_period = 5)
        # Extract the relevant outputs, handling potential NULLs
        pettit_breaks <- ifelse(length(pettit_result$breaks) > 0, pettit_result$breaks, NA)
        pettit_pvalue <- ifelse(!is.null(pettit_result$p.value), pettit_result$p.value, NA)
        
        # Store Pettit result in the results data frame
        met_breakpoint_df <- rbind(met_breakpoint_df, data.frame(site = site,
                                                                 station = stations,
                                                                 variable = variable,
                                                                 test_type = "Pettit",
                                                                 breaks = pettit_breaks,
                                                                 pvalue = pettit_pvalue,
                                                                 stringsAsFactors = FALSE))
        
      }
    }
  }
}


# View the results
print(met_breakpoint_df)

met_breakpoint_df <- met_breakpoint_df %>%
  group_by(site) %>%
  mutate(station_number = as.factor(as.numeric(factor(station)))) %>%
  ungroup()


######PLOTTING##########


c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)


ggplot(met_breakpoint_df[met_breakpoint_df$dpvalue <= 0.05 ,], aes(x = variable, y = breaks, shape = test_type, color = station_number)) +
  geom_point() +
  facet_wrap(~site) +
  scale_color_manual(values = c(c25)) +  
  labs(title = "meteorological breaks", shape = "Test Type")


ggplot(nut_breakpoint_df[nut_breakpoint_df$pvalue <= 0.05 ,], aes(x = variable, y = breaks, color = as.factor(station_number), shape = test_type)) +
  geom_point() +
  facet_wrap(~site) +
  scale_color_manual(values = c(c25)) + 
  labs(title = "nutrient breaks", color = "Station Number", shape = "Test Type")

ggplot(wq_breakpoint_df[wq_breakpoint_df$pvalue <= 0.05 ,], aes(x = variable, y = breaks, color = as.factor(station_number), shape = test_type)) +
  geom_point() +
  facet_wrap(~site) +
  scale_color_manual(values = c(c25)) +   
  labs(title = "water quality breaks", color = "Station Number", shape = "Test Type")


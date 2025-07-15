library(tidyverse)
library(car)
library(lme4)
library(lmer)
library(forecast)
library(purrr)
library(broom)
library(data.table)  
setwd("~/Documents/School/NERRSData")
  
# # Need to make data-list with the daily water quality data
# reserveFiles <- list.files(
#     path = "./QAQCd_daily",
#     pattern = "wq_daily\\.csv$",
#     full.names = TRUE,
#     recursive = FALSE
#   )
#   data_list <- list()
#   
#   # Loop through each CSV file
#   for (file in reserveFiles) {
#     # Read the CSV file
#     data <- tryCatch({
#       read.csv(file)
#     })
#     
#     # Extract the file
#     file_name <- basename(file)
#     
#     # Create the new name to match danny's heatwave sheet
#     first_three <- substr(file_name, 1, 3)
#     next_two <- substr(file_name, 4, 5)
#     data_name <- paste0(first_three, "_", next_two)
#     
#     
#     # Store the data in the list using the new name
#     data_list[[data_name]] <- data
#   }
#   
#   subsetMean <- function(data) {
#     select(data,
#            contains("station"),
#            contains("year"),
#            contains("date"),
#            contains("mean"))
#   }
#   data_list <- lapply(data_list, subsetMean)
#   #subset list of sites for analysis##
#   data_list <- data_list[grep("^(grb)", names(data_list))]
  
  
  #import percentile data
  
  pers <- read_csv("ALL PERCENTILES COMBINED 2025-04-23.csv")
  
  #lets seasonally detrend the TS
  var.list <- names(pers[c(4, 7, 10, 13)])
  
  for (i in unique(pers$site_station)) {
    subset_data <- pers[pers$site_station == i, ]
    
    for (var in var.list) {
      d_filled <- ifelse(is.na(subset_data[[var]]),
                         mean(subset_data[[var]], na.rm = TRUE),
                         subset_data[[var]])
      ts_data <- ts(d_filled, frequency = 365)
      decomposed <- try(decompose(ts_data, type = "additive"), silent = TRUE)
      if (!inherits(decomposed, "try-error")) {
        deseasonalized <- ts_data - decomposed$seasonal
        # Save deseasonalized values back into the main pers dataframe
        # need to update the matching rows in `pers`
        pers[pers$site_station == i, paste0(var, ".detrend")] <- as.numeric(deseasonalized)
        
        cat("Processed:", i, "-", var, "\n")
      } else {
        cat("Skipping:", i, "-", var, "- Decomposition failed\n")
        
        # 
        # acf(deseasonalized,
        #     main = paste("ACF of seasonally detrended", var, "in", name))
        # readline(prompt = "Press [Enter] to continue...")
        # pacf(deseasonalized,
        #      main = paste("PACF of seasonally detrended", var, "in", name))
        # readline(prompt = "Press [Enter] to continue...")
      }
    }
  }
  
  pers$site_station_factor <- factor(pers$site_station)

  # make a categorical variable for heatwave, recovery, and not in a heatwave
  
  names(pers)[names(pers) == 't'] <- 'date'
  

  pers <- pers %>%
    arrange(site_station, date) %>%
    group_by(site_station) %>%
    mutate(
  
      hot_day = temp.percentile > 90,
      run_id = rleid(hot_day)
    ) %>%
    group_by(site_station, run_id) %>%
    mutate(
      run_length = n(),
      is_heatwave = hot_day & run_length >= 5
    ) %>%
    ungroup()
  
  # Create heatwave column
  pers <- pers %>%
    mutate(heatwave = ifelse(is_heatwave, "during", "control"))
  

  pers <- pers %>%
    arrange(site_station, date) %>%
    group_by(site_station) %>%
    mutate(
      is_during = heatwave == "during",
      hw_event = ifelse(is_during, rleid(is_during), NA_integer_)
    ) %>%
    ungroup()
  
  # Get post-heatwave periods (7 days after each heatwave for now)
  post_dates_df <- pers %>%
    filter(heatwave == "during") %>%
    group_by(site_station, hw_event) %>%
    summarise(max_date = max(date), .groups = "drop") %>%
    rowwise() %>%
    mutate(post_dates = list(seq(max_date + 1, max_date + 7, by = "day"))) %>%
    unnest(post_dates) %>%
    rename(date = post_dates) %>%
    mutate(tag = "post")
  
  # join 
  pers <- pers %>%
    left_join(post_dates_df, by = c("site_station", "date")) %>%
    mutate(heatwave = case_when(
      tag == "post" ~ "post",
      TRUE ~ heatwave
    )) %>%
    select(-hot_day, -run_id, -run_length, -is_heatwave, -is_during, -tag, -hw_event.x, -hw_event.y, -max_date)
  
  
  # Identify heatwave events
  pers <- pers %>%
    arrange(site_station, date) %>%
    group_by(site_station) %>%
    mutate(
      is_during = heatwave == "during",
      hw_event = if_else(is_during, rleid(is_during), NA_integer_)
    ) %>%
    ungroup()
  
  # find start dates of each heatwave event
  pre_dates_df <- pers %>%
    filter(is_during) %>%
    group_by(site_station, hw_event) %>%
    summarise(start_date = min(date), .groups = "drop") %>%
    mutate(pre_window_start = start_date - 7)
  
  # pre dates
  pre_days <- pre_dates_df %>%
    rowwise() %>%
    mutate(pre_dates = list(seq(pre_window_start, start_date - 1, by = "day"))) %>%
    unnest(pre_dates) %>%
    rename(date = pre_dates) %>%
    mutate(tag = "pre")
  
  # Join 
  pers <- pers %>%
    left_join(pre_days, by = c("site_station", "date")) %>%
    mutate(
      heatwave = case_when(
        tag == "pre" ~ "pre",
        TRUE ~ heatwave
      )
    ) %>%
    select(-tag, -start_date, -pre_window_start)
  


pers <-  na.omit(pers)


ph.mod <- summary(lmer(ph.detrend ~ heatwave + (1|site_station), data = pers))
turb.mod <- summary(lmer(turb.detrend ~ heatwave + (1|site_station), data = pers))
do.mod <- summary(lmer(do.detrend ~ heatwave + (1|site_station), data = pers))
temp.mod <- summary(lmer(temp.detrend ~ heatwave + (1|site_station), data = pers))

summary(ph.mod)
summary(turb.mod)
summary(do.mod)
summary(temp.mod)
#Do again with an AR3 to better account for autocorrelations
library(nlme)

# pers <- pers %>%
#   arrange(site_station, date) %>%
#   group_by(site_station) %>%
#   mutate(time_index = row_number()) %>%
#   ungroup()
# 
# turb.mod.ar3 <- lme(
#   turb.detrend ~ heatwave,
#   random = ~ 1 | site_station,
#   correlation = corARMA(p = 3, form = ~ time_index | site_station),
#   data = pers,
#   method = "REML"
# )

#Cant use lme, so goign to do each site/station seperatly with Arima form the forecast package


# Split data by site_station
pers_list <- pers %>%
  group_by(site_station) %>%
  group_split()

#make a function
fit_site_arima_response <- function(df, response_var) {

  df <- df %>% arrange(date)
  
  y <- df[[response_var]]
  
  xreg <- model.matrix(~ heatwave, df)[, -1]
  

  tryCatch({
    model <- Arima(y,
                   order = c(3,0,0),
                   xreg = xreg)
    
    # Extract terms
    coef_df <- broom::tidy(model) %>%
      filter(grepl("heatwave", term)) %>%
      mutate(site_station = df$site_station[1],
             response = response_var)
    
    return(coef_df)
    
  }, error = function(e) {
    return(tibble(term = c("heatwaveduring", "heatwavepost"),
                  estimate = NA_real_,
                  std.error = NA_real_,
                  statistic = NA_real_,
                  p.value = NA_real_,
                  site_station = df$site_station[1],
                  response = response_var))
  })
}

#loop to run model
response_vars <- c("turb.detrend", "ph.detrend", "do.detrend", "temp.detrend")

mod_results_list <- map(response_vars, function(resp) {
  map(pers_list, fit_site_arima_response, response_var = resp)
}) %>%
  flatten() %>%
  bind_rows()

mod.results <- mod_results_list

ggplot(mod.results, aes(x = term, y = estimate, fill = as.factor(response))) +
  geom_boxplot() + 
  facet_wrap(~response, scales = "free_y")


#ok cool, lets see about adding in precip data 


pers <- pers %>%
  mutate(site = substr(site_station, 1, 3))

site <- unique(pers$site)


met_dir <- "QAQCd_daily/"  
met_files <- list.files(path = met_dir, pattern = "met_daily\\.csv$", full.names = TRUE)

all_met <- lapply(met_files, function(file) {
  site_name <- str_extract(basename(file), "^[A-Za-z]{3}")
  met <- read_csv(file, show_col_types = FALSE)
  met <- met %>%
    mutate(site = site_name,
           date = as.Date(date)) %>%  
    select(site, date, totprcp_total)
  return(met)
}) %>%
  bind_rows()

pers <- pers %>%
  mutate(date = as.Date(date)) %>%  
  left_join(all_met, by = c("site", "date")) 

# Update the heatwave column so that there are precip heatwaves or not, I'm not sure if precip should just be counted during the heatwave phase or both the heatwave and recovery phase. This is heatwave only during the heatwave.

pers <- pers %>%
  arrange(site_station, date) %>%
  group_by(site_station) %>%
  mutate(
    is_hw = heatwave %in% c("during", "post"),
    heatwave_event = if_else(is_hw, rleid(is_hw), NA_integer_) 
  ) %>%
  ungroup()



precip_flag <- pers %>%
  group_by(site_station, heatwave_event) %>%
  summarise(precip_during_event = any(totprcp_total > 0 & heatwave == "during", na.rm = TRUE), .groups = "drop")


pers <- pers %>%
  left_join(precip_flag, by = c("site_station", "heatwave_event")) %>%
  mutate(
    precip_heatwave = case_when(
      precip_during_event & heatwave == "during" ~ "precip_during",
      precip_during_event & heatwave == "post" ~ "precip_heatwave_recovory",
      TRUE ~ heatwave
    )
  ) %>%
  select(-is_hw, -precip_during_event)


# model and plot


pers_list <- pers %>%
  group_by(site_station) %>%
  group_split()


fit_site_arima_response <- function(df, response_var) {
  
  df <- df %>% arrange(date)
  
  y <- df[[response_var]]

  df$precip_heatwave <- factor(df$precip_heatwave,
                               levels = c("control", "during", "post", "precip_during", "precip_heatwave_recovory"))

  xreg <- model.matrix(~ precip_heatwave, data = df)[, -1]
  
  tryCatch({
    model <- forecast::Arima(y,
                             order = c(3, 0, 0),
                             xreg = xreg)
    

    coef_df <- broom::tidy(model) %>%
      filter(grepl("precip_heatwave", term)) %>%
      mutate(site_station = df$site_station[1],
             response = response_var)
    
    return(coef_df)
    
  }, error = function(e) {
    return(tibble(
      term = paste0("precip_heatwave", c("during", "post", "precip_during", "precip_heatwave_recovory")),
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      site_station = df$site_station[1],
      response = response_var
    ))
  })
}

#loop to run model
response_vars <- c("turb.detrend", "ph.detrend", "do.detrend", "temp.detrend")

mod_results_list <- map(response_vars, function(resp) {
  map(pers_list, fit_site_arima_response, response_var = resp)
}) %>%
  flatten() %>%
  bind_rows()

mod.results <- mod_results_list

ggplot(mod.results, aes(x = term, y = estimate, fill = as.factor(response))) +
  geom_boxplot() +
  facet_wrap(~response, scales = "free_y") +
  scale_x_discrete(labels = c(
    "precip_heatwaveduring" = "Heatwave",
    "precip_heatwavepost" = "Recovery",
    "precip_heatwaveprecip_during" = "Precip Heatwave",
    "precip_heatwaveprecip_heatwave_recovory" = "Precip Heatwave Recovery"
  )) +
  labs(x = "Phase", )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



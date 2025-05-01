library(tidyverse)
library(rlang)
library(furrr)
library(heatwaveR)


# read in met and wq data
daily_files <- list.files("Catalyst Project Data/QAQCd_daily", full.names = TRUE)

met <- map(daily_files[str_detect(daily_files, "met")], read_csv, show_col_types = FALSE) %>% 
  list_rbind()

wq <- map(daily_files[str_detect(daily_files, "wq")], read_csv, show_col_types = FALSE) %>% 
  list_rbind()

# filter to only sites with more than 10 years (of both?)
drop_10yr <- function(df) {
  df %>% 
    group_by(station) %>% 
    mutate(years = max(year(date)) - min(year(date))) %>% 
    filter(years >= 10) %>% 
    select(-years) %>% 
    ungroup()
}

# split station id into reserve and stations
# split_station <- function(df) {
#   df %>% 
#     mutate(
#       reserve = str_sub(station, 1, 3),
#       station = str_sub(station, 4, 5)
#     )
# }


met_10yr <- met %>% 
  drop_10yr() %>% 
  mutate(station = str_replace(station, "met", "")) %>% 
  select(station, date, airtemp = atemp_mean, totprcp = totprcp_total)

wq_10yr <- wq %>% 
  drop_10yr() %>% 
  mutate(station = str_replace(station, "wq", "")) %>% 
  select(station, date, watertemp = temp_mean, cond = spcond_mean, do = do_pct_mean, ph = ph_mean, turb = turb_mean)

rm(daily_files, met, wq)

clm_prep <- function(df, site, col){
  df %>%
    filter(station == site) %>%
    rename(t = date, temp = !!col) %>%
    select(t, temp)
}

clm_thresh <- function(j, cur.df, clim_period) {
  # ts2clm returns a tibble with columns including: t, temp, thresh, doy, seas, etc.
  climOutput <- ts2clm(cur.df, climatologyPeriod = clim_period, pctile = j)
  
  # Rename thresh to thresh.j so that when joining, each column is unique
  climOutput %>%
    rename(!!str_c("thresh_", j) := thresh) %>% 
    select(!!str_c("thresh_", j))
}

clm_thresh_wrap <- function(site, df, col, res = 1) {
  cur.df <- clm_prep(df, site, col)
  
  # Get overall dates to use as the climatology period
  clim_period <- c(min(cur.df$t), max(cur.df$t))
  climOutput <- ts2clm(cur.df, climatologyPeriod = clim_period) %>% 
    select(t, doy, temp, seas)
  
  # Loop over percentiles j and get a vector for each
  # bind together and format
  test <- future_map(seq(0, 100, by = res), clm_thresh, cur.df, clim_period, .options = furrr_options(seed = TRUE)) %>% 
    list_cbind() %>% 
    bind_cols(climOutput) %>%
    mutate(thresh_100 = if_else(temp >= thresh_100, temp + 1, thresh_100)) %>% 
    pivot_longer(cols = matches("[0-9]"), names_to = "percentile") %>% 
    filter(temp <= value | is.na(temp)) %>% 
    group_by(t, doy, temp, seas) %>% 
    slice_min(value, with_ties = FALSE) %>% 
    ungroup() %>% 
    mutate(percentile = str_replace(percentile, "thresh_", ""),
           percentile = as.numeric(percentile),
           percentile = if_else(is.na(temp), NA, percentile),
           station = site) %>%
    select(station, date = t, !!col := temp, !!str_c(col, "_seas") := seas, !!str_c(col, "_perc") := percentile)
}


clm_pct_wrap <- function(df, col, res = 1){
  map(unique(df$station), clm_thresh_wrap, df, col, res) %>% 
    list_rbind()
}

# set up parallel processing
plan(multisession)

airtemp_perc <- clm_pct_wrap(met_10yr, "airtemp")
totprcp_perc <- clm_pct_wrap(met_10yr, "totprcp")
met_perc <- full_join(airtemp_perc, totprcp_perc, by = join_by(station, date))
rm(airtemp_perc, totprcp_perc)


watertemp_perc <- clm_pct_wrap(wq_10yr, "watertemp")
cond_perc <- clm_pct_wrap(wq_10yr, "cond")
do_perc <- clm_pct_wrap(wq_10yr, "do")
ph_perc <- clm_pct_wrap(wq_10yr, "ph")
turb_perc <- clm_pct_wrap(wq_10yr, "turb")
wq_perc <- full_join(watertemp_perc, cond_perc, by = join_by(station, date)) %>% 
  full_join(do_perc, by = join_by(station, date)) %>% 
  full_join(ph_perc, by = join_by(station, date)) %>% 
  full_join(turb_perc, by = join_by(station, date))
rm(watertemp_perc, cond_perc, do_perc, ph_perc, turb_perc)


write_csv(met_perc, "Catalyst Project Data/percentiles/met_perc.csv")
write_csv(wq_perc, "Catalyst Project Data/percentiles/wq_perc.csv")

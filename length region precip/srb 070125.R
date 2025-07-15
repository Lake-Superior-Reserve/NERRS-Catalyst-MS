library(tidyverse)
library(dunn.test)
library(nlme)

wq_perc <- read_csv("Catalyst Project Data/percentiles/wq_perc.csv", show_col_types = FALSE)
met_perc <- read_csv("Catalyst Project Data/percentiles/met_perc.csv", show_col_types = FALSE) %>% 
  inner_join(wq_perc, by = join_by(station, date)) # only keep rows with met AND wq data

sites <- read_csv("sampling_stations.csv") %>% 
  mutate(station = str_sub(`Station Code`, 1, 5),
         State = str_to_lower(State)) %>% 
  group_by(station) %>% 
  summarise(name = first(`Station Name`), reserve = first(`NERR Site ID`), reserve_name = first(`Reserve Name`),
            state = first(State), region = first(Region), latitude = mean(Latitude), longitude = mean(as.numeric(Longitude)))

add_heat_flags <- function(df){
  
  # get list of heatwave dates, length
  waves_df <- df %>%
    arrange(station, date) %>%
    mutate(hw = watertemp_perc > 90,
           hw = replace_na(hw, FALSE)) %>%
    group_by(station) %>%
    mutate(wave_id = if_else(hw & !lag(hw),1L, 0L) %>% cumsum(),
           wave_id = if_else(hw, wave_id, NA)) %>%
    filter(!is.na(wave_id)) %>%
    group_by(station, wave_id) %>%
    summarize(
      hw_start  = min(date),
      hw_end    = max(date),
      hw_length = n(),
      .groups   = "drop"
    ) %>%
    filter(hw_length >= 5)
  
  # create df of dates surrounding/including each heatwave
  windows_df_1 <- waves_df %>%
    mutate(
      date = map2(hw_start, hw_end, ~ seq(.x - days(10), .y + days(5), by = "day"))
    ) %>%
    unnest(date) %>%
    # add label to each date
    mutate(
      wave_block = case_when(date <  hw_start ~ "pre",
                             date >= hw_start & date <= hw_end ~ "wave",
                             date >  hw_end ~ "post",
                             .default = NA
      )
    )  %>%    # remove duplicate rows - prioritize wave, then post, then pre
    group_by(station) %>% 
    mutate(dup = duplicated(date) | duplicated(date, fromLast = TRUE),
           drop = dup & wave_block == "pre") %>% 
    filter(!drop) %>% 
    mutate(dup = duplicated(date) | duplicated(date, fromLast = TRUE),
           drop = dup & wave_block == "post") %>% 
    filter(!drop) %>%
    ungroup() %>% 
    select(-c(dup, drop))
  
  # get counts of extreme heat days during pre and post heatwave blocks
  windows_df <- windows_df_1 %>%
    # add in watertemp
    left_join(df %>% select(station, date, watertemp_perc),
              by = c("station","date"))  %>% 
    group_by(station, wave_id, hw_start, hw_end, hw_length) %>%
    summarize(
      pre_n90   = sum(watertemp_perc > 90 & wave_block == "pre",  na.rm=TRUE),
      post_n90  = sum(watertemp_perc > 90 & wave_block == "post", na.rm=TRUE),
      .groups = "drop"
    ) 
  
  # add new columns back onto data
  df %>%
    left_join(windows_df_1, by = c("station","date")) %>%
    left_join(select(windows_df, station, wave_id, pre_n90, post_n90), by = c("station","wave_id")) 
  
}


add_prcp_flags <- function(df){
  df %>% 
    group_by(station, wave_id) %>% 
    mutate(prcp_pre = sum(wave_block == "pre"  & totprcp_perc > 90, na.rm = TRUE),
           prcp_wave  = sum(wave_block == "wave"  & totprcp_perc > 90, na.rm = TRUE),
           prcp_post = sum(wave_block == "post" & totprcp_perc > 90, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(across(c(prcp_pre, prcp_wave, prcp_post), ~if_else(is.na(wave_id), NA, .)))
}

wq_prep <- add_heat_flags(wq_perc) 
met_prep <- met_perc %>% 
  add_heat_flags() %>% 
  add_prcp_flags()


# check_range <- function(df){
#   df %>% 
#     summarise(min_date = min(date, na.rm = T), 
#               max_date = max(date, na.rm = T), .by = station)
# }
# 
# met_range <- check_range(met_perc)
# wq_range <- check_range(wq_perc)



check_diffs <- function(station_name = "acebb", data = wq_prep, param = "watertemp_perc"){
  
  df <- data %>% 
    filter(station == station_name) 
  
  if (all(is.na(df$wave_block))) return(tibble_row(station = station_name, n = 0))
  
  nwaves <- df %>% 
    summarise(.by = wave_id) %>% 
    nrow()
  
  predict_var <- df$wave_block
  response_var <- df[[param]]
  
  kruskal_res <- kruskal.test(response_var ~ predict_var)
  
  if (kruskal_res$p.value > 0.01) return(tibble_row(station = station_name, n = nwaves, sig = FALSE))
  
  sink("null") # silence output
  dunn_res <- invisible(dunn.test(response_var, predict_var, kw = FALSE, table = FALSE))
  sink()
  
  tibble(station = station_name, 
         n = nwaves, 
         comp = dunn_res$comparisons, 
         diff = dunn_res$Z,
         p = dunn_res$P.adjusted) %>% 
    mutate(sig = p < 0.01) %>% 
  return()
  
  
}

plot_results <- function(res, title = NULL){
  res %>% 
    filter(!is.na(comp)) %>% 
    filter(!state %in% c("ak", "pr", "hi")) %>% 
    # filter(sig) %>% 
    mutate(region = factor(region)) %>% 
    ggplot(aes(comp, diff)) +
    geom_boxplot() +
    ylab("Percentile Difference Between Groups") +
    xlab(NULL) +
    ggtitle(title) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.margin = margin(-5,0,0,0))
}

wq_prep_base <- wq_prep %>% 
  mutate(wave_block = factor(wave_block, levels = c("post", "wave", "pre")))

temp_res <- map(unique(wq_prep$station), check_diffs, wq_prep_base, "watertemp_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
do_res <- map(unique(wq_prep$station), check_diffs, wq_prep_base, "do_perc") %>% list_rbind()  %>% left_join(sites, by = join_by(station))
ph_res <- map(unique(wq_prep$station), check_diffs, wq_prep_base, "ph_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
cond_res <- map(unique(wq_prep$station), check_diffs, wq_prep_base, "cond_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
turb_res <- map(unique(wq_prep$station), check_diffs, wq_prep_base, "turb_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))

do_res %>% 
  filter(n > 0) %>% 
  filter(comp != "pre") %>% 
  select(name, reserve_name, region, comp, diff, sig) %>% 
  write_csv("do_res.csv")


plot_results(temp_res, "water temperature")
plot_results(do_res, "DO")
plot_results(ph_res, "pH")
plot_results(cond_res, "conductivity")
plot_results(turb_res, "turbidity")


# length
wq_prep_len <- wq_prep %>% 
  mutate(wave_block = case_when(
    hw_length < 10 ~ str_c(wave_block, "_short"),
    hw_length < 15 ~ str_c(wave_block, "_med"),
    hw_length >= 15 ~ str_c(wave_block, "_long"),
    .default = wave_block
  ),
  wave_block = factor(wave_block, levels = c("wave_short", "post_short",
                                             "wave_med", "post_med",
                                             "wave_long", "post_long"))
  )

temp_res <- map(unique(wq_prep$station), check_diffs, wq_prep_len, "watertemp_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
do_res <- map(unique(wq_prep$station), check_diffs, wq_prep_len, "do_perc") %>% list_rbind()  %>% left_join(sites, by = join_by(station))
ph_res <- map(unique(wq_prep$station), check_diffs, wq_prep_len, "ph_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
cond_res <- map(unique(wq_prep$station), check_diffs, wq_prep_len, "cond_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
turb_res <- map(unique(wq_prep$station), check_diffs, wq_prep_len, "turb_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))

plot_results(temp_res, "water temperature")
plot_results(do_res, "DO")
plot_results(ph_res, "pH")
plot_results(cond_res, "conductivity")
plot_results(turb_res, "turbidity")


# precip
wq_prep_prcp <- met_prep %>% 
  mutate(wave_block = case_when(
    prcp_wave > 0 & wave_block %in% c("wave", "post") ~ str_c(wave_block, "_prcp"),
    prcp_post > 0 & wave_block == "post" ~ str_c(wave_block, "_prcppost"),
    .default = wave_block),
  wave_block = factor(wave_block, levels = c("wave", "post",
                                             "wave_prcp", "post_prcp", 
                                             "post_prcppost")))

temp_res <- map(unique(wq_prep$station), check_diffs, wq_prep_prcp, "watertemp_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
do_res <- map(unique(wq_prep$station), check_diffs, wq_prep_prcp, "do_perc") %>% list_rbind()  %>% left_join(sites, by = join_by(station))
ph_res <- map(unique(wq_prep$station), check_diffs, wq_prep_prcp, "ph_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
cond_res <- map(unique(wq_prep$station), check_diffs, wq_prep_prcp, "cond_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
turb_res <- map(unique(wq_prep$station), check_diffs, wq_prep_prcp, "turb_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))

plot_results(temp_res, "water temperature")
plot_results(do_res, "DO")
plot_results(ph_res, "pH")
plot_results(cond_res, "conductivity")
plot_results(turb_res, "turbidity")


# season
wq_prep_sea <- wq_prep %>% 
  mutate(wave_block = case_when(
    month(date) %in% c(12, 1, 2) ~ str_c(wave_block, "_win"),
    month(date) %in% c(3, 4) ~ str_c(wave_block, "_spr"),
    month(date) %in% c(5, 6, 7, 8, 9) ~ str_c(wave_block, "_sum"),
    month(date) %in% c(10, 11) ~ str_c(wave_block, "_fal"),
    .default = wave_block
  ),
  wave_block = if_else(is.na(wave_block) | str_detect(wave_block, "pre"), "none", wave_block),
  wave_block = factor(wave_block, levels = c("none",
                                             "wave_win", "post_win",
                                             "wave_spr", "post_spr",
                                             "wave_sum", "post_sum",
                                             "wave_fal", "post_fal"))
  )

temp_res <- map(unique(wq_prep$station), check_diffs, wq_prep_sea, "watertemp_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
do_res <- map(unique(wq_prep$station), check_diffs, wq_prep_sea, "do_perc") %>% list_rbind()  %>% left_join(sites, by = join_by(station))
ph_res <- map(unique(wq_prep$station), check_diffs, wq_prep_sea, "ph_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
cond_res <- map(unique(wq_prep$station), check_diffs, wq_prep_sea, "cond_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
turb_res <- map(unique(wq_prep$station), check_diffs, wq_prep_sea, "turb_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))

plot_results(temp_res, "water temperature")
plot_results(do_res, "DO")
plot_results(ph_res, "pH")
plot_results(cond_res, "conductivity")
plot_results(turb_res, "turbidity")




## alternative model fits
# 
# 
# hw_mod <- lme(
#   fixed = turb_perc ~ wave_block, 
#   random = ~ wave_block | station,                        
#   #correlation = corAR1(form = ~ date | station),     
#   data = wq_prep,
#   na.action = na.omit
# )
# summary(hw_mod)
# plot(hw_mod, resid(., type="normalized") ~ fitted(.))
# ACF(hw_mod, maxLag = 20)
# 
# 
# 
# 
# 
# 
# 
# 
# df = wq_prep
# 
# 
# fit_arima_hw <- function(station_name, df, response_var) {
#   
#   df <- df %>%
#     filter(station == station_name) %>% 
#     arrange(date) %>% 
#     mutate(wave_block = if_else(is.na(wave_block), "none", wave_block),
#            wave_block = factor(wave_block, levels = c("none", "wave", "post", "pre")),
#            across(c(hw_length, pre_n90, post_n90), ~if_else(is.na(.), 0, .)))
#   
#   y <- df[[response_var]]
#   
#   tryCatch({
#     # get T/F matrix of heatwave blocks, dropping columns for levels that don't exist at site to prevent model failure
#     xreg <- model.matrix(~ wave_block, df)[, -1] %>%
#       as.data.frame() %>% 
#       select(where(~ !(is.numeric(.) && all(. == 0)))) %>% 
#       # bind_cols(select(df, hw_length, pre_n90, post_n90)) %>% 
#       as.matrix() 
#     
#     model <- arima(y, order = c(0,0,0), xreg = xreg)
#     # Extract terms
#     coef_df <- broom::tidy(model) %>%
#       filter(str_detect(term, "wave")) %>%
#       mutate(station = station_name)
#     return(coef_df)
#   }, error = function(e) {
#     return(tibble(
#       term = c("wave_blockwave"),
#       estimate = NA,
#       std.error = NA,
#       station = station_name
#     ))
#   })
# }
# 
# temp_mod <- map(unique(wq_prep$station), fit_arima_hw, wq_prep, "watertemp_perc") %>% list_rbind() %>% left_join(sites)
# do_mod <- map(unique(wq_prep$station), fit_arima_hw, wq_prep, "do_perc") %>% list_rbind()  %>% left_join(sites)
# ph_mod <- map(unique(wq_prep$station), fit_arima_hw, wq_prep, "ph_perc") %>% list_rbind() %>% left_join(sites)
# cond_mod <- map(unique(wq_prep$station), fit_arima_hw, wq_prep, "cond_perc") %>% list_rbind() %>% left_join(sites)
# turb_mod <- map(unique(wq_prep$station), fit_arima_hw, wq_prep, "turb_perc") %>% list_rbind() %>% left_join(sites)
# 
# plot_results <- function(df){
#   df %>% 
#     filter(!state %in% c("ak", "pr", "hi")) %>% 
#     ggplot(aes(x = term, y = estimate, fill = factor(region))) +
#     geom_boxplot() 
# }
# 
# 
# plot_results(temp_mod)
# plot_results(do_mod)
# plot_results(ph_mod)
# plot_results(cond_mod)
# plot_results(turb_mod)
# 
# 
# fit_arima_hw_len <- function(station_name, df, response_var) {
#   
#   df <- df %>%
#     filter(station == station_name) %>% 
#     arrange(date) %>% 
#     mutate(wave_block = case_when(
#              hw_length < 10 ~ str_c(wave_block, "_short"),
#              hw_length < 15 ~ str_c(wave_block, "_med"),
#              hw_length >= 15 ~ str_c(wave_block, "_long"),
#              .default = wave_block
#            ),
#            wave_block = if_else(is.na(wave_block), "none", wave_block),
#            wave_block = factor(wave_block, levels = c("none",
#                                                       "wave_short", "post_short", "pre_short",
#                                                       "wave_med", "post_med", "pre_med",
#                                                       "wave_long", "post_long", "pre_long")),
#            across(c(hw_length, pre_n90, post_n90), ~if_else(is.na(.), 0, .)))
#   
#   y <- df[[response_var]]
#   
#   tryCatch({
#     # get T/F matrix of heatwave blocks, dropping columns for levels that don't exist at site to prevent model failure
#     xreg <- model.matrix(~ wave_block, df)[, -1] %>%
#       as.data.frame() %>% 
#       select(where(~ !(is.numeric(.) && all(. == 0)))) %>% 
#       # bind_cols(select(df, hw_length, pre_n90, post_n90)) %>% 
#       as.matrix() 
#     
#     model <- arima(y, order = c(0,0,0), xreg = xreg)
#     # Extract terms
#     coef_df <- broom::tidy(model) %>%
#       filter(str_detect(term, "wave")) %>%
#       mutate(station = station_name)
#     return(coef_df)
#   }, error = function(e) {
#     return(tibble(
#       term = c("wave_blockwave"),
#       estimate = NA,
#       std.error = NA,
#       station = station_name
#     ))
#   })
# }
# 
# temp_mod <- map(unique(wq_prep$station), fit_arima_hw_len, wq_prep, "watertemp_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# do_mod <- map(unique(wq_prep$station), fit_arima_hw_len, wq_prep, "do_perc") %>% list_rbind()  %>% left_join(sites, by = join_by(station))
# ph_mod <- map(unique(wq_prep$station), fit_arima_hw_len, wq_prep, "ph_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# cond_mod <- map(unique(wq_prep$station), fit_arima_hw_len, wq_prep, "cond_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# turb_mod <- map(unique(wq_prep$station), fit_arima_hw_len, wq_prep, "turb_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# 
# plot_results(temp_mod)
# plot_results(do_mod)
# plot_results(ph_mod)
# plot_results(cond_mod)
# plot_results(turb_mod)
# 
# 
# fit_arima_hw_prcp <- function(station_name, df, response_var) {
#   
#   df <- df %>%
#     filter(station == station_name) %>% 
#     arrange(date) %>% 
#     mutate(wave_block = case_when(
#       prcp_wave > 0 & wave_block %in% c("wave", "post") ~ str_c(wave_block, "_prcp"),
#       #prcp_pre > 0 ~ str_c(wave_block, "_prcp_post"),
#       #prcp_post > 0 ~ str_c(wave_block, "_long"),
#       .default = wave_block
#     ),
#     wave_block = if_else(is.na(wave_block), "none", wave_block),
#     wave_block = factor(wave_block, levels = c("none",
#                                                "wave", "post", "pre",
#                                                "wave_prcp", "post_prcp", "pre_prcp")),
#     across(c(hw_length, pre_n90, post_n90), ~if_else(is.na(.), 0, .)))
#   
#   y <- df[[response_var]]
#   
#   tryCatch({
#     # get T/F matrix of heatwave blocks, dropping columns for levels that don't exist at site to prevent model failure
#     xreg <- model.matrix(~ wave_block, df)[, -1] %>%
#       as.data.frame() %>% 
#       select(where(~ !(is.numeric(.) && all(. == 0)))) %>% 
#       # bind_cols(select(df, hw_length, pre_n90, post_n90)) %>% 
#       as.matrix() 
#     
#     model <- Arima(y, order = c(0,0,0), xreg = xreg)
#     # Extract terms
#     coef_df <- broom::tidy(model) %>%
#       filter(str_detect(term, "wave")) %>%
#       mutate(station = station_name)
#     return(coef_df)
#   }, error = function(e) {
#     return(tibble(
#       term = c("wave_blockwave"),
#       estimate = NA,
#       std.error = NA,
#       station = station_name
#     ))
#   })
# }
# 
# temp_mod <- map(unique(met_prep$station), fit_arima_hw_prcp, met_prep, "watertemp_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# do_mod <- map(unique(met_prep$station), fit_arima_hw_prcp, met_prep, "do_perc") %>% list_rbind()  %>% left_join(sites, by = join_by(station))
# ph_mod <- map(unique(met_prep$station), fit_arima_hw_prcp, met_prep, "ph_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# cond_mod <- map(unique(met_prep$station), fit_arima_hw_prcp, met_prep, "cond_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# turb_mod <- map(unique(met_prep$station), fit_arima_hw_prcp, met_prep, "turb_perc") %>% list_rbind() %>% left_join(sites, by = join_by(station))
# 
# plot_results(temp_mod)
# plot_results(do_mod)
# plot_results(ph_mod)
# plot_results(cond_mod)
# plot_results(turb_mod)

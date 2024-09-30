# Calculating resistance and Resilience
# Author: Bennett McAfee

library(tidyverse)
library(BreakPoints)
#library(forecast) # may be used for detrending the time series
library(trend) # may also be used for detrending

# Collecting the data and breakpoints
source("Breakpoints.R")
rm(list = ls()[!ls() %in% c("data_list", "met_breakpoint_df", "nut_breakpoint_df", "wq_breakpoint_df")])
gc()


# Defining Resistance and Resilience
## Calculations are based on Orwin & Wardle (2004, Soil Biology & Biochemistry: https://doi.org/10.1016/j.soilbio.2004.04.036 )

### d0 = the difference from the control and the parameter after disturbance
### c0 = the control, a representation of antecedent conditions
resistance <- function(d0, c0){ 
  rs <- 1 - ((2*abs(d0))/(c0+abs(d0)))
  return(rs)
}

### d0 is as above
### dx is the difference between the parameter value and control at the time chosen to measure resilience
resilience <- function(d0, dx){ 
  rl <- ((2 * abs(d0))/(abs(d0)+abs(dx))) - 1
  return(rl)
}

### series = the time series to analyze as a vector
### breakpoint = the time of breakpoint (in timesteps)
### c_lookback = the size of the lookback window to be averaged to determine the antecedent conditions before the breakpoint (in timesteps)
### x = the number of timesteps after the breakpoint to measure resilience
resril <- function(series, breakpoint, c_lookback, x){
  c0 <- mean(series[(breakpoint-1)-c_lookback], na.rm = TRUE)
  d0 <- series[breakpoint] - c0
  dx <- series[breakpoint+x] - c0
  
  rs <- resistance(d0 = d0, c0 = c0)
  rl <- resilience(d0 = d0, dx = dx)
  
  output <- c(rs, rl)
  names(output) <- c("Resistance", "Resilience")
  return(output)
}


n_bp <- nrow(met_breakpoint_df) + nrow(nut_breakpoint_df) + nrow(wq_breakpoint_df)
row_analyzed <- 0

res_df <- data.frame(site = character(),
                     station = character(),
                     station_number = numeric(),
                     variable = character(),
                     testtype = character(),
                     breakpoint = numeric(),
                     breakpoint_pval = numeric(),
                     breakpoint_yearfrac = numeric(),
                     resistance = numeric(),
                     resilience = numeric())

# Finding the resistance and resilience in the time series
for(varcat in c("met", "nut", "wq")){
  bp_info <- get(paste0(varcat, "_breakpoint_df"))
  for(i in 1:nrow(bp_info)){
    # Gathering general information
    site <- bp_info[[i, "site"]]
    station <- bp_info[[i, "station"]]
    station_number <- bp_info[[i, "station_number"]]
    var <- bp_info[[i, "variable"]]
    testtype <- bp_info[[i, "test_type"]]
    breakpoint <- bp_info[[i, "breaks"]]
    breakpoint_pval <- bp_info[[i, "pvalue"]]
    data <- data_list[[which(names(data_list) == paste0(site, "_", varcat, "_", tolower(site), ".csv"))]]
    breakpoint_yearfrac <- data$yearfrac[breakpoint]
    
    row_analyzed <- row_analyzed + 1
    cat("\r", "Analyzing", row_analyzed, "of", n_bp, "-", site, var)
    
    # Time series decomposition
    
    ts_original <- ts(data = data[[var]], frequency = 12) # requires trend package
    ts_original[which(is.na(ts_original))] <- mean(ts_original,na.rm=TRUE)
    ts_decomp <- decompose(ts_original)
    
    # Calculate resistance and resilience in the de-seasoned time series using the breakpoint as the disturbance event
    ## Inspired by Thayne et al. (2021, Limnology & Oceanography: https://doi.org/10.1002/lno.11859 )
    
    res <- resril(series = ts_decomp$trend, #trend removes the seasonal component, limiting the influence of time-of-year
                  breakpoint = breakpoint,
                  c_lookback = 6, # 6-month lookback window to determine antecedent conditions was chosen arbitrarily
                  x = 3) # Looking 3 months into the future to measure resilience was also chosen arbitrarily.
    
    # Thayne et al. calculated the point at which resilience in measure dynamically based on when the "parameter under observation had returned to antecedent conditions", which seems antethetical to Orwin (2004) who made the index.
    
    tmp_df <- data.frame(site = site,
                         station = station,
                         station_number = station_number,
                         variable = var,
                         testtype = testtype,
                         breakpoint = breakpoint,
                         breakpoint_pval = breakpoint_pval,
                         breakpoint_yearfrac = breakpoint_yearfrac,
                         resistance = res[[1]],
                         resilience = res[[2]])
    res_df <- rbind(res_df, tmp_df)
    
  }
}
  
# Visualize the findings
ggplot(res_df[res_df$breakpoint_pval <= 0.05,], aes(x = variable, y = resistance))+
  geom_boxplot()+
  geom_jitter(aes(color = site, shape = as.factor(station_number)), width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Resistance by variable", x = "Variable", y = "Resistance", shape = "Station Number", color = "Site")

ggsave("figures/resistance/resistance_by_variable.png", width = 11, height = 8.5, units = "in")

ggplot(res_df[res_df$breakpoint_pval <= 0.05,], aes(x = site, y = resistance))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Resistance by site across all variables and stations", x = "Site", y = "Resistance")

ggsave("figures/resistance/resistance_by_site.png")
  
# ggplot(res_df[res_df$breakpoint_pval <= 0.05,], aes(x = breakpoint_yearfrac, y = resistance))+
#   geom_point(aes(color = variable))
  
  
  
  
  
  
  
  
  
  
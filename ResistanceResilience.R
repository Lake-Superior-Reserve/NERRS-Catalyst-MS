# Calculating resistance and Resilience
# Author: Bennett McAfee

library(tidyverse)
library(BreakPoints)
#library(forecast) # may be used for detrending the time series
library(trend) # may also be used for detrending

# Collecting the data and breakpoints
source("Breakpoints.R")
rm(list = ls()[!ls() %in% c("data_list", "met_breakpoint_df", "nut_breakpoint_df", "wq_breakpoint_df")])


# Defining Resistance and Resilience
## Calculations are based on Orwin & Wardle (2004, Soil Biology & Biochemistry: https://doi.org/10.1016/j.soilbio.2004.04.036)

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
  c0 <- mean(series[(breakpoint-1)-c_looback], na.rm = TRUE)
  d0 <- series[breakpoint] - c0
  dx <- series[breakpoint+x] - c0
  
  rs <- resistance(d0 = d0, c0 = c0)
  rl <- resilience(d0 = d0, dx = dx)
  
  output <- c(rs, rl)
  names(output) <- c("Resistance", "Resilience")
  return(output)
}


# Finding the resistance and resilience in the time series
for(varcat in c("met", "nut", "wq")){
  bp_info <- get(paste0(varcat, "_breakpoint_df"))
  for(i in 1:nrow(bp_info)){
    # Gathering general information
    site <- bp_info[[i, "site"]]
    station <- bp_info[[i, "station"]]
    var <- bp_info[[i, "variable"]]
    testtype <- bp_info[[i, "test_type"]]
    breakpoint <- bp_info[[i, "breaks"]]
    breakpoint_pval <- bp_info[[i, "pvalue"]]
    data <- data_list[[which(names(data_list) == paste0(site, "_", varcat, "_", tolower(site), ".csv"))]]
    breakpoint_yearfrac <- data$yearfrac[breakpoint]
    
    # TO DO: Time series decomposition, according to Thayne et al. (2021, Limnology & Oceanography: https://doi.org/10.1002/lno.11859)
    ## This section is a WIP
    
    ts_original <- ts(data = data[[var]], frequency = 12) # requires trend package
    ts_original[which(is.na(ts_original))] <- mean(ts_original,na.rm=TRUE)
    ts_decomp <- decompose(ts_original)
    
    # TO DO: Calculate resistance and resilience in the detrended time series using the breakpoint as the disturbance event
    
    # TO DO: Save the resistance and resiliance to a data frame along with the general information gathered above
    
  }
}
  
# TO DO: plot the resistance and resilience values calculated
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
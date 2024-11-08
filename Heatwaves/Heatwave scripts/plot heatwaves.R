library(heatwaveR)
library(tidyverse)

# plot heatwaves for sites of interest

# we just want to plot heatwaves for PDB, RKB, and Great Bay (GRB)

# read in the heatwaves
hw.all = read.csv("./Heatwaves/Heatwave results/NERR heatwave events 2024-10-21.csv")

# create columns for site and station
hw.all = hw.all %>% mutate(site = substr(site_station, 1, 3), station = substr(site_station, 5, 7))


hw.pdb = hw.all %>% filter(site == "pdb")

hw.rkb = hw.all %>% filter(site == "rkb")

#===============================================================================#
### Great Bay heatwaves ###
hw.grb = hw.all %>% filter(site == "grb")


# going to calculate heatwaves again by site


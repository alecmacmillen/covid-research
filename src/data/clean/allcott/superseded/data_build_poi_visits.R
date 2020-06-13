###################################################
# data_build_poi_visits.R
# Script to replicate data build for county-level POI and visits
# As described in Allcott et al (2020) Appendix A.1.1
###################################################

rm(list = ls())
library(tidyverse)
library(stringr)
library(sf)


###########
# STEP ONE: Merge county-level presidential vote shares (cleaned in 
# src/data/clean/political/elections/medsl-clean-allcott.R) with 2010
# TIGER county shapefile
###########
# Read in county vote share data and shapefile; drop Alaska
votes <- read_csv("data/interim/political/elections/medsl_allcott_clean.csv") %>% 
  filter(state_fips != "02")
tiger <- sf::st_read("data/raw/geographic/tl_2010_us_county10/tl_2010_us_county10.shp") %>% 
  filter(STATEFP10 != "02")

# Investigate county shapefile object
st_geometry_type(tiger)
st_crs(tiger) # shapefile in longlat
st_bbox(tiger)

# Join
# Resulting dataframe has 3111 observations
votes_tiger <- inner_join(
  votes, tiger,
  by = c("county_fips" = "GEOID10")
)



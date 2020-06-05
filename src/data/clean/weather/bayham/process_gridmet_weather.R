###################################################
# process_gridmet_weather.R
# Script to replicate data build for county-level POI and visits
# As described in Allcott et al (2020) Appendix A.1.1, step 5
# Process gridmet weather data at county level
###################################################

rm(list = ls())
library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)

weather2020 <- data.table::fread("data/raw/weather/bayham/weather_county_2020-01-01_yesterday.csv.gz")

# Vars:
# precip = daily precipitation (mm)
# rmax, rmin = max/min daily relative humidity (%)
# srad = surface downwelling solar radiation (W/m^2)
# tmax, tmin = max/min daily temperature (degrees F)
# wind_speed = mean daily wind speed (mph)

daily.weather <- weather2020 %>% 
  mutate(county_fips = str_pad(as.character(county), 5, c("left"), "0"),
         date = as.Date(date, "%Y-%m-%d")) %>% 
  select(county_fips, date, precip, tmin, tmax) 

# write_csv(daily.weather, "data/interim/weather/bayham/daily_weather_processed.csv.gz")

weekly.weather <- daily.weather %>% 
  mutate(week = lubridate::week(date),
         # Adjust week periods to align with safegraph data week periods: starting on 1/6, etc.
         weeknum = dplyr::lead(week, n=2L)) %>% 
  select(-c(week)) %>%
  group_by(county_fips, weeknum) %>% 
  mutate(daynum = row_number()) %>% 
  ungroup() %>% 
  mutate(week_start_date = ifelse(daynum==1, date, NA)) %>% 
  group_by(county_fips, weeknum) %>% 
  tidyr::fill(week_start_date) %>% 
  ungroup() %>% 
  mutate(week_start_date = as_date(week_start_date))

weekly.weather.summary <- weekly.weather %>% 
  filter(week_start_date >= as.Date("2020-01-27", "%Y-%m-%d")) %>% 
  group_by(county_fips, week_start_date) %>% 
  summarize(mean_precip = mean(precip, na.rm=TRUE),
            mean_tmin = mean(tmin, na.rm=TRUE),
            mean_tmax = mean(tmax, na.rm=TRUE)) %>% 
  ungroup()

# write_csv(weekly.weather.summary, "data/interim/weather/bayham/weekly_weather_processed.csv.gz")











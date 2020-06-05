###################################################
# data_build_social_distancing.R
# Script to replicate data build for county-level POI and visits
# As described in Allcott et al (2020) Appendix A.1.2
###################################################

rm(list = ls())
library(data.table)
library(stringr)
library(tidyverse)



###########
# STEP ONE: Read in safegraph social distancing cleaned data
# Cleaning process: src/data/clean/mobility/safegraph/social-distancing/process_social_distancing.R
###########
social.distancing <- read_csv("data/interim/mobility/safegraph/social-distancing/social_distancing_weekly.csv.gz") %>% 
  # Create share of devices leaving home var
  mutate(share_devices_leaving_home = (device_count - completely_home_device_count)/device_count)

###########
# STEP TWO: Merge in census data (2016 ACS)
# Cleaning process: src/data/clean/demographic/process_open_census.R
###########
census <- read_csv("data/interim/demographic/economic_controls_census.csv")
merge <- social.distancing %>% 
  left_join(census, by=c("county_fips"))

###########
# STEP THREE: Merge in weather data (gridMET, Bayham)
# Cleaning process: src/data/clean/weather/bayham/process_gridmet_weather.R
###########
weather <- read_csv("data/interim/weather/bayham/weekly_weather_processed.csv.gz")
merge2 <- merge %>% 
  left_join(weather, by=c("county_fips", "week_start_date"))

###########
# STEP FOUR: Merge in SIPO data
# Cleaning process: src/data/clean/responses/process_sipos.R
###########
sipos <- read_csv("data/interim/responses/weekly_sipo_timeseries.csv.gz")
merge3 <- merge2 %>% 
  left_join(sipos, by=c("county_fips", "week_start_date", "state", "county"))

###########
# STEP FOUR: Merge in NYT cases/deaths data
# Cleaning process: src/data/clean/cases-deaths/nyt/process_nyt_cases.R
###########
nyt <- read_csv("data/interim/cases-deaths/nyt/nyt_cases_deaths_weekly.csv.gz")
merge4 <- merge3 %>% 
  left_join(nyt, by=c("county_fips", "week_start_date", "state", "county"))

###########
# STEP FIVE: Merge in MIT county-level presidential vote shares
# Cleaning process: src/data/clean/political/elections/medsl-clean-allcott.R
###########
medsl <- read_csv("data/interim/political/elections/medsl_allcott_clean.csv") %>% 
  select(county_fips, trump_vote_share)
merge5 <- merge4 %>% 
  left_join(medsl, by=c("county_fips"))

###########
# STEP SIX: Minor variable prettifying/cleanup/reordering
###########
processed <- merge5 %>% 
  mutate(log_cases = log(1+mean_cases),
         log_deaths = log(1+mean_deaths)) %>% 
  select(-c(state_sip_start_date, state_sip_end_date, county_sip_start_date, county_sip_end_date, mean_cases, mean_deaths)) %>% 
  select(state, county, county_fips, week_start_date, device_count, completely_home_device_count,
         median_home_dwell_time, share_devices_leaving_home, trump_vote_share, everything())

# write_csv(processed, "data/processed/allcott/social-distancing/allcott_repl_social_distancing.csv.gz")

# THINGS TO GO BACK AND FIX LATER:
# Deal with/drop the NYC + KC counties (aggregate? This just takes a little time to think + work through)
# Add health controls for log of 1 + county pop density; share of pop 65+
# Double check nulls
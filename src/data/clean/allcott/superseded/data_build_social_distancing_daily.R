###################################################
# data_build_social_distancing_daily.R
# Script to replicate data build for county-level social distancing DAILY
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
social.distancing <- read_csv("data/interim/mobility/safegraph/social-distancing/social_distancing_daily.csv.gz")

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
weather <- read_csv("data/interim/weather/bayham/daily_weather_processed.csv.gz")
merge2 <- merge %>% 
  left_join(weather, by=c("county_fips", "date"))

###########
# STEP FOUR: Merge in SIPO data
# Cleaning process: src/data/clean/responses/process_sipos.R
###########
sipos <- read_csv("data/interim/responses/daily_sipo_timeseries.csv.gz",
                  col_types = cols(county_sip_start_date = col_date(),
                                   county_sip_end_date = col_date()))
merge3 <- merge2 %>% 
  left_join(sipos, by=c("county_fips", "date", "state", "county"))

###########
# STEP FOUR: Merge in NYT cases/deaths data
# Cleaning process: src/data/clean/cases-deaths/nyt/process_nyt_cases.R
###########
nyt <- read_csv("data/interim/cases-deaths/nyt/nyt_cases_deaths_daily.csv.gz")
merge4 <- merge3 %>% 
  left_join(nyt, by=c("county_fips", "date", "state", "county"))

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
  select(-c(state_sip_start_date, state_sip_end_date, county_sip_start_date, county_sip_end_date)) %>%
  select(state, county, county_fips, date, everything())

###########
# STEP SEVEN: Check coverage
###########
n <- nrow(processed)
for (col in names(processed)) {
  print(paste0(col, " summary:"))
  print(summary(processed[[col]]))
  print(paste0(col, " null %:"))
  print(sum(is.na(processed[[col]]))/n)
}

###########
# STEP EIGHT: Drop obs where independent variable (trump_vote_share) is null
# And generate indicator variable for Republican-leaning county, and percentile of Trump voteshare
###########
processed.out <- processed %>% 
  filter(!is.na(trump_vote_share)) %>% 
  mutate(county_republican = ifelse(trump_vote_share >= .5, 1, 0),
         republican_vtsh_ntile = ntile(trump_vote_share, 100))

# write_csv(processed.out, "data/processed/allcott/social-distancing/allcott_social_distancing_build_daily.csv.gz")

illinois <- processed.out %>% 
  filter(state == "illinois" & !is.na(state_sip))

# write_csv(illinois, "data/processed/allcott/social-distancing/illinois_social_distancing_build_daily.csv.gz")

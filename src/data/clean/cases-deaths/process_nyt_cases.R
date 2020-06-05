###################################################
# process_nyt_cases.R
# Build dataset of case and death counts 
###################################################

rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)

# Read in county shelter in place orders
responses.county <- read_csv("data/raw/responses/county_level_sip_updated.csv",
                             col_types = cols(state_code = col_character(), county_code = col_character()))

responses.county.fix <- responses.county %>%
  mutate(state_fips = str_pad(state_code, 2, pad = "0"),
         county_fips = str_pad(county_code, 3, pad = "0")) %>% 
  select(-c(citation1, citation2, citation3, citation4, citation5, state_code, county_code, state,
            state_sip_start_date, state_sip_end_date, county_sip_start_date, county_sip_end_date)) %>% 
  rename(state = state_name,
         county = county_name) %>% 
  # We're treating all of New York City as New York County, so drop NY counties for the other 4 boroughs
  # This is OK because all of NYC has the same start/end date for local SIP order
  filter(!(state=="New York" & 
             county %in% c("Kings County", "Queens County", "Richmond County", "Bronx County"))) %>% 
  mutate(popestimate2019 = ifelse(state=="New York" & county=="New York County", nyc.pop, popestimate2019))

# Generate daily time series from 1/27 - 5/31 for all counties
daily.county <- responses.county.fix %>% 
  distinct(state_fips, county_fips, state, county) %>% 
  group_by(state_fips, county_fips, state, county) %>% 
  do(data.frame(state=.$state, county=.$county, state_code=.$state_fips, county_fips=.$county_fips,
                date=seq(as.Date("01-27-20", format="%m-%d-%y"),
                         as.Date("05-31-20", format="%m-%d-%y"),
                         by="day"))) %>% 
  ungroup() %>% 
  mutate(county_fips = paste0(state_fips, county_fips),
         state = str_to_lower(state),
         county = str_to_lower(gsub(" County", "", county))) %>% 
  select(-c(state_fips, state_code))






# Read in NYT cases data
nyt <- read_csv("data/raw/cases-deaths/nyt/nyt-us-counties.csv") %>% 
  mutate(state_code = str_sub(fips, start = 1L, end = 2L),
         county_code = str_sub(fips, start = 3L, end = 5L)) %>% 
  # NYT treats the 5 counties that compose NYC as one, so we'll transform "New York City" to "New York County"
  mutate(county = ifelse(county=="New York City", "New York County", county),
         fips = ifelse(county=="New York County", "36061", fips),
         state_code = ifelse(county=="New York County", "36", state_code),
         county_code = ifelse(county=="New York County", "061", county_code),
         county_fips = paste0(state_code, county_code)) %>% 
  select(date, county_fips, cases, deaths)




# Merge daily time series with NYT case data
cases.daily <- daily.county %>% 
  left_join(nyt, by=c("county_fips", "date")) %>% 
  mutate(cases = ifelse(is.na(cases), 0, cases),
         deaths = ifelse(is.na(deaths), 0, deaths))

# Write out daily cases
# write_csv(cases.daily, "data/interim/cases-deaths/nyt/nyt_cases_deaths_daily.csv.gz")





# Create weekly cases/deaths summary (average of the measure for the week)
cases.weekly <- cases.daily %>% 
  group_by(county_fips) %>% 
  mutate(weeknum = lubridate::week(date),
         weekshift = dplyr::lead(weeknum, n=2L),
         weekshift = ifelse(is.na(weekshift), 22, weekshift)) %>% 
  group_by(county_fips, weekshift) %>% 
  mutate(daynum = row_number()) %>% 
  ungroup() %>% 
  mutate(week_start_date = ifelse(daynum==1, date, NA)) %>% 
  arrange(county_fips, date) %>% 
  group_by(county_fips) %>% 
  fill(week_start_date)

cases.weekly.summary <- cases.weekly %>% 
  arrange(county_fips, state, county, week_start_date) %>% 
  group_by(county_fips, state, county, week_start_date) %>% 
  summarize(mean_cases = mean(cases, na.rm=TRUE),
            mean_deaths = mean(deaths, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(week_start_date = as_date(week_start_date))

# Write out weekly cases summary
# write_csv(cases.weekly.summary, "data/interim/cases-deaths/nyt/nyt_cases_deaths_weekly.csv.gz")

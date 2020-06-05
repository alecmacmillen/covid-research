###################################################
# process_sipos.R
# Build dataset of daily and weekly SIPO timeseries
# by county
###################################################

rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)

all.counties <- read_csv("data/raw/demographic/co-est2019-alldata.csv") %>% 
  filter(COUNTY != "000") %>% 
  select(STATE:CTYNAME, POPESTIMATE2019) %>% 
  rename(state_fips = STATE,
         county_fips = COUNTY,
         state = STNAME,
         county = CTYNAME,
         popestimate2019 = POPESTIMATE2019)

# NYT reports cases for all five boroughs (which are their own counties) rolled into one,
# so let's compress these 5 rows in the all.counties dataset to 1. For merging purposes,
# we'll refer to all of New York as "New York County," which in reality is just Manhattan,
# but in our dataset will refer to all 5 boroughs
nyc.pop <- all.counties %>% 
  filter(state=="New York" & 
           county %in% c("New York County", "Kings County", "Queens County", "Richmond County", "Bronx County")) %>% 
  summarize(sum(popestimate2019)) %>% 
  .[[1]]

all.counties <- all.counties %>% 
  filter(!(state=="New York" & 
             county %in% c("New York County", "Kings County", "Queens County", "Richmond County", "Bronx County"))) %>% 
  add_row(state_fips="36", county_fips="061", state="New York", county="New York County", popestimate2019=nyc.pop) %>% 
  arrange(state_fips, county_fips)



# Read in state shelter in place orders
state.sip <- read_excel("data/raw/responses/all_actions_tracking_individual_sheets.xlsx", sheet = "shelter_in_place") %>% 
  select(state_code:sip_end_date) %>% 
  rename(state_sip_start_date = sip_start_date,
         state_sip_end_date = sip_end_date,
         state_fips = state_code,
         state = state_name)

# Join state-level SIP orders to county
all.merged <- all.counties %>% 
  left_join(state.sip, by=c("state_fips", "state"))

#write.csv(all.merged, "Intermediate/county_level_sip_to_update.csv", row.names=FALSE)



# Read in county shelter in place orders
responses.county <- read_csv("data/raw/responses/county_level_sip_updated.csv",
                             col_types = cols(state_code = col_character(), county_code = col_character()))

responses.county.fix <- responses.county %>%
  mutate(state_fips = str_pad(state_code, 2, pad = "0"),
         county_fips = str_pad(county_code, 3, pad = "0")) %>% 
  select(-c(citation1, citation2, citation3, citation4, citation5, state_code, county_code, state)) %>% 
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
                         by="day")))

# Join the county-level SIP orders to daily time series
daily.merge <- daily.county %>% 
  left_join(responses.county.fix, by=c("state_fips", "county_fips", "state", "county"))

# Create daily indicator for shelter in place
indicators <- daily.merge %>%
  mutate(state_sip_start_date = as.Date(state_sip_start_date, "%m/%d/%Y"),
         state_sip_end_date = as.Date(state_sip_end_date, "%m/%d/%Y")) %>% 
  
  mutate(county_sip_start_date = as.Date(county_sip_start_date, "%m/%d/%Y"),
         county_sip_end_date = as.Date(county_sip_end_date, "%m/%d/%Y")) %>% 
  
  # Set vals for comparisons
  mutate(state_sip_start_date = ifelse(!is.na(state_sip_start_date), state_sip_start_date, Inf),
         county_sip_start_date = ifelse(!is.na(county_sip_start_date), county_sip_start_date, Inf),
         state_sip_end_date = ifelse(!is.na(state_sip_end_date), state_sip_end_date, 0),
         county_sip_end_date = ifelse(!is.na(county_sip_end_date), county_sip_end_date, 0)) %>% 
  
  mutate(
    state_sip = ifelse((state_sip_start_date <= date & state_sip_end_date >= date) |
                         (state_sip_start_date <= date & state_sip_end_date == 0) |
                         (is.infinite(state_sip_start_date) & state_sip_end_date >= date), 1, 0),
    
    county_sip = ifelse((county_sip_start_date <= date & county_sip_end_date >= date) |
                          (county_sip_start_date <= date & county_sip_end_date == 0) |
                          (is.infinite(county_sip_start_date) & county_sip_end_date >= date), 1, 0), 
    
    state_sip_start_date = ifelse(is.infinite(state_sip_start_date), NA, state_sip_start_date),
    county_sip_start_date = ifelse(is.infinite(county_sip_start_date), NA, county_sip_start_date),
    state_sip_end_date = ifelse(state_sip_end_date == 0, NA, state_sip_end_date),
    county_sip_end_date = ifelse(county_sip_end_date == 0, NA, county_sip_end_date)) %>% 
  
  mutate(state_sip_start_date = as_date(state_sip_start_date),
         state_sip_end_date = as_date(state_sip_end_date),
         county_sip_start_date = as_date(county_sip_start_date),
         county_sip_end_date = as_date(county_sip_end_date)) %>% 
  
  select(-c(state_code, popestimate2019))


# Create daily timeseries of sipos at county-level
daily.sipos <- indicators %>% 
  ungroup() %>% 
  mutate(county_fips = paste0(state_fips, county_fips),
         state = str_to_lower(state),
         county = str_to_lower(gsub(" County", "", county))) %>% 
  select(county_fips, state, county, everything()) %>% 
  select(-c(state_fips)) %>% 
  mutate(sip_binary = ifelse(state_sip==1 | county_sip==1, 1, 0))

# write_csv(daily.sipos, "data/interim/responses/daily_sipo_timeseries.csv.gz")


# Create weekly timeseries of sipos at county-level
weekly.sipos <- daily.sipos %>% 
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

weekly.sipos.summary <- weekly.sipos %>% 
  arrange(county_fips, state, county, week_start_date) %>% 
  group_by(county_fips, state, county, week_start_date, 
           state_sip_start_date, state_sip_end_date, county_sip_start_date, county_sip_end_date) %>% 
  summarize(sip_days = sum(sip_binary, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(weekly_sipo_binary = ifelse(sip_days > 0, 1, 0),
         week_start_date = as_date(week_start_date))

# write_csv(weekly.sipos.summary, "data/interim/responses/weekly_sipo_timeseries.csv.gz")

###################################################
# process_open_census.R
# Script to replicate data build for county-level Open Census data
# As described in Allcott et al (2020) Appendix A.1.1, step 4 and FN 10
###################################################

rm(list = ls())
library(stringr)
library(sf)
library(data.table)
library(lubridate)
library(tidyverse)
library(tidycensus)

# The authors use the safegraph data, but this is huge to load
# open.census <- data.table::fread("data/raw/demographic/safegraph_open_census_data.tar.gz")

# A less unwieldy approach would be to use tidycensus to directly get desired variables
# at the county level, instead of downloading census block-level data and aggregating up
# Tables from 2016 5-year ACS
Sys.getenv("CENSUS_API_KEY")
v16 <- load_variables(2016, "acs5", cache=TRUE)

# Find variables using v16 manual inspection

open.census.rep <- get_acs(geography="county", year=2016, survey="acs5",
                           variables=c(bachelors = "B06009_005",
                                       white = "B02001_002",
                                       black = "B02001_003",
                                       asian = "B02001_005",
                                       current_undergrad = "B14001_008",
                                       poverty = "B06012_002",
                                       hhinc_100k = "B25121_092",
                                       total = "B00001_001")) %>% 
  select(-c(moe)) %>% 
  spread(variable, estimate)

occupations <- get_acs(geography="county", year=2016, survey="acs5",
                       variables=c(total_occupations="C24050_001",
                                   msta="C24050_015",
                                   services="C24050_029",
                                   sales="C24050_043",
                                   resources="C24050_057",
                                   public_transit="B08006_008")) %>% 
  select(-c(moe)) %>% 
  spread(variable, estimate) %>% 
  mutate(share_msta = msta / total_occupations,
         share_services = services / total_occupations,
         share_sales = sales / total_occupations,
         share_resources = resources / total_occupations,
         share_public_transit = public_transit / total_occupations) %>% 
  select(GEOID, NAME, share_msta, share_services, share_sales, share_resources, share_public_transit)

total.pop <- read_csv("data/raw/demographic/county_pop_2016.csv") %>% 
  mutate(NAME = str_trim(str_sub(county, start=2L, end=-1L))) %>% 
  select(-c(county))

# Final output with shares
output <- open.census.rep %>% 
  left_join(total.pop, by=c("NAME")) %>% 
  mutate(share_asian = asian / population,
         share_black = black / population,
         share_white = white / population,
         share_bachelors = bachelors / population,
         share_undergrad = current_undergrad / population,
         share_hhinc_100k = hhinc_100k / population,
         share_poverty = poverty / population) %>% 
  left_join(occupations, by=c("NAME", "GEOID")) %>% 
  separate(NAME, into=c("countyname", "statename"), sep=",") %>% 
  mutate(county = str_to_lower(gsub(" County", "", countyname)),
         state = str_to_lower(statename)) %>% 
  rename(county_fips = GEOID) %>% 
  select(county_fips, county, state, starts_with("share"))

write_csv(output, "data/interim/demographic/economic_controls_census.csv")

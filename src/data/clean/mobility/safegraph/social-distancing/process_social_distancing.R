###################################################
# process_social_distancing.R
# Script to replicate data build for Safegraph social distancing data
# As described in Allcott et al (2020) Appendix A.1.2, steps 1-2
###################################################

rm(list = ls())
library(stringr)
library(data.table)
library(lubridate)
library(tidyverse)

# Create list of daily dataframes to process
root.dir <- "data/raw/mobility/safegraph/social-distancing/"
all.files <- list.files(root.dir, recursive=TRUE)
n <- length(all.files)

# Process each daily df and store it in a list
all.dfs <- list()
i <- 1
for (path in all.files) {
  print(paste0("Processing file ", i))
  file.path <- paste0(root.dir, path)
  df <- data.table::fread(file.path) %>% 
    mutate(block_group = str_pad(as.character(origin_census_block_group), 12, side=c("left"), pad="0"),
           county_fips = str_sub(block_group, start=1L, end=5L),
           date = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d")) %>% 
    select(county_fips, date, device_count, completely_home_device_count, median_home_dwell_time)
  all.dfs[[i]] <- df
  i <- i + 1
}

# Combine daily dfs into one master df
all.social.distancing <- bind_rows(all.dfs)

# Identify county_fips that don't have full coverage
coverage <- all.social.distancing %>% 
  group_by(county_fips, date) %>% 
  summarize(count = n())

fips.not.full <- as.data.frame(table(coverage$county_fips)) %>% 
  filter(Freq != 128) %>% 
  mutate(Var1 = as.character(Var1)) %>% 
  pull(Var1)

asd.munge <- all.social.distancing %>% 
  mutate(state_fips = str_sub(county_fips, start=1L, end=2L)) %>% 
  # Drop Alaska
  filter(state_fips != "02") %>% 
  # Drop county_fips without full coverage
  filter(!(county_fips %in% fips.not.full))




# Create DAILY social distancing measure at county level by aggregating across block groups
social.distancing.daily <- asd.munge %>% 
  select(-c(state_fips)) %>% 
  group_by(county_fips, date) %>% 
  summarize(device_count = sum(device_count, na.rm=TRUE),
            completely_home_device_count = sum(completely_home_device_count, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(county_fips, date)

weighted.mean <- asd.munge %>% 
  rename(device_count_new = device_count) %>% 
  select(-c(completely_home_device_count)) %>% 
  left_join(select(social.distancing.daily, county_fips, date, device_count), by=c("county_fips", "date")) %>% 
  mutate(proportion = device_count_new / device_count,
         product = proportion*median_home_dwell_time) %>% 
  group_by(county_fips, date) %>% 
  summarize(median_home_dwell_time = mean(median_home_dwell_time, na.rm=TRUE))

social.distancing.daily.merge <- social.distancing.daily %>% 
  left_join(weighted.mean, by=c("county_fips", "date"))

# write_csv(social.distancing.daily.merge, "data/interim/mobility/safegraph/social-distancing/social_distancing_daily.csv.gz")




# Create WEEKLY social distancing measure at county level by aggregating across block groups
social.distancing.weekly <- social.distancing.daily.merge %>% 
  filter(date <= as.Date("2020-05-31", "%Y-%m-%d")) %>% 
  group_by(county_fips) %>% 
  mutate(weeknum = week(date),
         weekshift = dplyr::lead(weeknum, n=2L),
         weekshift = ifelse(is.na(weekshift), 22, weekshift)) %>% 
  group_by(county_fips, weekshift) %>% 
  mutate(daynum = row_number(),
         week_start_date = ifelse(daynum==1, date, NA)) %>% 
  fill(week_start_date)

social.distancing.weekly.summary <- social.distancing.weekly %>% 
  group_by(county_fips, week_start_date) %>% 
  summarize(device_count = sum(device_count, na.rm=TRUE),
            completely_home_device_count = sum(completely_home_device_count, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(county_fips, week_start_date)

weekly.weighted.mean <- social.distancing.weekly %>% 
  rename(device_count_new = device_count) %>% 
  select(-c(completely_home_device_count)) %>% 
  left_join(select(social.distancing.weekly.summary, county_fips, week_start_date, device_count), by=c("county_fips", "week_start_date")) %>% 
  mutate(proportion = device_count_new / device_count,
         product = proportion*median_home_dwell_time) %>% 
  group_by(county_fips, week_start_date) %>% 
  summarize(median_home_dwell_time = mean(median_home_dwell_time, na.rm=TRUE))

social.distancing.weekly.merge <- social.distancing.weekly.summary %>% 
  left_join(weekly.weighted.mean, by=c("county_fips", "week_start_date")) %>% 
  mutate(week_start_date = as_date(week_start_date)) %>% 
  filter(week_start_date != as.Date("2020-01-26", "%Y-%m-%d"))

# write_csv(social.distancing.weekly.merge, "data/interim/mobility/safegraph/social-distancing/social_distancing_weekly.csv.gz")






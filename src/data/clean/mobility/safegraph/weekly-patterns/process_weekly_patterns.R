###################################################
# process_weekly_patterns.R
# Script to replicate data build for county-level POI and visits
# As described in Allcott et al (2020) Appendix A.1.1, step 3
# Merge county name from POI data build
###################################################

rm(list = ls())
library(tidyverse)
library(stringr)
library(data.table)

# patterns <- data.table::fread("data/raw/mobility/safegraph/weekly-patterns/main-file/2020-01-27-weekly-patterns.csv.gz")
safegraph.counties <- data.table::fread("data/interim/geographic/safegraph/poi_counties.csv.gz")

# Create list of files to process
in.root.dir <- "data/raw/mobility/safegraph/weekly-patterns/main-file/"
out.root.dir <- "data/interim/mobility/safegraph/weekly-patterns/main-file/"
all.files <- list.files(root.dir, recursive=TRUE)
n <- length(all.files)

# Loop through each main file and process
for (path in all.files) {
  print(paste0("Processing file ", i, " of ", n))
  in.file.path <- paste0(in.root.dir, path)
  out.file.path <- paste0(out.root.dir, "processed_", path)
  print(paste0("    File path: ", in.file.path))
  df <- data.table::fread(in.file.path)
  
  print("    Producing weekly aggregates...")
  weekly <- process.weekly(df)
  write_csv(weekly[[1]], paste0(out.root.dir, "processed_weekly_", path))
  write_csv(weekly[[2]], paste0(out.root.dir, "processed_weekly_naics_", path))
  rm(weekly)
  rm(df)
  gc()
  
  print("    Producing daily aggregates...")
  daily <- process.daily(df)
  write_csv(daily[[1]], paste0(out.root.dir, "processed_daily_", path))
  write_csv(daily[[2]], paste0(out.root.dir, "processed_daily_naics_", path))
  rm(daily)
  rm(df)
  gc()
  print(paste0("Current memory size: ", memory.size()))
  i <- i + 1
}





### HELPER FUNCTIONS FOR PROCESSING RAW FILES
process.weekly <- function(patterns) {
  weekly <- short %>% 
    select(safegraph_place_id, date_range_start, visits_by_day,
           raw_visit_counts, raw_visitor_counts, poi_cbg, visitor_home_cbgs) %>% 
    mutate(date_week_start = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d")) %>% 
    left_join(select(safegraph.counties, safegraph_place_id, naics_code, state, county), by=c("safegraph_place_id"))
  
  visitor.cbg.weekly <- weekly %>% 
    mutate(visitor_cbgs = gsub("[{}]", "", gsub('\"', '', visitor_home_cbgs))) %>% 
    # Keep only count of visitors from same CBG as the POI
    separate(visitor_cbgs, into = c("vc1"), sep = ",") %>% 
    # Keep only the first CBG and count from that CBG. If there are visitors from the same CBG
    # as the POI in question, it is stored first
    separate(vc1, into = c("visitor_home_cbg", "count")) %>% 
    # Only keep counts if the visitor home CBG matches the POI CBG
    # Calculate visitors from same CBG as POI and different CBG
    mutate(count = ifelse(is.na(count), 0, as.integer(count)),
           raw_visitor_counts_same_cbg = ifelse(visitor_home_cbg==poi_cbg, count, 0),
           raw_visitor_counts_same_cbg = pmin(raw_visitor_counts, raw_visitor_counts_same_cbg),
           raw_visitor_counts_diff_cbg = raw_visitor_counts - raw_visitor_counts_same_cbg)
  
  weekly.agg <- visitor.cbg.weekly %>% 
    group_by(state, county, date_week_start) %>% 
    summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
              total_visitors = sum(raw_visitor_counts, na.rm=TRUE),
              total_visitors_same_cbg = sum(raw_visitor_counts_same_cbg, na.rm=TRUE),
              total_visitors_diff_cbg = sum(raw_visitor_counts_diff_cbg, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, date_week_start)
  
  weekly.naics <- visitor.cbg.weekly %>% 
    mutate(naics = str_sub(naics_code, start=1L, end=2L)) %>% 
    group_by(state, county, naics, date_week_start) %>% 
    summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
              total_visitors = sum(raw_visitor_counts, na.rm=TRUE),
              total_visitors_same_cbg = sum(raw_visitor_counts_same_cbg, na.rm=TRUE),
              total_visitors_diff_cbg = sum(raw_visitor_counts_diff_cbg, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, naics, date_week_start)
  
  return.list <- list(weekly.agg, weekly.naics)
  return.list
}


process.daily <- function(patterns) {
  
  daily <- patterns %>% 
    select(safegraph_place_id, date_range_start, visits_by_day) %>% 
    mutate(visits_by_day = str_sub(visits_by_day, start=2L, end=-2L)) %>% 
    separate(visits_by_day, into=c("d1", "d2", "d3", "d4", "d5", "d6", "d7"), sep=",") %>% 
    gather(key="day", value="visits", d1:d7) %>% 
    arrange(safegraph_place_id) %>% 
    group_by(safegraph_place_id) %>% 
    mutate(visits = as.integer(visits),
           count = row_number()) %>% 
    ungroup() %>% 
    mutate(week_start_date = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d"),
           date = week_start_date + count - 1) %>% 
    select(safegraph_place_id, date, visits) %>% 
    left_join(select(safegraph.counties, safegraph_place_id, naics_code, state, county), by=c("safegraph_place_id"))
  
  daily.agg <- daily %>% 
    group_by(state, county, date) %>% 
    summarize(total_visits = sum(visits, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, date)
  
  daily.naics <- daily %>% 
    mutate(naics = str_sub(naics_code, start=1L, end=2L)) %>% 
    group_by(state, county, naics, date) %>% 
    summarize(total_visits = sum(visits, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, naics, date)
  
  return.list <- list(daily.agg, daily.naics)
  return.list
}




### Read weekly patterns dfs back in and process
weekly.dfs <- list()
interim.root.dir <- "data/interim/mobility/safegraph/weekly-patterns/main-file/"
interim.files <- list.files(interim.root.dir, pattern="^processed_weekly_2020")
i <- 1
for (path in interim.files) {
  df <- read_csv(paste0(interim.root.dir, path))
  weekly.dfs[[i]] <- df
  i <- i + 1
}

countyfips <- read_csv("data/raw/geographic/counties/countyfips.csv.gz")

# Generate alternate dependent variable specifications
all.weekly <- bind_rows(weekly.dfs) %>% 
  mutate(log_total_visits = log(1 + total_visits),
         log_total_visitors = log(1 + total_visitors),
         share_visitors_same_cbg = total_visitors_same_cbg / total_visitors,
         share_visitors_diff_cbg = total_visitors_diff_cbg / total_visitors) %>% 
  select(state, county, date_week_start, 
         total_visits, log_total_visits,
         total_visitors, log_total_visitors,
         total_visitors_same_cbg, share_visitors_same_cbg,
         total_visitors_diff_cbg, share_visitors_diff_cbg) %>% 
  rename(week_start_date = date_week_start)

all.weekly <- all.weekly %>% 
  left_join(countyfips, by=c("state", "county")) %>% 
  select(state, county, county_fips, everything())

# write_csv(all.weekly, "data/interim/mobility/safegraph/weekly-patterns/processed_weekly_visits.csv.gz")

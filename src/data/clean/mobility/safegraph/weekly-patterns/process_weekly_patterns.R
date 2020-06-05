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

patterns <- data.table::fread("data/raw/mobility/safegraph/weekly-patterns/main-file/2020-01-27-weekly-patterns.csv.gz")
safegraph.counties <- data.table::fread("data/interim/geographic/safegraph/poi_counties.csv.gz")

# Create list of files to process
root.dir <- "data/raw/mobility/safegraph/weekly-patterns/main-file/"
all.files <- list.files(root.dir, recursive=TRUE)
n <- length(all.files)

# Loop through each main file and process
agg.dfs <- list()
naics.dfs <- list()
i <- 1
for (path in all.files) {
  print(paste0("Processing file ", i, " of ", n))
  file.path <- paste0(root.dir, path)
  print(paste0("    File path: ", file.path))
  df <- data.table::fread(file.path)
  
  print("    Merging to safegraph county...")
  weekly <- df %>% 
    select(safegraph_place_id, date_range_start, raw_visit_counts, raw_visitor_counts) %>% 
    mutate(date_week_start = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d")) %>% 
    left_join(select(safegraph.counties, safegraph_place_id, naics_code, state, county), by=c("safegraph_place_id"))
  
  print("    Producing weekly aggregates...")
  weekly.agg <- weekly %>% 
    group_by(state, county, date_week_start) %>% 
    summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
              total_visitors = sum(raw_visitor_counts, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, date_week_start)
  
  agg.dfs[[i]] <- weekly.agg
  
  print("    Producing weekly NAICS datasets.")
  weekly.naics <- weekly %>% 
    mutate(naics = str_sub(naics_code, start=1L, end=2L)) %>% 
    group_by(state, county, naics, date_week_start) %>% 
    summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
              total_visitors = sum(raw_visitor_counts, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, naics, date_week_start)
  
  naics.dfs[[i]] <- weekly.naics
  
  # Run some rm() and gc() here to free up memory
  
  i <- i + 1
}










short <- patterns %>% head(10000) %>% as_tibble()

weekly <- patterns %>% 
  select(safegraph_place_id, date_range_start, raw_visit_counts, raw_visitor_counts) %>% 
  mutate(date_week_start = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d")) %>% 
  left_join(select(safegraph.counties, safegraph_place_id, naics_code, state, county), by=c("safegraph_place_id"))

weekly.agg <- weekly %>% 
  group_by(state, county, date_week_start) %>% 
  summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
            total_visitors = sum(raw_visitor_counts, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(state, county, date_week_start)

weekly.naics <- weekly %>% 
  mutate(naics = str_sub(naics_code, start=1L, end=2L)) %>% 
  group_by(state, county, naics, date_week_start) %>% 
  summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
            total_visitors = sum(raw_visitor_counts, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(state, county, naics, date_week_start)


#daily <- short %>% 
#  select(safegraph_place_id, date_range_start, date_range_end, visits_by_day) %>% 
#  mutate(visits_by_day = str_sub(visits_by_day, start=2L, end=-2L)) %>% 
#  separate(visits_by_day, into=c("d1","d2","d3","d4","d5","d6","d7"), sep=",") %>% 
#  gather(key="day", value="visits", d1:d7) %>% 
#  arrange(safegraph_place_id) %>% 
#  group_by(safegraph_place_id) %>% 
#  mutate(visits = as.integer(visits),
#         count = row_number()) %>% 
#  ungroup() %>% 
#  mutate(week_start_date = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d"),
#         date = week_start_date + count - 1) %>% 
#  select(safegraph_place_id, week_start_date, date, visits)

#safegraph_ids <- daily %>% select(safegraph_place_id) %>% distinct() %>% .[[1]]
#counties.subset <- safegraph.counties %>% filter(safegraph_place_id %in% safegraph_ids)
#merged <- daily %>% 
#  left_join(counties.subset, by=c("safegraph_place_id"))

#weekly_aggregates <- merged %>% 
#  group_by(state, county, week_start_date) %>% 
# summarize(total_poi_visits = sum(visits, na.rm=TRUE))

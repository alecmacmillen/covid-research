###################################################
# get_poi_counties.R
# Script to get county of POIs in safegraph data
# As described in Allcott et al (2020) Appendix A.1.1, step 2
###################################################

library(sp)
library(maps)
library(maptools)
library(data.table)
library(tidyverse)

core1 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part1.csv.gz")
core2 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part2.csv.gz")
core3 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part3.csv.gz")
core4 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part4.csv.gz")
core5 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part5.csv.gz")

# Function for converting lat/lon to county
# Source: https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon per county
  counties <- maps::map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- maptools::map2SpatialPolygons(counties, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- sp::SpatialPoints(pointsDF, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- sp::over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

conversion.wrapper <- function(core.df) {
  # Wrapper function that calls latlong2county on a core POI dataframe,
  # find the county names, and rebinds with the safegraph ID and NAICS code
  core.sub <- core.df %>% select(safegraph_place_id, naics_code, longitude, latitude)
  lat.lon <- core.df %>% select(longitude, latitude)
  
  names <- tibble::enframe(latlong2county(lat.lon), value="result") %>% 
    select(-c(name)) %>% 
    separate(result, c("state", "county"), sep=",")
  
  paired <- cbind(core.sub, names)
  paired
}

core1.processed <- conversion.wrapper(core1)
core2.processed <- conversion.wrapper(core2)
core3.processed <- conversion.wrapper(core3)
core4.processed <- conversion.wrapper(core4)
core5.processed <- conversion.wrapper(core5)

core.final <- rbind(core1.processed, core2.processed, core3.processed, core4.processed, core5.processed)

# As a sanity check, about 4.37% of observations have null county values
nas <- core.final %>% filter(is.na(county)) %>% nrow()
nas / nrow(core.final)

#write_csv(core.final, "data/interim/geographic/safegraph/poi_counties.csv.gz")

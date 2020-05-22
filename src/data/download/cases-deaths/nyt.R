###################################################
# nyt.R
# Script for downloading NYT cases and deaths data
###################################################
library(tidyverse)

today <- str_sub(Sys.Date(), start = 6L)

counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")

county.name <- "nyt-us-counties.csv"
write.csv(counties, paste0("data/raw/cases-deaths/nyt/", county.name), row.names=FALSE)
prior.county.name <- paste0("nyt-us-counties-", today, ".csv")
write.csv(counties, paste0("data/raw/cases-deaths/nyt/prior/", prior.county.name), row.names=FALSE)

state.name <- "nyt-us-states.csv"
write.csv(states, paste0("data/raw/cases-deaths/nyt/", state.name), row.names=FALSE)
prior.state.name <- paste0("nyt-us-states-", today, ".csv")
write.csv(states, paste0("data/raw/cases-deaths/nyt/prior/", prior.state.name), row.names=FALSE)

us.name <- "nyt-us.csv"
write.csv(us, paste0("data/raw/cases-deaths/nyt/", us.name), row.names=FALSE)
prior.us.name <- paste0("nyt-us-", today, ".csv")
write.csv(us, paste0("data/raw/cases-deaths/nyt/prior/", prior.us.name), row.names=FALSE)

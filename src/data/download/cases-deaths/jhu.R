###################################################
# jhu.R
# Script for downloading JHU cases and deaths data
# JHU data repo: https://github.com/CSSEGISandData/COVID-19
# Subrepo: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
###################################################
library(tidyverse)

today <- str_sub(Sys.Date(), start = 6L)

confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

confirmed.name <- "jhu-timeseries-confirmed.csv"
write.csv(confirmed, paste0("data/raw/cases-deaths/jhu/", confirmed.name), row.names=FALSE)
prior.confirmed.name <- paste0("jhu-timeseries-confirmed-", today, ".csv")
write.csv(confirmed, paste0("data/raw/cases-deaths/jhu/prior/", prior.confirmed.name), row.names=FALSE)

deaths.name <- "jhu-timeseries-deaths.csv"
write.csv(deaths, paste0("data/raw/cases-deaths/jhu/", deaths.name), row.names=FALSE)
prior.deaths.name <- paste0("jhu-timeseries-deaths-", today, ".csv")
write.csv(deaths, paste0("data/raw/cases-deaths/jhu/prior/", prior.deaths.name), row.names=FALSE)

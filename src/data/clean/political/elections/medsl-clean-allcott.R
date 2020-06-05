###################################################
# medsl-allcott-clean.R
# Script for cleaning MIT elections data as per Allcott (2020)
# In preparation for replication analysis
###################################################

library(tidyverse)
library(elections)

data(presidential_precincts_2016)

county_totals <- presidential_precincts_2016 %>% 
  # It looks like some votes are double-counted, for example when people vote straight party tickets
  # in AL, IN or SC. Filter out to only obs where "US President" is explicitly listed
  filter(office == "US President") %>% 
  select(state, state_fips, state_postal, county_name, county_fips, votes) %>% 
  group_by(state, state_fips, state_postal, county_name, county_fips) %>% 
  summarize(totvotes = sum(votes, na.rm=TRUE)) %>% 
  ungroup() %>% 
  # There's some weird obs with too-long county fips codes, trim these
  mutate(county_fips = str_sub(county_fips, start=1L, end=5L))

# Highlight just trump vote totals
trump <- presidential_precincts_2016 %>% 
  select(state, state_postal, state_fips, county_name, county_fips,
         county_lat, county_long, candidate, party, candidate_normalized, votes) %>% 
  filter(candidate_normalized=="trump") %>% 
  group_by(state, state_fips, state_postal, county_name, county_fips, county_lat, county_long) %>% 
  summarize(trump_votes = sum(votes, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(county_fips = str_sub(county_fips, start=1L, end=5L))

# Merge trump totals with overall totals to calculate percentage; output
final <- county_totals %>% 
  left_join(trump, by=c("state", "state_postal", "state_fips",
                        "county_name", "county_fips")) %>% 
  mutate(trump_vote_share = trump_votes / totvotes,
         state_fips = str_pad(as.character(state_fips), 2, side = c("left"), "0")) %>% 
  select(-c(totvotes, trump_votes))

# Save dataset to intermediate cleaned section
write.csv(final, "data/interim/political/elections/medsl_allcott_clean.csv", row.names=FALSE)

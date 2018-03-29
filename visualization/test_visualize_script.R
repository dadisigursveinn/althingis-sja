library(tidyverse)
library(scales)
library(rvest)
library(lubridate)
library(readr)

votes <- read_csv("../data/votes.csv", 
                  col_types = cols(`time` = col_datetime(format = "%Y-%m-%d%.%H:%M:%S")))
members <- read_csv("../data/members_details.csv") %>% 
  filter(congress == 148) %>% 
  select(member_id, name, party_id, congress) %>% 
  distinct
parties <- read_csv("../data/parties.csv", na="") %>% 
  select(party_id, party_name=name, abr=abr_long)

votes_member <- merge(merge(members, votes, by = "member_id"), parties, by="party_id")


votes_member %>% 
  ggplot() +
  geom_jitter(mapping = aes(x=abr, y=vote), alpha=.5)

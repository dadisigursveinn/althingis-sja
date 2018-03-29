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

party_votes <- votes_member %>% 
  select(party_id, party_name, abr, vote_id, vote) %>% 
  group_by(party_id, abr, vote) %>% 
  summarise(vote_count=n())

sum_party_votes <- votes_member %>% 
  select(party_id, party_name, abr, vote_id) %>% 
  group_by(party_id) %>% 
  summarise(total_vote_count=n())

party_votes <- merge(party_votes, sum_party_votes, by="party_id") %>%
  mutate(percentage=(vote_count/total_vote_count))

party_votes %>% 
  ggplot() +
  geom_bar(mapping = aes(x=vote, y=percentage, fill=abr),
           stat="identity",
           position=position_dodge()) +
  theme_bw() +
  labs(
    title = "Types of votes in session 148 by party",
    y = "Percent",
    x = "Vote",
    fill = "Party"
  ) +
  scale_fill_manual(values = c('#EE4D9B',
                                 '#8EC83E',
                                 '#199094',
                                 '#522C7F',
                                 '#DA2128',
                                 '#00ADEF',
                                 '#F6A71D',
                                 '#488E41'))

votes_member %>% 
  ggplot() +
  geom_jitter(mapping = aes(x=abr, y=vote), alpha=.5) +
  labs(
    y = "Vote",
    x = "Party"
  ) 

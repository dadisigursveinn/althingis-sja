library(tidyverse)
library(scales)
library(rvest)
library(lubridate)
library(readr)

votes <- read_csv("../data/votes.csv")
members <- read_csv("../data/members_details.csv") %>% 
  filter(congress == 148) %>% 
  select(member_id, name, party_id, congress) %>% 
  distinct
parties <- read_csv("../data/parties.csv", na="") %>% 
  select(party_id, party_name=name, abr=abr_long)

votes_member <- merge(merge(members, votes, by = "member_id"), parties, by="party_id")

votes_member %>% 
  mutate(vote_time_hour=ifelse((vote_time_hour == 0), 24, vote_time_hour)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x=vote_time_hour, fill=vote),
           position="fill") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  scale_x_continuous(breaks=seq(7,24,by=1))+
  theme_bw()+
  labs(
    title = "How Members Vote Throughout the Day",
    subtitle="Data for session 148.",
    y = "",
    x = "Time of Day",
    fill = "Vote"
  ) +
  scale_fill_manual(values = c('#999999',
                               '#666666',
                               '#F0E442',
                               '#009E73',
                               '#D55E00'))


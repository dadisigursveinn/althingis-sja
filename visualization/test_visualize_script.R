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
parties <- read_csv("../data/parties.csv",skype
                    na="") %>% 
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

party_votes_details <- merge(party_votes, sum_party_votes, by="party_id") %>%
  mutate(percentage=(vote_count/total_vote_count)) %>% 
  mutate(did_vote=ifelse((vote == "já" | vote == "nei"), TRUE, FALSE))

party_votes_participation <- party_votes_details %>% 
  group_by(party_id, abr, did_vote) %>% 
  summarise(percentage=sum(percentage))

party_votes_participation <- arrange(party_votes_participation)

party_votes_participation %>% 
  ggplot() +
  geom_bar(mapping = aes(x=abr, y=percentage, fill=did_vote),
           stat="identity") +
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,vjust=0.5)
  ) +
  labs(
    title = "How Often Did Members Participate in Votes?",
    subtitle="Data for session 148. Voting yes or no considered participation,\nnot voting or being absent count for no participation.",
    y = "Participation",
    x = "Party",
    fill = "Did Participate"
  ) +
  scale_fill_manual(values = c('#D55E00',
                               '#009E73'))

party_votes_details %>% 
  ggplot() +
  geom_bar(mapping = aes(x=vote, y=percentage, fill=abr),
           stat="identity",
           position=position_dodge()) +
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,vjust=0.5)
  ) +
  labs(
    title = "Types of Votes In Session 148 by Party",
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
+
  scale_color_manual(values = c('#EE4D9B',
                                '#8EC83E',
                                '#199094',
                                '#522C7F',
                                '#DA2128',
                                '#00ADEF',
                                '#F6A71D',
                                '#488E41'))
rm(list = ls())
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
parties <- read_csv("../data/parties.csv",
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
  mutate(did_vote=ifelse((vote == "já" | vote == "nei" | vote == "greiðir ekki atkvæði"), TRUE, FALSE))

party_votes_participation <- party_votes_details %>% 
  group_by(party_id, abr, did_vote) %>% 
  summarise(percentage=sum(percentage))

party_votes_participation <- arrange(party_votes_participation)

party_votes_details %>%
  filter(vote == "já" | vote == "nei" | vote == "greiðir ekki atkvæði") %>%
  group_by(party_id, abr) %>% 
  summarise(percentage=sum(percentage)) %>% 
  ggplot(aes(fct_reorder(abr, percentage, .desc = FALSE), percentage)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,vjust=0.5)
  ) +
  scale_y_continuous(breaks = seq(0,1,by=.1),
                     labels = scales::percent(seq(0,1,by=.1)),
                     minor_breaks = 0) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "How Often Do Parties Participate in Votes?",
    subtitle="Data for session 148.",
    y = "",
    x = "",
    fill = "Did Participate"
  ) +
  coord_flip() +
  scale_colour_manual(values = c('#D55E00',
                               '#009E73'))

party_votes_participation2 <-party_votes_participation %>%
  rename(parti_percentage=percentage, par_did_vote=did_vote) %>%
  ungroup() %>% 
  select(parti_percentage,par_did_vote, party_id)
party_votes_details_w_participation <- merge(party_votes_details, party_votes_participation2, by="party_id") %>% 
  filter(par_did_vote)

party_votes_details_w_participation %>%
  ggplot(aes(fct_reorder(abr, parti_percentage, .desc = TRUE), percentage, fill=vote)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,vjust=0.5)
  ) +
  scale_y_continuous(breaks = seq(0,1,by=.1),
                     labels = scales::percent(seq(0,1,by=.1)),
                     minor_breaks = 0) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "How Parties Vote",
    subtitle="Data for session 148.",
    y = "",
    x = "",
    fill = "Vote"
  ) +
  scale_fill_manual(values = c('#AAAAAA',
                                 '#555555',
                                 '#F0E442',
                                 '#009E73',
                                 '#D55E00'))

party_votes_details %>%
  filter(vote == "greiðir ekki atkvæði") %>%
  group_by(party_id, abr) %>% 
  summarise(percentage=sum(percentage)) %>% 
  ggplot(aes(fct_reorder(abr, percentage, .desc = FALSE), percentage)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,vjust=0.5)
  ) +
  scale_y_continuous(breaks = seq(0,1,by=.05),
                     labels = scales::percent(seq(0,1,by=.05)),
                     minor_breaks = 0) +
  coord_flip(ylim = c(0, .25)) +
  labs(
    title = "Party neutrality frequence",
    subtitle="Data for session 148.",
    y = "",
    x = "",
    fill = "Did Participate"
  ) +
  scale_colour_manual(values = c('#D55E00',
                                 '#009E73'))

party_votes_details %>% 
  ggplot() +
  geom_bar(mapping = aes(fct_reorder(vote,percentage), percentage, fill=vote),
           stat="identity",
           position=position_dodge()) +
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,vjust=0.5)
  ) +
  scale_y_continuous(breaks = seq(0,1,by=.1),
                    labels = scales::percent(seq(0,1,by=.1)),
                    minor_breaks = 0,
                    expand = c(0,0)) +
  coord_cartesian(ylim = c(0, .8)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())+
  labs(
    title = "Types of Votes In Session 148 by Party",
    y = "",
    x = "",
    fill = "Vote"
  ) +
  scale_fill_manual(values = c('#AAAAAA',
                               '#555555',
                               '#F0E442',
                               '#009E73',
                               '#D55E00')) +
  facet_grid(. ~ abr)

votes_member %>% 
  ggplot() +
  geom_jitter(mapping = aes(x=abr, y=vote), alpha=.5) +
  labs(
    y = "",
    x = "")

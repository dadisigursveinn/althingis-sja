library(tidyverse)
library(scales)
library(lubridate)
library(readr)
library(rvest)
library(data.table)

parties <- read_csv("../data/parties.csv") %>% 
  select(party_id, party_name=name, party_abr_short=abr_short, party_abr_long=abr_long)
votes_combined <- list()
theListOfAll <- list(121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148)
#theListOfAll <- list(146, 147, 148);
for (i in theListOfAll) {
  filename <- sprintf("../data/votes/votes_%i.csv", i)
  votes_combined <- rbind(votes_combined, read_csv(filename))
}
votes <- votes_combined %>% 
  filter(vote != "f: óþekktur kóði")

members <- read_csv("../data/members_details.csv") %>%
  select(member_id, party_id, congress, memeber_name=name) %>% distinct

votes_and_members <- merge(members, votes, by = c("member_id", "congress"))
all_votes <- merge(parties, votes_and_members)
all_votes <- filter(all_votes, !is.na(vote_id))

# write all_votes to csv
write.csv(all_votes, file="../data/megaset/votes_121-148_detail.csv", fileEncoding="UTF-8", row.names=FALSE)
rm(list=ls())

members <- read_csv("../data/megaset/votes_121-148_detail.csv")

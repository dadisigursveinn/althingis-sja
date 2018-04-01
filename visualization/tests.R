library(tidyverse)
library(scales)
library(rvest)
library(lubridate)
library(readr)

# This formula 'voteDiscord' measures the level of discord on a scale of 0 to 1.
# It does not take into account abstaining votes.

# Reasons why including 'abstinence' would complicate measures of discord:

# 1. Congressman disagrees with his/her party but does not wish to go against it. (cordinal)
# 2. Congressman does not want to take part because of possible connections to affected parties (neither discordinal nor cordinal).
# 3. Congressman considers himself/herself too uninformed to make a choice (discordinal).

# Maximum discord would be a total split vote within the party
# Example: voteDiscord(50.0, 50.0, 100.0) -> Discord of 1

# Minimum discord would be total agreement within the party
# Example: voteDiscord(100.0, 0.0, 100.0) -> Discord of 0

# Medium discord would be a fourth of the party disagrees
# Example: voteDiscord(25.0, 75.0, 100.0) -> Discord of 0.5

# def voteDiscord(yes, no, numberOfVotes)
# return (numberOfVotes - (yes - no).abs) / numberOfVotes
# end

votes <- read_csv("../data/votes.csv") %>% 
  select(member_id, vote_id, vote, congress )

members <- read_csv("../data/members_details.csv") %>%
  select(member_id, party_id, congress) %>% distinct

votesByParty <- merge(members, votes, by = c("member_id", "congress"))

votesByParty %>% 
  filter(vote_id == 54952)
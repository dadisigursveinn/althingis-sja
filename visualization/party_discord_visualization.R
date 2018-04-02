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

votesPerIssue <- (merge(members, votes, by = c("member_id", "congress"))  %>%
  select(party_id, vote_id, vote))

# *party_id vote_id vote vote_count*
party_vote_summary <- summarise(group_by(votesPerIssue, party_id, vote_id, vote), vote_count = n())

party_vote_summary

# some_voting_session <- filter(party_vote_summary, vote_id == 54788)
# votes_of_some_party <- filter(some_voting_session, party_id == 43, vote %in% c("já", "nei"))
# votes_of_some_party

# Takes votes of some party in a particular voting session and returns discord for that session.
# Input: *party_id vote_id vote vote_count* 
# Output: Discord value from 0.0 to 1.0
# Example: partyDiscord(votes_of_some_party) # => 0.44
partyDiscord <- function(votes_of_some_party) {
  number_of_yes <- 0;
  number_of_no <- 0;
  
  for(i in 1:nrow(votes_of_some_party)) {
    ifelse (votes_of_some_party[i, "vote"] == "já", number_of_yes <- votes_of_some_party[i, "vote_count"], number_of_no <- votes_of_some_party[i, "vote_count"]);    
  }
  total_votes <- number_of_yes + number_of_no;
  discord <- abs(total_votes - abs(number_of_yes - number_of_no)) / total_votes;
  return(discord);
}
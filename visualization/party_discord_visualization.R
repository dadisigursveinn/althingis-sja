library(tidyverse)
library(scales)
library(rvest)
library(lubridate)
library(readr)
library(data.table)
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

votes <- read_csv("../data/votes.csv") %>% 
  select(member_id, vote_id, vote, congress )

members <- read_csv("../data/members_details.csv") %>%
  select(member_id, party_id, congress) %>% distinct

votesPerIssue <- (merge(members, votes, by = c("member_id", "congress"))  %>%
  select(party_id, vote_id, vote))

summarizePartyVotes <- function(votesPerIssue) {
  summary_of_how_parties_voted <- summarise(group_by(votesPerIssue, party_id, vote_id, vote), vote_count = n())
  summary_of_how_parties_voted <- filter(summary_of_how_parties_voted, vote %in% c("já", "nei"))
  data <- filter(summary_of_how_parties_voted, party_id == 43)
  data$ja <- ifelse(data$vote == "já", data$vote_count, 0)
  data$nei <- ifelse(data$vote == "nei", data$vote_count, 0)
  data <- select(data, "party_id", "vote_id", "ja", "nei")
  DT <- data.table(data)
  return(DT[, lapply(.SD, sum), by=list(vote_id)]);
}

summarizePartyVotes(votesPerIssue)
#partyDiscord(hello)

# Takes votes of some party in a particular voting session and returns discord for that session.
# Input: <table> *party_id vote_id vote vote_count* 
# Output: <float> Discord value from 0.0 to 1.0
# Example: partyDiscord(party_votes_for_a_session) # => 0.44
partyDiscord <- function(party_votes_for_a_session) {
  number_of_yes <- 0;
  number_of_no <- 0;
  
  for(i in 1:nrow(party_votes_for_a_session)) {
    ifelse (party_votes_for_a_session[i, "vote"] == "já", number_of_yes <- party_votes_for_a_session[i, "vote_count"], number_of_no <- party_votes_for_a_session[i, "vote_count"]);    
  }
  total_votes <- number_of_yes + number_of_no;
  discord <- abs(total_votes - abs(number_of_yes - number_of_no)) / total_votes;
  return(discord);
}

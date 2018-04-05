rm(list = ls())
library(tidyverse)
library(scales)
library(lubridate)
library(readr)
library(rvest)
library(data.table)

#
# INIT
#
#
parties <- read_csv("../data/parties.csv");
votes_combined <- list()
#theListOfAll <- list(121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148)
theListOfAll <- list(146, 147, 148);
for (i in theListOfAll) {
  filename <- sprintf("../data/votes/votes_%i.csv", i)
  votes_combined <- rbind(votes_combined, read_csv(filename))
}
votes <- votes_combined
members <- read_csv("../data/members_details.csv") %>%
  select(member_id, party_id, congress) %>% distinct;

summarizePartyVotes <- function(votesPerIssue) {
  data <- summarise(group_by(votesPerIssue, party_id, vote_id, vote), vote_count = n());
  data <- filter(data, vote %in% c("já", "nei", "fjarverandi"));
  #data <- filter(summary_of_how_parties_voted, party_id == 43);
  data$ja <- ifelse(data$vote == "já", data$vote_count, 0);
  data$nei <- ifelse(data$vote == "nei", data$vote_count, 0);
  data$fjarverandi <- ifelse(data$vote == "fjarverandi", data$vote_count, 0);
  data <- select(data, "party_id", "vote_id", "ja", "nei", "fjarverandi");
  DT <- data.table(data);
  return(DT[,list(ja=sum(ja),nei=sum(nei),fjarverandi=sum(fjarverandi)),by=list(party_id, vote_id)]);
}

votesPerIssue <- (merge(members, votes, by = c("member_id", "congress"))  %>%
                    select(party_id, vote_id, vote))

party_votes_summary <- summarizePartyVotes(votesPerIssue)

#
# KLOFNINGUR FLOKKS / YES, NO SPLIT
#
#
0 == 1
calculate2dHarmonyScore <- function(yes, no) {
  total_votes = yes + no;
  if(isTRUE(all.equal(total_votes, 0))) {
    return(1)
  }
  disHarmony = total_votes / 2; # When N = 6, disH is point (3,3), so then we just use 3
  harmony <- sqrt((yes - disHarmony)^2 + (no - disHarmony)^2 ) / total_votes;
  maxHarmony <- (sqrt((total_votes - disHarmony)^2 +(0 - disHarmony)^2 ) / total_votes);
  harmony <- harmony / maxHarmony
  return(harmony);
}

calculate3dHarmonyScore <- function(yes, no, abstains) {
  total_votes <- yes + no + abstains;
  disHarmony = total_votes / 3; # When N = 6, disH is point (2,2,2), so then we just use 2
  harmony <- (sqrt((yes - disHarmony)^2 + (no - disHarmony)^2 + (abstains - disHarmony)^2 ) / total_votes);
  return(harmony);
}

#calculate2dHarmonyScore <- function(yes, no) {
#  total_votes <- yes + no;
#  discord <- abs(total_votes - abs(yes - no)) / total_votes;
#  return(discord);
#}

party_votes_summary$party_discord <- calculate2dHarmonyScore(party_votes_summary$ja, party_votes_summary$nei);

DT <- data.table(party_votes_summary);
average_party_discord <- merge(DT[,list(party_discord=mean(party_discord)),by=list(party_id)], parties) %>% select("Flokkur" = "abr_long", "Klofningur" = "party_discord")

#counts <- table(average_party_discord$Flokkur)
discord_values <- average_party_discord$Klofningur
discord_values <- discord_values * 10 # 100 to make it a percent for nicer presentation

party_names <- average_party_discord$Flokkur # TODO: Correct colours
barplot(discord_values, col = c("darkblue", "darkolivegreen3", "blue", "red", "black", "yellow", "orange", "yellow", "darkgreen", rainbow(20)), main="Klofningur innan flokks (1996-2018)", horiz=TRUE,
        cex.names=0.8, names.arg=party_names, las=1)
        mtext(side=1, text="%", line=3, las=0)
        
#
# SUNDURLEITNI FLOKKS / PARTY DISHARMONY
#
#
      
party_votes_summary$party_discord <- calculate2dHarmonyScore(party_votes_summary$ja, party_votes_summary$nei);
DT <- data.table(party_votes_summary);
average_party_discord <- merge(DT[,list(party_discord=mean(party_discord)),by=list(party_id)], parties) %>% select("Flokkur" = "abr_long", "Klofningur" = "party_discord")        
        
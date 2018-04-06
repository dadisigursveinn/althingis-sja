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
theListOfAll <- list(121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148)
#theListOfAll <- list(146, 147, 148);
for (i in theListOfAll) {
  filename <- sprintf("../data/votes/votes_%i.csv", i)
  votes_combined <- rbind(votes_combined, read_csv(filename))
}
votes <- votes_combined
members <- read_csv("../data/members_details.csv") %>%
  select(member_id, party_id, congress) %>% distinct;
rm(list = c("filename", "i", "votes_combined", "theListOfAll"))

summarizePartyVotes2d <- function(votesPerIssue) {
  data <- summarise(group_by(votesPerIssue, party_id, vote_id, vote_time_year, vote), vote_count = n());
  data <- filter(data, vote %in% c("já", "nei"));
  #data <- filter(summary_of_how_parties_voted, party_id == 43);
  data$ja <- ifelse(data$vote == "já", data$vote_count, 0);
  data$nei <- ifelse(data$vote == "nei", data$vote_count, 0);
  data <- select(data, "party_id", "vote_id", "vote_time_year", "ja", "nei");
  DT <- data.table(data);
  return(DT[,list(ja=sum(ja),nei=sum(nei)),by=list(party_id, vote_time_year, vote_id)]);
}

summarizePartyVotes3d <- function(votesPerIssue) {
  data <- summarise(group_by(votesPerIssue, party_id, vote_id, vote_time_year, vote), vote_count = n());
  data <- filter(data, vote %in% c("já", "nei", "greiðir ekki atkvæði"));
  #data <- filter(summary_of_how_parties_voted, party_id == 43);
  data$ja <- ifelse(data$vote == "já", data$vote_count, 0);
  data$nei <- ifelse(data$vote == "nei", data$vote_count, 0);
  data$greidir_ekki_atkvaedi <- ifelse(data$vote == "greiðir ekki atkvæði", data$vote_count, 0);
  data <- select(data, "party_id", "vote_id", "vote_time_year", "ja", "nei", "greidir_ekki_atkvaedi");
  DT <- data.table(data);
  return(DT[,list(ja=sum(ja),nei=sum(nei),greidir_ekki_atkvaedi=sum(greidir_ekki_atkvaedi)),by=list(party_id, vote_time_year, vote_id)]);
}

summarizePartyVotes4d <- function(votesPerIssue) {
  data <- summarise(group_by(votesPerIssue, party_id, vote_id, vote_time_year, vote), vote_count = n());
  data <- filter(data, vote %in% c("já", "nei", "greiðir ekki atkvæði", "fjarverandi"));
  #data <- filter(summary_of_how_parties_voted, party_id == 43);
  data$ja <- ifelse(data$vote == "já", data$vote_count, 0);
  data$nei <- ifelse(data$vote == "nei", data$vote_count, 0);
  data$greidir_ekki_atkvaedi <- ifelse(data$vote == "greiðir ekki atkvæði", data$vote_count, 0);
  data$fjarverandi <- ifelse(data$vote == "fjarverandi", data$vote_count, 0);
  data <- select(data, "party_id", "vote_id", "vote_time_year", "ja", "nei", "greidir_ekki_atkvaedi", "fjarverandi");
  DT <- data.table(data);
  return(DT[,list(ja=sum(ja),nei=sum(nei),fjarverandi=sum(fjarverandi),greidir_ekki_atkvaedi=sum(greidir_ekki_atkvaedi)),by=list(party_id, vote_id, vote_time_year)]);
}

votesPerIssue <- (merge(members, votes, by = c("member_id", "congress"))  %>%
                    select(party_id, vote_id, vote, vote_time_year))

party_votes_2d_summary <- summarizePartyVotes2d(votesPerIssue)
party_votes_3d_summary <- summarizePartyVotes3d(votesPerIssue)
party_votes_4d_summary <- summarizePartyVotes4d(votesPerIssue)


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
  return(harmony)
}

calculate3dHarmonyScore <- function(yes, no, abstains) {
  total_votes <- yes + no + abstains;
  if(isTRUE(all.equal(total_votes, 0))) {
    return(1)
  }
  disHarmony = total_votes / 3; # When N = 8, disH is point (2,2,2,2), so then we just use 2
  maxHarmony <- (sqrt((total_votes - disHarmony)^2 +(0 - disHarmony)^2 +(0 - disHarmony)^2 ) / total_votes);
  harmony <- (sqrt((yes - disHarmony)^2 + (no - disHarmony)^2 + (abstains - disHarmony)^2) / total_votes);
  harmony <- harmony / maxHarmony
  return(harmony)
}

calculate4dHarmonyScore <- function(yes, no, abstains, absent) {
  total_votes <- yes + no + abstains + absent;
  if(isTRUE(all.equal(total_votes, 0))) {
    return(1)
  }
  disHarmony = total_votes / 4; # When N = 8, disH is point (2,2,2,2), so then we just use 2
  maxHarmony <- (sqrt((total_votes - disHarmony)^2 +(0 - disHarmony)^2 +(0 - disHarmony)^2 +(0 - disHarmony)^2 ) / total_votes);
  harmony <- (sqrt((yes - disHarmony)^2 + (no - disHarmony)^2 + (abstains - disHarmony)^2 + (absent - disHarmony)^2) / total_votes);
  harmony <- harmony / maxHarmony
  return(harmony)
}

#calculate2dHarmonyScore <- function(yes, no) {
#  total_votes <- yes + no;
#  discord <- abs(total_votes - abs(yes - no)) / total_votes;
#  return(discord);
#}

party_votes_2d_summary$harmony <- calculate2dHarmonyScore(party_votes_2d_summary$ja, party_votes_2d_summary$nei);

DT <- data.table(party_votes_2d_summary);
average_harmony <- merge(DT[,list(harmony=mean(harmony)),by=list(party_id, vote_time_year)], parties) %>% select("Flokkur" = "abr_long", "year" = "vote_time_year", "Klofningur" = "harmony")

#counts <- table(average_harmony$Flokkur)
harmony_values <- average_harmony$Klofningur
harmony_values <- harmony_values * 10 # 100 to make it a percent for nicer presentation

party_names <- average_harmony$Flokkur # TODO: Correct colours
barplot(harmony_values, col = c("darkblue", "darkolivegreen3", "blue", "red", "black", "yellow", "orange", "yellow", "darkgreen", rainbow(20)), main="Klofningur innan flokks (1996-2018)", horiz=TRUE,
        cex.names=0.8, names.arg=party_names, las=1)
mtext(side=1, text="%", line=3, las=0)

votes_each_party <-merge(votesPerIssue, parties)
votesPerIssue %>% 
  merge(votesPerIssue, parties, by="party_id")
ggplot(votes_each_party, aes(x=name)) +
  geom_bar() +
  coord_flip()

#
# Same vote score by year
#

average_harmony %>% 
  filter(Flokkur != "Utan þfl.") %>%
  ggplot(aes(year, Klofningur, colour=Flokkur)) +
  geom_line() +
  geom_point() +
  #geom_errorbar(aes(ymin=Klofningur, ymax=Klofningur)) +
  #geom_point(stat="identity", size=3) +
  #geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0,1,by=.01),
                     labels = scales::percent(seq(0,1,by=.01)),
                     minor_breaks = 0,
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1996, 2018, 2)) +
  coord_cartesian(ylim = c(.94, 1)) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(
    title = "Uniformity score, based on yes and no votes",
    subtitle="Data for sessions 121 - 148",
    y = "",
    x = "",
    colour="Party"
  ) +
  scale_colour_manual(values = c('#FF0000',
                                 '#951681',
                                 '#F58B3F',
                                 '#EE4D9B',
                                 '#8EC83E',
                                 '#00BFFF',
                                 '#969696',
                                 '#969696',
                                 '#199094',
                                 '#E1E014',
                                 '#522C7F',
                                 '#DA2128',
                                 '#00ADEF',
                                 '#969696',
                                 '#F6A71D',
                                 '#488E41'))

#
# Same vote score for parties that are no longer in Alþingi
#
average_harmony %>% 
  filter(Flokkur != "Utan þfl.") %>%
  filter(Flokkur != "F. fólksins" &
         Flokkur != "Framsfl." &
         Flokkur != "Miðfl." &
         Flokkur != "Píratar" &
         Flokkur != "Sjálfstfl." &
         Flokkur != "Viðreisn" &
         Flokkur != "Samf." &
         Flokkur != "Vinstri-gr." ) %>%
  ggplot(aes(year, Klofningur, colour=Flokkur)) +
  geom_line() +
  geom_point() +
  #geom_errorbar(aes(ymin=Klofningur, ymax=Klofningur)) +
  #geom_point(stat="identity", size=3) +
  #geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0,1,by=.01),
                     labels = scales::percent(seq(0,1,by=.01)),
                     minor_breaks = 0,
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1996, 2018, 2)) +
  coord_cartesian(ylim = c(.94, 1)) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(
    title = "Uniformity score, based on yes and no votes, for parties that are no longer in Alþingi",
    subtitle="Data for sessions 121 - 147",
    y = "",
    x = "",
    colour="Party"
  ) +
  scale_colour_manual(values = c('#FF0000',
                                 '#951681',
                                 '#F58B3F',
                                 '#00BFFF',
                                 '#969696',
                                 '#969696',
                                 '#E1E014',
                                 '#969696',
                                 '#000000',
                                 '#F6A71D',
                                 '#488E41'))


######
# Calculations for 3D
######
#
# Calculations for 3d
#

party_votes_3d_summary$harmony <- calculate3dHarmonyScore(party_votes_3d_summary$ja, party_votes_3d_summary$nei, party_votes_3d_summary$greidir_ekki_atkvaedi);
DT <- data.table(party_votes_3d_summary);
average_harmony <- merge(DT[,list(harmony=mean(harmony)),by=list(party_id, vote_time_year)], parties) %>% select("Flokkur" = "abr_long", "year"="vote_time_year", "Klofningur" = "harmony")

average_harmony %>% 
  filter(Flokkur != "Utan þfl.") %>% 
  ggplot(aes(year, Klofningur, colour=Flokkur)) +
  geom_point() +
  geom_line() +
  #geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0,1,by=.01),
                     labels = scales::percent(seq(0,1,by=.01)),
                     minor_breaks = 0,
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1996, 2018, 2)) +
  coord_cartesian(ylim = c(.81, 1)) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(
    title = "Uniformity Score, based on yes, no and neutral votes",
    subtitle="Data for sessions 121 - 148",
    y = "",
    x = "",
    colour="Party"
  ) +
  scale_colour_manual(values = c('#FF0000',
                                 '#951681',
                                 '#F58B3F',
                                 '#EE4D9B',
                                 '#8EC83E',
                                 '#00BFFF',
                                 '#969696',
                                 '#969696',
                                 '#199094',
                                 '#E1E014',
                                 '#522C7F',
                                 '#DA2128',
                                 '#00ADEF',
                                 '#969696',
                                 '#F6A71D',
                                 '#488E41'))

average_harmony %>% 
  filter(Flokkur != "Utan þfl.") %>% 
  filter(Flokkur != "F. fólksins" &
           Flokkur != "Framsfl." &
           Flokkur != "Miðfl." &
           Flokkur != "Píratar" &
           Flokkur != "Sjálfstfl." &
           Flokkur != "Viðreisn" &
           Flokkur != "Samf." &
           Flokkur != "Vinstri-gr." ) %>%
  ggplot(aes(year, Klofningur, colour=Flokkur)) +
  geom_point() +
  geom_line() +
  #geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0,1,by=.01),
                     labels = scales::percent(seq(0,1,by=.01)),
                     minor_breaks = 0,
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1996, 2018, 2)) +
  coord_cartesian(ylim = c(.81, 1)) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(
    title = "Uniformity Score, based on yes, no and neutral votes, for parties that are no longer in Alþingi",
    subtitle="Data for sessions 121 - 147",
    y = "",
    x = "",
    colour="Party"
  ) +
  scale_colour_manual(values = c('#FF0000',
                                 '#951681',
                                 '#F58B3F',
                                 '#00BFFF',
                                 '#969696',
                                 '#969696',
                                 '#E1E014',
                                 '#969696',
                                 '#000000',
                                 '#F6A71D',
                                 '#488E41'))




#
# SUNDURLEITNI FLOKKS / PARTY DISHARMONY
#
#

party_votes_4d_summary$harmony <- calculate4dHarmonyScore(party_votes_4d_summary$ja, party_votes_4d_summary$nei, party_votes_4d_summary$greidir_ekki_atkvaedi, party_votes_4d_summary$fjarverandi);
DT <- data.table(party_votes_4d_summary);
average_harmony <- merge(DT[,list(harmony=mean(harmony)),by=list(party_id, vote_time_year)], parties) %>% select("Flokkur" = "abr_long", "year"="vote_time_year", "Klofningur" = "harmony")

harmony_values <- average_harmony$Klofningur
harmony_values <- harmony_values * 10 # 100 to make it a percent for nicer presentation

party_names <- average_harmony$Flokkur # TODO: Correct colours
barplot(harmony_values, col = c("darkblue", "darkolivegreen3", "blue", "red", "black", "yellow", "orange", "yellow", "darkgreen", rainbow(20)), main="Einsleitni flokka í atkvæðagreiðslum (1996-2018)", horiz=TRUE,
        cex.names=1.0, names.arg=party_names, las=1)
mtext(side=1, text="Einsleitnieinkunn (Uniformity Score)", line=3, las=0)

average_harmony %>% 
  filter(Flokkur != "Utan þfl.") %>% 
  ggplot(aes(year, Klofningur, colour=Flokkur)) +
  geom_point() +
  geom_line() +
  #geom_errorbar(aes(ymin=Klofningur, ymax=Klofningur)) +
  #geom_point(stat="identity", size=3) +
  #geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0,1,by=.05),
                     labels = scales::percent(seq(0,1,by=.05)),
                     minor_breaks = 0,
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1996, 2018, 2)) +
  coord_cartesian(ylim = c(.6, 1)) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(
    title = "Uniformity Score, based on yes, no and neutral votes and absence",
    subtitle="Data for sessions 121 - 148",
    y = "",
    x = "",
    colour="Party"
  ) +
  scale_colour_manual(values = c('#FF0000',
                                 '#951681',
                                 '#F58B3F',
                                 '#EE4D9B',
                                 '#8EC83E',
                                 '#00BFFF',
                                 '#969696',
                                 '#969696',
                                 '#199094',
                                 '#E1E014',
                                 '#522C7F',
                                 '#DA2128',
                                 '#00ADEF',
                                 '#969696',
                                 '#F6A71D',
                                 '#488E41'))

average_harmony %>% 
  filter(Flokkur != "Utan þfl.") %>% 
  filter(Flokkur != "F. fólksins" &
           Flokkur != "Framsfl." &
           Flokkur != "Miðfl." &
           Flokkur != "Píratar" &
           Flokkur != "Sjálfstfl." &
           Flokkur != "Viðreisn" &
           Flokkur != "Samf." &
           Flokkur != "Vinstri-gr." ) %>%
  ggplot(aes(year, Klofningur, colour=Flokkur)) +
  geom_point() +
  geom_line() +
  #geom_errorbar(aes(ymin=Klofningur, ymax=Klofningur)) +
  #geom_point(stat="identity", size=3) +
  #geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0,1,by=.05),
                     labels = scales::percent(seq(0,1,by=.05)),
                     minor_breaks = 0,
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1996, 2018, 2)) +
  coord_cartesian(ylim = c(.65, 1)) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(
    title = "Uniformity Score, based on yes, no and neutral votes and absence,\n for parties that are no longer in Althingi",
    subtitle="Data for sessions 121 - 147",
    y = "",
    x = "",
    colour="Party"
  ) +
  scale_colour_manual(values = c('#FF0000',
                                 '#951681',
                                 '#F58B3F',
                                 '#00BFFF',
                                 '#969696',
                                 '#969696',
                                 '#E1E014',
                                 '#969696',
                                 '#000000',
                                 '#F6A71D',
                                 '#488E41'))

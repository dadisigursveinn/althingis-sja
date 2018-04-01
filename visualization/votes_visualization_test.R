library(tidyverse)
library(scales)
library(rvest)
library(lubridate)
library(readr)

votes121 <- read_csv("../data/votes/votes_121.csv")
votes122 <- read_csv("../data/votes/votes_122.csv")
votes123 <- read_csv("../data/votes/votes_123.csv")
votes124 <- read_csv("../data/votes/votes_124.csv")
votes125 <- read_csv("../data/votes/votes_125.csv")
votes126 <- read_csv("../data/votes/votes_126.csv")
votes127 <- read_csv("../data/votes/votes_127.csv")
votes128 <- read_csv("../data/votes/votes_128.csv")
votes129 <- read_csv("../data/votes/votes_129.csv")
votes130 <- read_csv("../data/votes/votes_130.csv")
votes131 <- read_csv("../data/votes/votes_131.csv")
votes132 <- read_csv("../data/votes/votes_132.csv")
votes133 <- read_csv("../data/votes/votes_133.csv")
votes134 <- read_csv("../data/votes/votes_134.csv")
votes135 <- read_csv("../data/votes/votes_135.csv")
votes136 <- read_csv("../data/votes/votes_136.csv")
votes137 <- read_csv("../data/votes/votes_137.csv")
votes138 <- read_csv("../data/votes/votes_138.csv")
votes139 <- read_csv("../data/votes/votes_139.csv")
votes140 <- read_csv("../data/votes/votes_140.csv")
votes141 <- read_csv("../data/votes/votes_141.csv")
votes142 <- read_csv("../data/votes/votes_142.csv")
votes143 <- read_csv("../data/votes/votes_143.csv")
votes144 <- read_csv("../data/votes/votes_144.csv")
votes145 <- read_csv("../data/votes/votes_145.csv")
votes146 <- read_csv("../data/votes/votes_146.csv")
votes147 <- read_csv("../data/votes/votes_147.csv")
votes148 <- read_csv("../data/votes/votes_148.csv")

all_votes <- Reduce(function(x,y) merge(x,y, all=TRUE), list(votes121,
                                                             votes122,
                                                             votes123,
                                                             votes124,
                                                             votes125,
                                                             votes126,
                                                             votes127,
                                                             votes128,
                                                             votes129,
                                                             votes130,
                                                             votes131,
                                                             votes132,
                                                             votes133,
                                                             votes134,
                                                             votes135,
                                                             votes136,
                                                             votes137,
                                                             votes138,
                                                             votes139,
                                                             votes140,
                                                             votes141,
                                                             votes142,
                                                             votes143,
                                                             votes144,
                                                             votes145,
                                                             votes146,
                                                             votes147,
                                                             votes148))

write.csv(all_votes, file="../data/votes/votes_121-148.csv", fileEncoding="UTF-8")

all_votes %>%
  filter(vote != "f. 'óþekktur kóði") %>% 
  ggplot() +
  geom_bar(mapping = aes(x=vote_time_hour, fill=vote),
           position="fill") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  labs(
    title = "How Members Vote Throughout the Day",
    subtitle="Data for session 148.",
    y = "",
    x = "Time of Day",
    fill = "Vote"
  ) +
  scale_fill_manual(values = c('#999999',
                               '#992222',
                               '#666666',
                               '#F0E442',
                               '#009E73',
                               '#D55E00'))

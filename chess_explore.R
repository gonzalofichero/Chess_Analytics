# Loading packages
library(tidyverse)
library(data.table)
library(R.utils)
library(lubridate)


# Try BigChess package to read and transform png to data.frame in R
library(bigchess)

# Loading lichess data base
#chess <- read.pgn(fread("lichess_db_standard_rated_2016-09.pgn.bz2", quote="", fill = TRUE), add.tags = c("WhiteElo", "BlackElo"))

denes <- read.pgn("lichess_denes7_2020-11-24.pgn", add.tags = c("WhiteElo", "BlackElo"))

glimpse(denes)

# Quick plot on ELO through time
denes <- denes %>% 
          mutate(my_elo = case_when(White == "denes7" ~ WhiteElo,
                                    TRUE ~ BlackElo),
                 type_game = as.factor(Event),
                 date_v2 = ymd(Date),
                 week_n = isoweek(date_v2),
                 diff_elo = case_when(White == "denes7" ~ WhiteElo - BlackElo,
                                      TRUE ~ BlackElo - WhiteElo),
                 won = case_when( Result == "1/2-1/2" ~ "Tie",
                                  White == "denes7" & substr(Result,1,1) == "1" & Result != "1/2-1/2" ~ "Won",
                                  Black == "denes7" & substr(Result,3,3) == "1" & Result != "1/2-1/2"~ "Won",
                                  TRUE ~ "Loss"))

# ELO through time
denes %>% 
  filter(type_game %in% c("Rated Rapid game", "Rated Blitz game")) %>% 
  ggplot(aes(x=date_v2, y=my_elo, color = type_game)) + geom_line()


# ELO through time
denes %>% 
  filter(type_game %in% c("Rated Rapid game", "Rated Blitz game")) %>% 
  ggplot(aes(x=date_v2, y=my_elo, color = type_game, size=diff_elo)) + geom_point(alpha=0.7) +
  facet_grid(~ won)


# Diff ELO by Result and Type of Game
denes %>% 
  filter(type_game %in% c("Rated Rapid game", "Rated Blitz game"), won != "Tie") %>% 
  ggplot(aes(x=diff_elo, color = type_game)) + geom_density() +
  facet_grid(~ won)


# Games per Week
denes %>% 
  filter(type_game %in% c("Rated Rapid game", "Rated Blitz game")) %>% 
  group_by(week_n, type_game) %>% 
  summarize(games = n()) %>% 
  ggplot(aes(x=week_n, y = games, color = type_game)) + geom_line()





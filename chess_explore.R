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
                 date_v2 = ymd(Date))


denes %>% 
  filter(type_game %in% c("Rated Rapid game", "Rated Blitz game")) %>% 
  ggplot(aes(x=date_v2, y=my_elo, color = type_game)) + geom_line()




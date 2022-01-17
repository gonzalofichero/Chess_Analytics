library(tidyverse)
library(bigchess)
library(chess)
library(lubridate)
library(rchess)



# Import own pgn file from Lichess (up to 20220117)
denes <- read.pgn("lichess_denes7_2022-01-17.pgn",
                  add.tags = c("WhiteElo", "BlackElo", "ECO", "Opening", "Termination"))

glimpse(denes)



# Most used Opening with White
denes %>% 
  filter(White == "denes7") %>% 
  browse_eco_opening() %>% 
  filter(N >= 10)



# Move History in nice format for plotting movement of pieces
library(furrr)
plan(multisession)
set.seed(42)

denes <- denes %>%
  mutate(game_id = row_number()) %>% 
  rename(pgn = Movetext)

test <- denes %>% filter(game_id <= 150, ECO != "?")


dfmoves <- test %>% 
  select(game_id, pgn) %>% 
  mutate(
    data = map(pgn, function(p) {
      chss <- Chess$new()
      chss$load_pgn(p)
      chss$history_detail()
    })
  ) %>% select(-pgn) %>% 
  unnest()


# Taking the board directly from rchess
dfboard <- rchess:::.chessboarddata() %>%
  select(cell, col, row, x, y, cc)


dfpaths <- dfmoves %>%
  left_join(
    dfboard %>% rename(from = cell, x.from = x, y.from = y),
    by = "from"
  ) %>%
  left_join(
    dfboard %>% rename(to = cell, x.to = x, y.to = y) %>% select(-cc, -col, -row),
    by = "to"
  ) %>%
  mutate(
    x_gt_y = abs(x.to - x.from) > abs(y.to - y.from),
    xy_sign = sign((x.to - x.from)*(y.to - y.from)) == 1,
    x_gt_y_equal_xy_sign = x_gt_y == xy_sign)


# f1 Bishop movement

pieces <- "White Queen"

dfpaths_piece <- dfpaths %>% 
  filter(piece == pieces)

ggplot() +
  geom_tile(data = dfboard, aes(x, y, fill = cc)) +
  geom_curve(
    data = dfpaths_piece %>% filter(x_gt_y_equal_xy_sign),
    aes(
      x = x.from,
      y = y.from,
      xend = x.to,
      yend = y.to
    ),
    position = position_jitter(width = 0.2, height = 0.2),
    curvature = 0.50,
    angle = -45,
    alpha = 0.02,
    color = "white",
    size = 1.05
  ) +
  geom_curve(
    data = dfpaths_piece %>% filter(!x_gt_y_equal_xy_sign),
    aes(
      x = x.from,
      y = y.from,
      xend = x.to,
      yend = y.to
    ),
    position = position_jitter(width = 0.2, height = 0.2),
    curvature = -0.50,
    angle = 45,
    alpha = 0.02,
    color = "white",
    size = 1.05
  ) +
  scale_fill_manual(values =  c("gray10", "gray20")) +
  ggtitle("White Queen") +
  
  coord_equal()




# setwd
setwd("C:/Users/Cooper/Desktop/STAT479/big_data_bowl")
library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(gganimate)
library(sportyR) 

animate_play <- function(week, game_id_arg, play_id_arg, nfl_id_arg = "NA", home_color = "red", visitor_color = "blue") {
  
  # ---- 1. Load Tracking Data ----
  input_file = sprintf("data/input_edited_w%02d.csv", week)
  input_file_OG = sprintf("data/input_2023_w%02d.csv", week)
  output_file = sprintf("data/output_2023_w%02d.csv", week)
    # paste("data/output_2023_w", week, ".csv", sep = "")
  input_data = read_csv(input_file, show_col_types = FALSE)
  input_data_OG = read_csv(input_file_OG, show_col_types = FALSE)
  output_data = read_csv(output_file, show_col_types = FALSE)
  
  input_edited = input_data %>%
    mutate(player_to_predict = as.logical(player_to_predict))
  
  inputweek1OG = input_data_OG %>%
    mutate(player_to_predict = as.logical(player_to_predict))
  
  input_total = bind_rows(input_edited, inputweek1OG)
  
  play_input = input_total %>%
    # Ensure game_id and play_id are character for comparison
    mutate(
      game_id = as.character(game_id),
      play_id = as.character(play_id)
    ) %>%
    dplyr::filter(game_id == as.character(game_id_arg),
           play_id == as.character(play_id_arg)) %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      frame_id = as.numeric(frame_id)
    ) %>%
    dplyr::filter(!is.na(x), !is.na(y))
  
  
  play_output_raw = output_data %>%
    mutate(
      game_id = as.character(game_id),
      play_id = as.character(play_id)
    ) %>%
    dplyr::filter(game_id == as.character(game_id_arg),
           play_id == as.character(play_id_arg)) %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      frame_id = as.numeric(frame_id)
    ) %>%
    dplyr::filter(!is.na(x), !is.na(y))
  
  
  # Aggregation
  # Last frame_id
  F_max = play_input %>%  
    summarise(max_frame = max(frame_id, na.rm = TRUE)) %>%  
    pull(max_frame)
  
  # Shift frame_id for output data
  play_output = play_output_raw %>%
    mutate(frame_id = frame_id + F_max)
  
  # Select cols
  common_cols = c("game_id", "play_id", "nfl_id", "frame_id", "x", "y", "nfl_id")
  
  play_input_subset = play_input %>% select(all_of(common_cols), player_side, is_cp)
  play_output_subset = play_output %>% select(all_of(common_cols))
  
  # Combine data into one timeline
  # play_combined = bind_rows(play_input_subset, play_output_subset)
  
  play_combined <- bind_rows(play_input_subset, play_output_subset) %>%
    arrange(nfl_id, frame_id) %>%
    group_by(nfl_id) %>%
    mutate(
      is_cp = coalesce(is_cp, FALSE),
      has_cp = cumsum(is_cp) > 0
    ) %>%
    tidyr::fill(player_side, .direction = "down") %>%
    ungroup()
  
  
  play = play_combined %>%
    mutate(
      base_color = ifelse(player_side == "Defense", visitor_color, home_color),
      color = dplyr::case_when(
        nfl_id == nfl_id_arg & !has_cp ~ "yellow",
        nfl_id == nfl_id_arg & has_cp  ~ "hotpink",
        has_cp                          ~ "green",
        TRUE                            ~ base_color
      )
    )
  
  # Ball position
  ball_land = play_input %>%
    dplyr::filter(!is.na(ball_land_x), !is.na(ball_land_y)) %>%
    distinct(ball_land_x, ball_land_y)
  

  anim <- ggplot() +
    annotate("rect", xmin=0, xmax=120, ymin=0, ymax=53.3,
             fill="transparent", color="black") +
    geom_path(
      data = play,
      aes(x = x, y = y, group = nfl_id, color = color),
      alpha = 0.3, 
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = play,
      aes(x = x, y = y, group = nfl_id, color = color),
      size = 4,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = ball_land,
      aes(x = ball_land_x, y = ball_land_y),
      color = "black",
      shape = 4, 
      size = 6,
      stroke = 2,
      inherit.aes = FALSE
    ) +
    scale_color_identity() +
    labs(
      title = paste("Tracking Data: Week", week, "- Game", game_id_arg, "- Play", play_id_arg)
    ) +
    transition_time(frame_id) +
    ease_aes("linear") +
    coord_fixed()
  
  return(anim)
}



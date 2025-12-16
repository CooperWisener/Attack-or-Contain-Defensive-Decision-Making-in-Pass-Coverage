setwd("C:/Users/Cooper/Desktop/STAT479/big_data_bowl")
library(tidyverse)
library(signal)
library(changepoint.np)
library(nflfastR)
library(caret)
library(scales)
library(ranger)
source("helper_functions.R") 


#Read in 
supp = read.csv("C:/Users/Cooper/Desktop/STAT479/project3/data/supplementary_data.csv")
temp_list = vector("list", 18)
input_files = vector("list", 18)

#Change to what weeks you want to load, in our case weeks 1 through 18.  
for(i in 1:18) {
  temp_list[[i]] = process_week(i, supp)
  input_files[[i]] = add_time_for_action(temp_list[[i]])
}

#Here we use a function from our helper file to determine if a PBU (pass break up) occurred in a given play
supp_pbu = add_pbu(supp)

temps_files2 = vector("list", 18)
final_files = vector("list", 18)
for(i in 1:18) {
  temp = input_files[[i]] %>%
    group_by(game_id, play_id, nfl_id) %>%
    summarise(
      player_name, player_to_predict, player_position, cp_index, time_for_action, dist_to_ball_at_cp, vel_to_ball_at_cp, .groups = "drop") %>%
    ungroup() %>%
    distinct(game_id, play_id, nfl_id, .keep_all = TRUE)
  temps_files2[[i]] = temp %>%
    left_join(supp_pbu, by = c("game_id", "play_id"))
  
  #This line uses a function from the helper file to determine which player in a given play had a PBU and correctly assigns the PBU to the correct player. 
  final_files[[i]] = correct_pbus(temps_files2[[i]])
  
  #Add binary variables for INT, Incomplete, and Completion
  final_files[[i]] = final_files[[i]] %>%
    mutate(
      # pass_result = ifelse(pass_result == "I" & pbu, "pbu", pass_result),
      pass_result = factor(pass_result, levels = c("C", "I", "IN")) #Add pbu 
    )
}

set.seed(479)
data = dplyr::bind_rows(final_files)

train_idx = sample(seq_len(nrow(data)), size = 0.8 * nrow(data))

train_data = data[train_idx, ]
test_data  = data[-train_idx, ]



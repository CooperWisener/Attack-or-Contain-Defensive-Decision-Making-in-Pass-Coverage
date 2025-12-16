library(dplyr)
library(circular)
library(changepoint.np)
library(signal)
library(nflfastR)

#To ensure your using this right, make a folder and set this folder as your working directory. 
#Make sure in the wd there is a folder called data with all of the input files contained in it. 

#here pass in a week (just the number ex: 1 or 12. No need to load the input files by yourself), and the supp file. 
#This function is used to process a given week in the 2023 NFL season. It will add variables that will be important when training our random forest model. 
process_week = function(week, supp) {
  
  #Read in raw data
  input_week_file = sprintf("data/input_2023_w%02d.csv", week)
  
  input_week = read_csv(input_week_file, show_col_types = FALSE)
  
  #Grab all plays where Zone Coverage is ran and a specific group of routes is ran. Also filter out scrambles and plays negated by penalty.  
  # routes = c("IN", "OUT", "CROSSER", "HITCH", "FLAT", "SLANT", "ANGLE")
  coverages = c("ZONE_COVERAGE")
  
  get_plays <- function(df, routes, coverages) {
    df %>%
      # dplyr::filter(route_of_targeted_receiver %in% routes, team_coverage_man_zone %in% coverages) %>%
      dplyr::filter(team_coverage_man_zone %in% coverages) %>%
      dplyr::filter(
        pass_result != "R" | pass_result != "S",
        play_nullified_by_penalty == "N") %>% 
      distinct(game_id, play_id) 
  }
  
  plays = get_plays(supp, routes, coverages)
  
  input_plays_week = input_week %>%
    dplyr::semi_join(plays, by = c("game_id", "play_id")) %>%
    #here liese the issue
    dplyr::filter(player_side == "Defense")
  
  
  #Create the change metric (a measure of speed to the ball and angle to the ball)
  angle_to_ball = function(player_x, player_y, ball_x, ball_y){
    dx = ball_x - player_x
    dy = ball_y - player_y
    
    # angle in radians from -pi to pi
    theta = atan2(dy, dx)
    
    # convert to degrees
    theta_deg = theta * 180 / pi
    
    #ensure degrees is positive
    theta_deg = (theta_deg + 360) %% 360
    
    return(theta_deg)
  }
  
  distance_to_ball = function(player_x, player_y, ball_x, ball_y){
    return(sqrt((player_x - ball_x)^2 + (player_y - ball_y)^2))
  }
  
  #Here we implement our change point detection system.
  detect_cp_distance_angle = function(df) {
    
    df = df %>%
      mutate(
        dist_to_ball = distance_to_ball(x, y, ball_land_x, ball_land_y),
        angle_to_ball = angle_to_ball(x, y, ball_land_x, ball_land_y),
        angle_unwrapped = signal::unwrap(angle_to_ball * pi/180) * 180/pi,
        #angular velocity
        ang_vel = c(NA, diff(angle_unwrapped)),
        #distance velocity
        dist_vel = c(NA, diff(dist_to_ball)),
        #acceleartion
        accel_to_ball = c(NA, diff(dist_vel)),
        #change metric
        change_metric = sqrt(ang_vel^2 + dist_vel^2 + accel_to_ball^2)
      )
    
    change_metric_clean = df$change_metric[!is.na(df$change_metric)]
  
    
    if (length(change_metric_clean) > 1) {
      
      # Smooth the change metric
      change_metric_s = zoo::rollmean(change_metric_clean, k = 3, fill = NA, align = "right")
      
      # Baseline: first few frames
      baseline_window = 10
      k = min(baseline_window, sum(!is.na(change_metric_s)))
      baseline_mu = median(change_metric_s[1:k], na.rm = TRUE)
      baseline_sd = sd(change_metric_s[1:k], na.rm = TRUE)
      
      # Threshold for reaction
      thresh = baseline_mu + 3 * baseline_sd
      
      # First frame exceeding threshold
      reaction_idx = which(
        change_metric_s > thresh &
          dplyr::lag(change_metric_s, 1) <= thresh
      )[1]
      
      df = df %>%
        mutate(is_cp = if (!is.na(reaction_idx)) row_number() == reaction_idx else FALSE)
      
    } else {
      df$is_cp = FALSE
    }
    
    return(df)
  }
  
  
  # Apply function by game_id, play_id, and player_name
  input_plays_week = input_plays_week %>%
    group_by(game_id, play_id, player_name) %>%
    group_modify(~detect_cp_distance_angle(.x)) %>%
    ungroup()
  
  
  add_dist_to_ball_at_cp <- function(df) {
    df %>%
      group_by(game_id, play_id, nfl_id) %>%
      # compute CP index per group
      mutate(cp_index = which(is_cp)[1]) %>%
      # compute distance/velocity at CP, fallback to last value if no CP
      mutate(
        dist_to_ball_at_cp = ifelse(
          !is.na(cp_index),
          dist_to_ball[cp_index],
          dist_to_ball[length(dist_to_ball)]
        ),
        vel_to_ball_at_cp = ifelse(
          !is.na(cp_index),
          dist_vel[cp_index],
          dist_vel[length(dist_vel)]
        )
      ) %>%
      ungroup()
  }
  
  input_plays_week = add_dist_to_ball_at_cp(input_plays_week) 
  
  return(input_plays_week)
}

#This function adds the "time for action" variable
#This variable is the time from initial reaction determine by the change point to the end of the play
#The higher the number the more time the player has to react before the ball lands. 
add_time_for_action = function(input){
  
  react_bonus = input %>%
    group_by(game_id, play_id, nfl_id) %>%
    mutate(
      cp_index = ifelse(any(is_cp), which(is_cp), NA_integer_),
      react_bonus = case_when(
        is.na(cp_index) ~ NA_integer_,  # no TRUE in this group
        TRUE ~ sum(!is_cp & row_number() > cp_index)
      )
    ) %>%
    ungroup() %>%
    mutate(
      time_for_action = ifelse(is.na(react_bonus), num_frames_output*.1, (num_frames_output*.1 + react_bonus*.1))
    )
  
  return(react_bonus)
}

#This function adds pass break ups to the support file.
#Uses nflfastR to determine what plays have a PBU and marks the corresponding play in the supp file.
add_pbu = function(supp){
  
  #load in pbp data from 2023
  nfl2023 = load_pbp(2023)
  
  #add in pbu to nflfastR data
  pbus = nfl2023 %>%
    select(old_game_id, play_id, pass_defense_1_player_name, pass_defense_2_player_name) %>%
    mutate(
      pbu = ifelse(
        (!is.na(pass_defense_1_player_name) & pass_defense_1_player_name != "") |
          (!is.na(pass_defense_2_player_name) & pass_defense_2_player_name != ""),
        TRUE, FALSE)
    ) %>%
    mutate(game_id = as.numeric(old_game_id))
  
  #join supplementary file with nflfastr data with pbus, effectively adding pbus to the supp file
  supp_pbu = supp %>%
    left_join(pbus, by = c("game_id", "play_id"))
  
  return(supp_pbu)
}

#Corrects issues in the PBU creations
#Specifically ensures only one player gets credit for a PBU in a given play.
correct_pbus = function(df) {
  
  normalize_name = function(name_vec) {
    vapply(name_vec, FUN.VALUE = character(1), FUN = function(name) {
      if (is.na(name) || trimws(name) == "") return(NA_character_)
      parts = unlist(strsplit(trimws(name), "\\s+"))
      first_init = substr(parts[1], 1, 1)
      last = parts[length(parts)]
      last_clean = gsub("[^A-Za-z'-]", "", last)
      paste0(first_init, ".", last_clean)
    })
  }
  
  # ensure whitespace is removed in the defensive name
  df = df %>%
    mutate(
      pbu = if_else(
        pbu == TRUE &
          normalize_name(player_name) != gsub("\\s+", "", pass_defense_1_player_name),
        FALSE,
        pbu
      )
    )
  
  return(df)
}



#Visualization helpers:

make_reaction_scenarios <- function(df, game_id_arg, play_id_arg, nfl_id_arg, time_for_action, 
                                    deltas = seq(-0.4, 0.4, by = 0.05)) {
  base_row = df %>%
    dplyr::filter(game_id == game_id_arg, 
                  play_id == play_id_arg,
                  nfl_id == nfl_id_arg,
                  player_to_predict) %>%
    dplyr::slice(1)
  
  if (nrow(base_row) == 0) {
    stop("No matching defender found for that game_id/ play_id/ nfl_id.")
  }
  
  base_rt = time_for_action
  
  scenario_table = tibble::tibble(delta = deltas) %>%
    dplyr::mutate(
      time_for_action = pmax(0.1, base_rt + delta)
    )
  
  
  scenario_table = scenario_table %>%
    dplyr::mutate(
      dist_to_ball_at_cp = base_row$dist_to_ball_at_cp,
      vel_to_ball_at_cp = base_row$vel_to_ball_at_cp,
      player_position = base_row$player_position,
      player_name = base_row$player_name,
      pass_length = base_row$pass_length,
      route_of_targeted_receiver = base_row$route_of_targeted_receiver,
      play_action = base_row$play_action,
      pass_location_type = base_row$pass_location_type,
      base_rt
    )
  
  scenario_table  
}



plot_what_if_for_case <- function(case_row, title_prefix = "") {
  
  g = case_row$game_id
  p = case_row$play_id
  id = case_row$nfl_id
  nm = case_row$player_name
  re = case_row$time_for_action
  
  
  scenarios = make_reaction_scenarios(
    df = test_data,
    game_id = g,
    play_id = p,
    nfl_id = id,
    time_for_action = re,
    deltas = seq(-1, 1, by = 0.2)
  )
  
  
  scenario_x = scenarios %>%
    dplyr::select(
      time_for_action,
      dist_to_ball_at_cp,
      vel_to_ball_at_cp,
      player_position,
      play_action,
      player_name,
      pass_length,
      route_of_targeted_receiver,
      pass_location_type,
      base_rt
    )
  
  feature_cols = c("player_name", "player_position", "time_for_action", "pass_length", 
                   "route_of_targeted_receiver", "play_action", "dist_to_ball_at_cp", 
                   "vel_to_ball_at_cp", "pass_location_type") 
  
  test_matrix = xgb.DMatrix(
    data = as.matrix(sapply(
      scenario_x[, feature_cols],
      function(x) if (is.factor(x) || is.character(x)) as.numeric(factor(x)) else x
    ))
  )

  
  pred_probs = predict(xgb_model, test_matrix)
  pred_matrix = matrix(pred_probs, ncol = num_classes, byrow = TRUE)
  pred_labels = max.col(pred_matrix) - 1
  class_levels = levels(factor(train_data$pass_result))
  pred_class_names = class_levels[pred_labels + 1]  # add 1 because R indices start at 1
  scenarios$predicted_class = pred_class_names
  
  interception_col = which(class_levels == "IN")
  scenarios$interception_prob = pred_matrix[, interception_col]
  
  
  scenarios_plot = scenarios %>%
    mutate(
      play_on_ball = ifelse(predicted_class == "C" | predicted_class == "I", FALSE, TRUE),
      play_on_ball_prob = interception_prob
    )
  
  write.csv(scenarios_plot, "data/case_close_fixed.csv")
  
  ggplot2::ggplot(scenarios_plot,
                  ggplot2::aes(x = time_for_action, y = play_on_ball_prob)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    geom_vline(xintercept = case_row$time_for_action, linetype = "dashed") + 
    ggplot2::labs(
      title = paste0(title_prefix, "What-if Reaction Time: ", nm),
      x = "Time For Action (sec)",
      y = "Predicted Probability of Attacking"
    )
}
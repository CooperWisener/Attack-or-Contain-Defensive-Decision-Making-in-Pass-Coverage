library(xgboost)
library(dplyr)



feature_cols = c("player_name", "player_position", "time_for_action", "pass_length", "route_of_targeted_receiver", "play_action", "dist_to_ball_at_cp", "vel_to_ball_at_cp", "pass_location_type") 

train_matrix = xgb.DMatrix(
  data = as.matrix(sapply(train_data[, feature_cols], function(x) {
    if(is.factor(x) || is.character(x)) as.numeric(factor(x)) else x
  })),
  label = as.numeric(factor(train_data$pass_result)) - 1  # , labels start at 0 weight = train_data$weight
)

test_matrix = xgb.DMatrix(
  data = as.matrix(sapply(test_data[, feature_cols], function(x) {
    if(is.factor(x) || is.character(x)) as.numeric(factor(x)) else x
  })),
  label = as.numeric(factor(test_data$pass_result)) - 1
)

num_classes = 3

#Define parameters for multiclass XGBoost
params = list(
  objective = "multi:softprob", 
  eval_metric = "mlogloss",
  num_class = num_classes,
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model = xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 200,
  watchlist = list(train = train_matrix, test = test_matrix),
  early_stopping_rounds = 20,
  print_every_n = 10
)

pred_probs = predict(xgb_model, test_matrix)
pred_matrix = matrix(pred_probs, ncol = num_classes, byrow = TRUE)
pred_labels = max.col(pred_matrix) - 1

# Map numeric labels back to original class names
class_levels = levels(factor(train_data$pass_result))
pred_class_names = class_levels[pred_labels + 1]  # add 1 because R indices start at 1

# Add predicted class column to training and testing dataframes
train_pred_probs = predict(xgb_model, train_matrix)
train_pred_matrix = matrix(train_pred_probs, ncol = num_classes, byrow = TRUE)
train_pred_labels = max.col(train_pred_matrix) - 1
train_pred_class_names = class_levels[train_pred_labels + 1]

train_data$predicted_class = train_pred_class_names
test_data$predicted_class = pred_class_names

conf_mat = confusionMatrix(
  factor(pred_labels, levels = 0:(num_classes-1)),
  factor(as.numeric(factor(test_data$pass_result)) - 1)
)

train_accuracy = mean(train_data$predicted_class == train_data$pass_result)
test_accuracy  = mean(test_data$predicted_class  == test_data$pass_result)


accuracy = data.frame(
  Metric = c("Train Accuracy", "Test Accuracy"),
  Accuracy = round(c(train_accuracy, test_accuracy), 5)
)

cm <- conf_mat$table

per_class_accuracy <- diag(cm) / rowSums(cm)

f1_by_class = conf_mat$byClass[, "F1"]      
macro_f1 = mean(f1_by_class, na.rm = TRUE)  

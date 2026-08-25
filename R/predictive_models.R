# R/predictive_models.R
# CricLens — Predictive Analytics Engine
# Machine learning model for 2nd innings match-state win probability prediction

suppressMessages({
  library(dplyr)
  library(stats)
})

MODEL_CACHE_PATH <- "data/processed/win_prob_model.rds"

#' Calculate Area Under ROC Curve (ROC-AUC) using Exact Trapezoidal Integration
calculate_roc_auc <- function(actual, predicted) {
  ord <- order(predicted, decreasing = TRUE)
  actual <- actual[ord]
  predicted <- predicted[ord]

  n_pos <- sum(actual == 1)
  n_neg <- sum(actual == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)

  tp_cumsum <- cumsum(actual == 1)
  fp_cumsum <- cumsum(actual == 0)

  auc <- sum((fp_cumsum[-1] - fp_cumsum[-length(fp_cumsum)]) * (tp_cumsum[-1] + tp_cumsum[-length(tp_cumsum)]) / 2) / (n_pos * n_neg)
  return(round(auc, 4))
}

#' Train & Validate Win Probability Logistic Regression Model using Chronological Train/Test Split
#' @param df Featured IPL dataframe
#' @param train_end_year Year marking end of training period (default 2023)
#' @return List containing trained model object and test evaluation metrics
train_and_validate_win_prob_model <- function(df, train_end_year = 2023) {
  cat("Preparing delivery snapshots for Win Probability Model...\n")

  chase_df <- df %>%
    filter(innings == 2 & !is.na(runs_target) & valid_ball == 1) %>%
    mutate(
      is_chase_winner = as.integer(batting_team == match_won_by),
      balls_remaining = pmax(1, 120 - cum_valid_balls),
      wickets_remaining = pmax(0, 10 - cum_team_wickets),
      runs_required = pmax(0, runs_target - cum_team_runs),
      req_rr = ifelse(balls_remaining > 0, runs_required / (balls_remaining / 6), 30),
      current_rr = ifelse(cum_valid_balls > 0, cum_team_runs / (cum_valid_balls / 6), 0)
    ) %>%
    select(season_year, match_id, is_chase_winner, runs_required, balls_remaining, wickets_remaining, req_rr, current_rr) %>%
    filter(!is.na(is_chase_winner) & !is.na(req_rr) & !is.na(current_rr))

  # Chronological Train / Test Split
  train_df <- chase_df %>% filter(season_year <= train_end_year)
  test_df  <- chase_df %>% filter(season_year > train_end_year)

  cat("Chronological Split Summary:\n")
  cat("  - Training Set (Seasons <= ", train_end_year, "): ", nrow(train_df), " deliveries\n", sep = "")
  cat("    Class Dist: ", sum(train_df$is_chase_winner == 1), " Chase Wins / ", sum(train_df$is_chase_winner == 0), " Defend Wins\n", sep = "")
  cat("  - Testing Set  (Seasons > ", train_end_year, "): ", nrow(test_df), " deliveries\n", sep = "")
  cat("    Class Dist: ", sum(test_df$is_chase_winner == 1), " Chase Wins / ", sum(test_df$is_chase_winner == 0), " Defend Wins\n", sep = "")

  # Fit Logistic Regression Model on Training Set ONLY
  cat("\nFitting Logistic Regression baseline on Training Set...\n")
  win_model <- glm(
    is_chase_winner ~ runs_required + balls_remaining + wickets_remaining + req_rr + current_rr,
    data = train_df,
    family = binomial(link = "logit")
  )

  # Evaluate Model strictly on UNSEEN Test Set
  cat("Evaluating model on unseen Test Set...\n")
  test_pred_prob <- predict(win_model, newdata = test_df, type = "response")
  test_pred_class <- ifelse(test_pred_prob >= 0.5, 1, 0)

  # Compute Metrics on Test Set
  test_acc <- round(mean(test_pred_class == test_df$is_chase_winner) * 100, 2)
  brier_score <- round(mean((test_pred_prob - test_df$is_chase_winner)^2), 4)
  log_loss <- round(-mean(test_df$is_chase_winner * log(pmax(1e-15, test_pred_prob)) + (1 - test_df$is_chase_winner) * log(pmax(1e-15, 1 - test_pred_prob))), 4)
  roc_auc <- calculate_roc_auc(test_df$is_chase_winner, test_pred_prob)

  cat("-----------------------------------------------\n")
  cat("  TEST SET EVALUATION RESULTS (Seasons 2024-2026)\n")
  cat("-----------------------------------------------\n")
  cat("  Test Accuracy: ", test_acc, "%\n", sep = "")
  cat("  ROC-AUC:       ", roc_auc, "\n", sep = "")
  cat("  Brier Score:   ", brier_score, "\n", sep = "")
  cat("  Log Loss:      ", log_loss, "\n", sep = "")
  cat("-----------------------------------------------\n\n")

  result_list <- list(
    model = win_model,
    train_count = nrow(train_df),
    test_count = nrow(test_df),
    train_class_dist = table(train_df$is_chase_winner),
    test_class_dist = table(test_df$is_chase_winner),
    test_accuracy = test_acc,
    roc_auc = roc_auc,
    brier_score = brier_score,
    log_loss = log_loss
  )

  return(result_list)
}

#' Load or Train Cached Win Probability Model
get_win_prob_model <- function(df = NULL) {
  if (file.exists(MODEL_CACHE_PATH)) {
    model_obj <- readRDS(MODEL_CACHE_PATH)
    if (is.list(model_obj) && "model" %in% names(model_obj)) {
      return(model_obj$model)
    }
    return(model_obj)
  } else if (!is.null(df)) {
    res <- train_and_validate_win_prob_model(df)
    saveRDS(res, MODEL_CACHE_PATH)
    return(res$model)
  } else {
    stop("Model file not found and no dataset provided to train.")
  }
}

#' Predict Match-State Win Probability for Chasing Team
predict_live_win_prob <- function(target, current_runs, overs_bowled, wickets_down, model = NULL) {
  if (is.null(model)) {
    model <- get_win_prob_model()
  }

  balls_bowled <- min(120, floor(overs_bowled) * 6 + round((overs_bowled %% 1) * 10))
  balls_remaining <- max(1, 120 - balls_bowled)
  wickets_remaining <- max(0, 10 - wickets_down)
  runs_required <- max(0, target - current_runs)
  req_rr <- ifelse(balls_remaining > 0, runs_required / (balls_remaining / 6), 30)
  current_rr <- ifelse(balls_bowled > 0, current_runs / (balls_bowled / 6), 0)

  new_obs <- data.frame(
    runs_required = runs_required,
    balls_remaining = balls_remaining,
    wickets_remaining = wickets_remaining,
    req_rr = req_rr,
    current_rr = current_rr
  )

  log_odds <- predict(model, newdata = new_obs, type = "response")
  chase_win_prob <- round(as.numeric(log_odds) * 100, 1)
  chase_win_prob <- pmax(1, pmin(99, chase_win_prob))

  defend_win_prob <- round(100 - chase_win_prob, 1)

  return(list(
    chase_win_prob = chase_win_prob,
    defend_win_prob = defend_win_prob,
    runs_required = runs_required,
    balls_remaining = balls_remaining,
    req_rr = round(req_rr, 2),
    current_rr = round(current_rr, 2)
  ))
}

# Run training & validation if executed directly
if (sys.nframe() == 0) {
  df <- readRDS("data/processed/ipl_featured.rds")
  res <- train_and_validate_win_prob_model(df)
  saveRDS(res, MODEL_CACHE_PATH)
  cat("Saved validated win probability model & metadata to:", MODEL_CACHE_PATH, "\n")
}
